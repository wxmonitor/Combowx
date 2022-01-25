library(tidyverse)
library(shiny)
library(jsonlite)
library(ggtext)
library(data.table)
library(lubridate)
library(scales)

###### Combowx monitor full #########


# Force local time zone
Sys.setenv(TZ="America/Los_Angeles")

# Wind rose function to convert wind direction degrees to compass points
wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}


# Disco Bay chunk - runs first to get sunrise/sunset data

# Call API and decode JSON
url <- "https://api.openweathermap.org/data/2.5/onecall?lat=47.9936&lon=-122.88248&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9"
d.weather.page <- fromJSON(url, flatten = TRUE)

# Strip and format hourly data
hourly.forecast <- data.frame(d.weather.page$hourly) %>%
  mutate(dt = as.POSIXct(dt, origin="1970-01-01")) %>%
  mutate_at(vars(wind_speed, wind_gust), ~ . * 0.868976)

# Strip and format current data
current <- data.frame(d.weather.page$current) %>%
  mutate_at(vars(dt, sunrise, sunset), ~ as.POSIXct(., origin="1970-01-01")) %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_deg = as.integer(wind_deg)) %>%
  mutate(mod_deg = case_when(wind_deg > 352 && wind_deg < 356 ~ 352L,
                             wind_deg >= 356 && wind_deg <= 360 ~ 0L,
                             TRUE ~ wind_deg))

# Create night-time shade limits
d.shade <- data.frame(dusk = seq.POSIXt(current$sunset, by = 'day', length.out = 3), 
                    dawn = seq.POSIXt(current$sunrise+86400, by = 'day', length.out = 3),
                    top = Inf,
                    bottom = -Inf)

d.shade <- d.shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(hourly.forecast$dt, 1)), tail(hourly.forecast$dt, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(hourly.forecast$dt, 1)), head(hourly.forecast$dt, 1)))

# Wind rose plot
d.rose <- ggplot(current, aes(x = mod_deg)) +
  coord_polar(theta = "x", start = -pi/45, direction = 1) +
  geom_bar(width = 7, color = "gray10", fill = "red") +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

# Wind direction plot
d.dir.plot <- ggplot() +
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_point(data = hourly.forecast, aes(x = dt, y = wind.rose(wind_deg)), size = 1) +
  theme_bw() +
  labs(title = "**Wind Direction**") +
  theme(plot.title = element_markdown()) +
  ylab("") +
  xlab("") +
  scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

# Barometer plot
d.bar.plot <- ggplot() + 
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = pressure), size = 1) +
  geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
  theme_bw() +
  labs(title = "**Barometric Pressure**") +
  theme(plot.title = element_markdown()) +
  ylab("Millibars") +
  xlab("") +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

# Wind plot
d.weather.plot <- ggplot() +
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = wind_speed), size = 1) +
  geom_line(data = hourly.forecast, aes(x = dt, y = wind_gust), color = "#FF0000") +
  theme_bw() +
  labs(
    title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
  theme(plot.title = element_markdown()) +
  ylab("Knots") +
  xlab("") + 
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

# Rain plot
if ("rain.1h" %in% colnames(hourly.forecast)) {

d.rain.plot <- ggplot() +
  geom_rect(data = d.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
  geom_col(data = hourly.forecast, aes(x = dt, y = rain.1h/5), color = "darkgrey", fill = "#28d0eb") +
  geom_text(data = hourly.forecast, aes(x = dt, y = rain.1h/5, label = rain.1h), size = 2, vjust = -0.5) +
  theme_bw() +
  labs(
    title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
  theme(plot.title = element_markdown()) +
  ylab("Percent") + 
  xlab("") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_cartesian(ylim = c(0,1)) +
  scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))

} else (
  d.rain.plot <- ggplot() +
    geom_rect(data = d.shade, 
              aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
              fill = 'light grey', alpha = 0.5) +
    geom_line(data = hourly.forecast, aes(x = dt, y = pop), size = 1) +
    theme_bw() +
    labs(
      title = "**Chance of Rain** and <span style='color:#28d0eb;'>**Accumulation**</span></span> (mm)") +
    theme(plot.title = element_markdown()) +
    ylab("Percent") + 
    xlab("") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    coord_cartesian(ylim = c(0,1)) +
    scale_x_datetime(limits = c(min(hourly.forecast$dt), max(hourly.forecast$dt)), expand = c(0, 0))
)

# Ediz Hook chunk

# Call API and decode JSON
url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=KNOW&obtimezone=local&recent=1440&vars=wind_speed,wind_gust,wind_direction,sea_level_pressure&units=english&token=652e4bd32bbf4621b835895f8c769bb6"
total.page <- fromJSON(url, flatten = TRUE)

# Strip and format data
weather.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.wind_speed_set_1) %>%
  setNames(c("Time", "Wind Speed")) %>%
  mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z"))

dir.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.wind_direction_set_1) %>%
  setNames(c("Time", "Wind Direction")) %>%
  mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z")) %>%
  mutate(`Mod Direction` = case_when(`Wind Direction` > 352 && `Wind Direction` < 356 ~ 352,
                                     `Wind Direction` >= 356 && `Wind Direction` <= 360 ~ 0,
                                     TRUE ~ `Wind Direction`))

pressure.table <- data.frame(pressure.page[["STATION"]]$OBSERVATIONS.date_time, pressure.page[["STATION"]]$OBSERVATIONS.sea_level_pressure_set_1d) %>%
  setNames(c("Time", "Pressure (mb)")) %>%
  mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z"))

# Create night-time shade limits
shade <- data.frame(dusk = seq.POSIXt(current$sunset-172800, by = 'day', length.out = 3), 
                      dawn = seq.POSIXt(current$sunrise-86400, by = 'day', length.out = 3),
                      top = Inf,
                      bottom = -Inf)

# Ediz Hook direction shade limits
e.dir.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(dir.table$Time, 1)), tail(dir.table$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(dir.table$Time, 1)), head(dir.table$Time, 1)))

# Wind direction plot
dir.plot <- ggplot() + 
  geom_rect(data = e.dir.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_point(data = dir.table, aes(x = Time, y = wind.rose(`Wind Direction`)), size = 1) +
  theme_bw() +
  labs(title = "**Wind Direction**") +
  theme(plot.title = element_markdown()) +
  ylab("") +
  xlab("") +
  scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
  scale_x_datetime(limits = c(min(dir.table$Time), max(dir.table$Time)), expand = c(0, 0))

# Ediz Hook barometer shade limits
e.bar.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(pressure.table$Time, 1)), tail(pressure.table$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(pressure.table$Time, 1)), head(pressure.table$Time, 1)))

# Barometer plot
bar.plot <- ggplot() + 
  geom_rect(data = e.bar.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = pressure.table, aes(x = Time, y = `Pressure (mb)`), size = 1) +
  geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
  theme_bw() +
  labs(title = "**Barometric Pressure**") +
  theme(plot.title = element_markdown()) +
  ylab("Millibars") +
  xlab("") +
  scale_x_datetime(limits = c(min(pressure.table$Time), max(pressure.table$Time)), expand = c(0, 0))

# Wind rose
rose <- ggplot(tail(dir.table, 1), aes(x = `Mod Direction`)) +
  coord_polar(theta = "x", start = -pi/45, direction = 1) +
  geom_bar(width = 7, color = "gray10", fill = "red") +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

# Ediz Hook wind shade limits
e.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(weather.table$Time, 1)), tail(weather.table$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(weather.table$Time, 1)), head(weather.table$Time, 1)))

# Catch errors due to missing gust data

a <- try({
  gust.table <- data.frame(total.page[["STATION"]]$OBSERVATIONS.date_time, total.page[["STATION"]]$OBSERVATIONS.wind_gust_set_1) %>%
    setNames(c("Time", "Wind Speed")) %>%
    mutate(Time = as.POSIXct(Time, format = "%Y-%m-%dT%H:%M:%S%z"))
  
  gust.table$Time <- gust.table$Time
  
  # Wind + gust plot
  weather.plot <- ggplot() + 
    geom_rect(data = e.shade, 
              aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
              fill = 'light grey', alpha = 0.5) +
    geom_line(data = weather.table, aes(x = Time, y = `Wind Speed`), color = "black", size = 1) +
    geom_point(data = gust.table, aes(x = Time, y = `Wind Speed`), color = "#FF0000") +
    theme_bw() +
    labs(
      title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
    theme(plot.title = element_markdown()) +
    scale_y_continuous(breaks = seq(0, max(na.omit(gust.table$`Wind Speed`)),5)) +
    scale_x_datetime(limits = c(min(weather.table$Time), max(weather.table$Time)), expand = c(0, 0)) +
    ylab("Knots") +
    xlab("")
})

if (class(a) == "try-error") {
  
  # Wind only plot
  weather.plot <- ggplot() +
    geom_rect(data = e.shade, 
              aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
              fill = 'light grey', alpha = 0.5) +
    geom_line(data = weather.table, aes(x = Time, y = `Wind Speed`), size = 1) +
    theme_bw() +
    labs(
      title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
    theme(plot.title = element_markdown()) +
    scale_y_continuous(breaks = seq(0, max(weather.table$`Wind Speed`),5)) +
    scale_x_datetime(limits = c(min(weather.table$Time), max(weather.table$Time)), expand = c(0, 0)) +
    ylab("Knots") +
    xlab("")
}


# PT Ferry Chunk

# Call API 
weather <- fread("https://www.ndbc.noaa.gov/data/realtime2/PTWW1.txt", 
                 skip = 2,
                 na.strings = "MM",
                 encoding = "UTF-8",
                 colClasses = c(rep("character", 5), rep("numeric", 3), 
                                rep("factor", 4), rep("numeric", 3), rep("factor", 4)),
                 col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", 
                               "Wind.Speed", "Gust", NA, NA, NA, NA, "Pressure", 
                               "Air.Temp", "Water.Temp", NA, NA, NA, NA))

# Subset past 24 hours of observations, clean and format data
weather <- weather[1:240,] %>%
  select(c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", "Wind.Speed",
           "Gust", "Pressure", "Air.Temp", "Water.Temp")) %>%
  unite(Date, Year, Month, Day, sep = "-", remove = TRUE) %>%
  unite(Hours, Hour, Minute, sep = ":", remove = TRUE) %>%
  unite(Time, Date, Hours, sep = " ", remove = TRUE) %>%
  mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M")) %>%
  mutate(Time = case_when(
    dst(Time[1]) == TRUE ~ Time - 25200,
    dst(Time[1]) == FALSE ~ Time - 28800)) %>%
  mutate(Wind.Speed = Wind.Speed * 1.94384) %>%
  mutate(Gust = Gust *1.94384) %>%
  mutate(Mod.Dir = case_when(Wind.Dir > 350 ~  0,
                             TRUE ~ Wind.Dir))

# Create night-time shade limits
pt.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < tail(weather$Time, 1)), tail(weather$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > head(weather$Time, 1)), head(weather$Time, 1)))

# Wind direction plot
pt.dir.plot <- ggplot() + 
  geom_rect(data = pt.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_point(data = weather, aes(x = Time, y = wind.rose(Wind.Dir)), size = 1) +
  theme_bw() +
  labs(title = "**Wind Direction**") +
  theme(plot.title = element_markdown()) +
  ylab("") +
  xlab("") +
  scale_y_discrete(limits = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')) +
  scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))

# Barometer plot
pt.bar.plot <- ggplot() + 
  geom_rect(data = pt.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = weather, aes(x = Time, y = Pressure), size = 1) +
  geom_hline(aes(yintercept = 1013.25), linetype = "dashed", color = "gray") +
  theme_bw() +
  labs(title = "**Barometric Pressure**") +
  theme(plot.title = element_markdown()) +
  ylab("Millibars") +
  xlab("") +
  scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0))

# Wind rose
pt.rose <- ggplot(first(na.omit(weather[,8])), aes(x = Mod.Dir)) +
  coord_polar(theta = "x", start = -pi/45, direction = 1) +
  geom_bar(width = 7, color = "gray10", fill = "red") +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(-4, 356), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

# Handle missing gust erros
if (any(is.na(weather$Gust)) == TRUE){
  weather <- weather %>%
    drop_na(Gust)
}

# Wind plot
pt.weather.plot <- ggplot() + 
  geom_rect(data = pt.shade, 
            aes(xmin = dusk, xmax = dawn, ymin = bottom, ymax = top), 
            fill = 'light grey', alpha = 0.5) +
  geom_line(data = weather, aes(x = Time, y = Wind.Speed), size = 1) +
  geom_line(data = weather, aes(x = Time, y = Gust), color = "#FF0000") +
  theme_bw() +
  labs(
    title = "**Wind Speed** and <span style='color:#FF0000;'>**Gust**</span></span>") +
  theme(plot.title = element_markdown()) +
  scale_y_continuous(breaks = seq(0, max(weather$Gust), 5)) +
  scale_x_datetime(limits = c(min(weather$Time), max(weather$Time)), expand = c(0, 0)) +
  ylab("Knots") +
  xlab("")

# Shiny UI
ui <- tabsetPanel(
  tabPanel("Disco Bay",
           fluidPage(
             h5("WX Monitor", align = "center"),
             h3("Disco Bay 48 hour forecast", align = "center"),
             h4(textOutput("d.time.current"), align = "center"),
             h4(textOutput("d.weather.label"), align = "center"),
             fluidRow(column(12, align = "center",
              plotOutput(outputId = "d.rose", width = "50%", height = "200px"))),
             plotOutput(outputId = "d.weather.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.dir.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.rain.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "d.bar.plot", width = "100%", height = "400px")
           )
  ),
  tabPanel("PT Ferry",
           fluidPage(
             h5("WX Monitor", align = "center"),
             h3("Port Townsend Ferry Dock Reports", align = "center"),
             h4(textOutput("pt.time.current"), align = "center"),
             h4(textOutput("pt.time.label"), align = "center"),
             h4(textOutput("pt.weather.label"), align = "center"),
             fluidRow(column(12, align = "center",
              plotOutput(outputId = "pt.rose", width = "50%", height = "200px"))),
             plotOutput(outputId = "pt.weather.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "pt.dir.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "pt.bar.plot", width = "100%", height = "400px")
           )
  ),
  tabPanel("Ediz Hook",
           fluidPage(
             h5("WX Monitor", align = "center"),
             h3("Ediz Hook Reports", align = "center"),
             h4(textOutput("time.current"), align = "center"),
             h4(textOutput("time.label"), align = "center"),
             h4(textOutput("weather.label"), align = "center"),
             fluidRow(column(12, align = "center",
              plotOutput(outputId = "rose", width = "50%", height = "200px"))),
             plotOutput(outputId = "weather.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "dir.plot", width = "100%", height = "400px"),
             plotOutput(outputId = "bar.plot", width = "100%", height = "400px")
           )
  )
)

# Shiny server
server <- function(input, output) {
  
  # Disco Bay current weather label
  output$d.weather.label <- renderText({
    paste0(wind.rose(current$wind_deg), " ",
           round(current$wind_speed, 0), " knots ",
           "(", current$wind_deg, "°)")
    
  }) 
  
  # Disco Bay weather plot output
  output$d.weather.plot <- renderPlot({
    d.weather.plot
  }) 
  
  # Disco Bay current time output
  output$d.time.current <- renderText({
    paste("",format(Sys.time(), "%a %m-%d %H:%M"))
  })
  
  # Disco Bay direction plot output
  output$d.dir.plot <- renderPlot({
    d.dir.plot
  })
  
  # Disco Bay rain plot output
  output$d.rain.plot <- renderPlot({
    d.rain.plot
  })
  
  # Disco Bay barometer plot output
  output$d.bar.plot <- renderPlot({
    d.bar.plot
  }) 
  
  # Disco Bay wind rose output
  output$d.rose <- renderPlot({
    d.rose
  }) 
  
  # PT wind plot output
  output$pt.weather.plot <- renderPlot({
    pt.weather.plot
  }) 
  
  # PT current time output
  output$pt.time.current <- renderText({
    paste("Current time:", format(Sys.time(), "%m-%d %H:%M"))
  })
  
  # PT last reading output
  output$pt.time.label <- renderText({
    paste("Last reading:", format(first(na.omit(weather$Time)), "%m-%d %H:%M"))
  }) 
  
  # PT current weather label output
  output$pt.weather.label <- renderText({
    paste0(wind.rose(first(na.omit(weather$Wind.Dir))), " ",
           round(first(na.omit(weather$Wind.Speed)), 0), " knots ",
           "(", first(na.omit(weather$Wind.Dir)), "°)")
    
  }) 
  
  # PT wind direction plot output
  output$pt.dir.plot <- renderPlot({
    pt.dir.plot
  })
  
  # PT barometer plot output
  output$pt.bar.plot <- renderPlot({
    pt.bar.plot
  }) 
  
  # PT wind rose output
  output$pt.rose <- renderPlot({
    pt.rose
  })
  
  # Ediz Hook wind plot output
  output$weather.plot <- renderPlot({
    weather.plot
  }) 
  
  # Ediz Hook current time output
  output$time.current <- renderText({
    paste("Current time:", format(Sys.time(), "%m-%d %H:%M"))
  })
  
  # Ediz Hook last reading output
  output$time.label <- renderText({
    paste("Last reading:", format(tail(weather.table$Time, 1), "%m-%d %H:%M"))
  }) 
  
  # Ediz Hook current weather label output
  output$weather.label <- renderText({
    paste0(wind.rose(last(na.omit(dir.table$`Wind Direction`))), " ",
           last(na.omit(weather.table$`Wind Speed`)), " knots ",
           "(", last(na.omit(dir.table$`Wind Direction`)), "°)")
    
  }) 
  
  # Ediz Hook wind direction plot output
  output$dir.plot <- renderPlot({
    dir.plot
  })
  
  # Ediz Hook barometer plot output
  output$bar.plot <- renderPlot({
    bar.plot
  }) 
  
  # Ediz Hook wind rose output
  output$rose <- renderPlot({
    rose
  })
  
}


# Execute app
shinyApp(ui, server)
