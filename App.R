library(tidyverse)
library(shiny)
library(jsonlite)
library(ggtext)
library(data.table)
library(lubridate)
library(scales)

###### Combowx monitor full #########

Sys.setenv(TZ="America/Los_Angeles")

wind.rose <- function(x) {
  upper <- seq(from = 11.25, by = 22.5, length.out = 17)
  card1 <- c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW', 'N')
  ifelse(x>360 | x<0,NA,card1[findInterval(x,upper,rightmost.closed = T)+1])
}


# Disco Bay chunk - runs first to get sunrise/sunset data

url <- "https://api.openweathermap.org/data/2.5/onecall?lat=47.9936&lon=-122.88248&exclude=minutely&units=imperial&appid=8d5cf85099c375dcad074eff91b0d5d9"
d.weather.page <- fromJSON(url, flatten = TRUE)
hourly.forecast <- data.frame(d.weather.page$hourly)
hourly.forecast$dt <- as.POSIXct(hourly.forecast$dt, origin="1970-01-01")

current <- data.frame(d.weather.page$current)
current$dt <- as.POSIXct(current$dt, origin="1970-01-01")
current$sunrise <- as.POSIXct(current$sunrise, origin="1970-01-01")
current$sunset <- as.POSIXct(current$sunset, origin="1970-01-01")

current <- current %>%
  mutate(wind_speed = wind_speed * 0.868976)

hourly.forecast <- hourly.forecast %>%
  mutate(wind_speed = wind_speed * 0.868976) %>%
  mutate(wind_gust = wind_gust * 0.868976)

d.shade <- data.frame(dusk = seq.POSIXt(current$sunset, by = 'day', length.out = 3), 
                    dawn = seq.POSIXt(current$sunrise+86400, by = 'day', length.out = 3),
                    top = Inf,
                    bottom = -Inf)

d.shade <- d.shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(hourly.forecast$dt, 1)), tail(hourly.forecast$dt, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(hourly.forecast$dt, 1)), head(hourly.forecast$dt, 1)))


d.rose <- ggplot(current, aes(x = wind_deg)) +
  coord_polar(theta = "x", start = 0, direction = 1) +
  geom_histogram(fill = "red", color = "gray10", bins = 30) +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(0, 359), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

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

# Query and clean data

url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=KNOW&obtimezone=local&recent=1440&vars=wind_speed&units=english&token=652e4bd32bbf4621b835895f8c769bb6"
weather.page <- fromJSON(url, flatten = TRUE)
weather.table <- data.frame(weather.page[["STATION"]][16][[1]], weather.page[["STATION"]][17][[1]])

url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=KNOW&obtimezone=local&recent=1440&vars=wind_gust&units=english&token=652e4bd32bbf4621b835895f8c769bb6"
gust.page <- fromJSON(url, flatten = TRUE)
gust.table <- data.frame(gust.page[["STATION"]][16][[1]], gust.page[["STATION"]][17][[1]])

url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=KNOW&obtimezone=local&recent=1440&vars=wind_direction&units=english&token=652e4bd32bbf4621b835895f8c769bb6"
dir.page <- fromJSON(url, flatten = TRUE)
dir.table <- data.frame(dir.page[["STATION"]][16][[1]], dir.page[["STATION"]][17][[1]])

url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=KNOW&obtimezone=local&recent=720&vars=sea_level_pressure&units=english&token=652e4bd32bbf4621b835895f8c769bb6"
pressure.page <- fromJSON(url, flatten = TRUE)
pressure.table <- data.frame(pressure.page[["STATION"]][17][[1]], pressure.page[["STATION"]][18][[1]])

names(weather.table) <- c("Time", "Wind Speed")
names(dir.table) <- c("Time", "Wind Direction")
names(pressure.table) <- c("Time", "Pressure (mb)")

weather.table$Time <- as.POSIXct(weather.table$Time, format = "%Y-%m-%dT%H:%M:%S%z")
dir.table$Time <- as.POSIXct(dir.table$Time, format = "%Y-%m-%dT%H:%M:%S%z")
pressure.table$Time <- as.POSIXct(pressure.table$Time, format = "%Y-%m-%dT%H:%M:%S%z")

shade <- data.frame(dusk = seq.POSIXt(current$sunset-172800, by = 'day', length.out = 3), 
                      dawn = seq.POSIXt(current$sunrise-86400, by = 'day', length.out = 3),
                      top = Inf,
                      bottom = -Inf)

e.dir.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(dir.table$Time, 1)), tail(dir.table$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(dir.table$Time, 1)), head(dir.table$Time, 1)))

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

e.bar.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(pressure.table$Time, 1)), tail(pressure.table$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(pressure.table$Time, 1)), head(pressure.table$Time, 1)))


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

rose <- ggplot(tail(dir.table, 1), aes(x = `Wind Direction`)) +
  coord_polar(theta = "x", start = 0, direction = 1) +
  geom_histogram(fill = "red", color = "gray10", bins = 30) +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(0, 359), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

e.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > tail(weather.table$Time, 1)), tail(weather.table$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < head(weather.table$Time, 1)), head(weather.table$Time, 1)))

# Catch errors due to missing gust data

a <- try({
  names(gust.table) <- c("Time", "Wind Speed")
  gust.table$Time <- as.POSIXct(gust.table$Time, format = "%Y-%m-%dT%H:%M:%S%z")
  gust.table$Time <- gust.table$Time
  
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

# Query and clean data

weather <- fread("https://www.ndbc.noaa.gov/data/realtime2/PTWW1.txt", 
                 skip = 2,
                 na.strings = "MM",
                 encoding = "UTF-8",
                 colClasses = c(rep("character", 5), rep("numeric", 3), 
                                rep("factor", 4), rep("numeric", 3), rep("factor", 4)),
                 col.names = c("Year", "Month", "Day", "Hour", "Minute", "Wind.Dir", 
                               "Wind.Speed", "Gust", NA, NA, NA, NA, "Pressure", 
                               "Air.Temp", "Water.Temp", NA, NA, NA, NA))

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
  mutate(Gust = Gust *1.94384)

pt.shade <- shade %>% 
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. < tail(weather$Time, 1)), tail(weather$Time, 1))) %>%
  mutate_at(vars(dusk, dawn),
            ~ replace(., which(. > head(weather$Time, 1)), head(weather$Time, 1)))


# Build plots 

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


pt.rose <- ggplot(first(na.omit(weather[,1:3])), aes(x = Wind.Dir)) +
  coord_polar(theta = "x", start = 0, direction = 1) +
  geom_histogram(fill = "red", color = "gray10", bins = 30) +
  scale_x_continuous(breaks = seq(0, 359, 22.5), limits = c(0, 359), 
                     labels = c('N', 'NNE', 'NE', 'ENE', 'E', 'ESE', 'SE', 'SSE', 
                                'S', 'SSW', 'SW', 'WSW', 'W', 'WNW', 'NW', 'NNW')) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.title = element_blank())

if (any(is.na(weather$Gust)) == TRUE){
  weather <- weather %>%
    drop_na(Gust)
}

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


ui <- tabsetPanel(
  tabPanel("Disco Bay",
           fluidPage(
             h1("Disco Bay Weather", align = "center"),
             h3("48 hour forecast", align = "center"),
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
             h1("Port Townsend Ferry Dock Weather", align = "center"),
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
             h1("Ediz Hook Weather", align = "center"),
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


server <- function(input, output) {
  
  output$d.weather.label <- renderText({
    paste0(wind.rose(current$wind_deg), " ",
           round(current$wind_speed, 0), " knots ",
           "(", current$wind_deg, "°)")
    
  }) 
  
  output$d.weather.plot <- renderPlot({
    d.weather.plot
  }) 
  
  output$d.time.current <- renderText({
    paste("",Sys.time())
  })
  
  
  output$d.dir.plot <- renderPlot({
    d.dir.plot
  })
  
  
  output$d.rain.plot <- renderPlot({
    d.rain.plot
  })
  
  
  output$d.bar.plot <- renderPlot({
    d.bar.plot
  }) 
  
  output$d.rose <- renderPlot({
    d.rose
  }) 
  
  output$pt.weather.plot <- renderPlot({
    pt.weather.plot
  }) 
  
  output$pt.time.current <- renderText({
    paste("Current time:", Sys.time())
  })
  
  output$pt.time.label <- renderText({
    paste("Last reading:", first(na.omit(weather$Time)))
  }) 
  
  output$pt.weather.label <- renderText({
    paste0(wind.rose(first(na.omit(weather$Wind.Dir))), " ",
           round(first(na.omit(weather$Wind.Speed)), 0), " knots ",
           "(", first(na.omit(weather$Wind.Dir)), "°)")
    
  }) 
  
  output$pt.dir.plot <- renderPlot({
    pt.dir.plot
  })
  
  output$pt.bar.plot <- renderPlot({
    pt.bar.plot
  }) 
  
  output$pt.rose <- renderPlot({
    pt.rose
  })
  
  output$weather.plot <- renderPlot({
    weather.plot
  }) 
  
  output$time.current <- renderText({
    paste("Current time:", Sys.time())
  })
  
  output$time.label <- renderText({
    paste("Last reading:", tail(weather.table$Time, 1))
  }) 
  
  output$weather.label <- renderText({
    paste0(wind.rose(last(na.omit(dir.table$`Wind Direction`))), " ",
           last(na.omit(weather.table$`Wind Speed`)), " knots ",
           "(", last(na.omit(dir.table$`Wind Direction`)), "°)")
    
  }) 
  
  output$dir.plot <- renderPlot({
    dir.plot
  })
  
  output$bar.plot <- renderPlot({
    bar.plot
  }) 
  
  output$rose <- renderPlot({
    rose
  })
  
}


# Execute app
shinyApp(ui, server)
