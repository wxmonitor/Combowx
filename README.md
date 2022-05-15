**WX Monitor web app**

  This shiny app displays personalized local weather information for three locations.
    
   Disco Bay tab pulls open source weather data from OpenWeather API.
    Current time, wind speed and direction are displayed.
    Ggplot objects are generated and displayed for 48 hour forecast data for the following:
  
    Wind Speed (kts)
    Wind Direction (compass points)
    Rainfall chance (%) and accumulation totals (mm)
    Air temperature and dew point (°F)
    Barometric pressure (mb)
   
  PT Ferry tab pulls NOAA recorded weather data for PTWW1 station (Port Townsend, WA ferry dock).
    Current time, wind speed and direction are displayed along with time of latest reading.
    Ggplot objects are generated and displayed for most recent 24 hours of recorded data for the following:
  
    Wind Speed and gust (kts)
    Wind Direction (compass points)
    Barometric pressure (mb)
    
  Ediz Hook tab pulls recorded weather data from Mesonet API via SynopticData for Ediz Hook Coast Guard station (Port Angeles, WA).
    Current time, wind speed and direction are displayed along with time of latest reading.
    Ggplot objects are generated and displayed for most recent 24 hours of recorded data for the following:
  
    Wind Speed and gust (kts)
    Wind Direction (compass points)
    Barometric pressure (mb) 
   
   
  Web app is deployed on shiny server running on remote ubuntu 18.04 server. 
  Accesible at https://monitor.wxnw.net/geo

