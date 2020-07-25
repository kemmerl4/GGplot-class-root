{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE)
  
# grepl returns a true/false vector (l for logical)
snow_rain_days = grepl(x=weatherData$weatherType, pattern="RA|SN")
  
# for loop for 
  for(day in 1:nrow(weatherData))  
  {
    if(snow_rain_days[day] == TRUE) #if weatherType was RA rain or SN snow
    {
      weatherData$snow_rain_days[day] = 1 #the value of snow_rain_days will be 1
    }
    else #if weatherType was not RA or SN
    {
      weatherData$snow_rain_days[day] = 0 #the value of snow_rain_days will be 0
    }
  }

#add vector back to dataframe
weatherData$snow_rain_days = as.character(weatherData$snow_rain_days)

#make blow

}