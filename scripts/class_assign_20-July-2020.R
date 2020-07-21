{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE)
}
  
# grepl returns a true/false vector (l for logical)
snow_rain_days = grepl(x=weatherData$weatherType, pattern="RA|SN");
  
# for loop for 
  for(day in 1:nrow(weatherData))  
  {
    if(snow_rain_days[day] == TRUE)  # day had either RA or SN
    {
      weatherData$snow_rain_days[day] = 1;   # set precip to 1
    }
    else   # day had neither RA nor SN
    {
      weatherData$snow_rain_days[day] = 0;   # set precip to 0
    }
  }

#add vector back to dataframe
weatherData$snow_rain_days = as.character(weatherData$snow_rain_days)
