# Application 7
# 25 July 2020

source( file="scripts/reference.R" )
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE )

#create a vector that holds quantile values for .3 and .7
relHumidityQuant = quantile(weatherData$relHum, probs=c(.30, .70))

#Create three levels of humidity
#create a new column called humidityLevel
for(day in 1:nrow(weatherData)) # nrow(weatherData) = 366
{
  # if the value in relHum is less than or equal to the low quant value
  if(weatherData$relHum[day] <= relHumidityQuant[1]) # <= 63
  {
    weatherData$humidityLevel[day] = "Low"
  }
  # if the value in relHum is greater than or equal to the high quant value
  else if(weatherData$relHum[day] >= relHumidityQuant[2]) # >= 75
  {
    weatherData$humidityLevel[day] = "High"
  }
  else # the value in relHum is between 63 and 75
  {
    weatherData$humidityLevel[day] = "Medium"
  }
}

windDirFact = factor(weatherData$windDir,
                     levels=c("North", "East", "South", "West"))

#Create a boxplot of stnPressure vs humidityLevel
thePlot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=humidityLevel, y=stnPressure,
                           fill=factor(windDir,
                                       levels=c("North", "East", "South", "West"))),
                           na.rm=TRUE) +
  theme_bw() +
  scale_x_discrete(limits = c("Low", "Medium", "High")) + #reorder humidityLevel
  facet_grid(facets= . ~ factor(windSpeedLevel, #facet in horizantal direction
                                levels=c("Low", "Medium", "High"))) + #reorder horiz facets
  labs(title = "Pressure vs. Humidity Level",
       subtitle = "Lansing, Michigan: 2016",
       x = "Humidity Level",
       y = "Pressure",
       fill = "Wind Direction")
plot(thePlot)
