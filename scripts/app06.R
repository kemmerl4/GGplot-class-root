#Application 6
#16 July 20

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-2.csv", 
                        stringsAsFactors = FALSE )

#Create a vector that holds the quantile values for the 20, 40, 60, and 80th 
#percentile of stnPressure
#categorize wind speed into quantiles
stnPressureQuant = quantile(weatherData$stnPressure, probs=c(.20, .40, .60, .80));

#Create a new column called pressureLevel that creates five evenly spaced levels 
#for stnPressure
for(day in 1:nrow(weatherData)) # nrow(weatherData) = 366
{
  # if the value in stnPressure is less than or equal to the low quant value
  if(weatherData$stnPressure[day] <= stnPressureQuant[1]) # <=28.9
  {
    weatherData$stnPressureLevel[day] = "1"
  }
  # if the value in stnPressure is in between the first and second quant value
  else if(weatherData$stnPressure[day] > stnPressureQuant[1] && 
     weatherData$stnPressure[day] <= stnPressureQuant[2]) 
  {
    weatherData$stnPressureLevel[day] = "2"
  }
  # if the value in stnPressure is in between the second and third quant value
  else if(weatherData$stnPressure[day] > stnPressureQuant[2] && 
    weatherData$stnPressure[day] <= stnPressureQuant[3]) 
  {
    weatherData$stnPressureLevel[day] = "3"
  }
  
  # if the value in stnPressure is greater than or equal to the high quant value
  else if(weatherData$stnPressure[day] > stnPressureQuant[4]) 
  {
    weatherData$stnPressureLevel[day] = "5"
  }
  else # the value in stnPressure is between the third and fourth quant value
  {
    weatherData$stnPressureLevel[day] = "4"
  }
}

#Find the dates for the three outliers at the lowest pressure level.  
#The three outliers are also the three highest windSusSpeed for the year.
level_1_pressure_outliers=which(weatherData$windSusSpeed > 40)

# Make a boxplot of windSusSpeed vs pressureLevel
speed_pressure_plot = ggplot(data=weatherData) +
  geom_boxplot(mapping=aes(x=stnPressureLevel, y=windSusSpeed)) +
  theme_bw() +
  labs(title = "Wind Speed vs. Pressure",
       subtitle = "Lansing, Michigan: 2016",
       x = "Pressure Level",
       y = "Wind Speed")+
  annotate(geom="text", # outlier 1
           x=1.5,
           y=47, 
           color="blue",
           label=paste("02-19-2016")) +
  annotate(geom="text", #Outlier 2
           x=1.5, 
           y=41, 
           color="blue",
           label=paste("03-16-2016") ) +
  annotate(geom="text", # Outlier 3
           x=1.5,
           y=48, 
           color="blue", 
           label=paste("07-08-2016") )
plot(speed_pressure_plot)



