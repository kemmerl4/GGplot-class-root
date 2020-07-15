#Application 4: Date objects and canvas styles
#11 July 2020

source(file="scripts/reference.R")
weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                       stringsAsFactors = FALSE)

#Create a date column that includes year
#Add year to date values
theDate = weatherData$date

#paste "-2016" to all values in theDate
theDate = paste(theDate, "-2016", sep="")

#Save the values in Date format
theDate = as.Date(theDate, format="%m-%d-%Y")

#Save theDate back to the data frame as a new column
weatherData$dateYr = theDate

#Convert the three temperature columns in the dataframe for F to C temp
#get all values from temperature columns
maxTempC = weatherData$maxTemp
minTempC = weatherData$minTemp
avgTempC = weatherData$avgTemp

#calculate F to C
maxTempC = (maxTempC - 32) * (5 / 9)
minTempC = (minTempC - 32) * (5 / 9)
avgTempC = (avgTempC - 32) * (5 / 9)

#Save the C temperature back to the data frame as a new column
weatherData$maxTemp = maxTempC
weatherData$minTemp = minTempC
weatherData$avgTemp = avgTempC

#Plot max temp vs date, min temp vs date, and (smooth) avg temp vs date
plotData = ggplot(data=weatherData) +
  geom_line(mapping=aes(x=dateYr, y=maxTemp),
            color="palevioletred1") +
  geom_line(mapping=aes(x=dateYr, y=minTemp),
            color="aquamarine2") +
  geom_smooth(mapping=aes(x=dateYr, y=avgTemp), 
              color="orange", 
              method="loess", 
              linetype=4, 
              fill="lightblue") +
  labs(title = "Temperature vs. Date",
       subtitle = "Lansing, Michigan: 2016",
       x = "Date",
       y = "Temperature (C)") +
  theme(plot.title = element_text(hjust = 0.85),
        plot.subtitle = element_text(hjust = 0.85),
        axis.text.x = element_text(color="blue"),
        axis.text.y = element_text(color="red"))+
  scale_x_date(limits=c(as.Date("2016-03-21"), 
                        as.Date("2016-9-21")),
               date_breaks = "8 weeks", 
               date_labels = format("%b-%d-%Y"))+
  scale_y_continuous(limits = c(-10,40),
               breaks = seq(from=-10, to=40, by=15))
plot(plotData)

