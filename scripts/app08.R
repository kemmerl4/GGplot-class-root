# Application 8
# 29 July 2020

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE )
library(stringr)

#create a month column
dates = as.Date(weatherData$dateYr) # save the date column to a vector
months = format(dates, format="%b") # extract the month -- save to vector
weatherData$month = months# save months to data frame as new column

#Extract first two values in the weatherType column
weatherType_fill = str_sub(weatherData$weatherType, 1, 2)

#plot cooling days vs. month
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays, fill=weatherType_fill),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_manual(values=c("blue", "purple", "yellow", "black", "orange", "green", "pink", "grey"))+
  theme_bw() +
  labs(title = "Monthly Cooling Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Cooling Days")
plot(thePlot)


#Find the sum of all coolDays for the year for adding horiz line in plot
coolDayssum = sum(weatherData$coolDays)

#Create a bar plot of Heating Days (heatDays) for each month
#plot heatDays vs. month
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=heatDays, fill=weatherType_fill),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  scale_fill_manual(values=c("blue", "purple", "yellow", "black", "orange", "green", "pink", "grey"))+
  theme_bw() +
  geom_hline(mapping = aes( yintercept = coolDayssum),
             color="red", 
             size=1.5, 
             linetype=1) +
  annotate(geom="text", # North median
           x=7,
           y=880, 
           color="red",
           label=paste("Sum of all cool days") ) +
  labs(title = "Monthly Heating Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Heating Days")
plot(thePlot)


#Combine Heating and Cooling Days into one barplot
#plot cooling days vs. month
thePlot = ggplot(data=weatherData) +
  geom_col(mapping=aes(x=month, y=coolDays, 
                       fill="blue"),
           width=0.6) +
  geom_col(mapping=aes(x=month, y=heatDays,
                       fill="red"),
           width=0.6) +
  scale_x_discrete(limits = month.abb) +
  theme_bw() +
  labs(title = "Monthly Heating and Cooling Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Month",
       y = "Heating and Cooling Days")
plot(thePlot)


