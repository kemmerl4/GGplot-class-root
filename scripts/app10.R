#Application 09
#8 August 2020

source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE )

#gridExtra needed for multiple plot
library(package=gridExtra)

####use grep to find all days with sun
sunnyDays = grep(weatherData$weatherType, pattern="SN") # any day with sun
sunny_data=weatherData[sunnyDays,] #create a df of days with sun

#find average tempDept for sunny days
avg_sun = mean(sunny_data$tempDept)

#plot a histogram of tempDept for sunny days
plot_sun = ggplot(data=sunny_data) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darksalmon") +
  #insert line for average tempDept on sunny days
  geom_vline(mapping=aes(xintercept=avg_sun),
             color="darkslateblue",
             size=1.2) + 
  #label the line
  annotate(geom="text",
           x=1,
           y=6, 
           color="blue",
           label=paste("average:", avg_sun) ) +
  theme_classic() +
  labs(title = "Temperature Departure on Sunny Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature Departure",
       y = "Number of Days")
plot(plot_sun)



####use grep to find all days with fog
foggyDays = grep(weatherData$weatherType, pattern="FG") # any day with fog
foggy_data=weatherData[foggyDays,] #create a df of days with fog

#find average tempDept for days with fog
avg_fog = mean(foggy_data$tempDept)

#plot a histogram of tempDept for sunny days
plot_fog = ggplot(data=foggy_data) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darksalmon") +
  #insert line for average tempDept on foggy days
  geom_vline(mapping=aes(xintercept=avg_fog),
             color="darkslateblue",
             size=1.2) + 
  #label the line
  annotate(geom="text",
           x=1,
           y=4, 
           color="blue",
           label=paste("average:", avg_fog) ) +
  theme_classic() +
  labs(title = "Temperature Departure on Foggy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature Departure",
       y = "Number of Days")
plot(plot_fog)



####use grep to find all days with haze
hazyDays = grep(weatherData$weatherType, pattern="HZ") # any day with haze
hazy_data=weatherData[hazyDays,] #create a df of days with haze

#find average tempDept for days with haze
avg_haze = mean(hazy_data$tempDept)

#plot a histogram of tempDept for hazy days
plot_haze = ggplot(data=hazy_data) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darksalmon") +
  #insert line for average tempDept on hazy days
  geom_vline(mapping=aes(xintercept=avg_haze),
             color="darkslateblue",
             size=1.2) + 
  #label the line
  annotate(geom="text",
           x=1,
           y=4, 
           color="blue",
           label=paste("average:", avg_haze) ) +
  theme_classic() +
  labs(title = "Temperature Departure on Hazy Days",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature Departure",
       y = "Number of Days")
plot(plot_haze)



#####use grep to find all days with sun and fog
sun_fog_Days = intersect(sunnyDays, foggyDays)
sun_fog_data=weatherData[sun_fog_Days,] #create a df of days with sun and fog

#find average tempDept for days with sun and fog
avg_sun_fog = mean(sun_fog_data$tempDept)

#plot a histogram of tempDept for days with sun and fog
plot_sun_fog = ggplot(data=sun_fog_data) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darksalmon") +
  #insert line for average tempDept on days with sun and fog
  geom_vline(mapping=aes(xintercept=avg_sun_fog),
             color="darkslateblue",
             size=1.2) + 
  #label the line
  annotate(geom="text",
           x=1,
           y=3, 
           color="blue",
           label=paste("average:", avg_sun_fog) ) +
  theme_classic() +
  labs(title = "Temperature Departure on Days with Sun & Fog",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature Departure",
       y = "Number of Days")
plot(plot_sun_fog)



####use grep to find all days with sun or fog
sun_or_fog_Days = union(sunnyDays, foggyDays)
sun_or_fog_data=weatherData[sun_or_fog_Days,] #create a df of days with sun or fog

#find average tempDept for days with sun or fog
avg_sun_or_fog = mean(sun_or_fog_data$tempDept)

#plot a histogram of tempDept for days with sun or fog 
plot_sun_or_fog = ggplot(data=sun_or_fog_data) +
  geom_histogram(mapping=aes(x=tempDept, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darksalmon") +
  #insert line for average tempDept on days with sun or fog
  geom_vline(mapping=aes(xintercept=avg_sun_or_fog),
             color="darkslateblue",
             size=1.2) + 
  #label the line
  annotate(geom="text",
           x=1,
           y=6.25, 
           color="blue",
           label=paste("average:", avg_sun_or_fog) ) +
  theme_classic() +
  labs(title = "Temperature Departure on Days with Sun or Fog",
       subtitle = "Lansing, Michigan: 2016",
       x = "Temperature Departure",
       y = "Number of Days")
plot(plot_sun_or_fog)


####Plot on one canvas by rows 
grid.arrange(plot_sun, plot_fog, plot_haze, plot_sun_fog, plot_sun_or_fog, 
             nrow=3)

