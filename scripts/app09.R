#application 9
# 5 August 2020
 
source( file="scripts/reference.R" ); 
weatherData = read.csv( file="data/LansingNOAA2016-3.csv", 
                        stringsAsFactors = FALSE )

# Convert trace rain to the numeric value 0.005
# Copy precip values to a new column, precipNum
weatherData$precipNum = weatherData$precip
# Go through all rows in weatherData
for(i in 1:nrow(weatherData))
{
  # check precipNum value -- if the value is T, change to 0.05
  if(weatherData$precipNum[i] == "T")
  {
    weatherData$precipNum[i] = 0.005
  }
}

weatherData$precipNum = as.numeric(weatherData$precipNum)

# Create a text plot of Precipitation vs Humidity
thePlot = ggplot(data=weatherData) +
  geom_text(mapping=aes(x=relHum, y=precipNum, 
                        color=avgTemp, 
                        label=avgTemp),
            size=2.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.key.height = unit(30, units="pt"), # height
        legend.key.width = unit(15, units="pt"),    # width
        legend.direction = "vertical",            # alignment
        legend.position = c(.2,.6)) +          # position +
  scale_color_gradientn(colors=c("blue", "darkgreen","red")) +
  scale_y_continuous(trans="log10") + #change y to logarithmic scale
  labs(title = "Humidity vs. Temperature",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity",
       y = "Precipitation",
       color = "Average Temperature")
plot(thePlot)
