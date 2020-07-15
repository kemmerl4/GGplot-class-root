source(file="scripts/reference.R"); # include the reference.r file
weatherData = read.csv(file="data/LansingNOAA2016.csv",
                       stringsAsFactors = FALSE)

#Plot average wind speed vs. daily temperature departure
plotData = ggplot( data=weatherData ) +
  geom_point( mapping=aes(x=abs(tempDept),y=windSpeed), 
              color=rgb(red=0, green=.6, blue=.6), 
              size=2, 
              pch=23,
              alpha = 0.7, 
              bg=rgb(red=0, green=.6, blue=.6) ) +
  labs(title = "Wind Speed vs. Daily Temperature Departure",
       subtitle = "Lansing, Michigan: 2016",
       x = "Daily Temperature Departure (F)",
       y = "Wind Speed") +
  theme_bw() +
  theme(axis.title.x=element_text(size=14, 
                                  color="darkgoldenrod4", 
                                  face="italic"), 
        plot.title=element_text(size=18, 
                                face="bold", 
                                color ="darkblue"),
        plot.subtitle=element_text(size=12, 
                                   face="bold.italic", 
                                   color ="darkgoldenrod3", 
                                   family="serif"),
        axis.text.y=element_text(angle=45, 
                                 hjust=1)) +
  geom_smooth(mapping=aes(x=abs(tempDept), y=windSpeed),
              method="lm",
              color="black", 
              size=0.8, 
              linetype=2, 
              fill="azure2")+
  #scale_x_continuous()+
  #scale_y_continuous()+
  theme(aspect.ratio=16/9)
   #     axis.text.x=element_text(),
   #     axis.text.y=element_text(),
   #     axis.tit)
plot(plotData)

ggsave(filename = "images/app_03_1.jpeg", 
       plot=plotData, 
       width = 12, 
       height = 7, 
       units = "cm")

ggsave(filename= "images/pngFiles/app_03_2.png", 
       plot=plotData, 
       width = 6, 
       height = 8, 
       units = "in")



