{
  source(file="scripts/reference.R");  # this line will be in all your scripts
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE)

  library(package=gganimate)  # package used to create the animation mappings
  library(package=gifski)  # package used to create animated gifs
  library(package=av)  # package used to create video
  
  
  # animation mapping -- using season, a discrete variable
  # closest_state is a "Label variable" (look on cheat sheet)
  plot1 = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=maxTemp, y=heatDays)) +
    labs(title = 'heatDays (y) vs. maxTemp (x) by {closest_state} (animation)',  
         subtitle = 'Lansing, MI - 2016',
         x = 'Average Temp', 
         y = 'Humidity') +
    theme_bw() +
    transition_states(states=season); # maps season to animation 
  
  # You need to use print() instead of plot() -- 
  # The animation is sent to the Viewer tab
  print(plot1)  
  
  
  
  
  
######Create other animations: 1. histogram
  
  #create histogram of avg humidity
  plotData = ggplot( data=weatherData ) +
    geom_histogram( mapping=aes(x=relHum, y=..count..),
                    bins=40,
                    color="grey20",
                    fill="darksalmon") +
    theme_classic() +
    labs(title = "Humidity Histogram",
         subtitle = "Lansing, Michigan: 2016",
         x = "Relative Humidity",
         y = "Count")+
    transition_states(states=windDir)
  print(plotData)
 
  
  
#Create other animations: 2. barplot  
  
  #create a month column
  dates = as.Date(weatherData$dateYr) # save the date column to a vector
  months = format(dates, format="%b") # extract the month -- save to vector
  weatherData$month = months# save months to data frame as new column
  
  #plot sunset vs. sunrise by month
  thePlot = ggplot(data=weatherData) +
    geom_col(mapping=aes(x=sunrise, y=sunset),
             width=0.6) +
    theme_bw() +
    labs(title = "sunset vs. sunrise by month",
         subtitle = "Lansing, Michigan: 2016",
         x = "sunrise",
         y = "sunset")+
    transition_states(states=month)
  print(thePlot)
  
  
  
  
  ##### Assigment 2: 
  #Save a plot as both a gif and a video with modified timing
  #save the files to a folder called media in your class project
  
  #create histogram of avg humidity
  mediaData = ggplot( data=weatherData ) +
    geom_histogram( mapping=aes(x=relHum, y=..count..),
                    bins=40,
                    color="grey20",
                    fill="darksalmon") +
    theme_classic() +
    labs(title = "Humidity Histogram",
         subtitle = "Lansing, Michigan: 2016",
         x = "Relative Humidity",
         y = "Count")+
    shadow_mark(color="gray80") +            # keeps old values
    enter_fly(x_loc=10, y_loc=50) +          # ????
    ease_aes(default = "elastic-in-out") +
    transition_time(time = maxTemp,
                    range = c(16, 60))   # range can be changed to limit the "time"
  print(mediaData)

  # anim_save() -- saving as a gif
  anim_save(filename = "media/anim_practice.gif",
            animation = mediaData,
            nframes = 60,       # number of frames in animation
            fps = 3)           # frames per second
  
  # anim_save() -- saving as an mp4 video
  anim_save(filename = "anim_practice2.mp4",
            animation = mediaData,
            renderer = av_renderer(),
            nframes = 60,       # number of frames in animation
            fps = 3)           # frames per second
  
}

