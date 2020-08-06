{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016.csv", 
                         stringsAsFactors = FALSE);
  
  #### Create a column the give whether there was precipitation
  
  #### grepl produces a Boolean vector the same length as the data: 
  #      values are TRUE if they contain RA or SN, FALSE otherwise
  daysWithPrecip = grepl(x=weatherData$weatherType, pattern="RA|SN");
  
  # days will take values from 1 to 366 (the number of rows)
  for(day in 1:nrow(weatherData))  
  {
    if(daysWithPrecip[day] == TRUE)  # day had either RA or SN
    {
      weatherData$precipitation[day] = 1;   # set precip to 1
    }
    else   # day had neither RA nor SN
    {
      weatherData$precipitation[day] = 0;   # set precip to 0
    }
  }

  # GGPlot cannot factor a numeric column -- need to convert column to string (characters)
  weatherData$precipitation = as.character(weatherData$precipitation);
  
  # Original boxplot to replicate
  plot1 = ggplot(data=weatherData) +   
    geom_boxplot(mapping=aes(x=precipitation, y=relHum)) +
    theme_bw() +
    scale_x_discrete(labels=c("No Precip", "Precip")) +  
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Precipitation",
         y = "Humidity");
  plot(plot1);
  
  ##### Assignment:
  # - Make a copy of the above plot using annotate()
  # - add a line to represent the mean value for each box
  # - add text to represent the standard deviations for each box
  # - get *one box* working fully before trying the second
  # - Challenge: get both boxes working with a for loops: for(i in 1:2) ...
  
  ##### To manually get data for boxplots (don't read if you want a challenge)
  # 1) get all relHum values for days where there is no precipitation
   relHum_noPrecip = weatherData$relHum[weatherData$precipitation==0]
  # 2) get the mean, median, and standard deviation for the data from #1
   mean_relHum_noPrecip = mean(relHum_noPrecip)
   median_relHum_noPrecip = median(relHum_noPrecip)
   sd_relHum_noPrecip = sd(relHum_noPrecip)
  # 3) find the 1st and 3rd quartile (0.25 and 0.75 quantile) 
  #    of the data using quantile() 
   first_quart = quantile(relHum_noPrecip, c(0.25))
   third_quart = quantile(relHum_noPrecip, c(0.75))
  # 4) solve for interquartile range (IQR -- the box height): 
  #    IQR = 3rd quartile - 1st quartile
   IQR_relHum_noPrecip = third_quart - first_quart
  # 5) solve for extreme whisker ends: 
  #    - high: (3rd quartile) + 1.5*IQR
   high_whisker = third_quart + 1.5 * IQR_relHum_noPrecip
  #    - low: (1st quartile) - 1.5*IQR
   low_whisker = first_quart - 1.5 * IQR_relHum_noPrecip
  # 6) Solve for actual whisker end:
  #    - high: the highest value in the data less than the high whisker value
   relHum_noPrecip_High = relHum_noPrecip[relHum_noPrecip > third_quart & relHum_noPrecip <= high_whisker] # relHum_noPrecip_Low such that relHum_noPrecip_Low is between 3/4 and high whisker point
   high_whisker_actual = max(relHum_noPrecip_High)                  # maximum of these values (becomes the high whisker point)
   
  #    - low: the lowest value in the data greater than the low whisker value
   relHum_noPrecip_Low = relHum_noPrecip[relHum_noPrecip >= low_whisker & relHum_noPrecip < first_quart]   # relHum_noPrecip_Low such that relHum_noPrecip_Low is between low whisker point and 1/4
   low_whisker_actual = min(relHum_noPrecip_Low)                    # minimum of these values (becomes the low whisker point)
   
  # 7) Repeat 1-6 for relHum values for days where there is precipitation
  
     # 1) get all relHum values for days where there is no precipitation
      relHum_Precip = weatherData$relHum[weatherData$precipitation!=0]
     # 2) get the mean, median, and standard deviation for the data from #1
      mean_relHum_Precip = mean(relHum_Precip)
     median_relHum_Precip = median(relHum_Precip)
     sd_relHum_Precip = sd(relHum_Precip)
    # 3) find the 1st and 3rd quartile (0.25 and 0.75 quantile) 
     #    of the data using quantile() 
     first_quart = quantile(relHum_Precip, c(0.25))
     third_quart = quantile(relHum_Precip, c(0.75))
     # 4) solve for interquartile range (IQR -- the box height): 
     #    IQR = 3rd quartile - 1st quartile
     IQR_relHum_Precip = third_quart - first_quart
     # 5) solve for extreme whisker ends: 
     #    - high: (3rd quartile) + 1.5*IQR
     high_whisker = third_quart + 1.5 * IQR_relHum_Precip
     #    - low: (1st quartile) - 1.5*IQR
     low_whisker = first_quart - 1.5 * IQR_relHum_Precip
     # 6) Solve for actual whisker end:
     #    - high: the highest value in the data less than the high whisker value
     relHum_Precip_High = relHum_Precip[relHum_Precip > third_quart & relHum_Precip <= high_whisker] # relHum_Precip_Low such that relHum_Precip_Low is between 3/4 and high whisker point
     high_whisker_actual = max(relHum_Precip_High)                  # maximum of these values (becomes the high whisker point)
   
     #    - low: the lowest value in the data greater than the low whisker value
     relHum_Precip_Low = relHum_Precip[relHum_Precip >= low_whisker & relHum_Precip < first_quart]   # relHum_Precip_Low such that relHum_Precip_Low is between low whisker point and 1/4
    low_whisker_actual = min(relHum_Precip_Low)                    # minimum of these values (becomes the low whisker point)
   
   
   
   
  ##### Use annotate() to create humidity vs precipitation boxplots ######
  plot2 = ggplot() +     # no dataframe because the data is being manually entered
    
    # Do not need to call geom_boxplot() -- we are drawing everything using annotate()
    
    # Use annotate(), geom="segment" to create a line 
    annotate(geom="segment",
             x=0.5,     y=20,      # line starts at (x,y)
             xend=1.4,  yend=55,   # line ends at (xend,yend)
             color = rgb(red=1, green=.4, blue=0)) +   # all red, some green makes orange
    
    # Use annotate(), geom="text" to add text 
    annotate(geom="text",
             x = 1, y = 43,         # text is at (x,y)
             label = "Unicode Text: \u03A9 \u0449 \u2022",  # https://en.wikipedia.org/wiki/List_of_Unicode_characters
             color = "blue",
             family="sans", # windowsFonts() gives your options
             size=8) +
 
    # Use annotate(), geom="rect" to add a rectangle    
    annotate(geom="rect", 
             xmin = 1.1, ymin = 32, # starting x and y coords
             xmax = 1.6, ymax = 51, # ending x and y coords
             alpha = 0.5,    # transparency: 0=fully transparent, 1=fully opaque
             fill= "green",  # background color
             color= "blue",  # outline color
             linetype = 4) + # outline shape
  
    # Need to use scale_x_continuous instead of scale_x_discrete when doing manual boxplots
    scale_x_continuous(breaks = c(0.75, 1.25),       # probably want to change these positions...
                       labels = c("this", "that")) +
    
    theme_classic() + 
    
    labs(title = "Some Stuff",
         subtitle = "goes here",
         x = "and",
         y = "there")
  
  plot(plot2)
}  