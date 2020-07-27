{
  source(file="scripts/reference.R");  
  weatherData = read.csv(file="data/LansingNOAA2016-3.csv", 
                         stringsAsFactors = FALSE);
 
  ### Replace "T" in precip column with 0.005 using gsub()
  weatherData$precip2 = gsub(x=weatherData$precip, 
                             pattern="T", 
                             replacement="0.005");
  
  ### Probably want the column to be numeric...
  weatherData$precip3 = as.numeric(gsub(x=weatherData$precip, 
                             pattern="T", 
                             replacement="0.005"));
  
  ### Plot humidity vs. precip2 (precip2 is string value -- this will cause problems!) 
  thePlot = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=relHum, y=precip2)) +
    theme_bw() +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity",
         y = "Precipitation (string/character/factor/categorical values)");
  plot(thePlot);


  ### Plot humidity vs. precip3 (much better)  
  thePlot = ggplot(data=weatherData) +
    geom_point(mapping=aes(x=relHum, y=precip3)) +
    theme_bw() +
    labs(title = "Relative Humidity vs. Precipitation",
         subtitle = "Lansing, Michigan: 2016",
         x = "Humidity",
         y = "Precipitation (numeric values)");
  plot(thePlot);
  
  # First 10 days of precipitaion (using precip3)
  precipAmount = 0;   # starts at zero (this is sometimes called a "state" variable)

  ## show with debugger values going up  
  for(i in 1:10)
  {
    precipAmount = precipAmount + weatherData$precip3[i]; 
  }
  
  # February precipitation take 1
  FebPrecip = 0;
  for(i in 32:60)
  {
    FebPrecip = FebPrecip + weatherData$precip3[i]; 
  }
  
  # February precipitation take 2
  FebPrecip2 = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(i >= 32 & i <= 60)      # & vs && (| vs ||)
    {
      FebPrecip2 = FebPrecip2 + weatherData$precip3[i]; 
    }
  }
  
  # February precipitation take 3
  FebPrecip3 = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="02-"))
    {
      FebPrecip3 = FebPrecip3 + weatherData$precip3[i]; 
    }
  }
  
  # 1) Using one for loop:
  #    Find the total rainfall in April, July, and November
  #    - you will need three state variables
  
  AprilPrecip = 0;
  JulyPrecip = 0;
  NovPrecip = 0;
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="04-"))
    {
      AprilPrecip = AprilPrecip + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="07-"))
    {
      JulyPrecip = JulyPrecip + weatherData$precip3[i]; 
    }
    if(grepl(x=weatherData$date[i], pattern="11-"))
    {
      NovPrecip = NovPrecip + weatherData$precip3[i]; 
    }
  }
  
  
  # 2) Using one for loop:
  #    Find the total rainfall for the first three months and the last three months
  #    - you can extend the grep pattern (e.g., pattern="RA|SN|FG"  
  #                                             looks for rain, snow, or fog)
  
  JanFebMarchPrecip = 0;
  OctNovDecPrecip = 0;
  
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="01- | 02- | 03-"))
    {
      JanFebMarchPrecip = JanFebMarchPrecip + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="10- | 11- | 12-"))
    {
      OctNovDecPrecip = OctNovDecPrecip + weatherData$precip3[i]; 
    }
  }
  
  
  # 3) Using one for loop:
  #    Find the month with the most amount of rain
  
  Precip = numeric(12)
  
  for(i in 1:length(weatherData$precip3))
  {
    if(grepl(x=weatherData$date[i], pattern="01-"))
    {
      Precip[1] = Precip[1] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="02-"))
    {
      Precip[2] = Precip[2] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="03-"))
    {
      Precip[3] = Precip[3] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="04-"))
    {
      Precip[4] = Precip[4] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="05-"))
    {
      Precip[5] = Precip[5] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="06-"))
    {
      Precip[6] = Precip[6] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="07-"))
    {
      Precip[7] = Precip[7] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="08-"))
    {
      Precip[8] = Precip[8] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="09-"))
    {
      Precip[9] = Precip[9] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="10-"))
    {
      Precip[10] = Precip[10] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="11-"))
    {
      Precip[11] = Precip[11] + weatherData$precip3[i]; 
    }
    else if(grepl(x=weatherData$date[i], pattern="12-"))
    {
      Precip[12] = Precip[12] + weatherData$precip3[i]; 
    }
    monthPrecipMax = which.max(Precip)
    }
  
  
  
   
}  

