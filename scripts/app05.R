source( file="scripts/reference.R" )
weatherData = read.csv(file="data/LansingNOAA2016-2.csv", 
                       stringsAsFactors = FALSE)

#### Part 1: Add year to date vector and save back to data frame
theDate = weatherData$date # save date column to vector
theDate = paste(theDate, "-2016", sep="") # append -2016 to vector
theDate = as.Date(theDate, format="%m-%d-%Y"); # format vector as Date
weatherData$dateYr = theDate # save vector to Data Frame

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
       y = "Count")
plot(plotData)

#Create a column in your data frame called biMonth 
#that divides the year into 6 categories: 
#JanFeb, MarApr, MayJun, JulAug, SepOct, and NovDec
# create a biMonth vector that has the same length as theDate vector
biMonth = vector(mode="character", length=length(theDate))

# create date variables for the beginning of each season
JanFebStart = as.Date("01-01-2016", format="%m-%d-%Y")
MarAprStart = as.Date("03-01-2016", format="%m-%d-%Y")
MayJunStart = as.Date("05-01-2016", format="%m-%d-%Y")
JulAugStart = as.Date("07-01-2016", format="%m-%d-%Y")
SepOctStart = as.Date("09-01-2016", format="%m-%d-%Y")
NovDecStart = as.Date("11-01-2016", format="%m-%d-%Y")

#Create a biMonth vector based on theDates vector
for(i in 1:length(theDate)) # go through each date
{
  # if the date falls within Jan or Feb
  if(theDate[i] >= JanFebStart && theDate[i] < MarAprStart)
  {
    biMonth[i] = "JanFeb";
  }
  # if the date falls within Mar or Apr
  else if(theDate[i] >= MarAprStart && theDate[i] < MayJunStart)
  {
    biMonth[i] = "MarApr";
  }
  # if the date falls within May or Jun
  else if(theDate[i] >= MayJunStart && theDate[i] < JulAugStart)
  {
    biMonth[i] = "MayJun";
  }
  # if the date falls within Jul or Aug
  else if(theDate[i] >= JulAugStart && theDate[i] < SepOctStart)
  {
    biMonth[i] = "JulAug";
  }
  # if the date falls within Sep or Oct
  else if(theDate[i] >= SepOctStart && theDate[i] < NovDecStart)
  {
    biMonth[i] = "SepOct";
  }
  # if the date falls within Nov or Dec --
  #using || because dates are not continuous)
  else if(theDate[i] >= NovDecStart || theDate[i] < JanFebStart)
  {
    biMonth[i] = "NovDec";
  }
  else # something went wrong... always good to check
  {
    biMonth[i] = "Error";
  }
}


#create a new column in weatherData called biMonth and set it to the 
#biMonth vector
weatherData$biMonth = biMonth


#Create a histogram for each biMonth with facet grid
plotData = ggplot(data=weatherData) +
  geom_histogram(mapping=aes(x=relHum, y=..count..),
                 bins=40,
                 color="grey20",
                 fill="darksalmon") +
  theme_classic() +
  facet_grid( facet= biMonth ~ .) +
  labs(title = "Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity",
       y = "Number of Days")
plot(plotData)


#find the mean of JanFeb and JulAug rel humidity
data.JanFeb<-weatherData[weatherData$biMonth=="JanFeb",]
avg.JanFeb<-mean(data.JanFeb$relHum)

data.JulAug<-weatherData[weatherData$biMonth=="JulAug",]
avg.JulAug<-mean(data.JulAug$relHum)


#Create a stacked histogram, using fill subcomponent, of 
#average humidity using biMonth
plotData = ggplot(data=weatherData) + 
  geom_histogram(mapping=aes(x=relHum, y=..count.., fill=biMonth),
                 bins=40,
                 color="grey20",
                 position="stack") +
  theme_classic() +
  labs(title = "Humidity Histogram",
       subtitle = "Lansing, Michigan: 2016",
       x = "Relative Humidity",
       y = "Count")+
  #add lines for avg Humidity of JanFeb and JulAug
  geom_vline(mapping=aes(xintercept=avg.JanFeb),
             color="deeppink1",
             size=1.2) +
  geom_vline(mapping=aes(xintercept=avg.JulAug),
             color="darkslateblue",
             size=1.2) 
plot(plotData)





