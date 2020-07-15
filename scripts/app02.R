# read in the lines of code from reference.r
source(file="scripts/reference.r");

# read in CSV file and save the content to packageData
packageData = read.csv(file="data/USAccDeaths.csv");

#Create a scatterplot with layers
plotData = ggplot( data=packageData ) + 
  geom_point( mapping=aes(x=time, y=value) ) +
  ggtitle( label="Accidential deaths in the US 1973-1978" ) +
  scale_y_continuous(breaks = seq(from=7000, to=11000, by=2000)) +
  scale_x_continuous( breaks = seq(from=1973, to=1979, by=.5) ) +
  theme( axis.text.x=element_text(angle=45, hjust=1) )
plot(plotData) #no parameter names here

