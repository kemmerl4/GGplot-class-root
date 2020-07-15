# execute the lines of code from reference.r
source(file="scripts/reference.r");

# read in CSV file and save the content to packageData
packageData = read.csv(file="data/CRANpackages.csv")

# read in the lines of code from reference.r
source(file="scripts/reference.r");

# read in CSV file and save the content to packageData
packageData = read.csv(file="data/CRANpackages.csv");

#Part 1 create a scatterplot
plotData = ggplot( data=packageData ) + 
  geom_point( mapping=aes(x=Date, y=Packages) )
plot(plotData);

#Part 2 add layers
plotData = ggplot( data=packageData ) + 
  geom_point( mapping=aes(x=Date, y=Packages) ) +
  ggtitle( label="Packages in CRAN (2001-2014)" ) +
  scale_y_continuous( breaks = seq(from=0, to=6000, by=500) ) +
  theme( axis.text.x=element_text(angle=90, hjust=1) )
plot(plotData) #no parameter names here
