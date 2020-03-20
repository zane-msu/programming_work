## FINAL PROJECT ##
## ZANE SHANGO, NICOLE JEDDING ##

# load in necessary packages
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(maps) 
library(reshape2) # needed for melting the dataframe
library(zipcode) # gives the longitude and latitude of the center of each US zipcode
data(zipcode) # this sets the zipcode package as data

# load in data
three_test_lead <- read.csv("flint_water_data.csv") # Flint water data
three_test_lead$Ward <- NULL # get rid of ward column to simplify dataframe
three_test_lead$Notes <- NULL # get rid of mostly empty notes column
three_test_lead$SampleID <- NULL # get rid of sampleID

# turn Zip.Code column to character string, useful for plotting
three_test_lead$Zip.Code <- as.character(three_test_lead$Zip.Code)

# set county and mi subsets to map Michigan's counties
county <- map_data("county") #  US counties
mi <- subset(county,region=="michigan") # limits the counties to just michigan counties

## CREATE MAP OF MI COUNTIES TO SHOW WHERE GENESSEE COUNTY IS ##
mi_county <- ggplot(data=mi,aes(x=long, y=lat)) + 
  geom_polygon(aes(group=group), color = "black", fill=NA) + 
  coord_fixed(1.3)

# fill in Genesee county
subsetCounty <- c("genesee") 
county2 <- subset(mi, subregion %in% subsetCounty)

# new map with Genesee country filled in
genesee_county <- mi_county + 
  geom_polygon(data = county2, aes(x=long, y=lat, group=group),fill = "green", color = "black") + 
  coord_fixed(1.3) 

## NOW BEGIN WORKING WITH THREE TEST LEAD DATA##
# find unique zip code values for three test lead
flint_zipcodes <- unique(three_test_lead$Zip.Code) 

# create plots showing the lead levels for the 1st test, a test 45 seconds after flushing, and a test 2 minutes after flushing
# hjust of .5 in order to center the plot's title

# plot for the first test, done with initial water sample 
first_draw <- ggplot(three_test_lead,aes(x=Zip.Code, y=Pb.Bottle.1..ppb....First.Draw )) + geom_point() + 
  ggtitle("First Draw") + theme(plot.title = element_text(hjust = .5))
# average lead level for first draw
avg_first_test <- mean(three_test_lead$Pb.Bottle.1..ppb....First.Draw)


# plot for second test, done with water sample 45 seconds after flushing, set range up to 300, so it will
# not plot the outlier value over 1,000 (I believe it was entered incorrectly)
forty_five <- ggplot(three_test_lead, aes(x=Zip.Code, y=Pb.Bottle.2..ppb....45.secs.flushing)) + geom_point() + 
  ggtitle("45 Seconds after Flushing") + theme(plot.title = element_text(hjust = .5)) + ylim(0,300)
# average lead level for 45 sec test
avg_45sec_test <- mean(three_test_lead$Pb.Bottle.2..ppb....45.secs.flushing)

# plot for the third test, done with water sample 2 minutes after flushing
two_min <- ggplot(three_test_lead, aes(x=Zip.Code, y=Pb.Bottle.3..ppb....2.mins.flushing)) + geom_point() + 
  ggtitle("2 Minutes after Flushing") + theme(plot.title = element_text(hjust = .5))
# average lead level for 2 min test
avg_2min_test <- mean(three_test_lead$Pb.Bottle.3..ppb....2.mins.flushing)

# all of these on one plot 
# melt the dataframe, set the identification variable as the zipcode, and the variable we're interested
# in as which test it is, the value is the lead level result from each test
flint_melt <-  melt(three_test_lead ,  id.vars = 'Zip.Code', variable.name = 'test') 
# make a plot with the melted dataframe that colors the dots according to which test was done 
# the horizontal line with the annotation of Unhealthy represents the level of lead that is considered
# unhealthy in tap water, where action needs to be taken (15 ppb)
melt_plot <- ggplot(flint_melt,aes(x=Zip.Code, y=value, color = test)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Test Results by Zipcode") +
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Zip Code") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "         Unhealthy", vjust = -0.5)

# plots for each zipcode
# these will show all of the observations for each zipcode, the x axis will be each test, y axis the results
# the unhealthy horizontal line is on these as well to put the unhealthy level into perspective
five_zero_two <- subset(flint_melt, Zip.Code == 48502)
five_zero_two_plot <- ggplot(five_zero_two, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48502 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level for 48502
avg_502_alltests <- mean(five_zero_two$value)

five_zero_three <- subset(flint_melt, Zip.Code == 48503)
five_zero_three_plot <- ggplot(five_zero_three, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48503 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level for 48503
avg_503_alltests <- mean(five_zero_three$value)

five_zero_four <- subset(flint_melt, Zip.Code == 48504)
five_zero_four_plot <- ggplot(five_zero_four, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48504 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level for 48504
avg_504_alltests <- mean(five_zero_four$value)

five_zero_five <- subset(flint_melt, Zip.Code == 48505)
five_zero_five_plot <- ggplot(five_zero_five, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48505 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level fro 48505
avg_505_alltests <- mean(five_zero_five$value)

five_zero_six <- subset(flint_melt, Zip.Code == 48506)
five_zero_six_plot <- ggplot(five_zero_six, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48506 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level for 48506
avg_506_alltests <- mean(five_zero_six$value)

five_zero_seven <- subset(flint_melt, Zip.Code == 48507)
five_zero_seven_plot <- ggplot(five_zero_seven, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48507 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
avg_507_alltests <- mean(five_zero_seven$value)

five_two_nine <- subset(flint_melt, Zip.Code == 48529)
five_two_nine_plot <- ggplot(five_two_nine, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48529 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level for 48529
avg_529_alltests <- mean(five_two_nine$value)

five_three_two <- subset(flint_melt, Zip.Code == 48532)
five_three_two_plot <- ggplot(five_three_two, aes(x=test, y=value)) + geom_point() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("48532 Zipcode") + 
  theme(plot.title = element_text(hjust = .5)) + ylab("Lead Pb (ppb)") + xlab("Test") + ylim(0,300) +
  geom_hline(yintercept=15, color = "purple") + annotate("text", x ="", y = 15, label = "Unhealthy", vjust = -0.5)
# average lead level for 48532
avg_532_alltests <- mean(five_three_two$value)

## MAP ##
# subset zipcode package data by the zipcodes corresponding to the flint_date
flint_zips <- subset(zipcode, zip == 48504 | zipcode$zip == 48507 | zipcode$zip == 48505 | zipcode$zip == 48503 | zipcode$zip == 48506 | zipcode$zip == 48529 | zipcode$zip == 48532 | zipcode$zip == 48502)

avg_lead <- read.csv("flint_water_data2.csv")

avg_lead_raw <- read.csv("flint_water_data2.csv") # Flint water data
avg_lead <- avg_lead_raw
avg_lead <- na.omit(avg_lead) #remove missing info

# find unique zipcodes in avg_lead data
flint_zipcodes <- unique(avg_lead$Zip.Code)




#calculate the average lead levels from our data of each zip code for the graph
#calculate the average lead levels from our data of each zip code for the graph
avg_lead$Lead..ppb. <- as.numeric(avg_lead$Lead..ppb.)  #convert the column to numerics

avg502 <- mean( avg_lead[avg_lead$Zip.Code == 48502, ]$Lead..ppb. )
avg503 <- mean( avg_lead[avg_lead$Zip.Code == 48503, ]$Lead..ppb. )
avg504 <- mean( avg_lead[avg_lead$Zip.Code == 48504, ]$Lead..ppb. )
avg505 <- mean( avg_lead[avg_lead$Zip.Code == 48505, ]$Lead..ppb. )
avg506 <- mean( avg_lead[avg_lead$Zip.Code == 48506, ]$Lead..ppb. )
avg507 <- mean( avg_lead[avg_lead$Zip.Code == 48507, ]$Lead..ppb. )
avg529 <- mean( avg_lead[avg_lead$Zip.Code == 48529, ]$Lead..ppb. )
avg532 <- mean( avg_lead[avg_lead$Zip.Code == 48532, ]$Lead..ppb. )

average_pb <- c(avg502, avg503, avg504, avg505, avg506, avg507, avg529, avg532) #concatenate the averages in a single column

flint_zips$Pb <- average_pb #add the average lead level column to the zipcode dataset

# draw a plot of Genesee county with the center of the zip codes labeled with the magnitude of average lead levels
Pb_zip_map <- ggplot( ) +
  geom_polygon( data = county2, aes( x = long, y = lat), fill = "lightgreen", size = 1) + # plot the county
  geom_point( data = flint_zips, aes (x = longitude, y = latitude, fill = zip, size = Pb), color = "black", pch=21, position = position_dodge(.9)) + #plot the zip code points
  geom_text(data = flint_zips, aes(x = longitude, y = latitude, label=zip),hjust=1, vjust=-1, check_overlap = T) + #add zip labels
  coord_fixed(1.3) +
  theme_void() +
  ggtitle("City of Flint in Genesee County") +
  theme( plot.title = element_text(face = 'bold', size = 18, hjust = 0.5)) +
  guides( fill = FALSE) + #delete the color of point legend
  theme(legend.position = "left", legend.text =  element_text(size = 14), legend.title = element_text(size = 18) )

Pb_zip_map






