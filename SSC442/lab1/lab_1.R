#lab 1 
library(gapminder)
library(ggplot2)
library(scales)
gapminder
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point()

a <- ggplot(data = mpg,
            mapping = aes(x = displ, y = hwy,color=class))
a + geom_point()
#1
#the plot does show an intuitive relationship in that a vehicle with a larger engine gets less mpg. This may not be causal as we do not know if other characteristics of the vehicle are similar. 

b <- ggplot(data = mpg,
            mapping = aes(x = class, y = drv))
b + geom_point()
#the plot isn't very easy to read and it is not clear how many points could be on top of one another.

#1b
#The class of the vehicle and engine displacement seem to be related. 
p + geom_point()
p + geom_smooth()
p + geom_point() + geom_smooth() + geom_smooth(method = ...) + geom_smooth(method = ...)
p + geom_point() + geom_smooth() + geom_smooth(method = ...) + geom_smooth(method = ..., color = "red")
p + geom_point() + geom_smooth(method = "lm") + scale_x_log10()

#What does scale_x_log10 do? explain in 2-3 sentences
#scale_c_log10 rescales the x axis based upon a logarithmic scale with log base 10. 
#the data becomes more evenly distributed as much of it was clumped to the left and the log
#rescaling has brought these larger closer to where most of the data is. 

p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::dollar)
p + geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::...)

#what does dollar() do?
#dollar() formats a vector of values as a currency. 

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = 'yellow'))
p + geom_point() + scale_x_log10()

#we can tell ggplot to draw yellow points by specifying color in the geom_point() function
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(color="yellow") + scale_x_log10()

#describe in your words what is going on. One way to avoid such mistakes is to read arguments inside aes(<property> = <variable>)as *the property in the graph is determined by the data in *.
#we have told ggplot to group the data by a column labeled as yellow and apply colors to each unique group in this column
#we corrected this by specifying the color in the geom_point() function instead

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point() + geom_smooth(color = "orange", se = FALSE, size = 8, method = "lm") + scale_x_log10()

#color makes the line orange, se is whether or not a confidence interveral should be displayed arounf smooth
#size changes the thickness of the line. method is the method to create the line such as lm for linear. 

p + geom_point(alpha = 0.3) +
  geom_smooth(method = "gam") +
  scale_x_log10(labels = scales::dollar) +
  labs(x = "GDP Per Capita", y = "Life Expectancy in Years",
       title = "Economic Growth and Life Expectancy",
       subtitle = "Data Points are country-years",
       caption = "Source: Gapminder")

library(scales)
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp, color = continent, fill = continent))
p + geom_point()
p + geom_point() + scale_x_log10(labels = dollar)
p + geom_point() + scale_x_log10(labels = dollar) + geom_smooth()

#What does fill = continent do? What do you think about the match of colors between lines and error bands?
#fill = continent tells ggplot to assign a fill for each continent, these are for the error bands.
#The line colors are a darker shade and the error bands use a lighter shade of the same color. They 
#show a nice contrast while still clearly matching.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) + geom_smooth() + scale_x_log10()

# Notice how the above code leads to a single smooth line, not one per continent. Why?
#the data is not separated by continent in the ggplot mapping as was done previous. 

#What is bad about the following example, assuming the graph is the one we want? Think about why you should set aesthetics at the top level rather than at the individual geometry level if that’s your intent.
#there are multiple lines for each continent. Asthetics should be set as top level so multiple operations do not
#cause confusion.

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap, y = lifeExp))
p + geom_point(mapping = aes(color = continent)) +
  geom_smooth(mapping = aes(color = continent, fill = continent)) +
  scale_x_log10() +
  geom_smooth(mapping = aes(color = continent), method = "gam")

#exercise 2
#library needed for melt function
library(reshape2)
#reads the csv
data <-read.csv("bank.csv",header=TRUE)

#segments data based upon age ranges
data_20 <- data[data$age %in% c(10:29),]
data_30 <- data[data$age %in% c(30:39),]
data_40 <- data[data$age %in% c(40:49),]
data_50 <- data[data$age %in% c(50:59),]
data_60 <- data[data$age %in% c(60:69),]
data_70 <- data[data$age %in% c(70:99),]

#success rate for each age range
success_20 <- sum(data_20$y=="yes")/length(data_20$y)
success_30 <- sum(data_30$y=="yes")/length(data_30$y)
success_40 <- sum(data_40$y=="yes")/length(data_40$y)
success_50 <- sum(data_50$y=="yes")/length(data_50$y)
success_60 <- sum(data_60$y=="yes")/length(data_60$y)
success_70 <- sum(data_70$y=="yes")/length(data_70$y)

#plots success rate for each age range
success <- data.frame(success_20,success_30,success_40,success_50,success_60,success_70)
names(success)<-c("10-29","30-39","40-49","50-59","60-69","70+")
success_melted <- melt(data=success)
success_plot <- ggplot(data=success_melted,aes(x=variable,y=value))
success_plot + geom_bar(stat="identity") +xlab("Age Ranges") +ylab("Success Rate") + 
  ggtitle("Success Rate by Age Range")+theme(plot.title=element_text(hjust=.5))

#duration for each age range
duration_20 <- mean(data_20$duration)
duration_30 <- mean(data_30$duration)
duration_40 <- mean(data_40$duration)
duration_50 <- mean(data_50$duration)
duration_60 <- mean(data_60$duration)
duration_70 <- mean(data_70$duration)

#plots duration for each age range
duration <- data.frame(duration_20,duration_30,duration_40,duration_50,duration_60,duration_70)
names(duration)<-c("10-29","30-39","40-49","50-59","60-69","70+")
duration_melted <-melt(data=duration)
duration_plot <- ggplot(data=duration_melted,aes(x=variable,y=value))
duration_plot + geom_bar(stat="identity") +xlab("Age Ranges") +ylab("Average Duration (seconds)") +
  ggtitle("Duration of Phone Call by Age Ranges")+theme(plot.title=element_text(hjust=.5))

#rate of deposits per hour calling for each age range
rate_20 <- 3600*(sum(data_20$y=="yes")/sum(data_20$duration))
rate_30 <- 3600*(sum(data_30$y=="yes")/sum(data_30$duration))
rate_40 <- 3600*(sum(data_40$y=="yes")/sum(data_40$duration))
rate_50 <- 3600*(sum(data_50$y=="yes")/sum(data_50$duration))
rate_60 <- 3600*(sum(data_60$y=="yes")/sum(data_60$duration))
rate_70 <- 3600*(sum(data_70$y=="yes")/sum(data_70$duration))

#plots rate for each age range
rate <- data.frame(rate_20,rate_30,rate_40,rate_50,rate_60,rate_70)
names(rate)<-c("10-29","30-39","40-49","50-59","60-69","70+")
rate_melted <-melt(data=rate)
rate_plot <- ggplot(data=rate_melted,aes(x=variable,y=value))
rate_plot + geom_bar(stat="identity") +xlab("Age Ranges") +ylab("Deposits per Hour")+
  ggtitle("Number of Deposits per Hour of Phone Call by Age Range")+theme(plot.title=element_text(hjust=.5))


