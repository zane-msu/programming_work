# Load in data
ccv17 <- X2017_CCV_Sales
ccv18 <- X2018_CCV_Sales
ccv19 <- X2019_CCV_Sales
ccv_totals <- total_ccv_sales

# make a total dataframe w all of them to plot by year

library(ggplot2)
library(reshape2)
library(tidyverse)
library(dplyr)

ccv17_melt = melt(ccv17, "Month")
ccv17_plot <- ggplot(data = ccv17_melt, aes(x = Month, y = value, color = variable)) + geom_point()
ccv17_plot

ccv18_melt = melt(ccv18, "Month")
ccv18_plot <- ggplot(data = ccv18_melt, aes(x = Month, y = value, color = variable)) + geom_point()
ccv18_plot


ccv17_long <- ccv17 %>%
  pivot_longer(
    cols = c('Cigarettes','Vaping','Cigars')
  )







