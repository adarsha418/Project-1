#Importing Hotel data 
read.csv('C:/Users/adars/OneDrive/Desktop/Hotel.csv')

hotel = read.csv('C:/Users/adars/OneDrive/Desktop/Hotel.csv')


#Installing Packages
install.packages('dplyr')
install.packages('colortools')


library(ggplot2)
library(dplyr)
library(colortools)

class(hotel)

#Checking if the date is in date class 
class(hotel$Date)

#Changing the format of date
hotel$Date <- as.Date(hotel$Date, format = "%m/%d/%Y")

#Checking the format of date attribute
class(hotel$Date)

hotel

#Time Series Plotting
(time_plot <- ggplot(hotel, aes(x = Date, y = Final.Price)) +
    geom_line(aes(group=1), colour="black", size = 1) +
    geom_point(size=1, colour="#CC0000")+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())


