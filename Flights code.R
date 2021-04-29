#Importing Flight data 
read.csv('C:/Users/adars/OneDrive/Documents/Flights.csv')

Flights = read.csv('C:/Users/adars/OneDrive/Documents/Flights.csv')

#Exploring data
head(Flights)
tail(Flights)
str(Flights)
nrow(Flights)
ncol(Flights)
names(Flights)

sum(Flights$Final.Price<0) 
#checking if there are any negative finalprice 

#spliting the data in 70-30 ratio for prediction
library(caret)
set.seed(1031)
split = createDataPartition(y=Flights$Final.Price,p = 0.7,list = F,groups = 20)
train = Flights[split,]
test = Flights[-split,]

#data summary
library(skimr)
skim(train)

#simple regression
library(ggplot2)
ggplot(data=train,aes(x=Final.Price,y=Price))+
  geom_point()+
  coord_cartesian(ylim=c(0,2000))

#correlation regression
cor(train$Final.Price,train$Price)
#+1 indicates a perfect positive linear relationship 

ggplot(data=train,aes(x=Final.Price,y=Price))+
  geom_point()+
  geom_smooth(method='lm',size=1.3,color='steelblue3')+
  coord_cartesian(ylim=c(0,2000))

#Estimate 
model1 = lm(Final.Price~Price,data=train)
paste('Final.Price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'Price')

#Predict
anova(model1)

summary(model1)


pred = predict(model1)
data.frame(Final.Price = train$Final.Price, prediction = pred)
#The predicted value is the value of the variable predicted based on the regression analysis.

library(ggplot2)
library(dplyr)
library(colortools)

class(Flights)

#Checking if the date is in date class 
class(Flights$Date)

#Changing the format of date
Flights$Date <- as.Date(Flights$Date, format = "%m/%d/%Y")

#Checking the format of date attribute
class(Flights$Date)

Flights

#Time Series Plotting
(time_plot <- ggplot(Flights, aes(x = Date, y = Final.Price)) +
    geom_line(aes(group=1), colour="orange", size = 2) +
    geom_point(colour="Black")+
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic())
