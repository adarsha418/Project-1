#Importing Hotel data 
read.csv('C:/Users/adars/OneDrive/Desktop/Hotel.csv')

hotel = read.csv('C:/Users/adars/OneDrive/Desktop/Hotel.csv')

#Installing Packages
install.packages('dplyr')
install.packages('colortools')


library(ggplot2)
library(dplyr)
library(colortools)

#exploring data

head(hotel)

tail(hotel)
str(hotel)
nrow(hotel)
ncol(hotel)
names(hotel)

sum(hotel$Final.Price<0) #checking if there are any negative finalprice 

#spliting the data in 70-30 ratio for prediction
install.packages('caret')
library(caret)
set.seed(1031)
split = createDataPartition(y=hotel$Final.Price,p = 0.7,list = F,groups = 20)
train = hotel[split,]
test = hotel[-split,]

#data summary
install.packages('skimr')
library(skimr)
skim(train)

#simple regression
library(ggplot2)
ggplot(data=train,aes(x=Final.Price,y=Total))+
  geom_point()+
  coord_cartesian(ylim=c(0,1500))

#correlation regression
cor(train$Final.Price,train$Total)
#+1 indicates a perfect positive linear relationship 

ggplot(data=train,aes(x=Final.Price,y=Total))+
  geom_point()+
  geom_smooth(method='lm',size=1.3,color='steelblue3')+
  coord_cartesian(ylim=c(0,2000))

#Estimate 
model1 = lm(Final.Price~Total,data=train)
paste('Final.Price','=',round(coef(model1)[1],0),'+',round(coef(model1)[2],0),'Total')

#Predict
anova(model1)

summary(model1)

pred = predict(model1)
data.frame(Final.Price = train$Final.Price, prediction = pred)
#The predicted value is the value of the variable predicted based on the regression analysis.


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