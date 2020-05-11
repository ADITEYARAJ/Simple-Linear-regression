wc <- read.csv(file.choose())
View(wc)
summary(wc)
ggplot(wc,aes(wc$Calories.Consumed))+geom_dotplot(color="blue")
library("lattice")
dotplot(wc$Calories.Consumed,main="dotplot calories")
ggplot(wc,aes(wc$Calories.Consumed))+geom_boxplot(color="black",fill="green")+coord_flip()
ggplot(wc,aes(wc$Calories.Consumed))+geom_histogram(color="yellow")
qqnorm(wc$Calories.Consumed)
qqline(wc$Calories.Consumed)
plot(wc$Calories.Consumed,wc$Weight.gained..grams.)
cor(wc$Calories.Consumed,wc$Weight.gained..grams.)
#we get a 95% correlation value.
#performing the linear regression
model <- lm(wc$Weight.gained..grams.~wc$Calories.Consumed)
model
summary(model)
#we get the r square value of 90%.
confint(model,level = 0.95)
predict(model,interval="predict")
plot(model)
#for better accuracy transforming the variable 
#1> log
model1 <- lm(wc$Calories.Consumed~log(wc$Weight.gained..grams.))
summary(model1)
#we se the r square  value decreses by this transformation.
#2>exp()
model2 <- lm(log(wc$Calories.Consumed)~(wc$Weight.gained..grams.))
summary(model2)
#we se the r square  value decreses by this transformation.
#3>square root
model3 <- lm((wc$Calories.Consumed)~sqrt(wc$Weight.gained..grams.))
summary(model3)
#we see that sqrt tranfirmation increases the r squared value
#4>square
model4 <- lm((wc$Calories.Consumed)^2~wc$Weight.gained..grams.)
summary(model4)
#we see that square tranfirmation increases the r squared value.
confint(model4,level = 0.95)
predict(model4,interval="predict")
plot(model4)
