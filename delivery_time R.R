dt <- read.csv(file.choose())
View(dt)
summary(dt)
ggplot(dt,aes(x=dt$Sorting.Time))+geom_dotplot(color="blue")
plot(dt$Sorting.Time)
ggplot(dt,aes(dt$Sorting.Time))+geom_boxplot(color="black",fill="blue")+coord_flip()
ggplot(dt,aes(dt$Sorting.Time))+geom_histogram(color="black",fill="green")
qqplot(dt$Sorting.Time,dt$Delivery.Time)
qqnorm(dt$Sorting.Time)
qqline(dt$Sorting.Time)
plot(dt$Sorting.Time,dt$Delivery.Time)
cor(dt$Sorting.Time,dt$Delivery.Time)
#83% is a good coorelation score
#performing liner model
model <- lm(dt$Delivery.Time~dt$Sorting.Time)
model
summary(model)
#getting the ADJUSTED  R^2 Value of 67%
#transformation of independent variable
#1>log()
model1 <- lm(dt$Delivery.Time~log10(dt$Sorting.Time))
summary(model1)
#getting the ADJUSTED  R^2 Value of 68%
#2>square()
model2 <- lm((dt$Delivery.Time)~(dt$Sorting.Time)^2)
summary(model2)
#GETTING THE ADJUSTED R^2 VALUE OF 67%
#3>SQRT()
model3 <- lm(dt$Delivery.Time~sqrt(dt$Sorting.Time))
summary(model3)
#GETTING THE ADJUSTED R^2 VALUE OF 68%
#4>log(dt$Delivery.Time)
model4 <- lm(log(dt$Delivery.Time)~dt$Sorting.Time)
summary(model4)
#getting an adjusted r^value of 80%
confint(model4)
predict(model4,interval="predict")
plot(model4)

