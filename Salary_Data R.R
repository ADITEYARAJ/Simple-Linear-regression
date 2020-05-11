sala <- read.csv(file.choose())
View(sala)
summary(sala)
var(sala$YearsExperience)
sd(sala$YearsExperience)
var(sala$Salary)
sd(sala$Salary)
ggplot(sala,aes(x=sala$YearsExperience))+geom_dotplot(color="blue")
plot(sala$YearsExperience,col="blue")
ggplot(sala,aes(sala$YearsExperience))+geom_boxplot(color="black",fill="blue")+coord_flip()
ggplot(sala,aes(sala$YearsExperience))+geom_histogram(color="black",fill="green")
qqplot(sala$YearsExperience,sala$Salary)
qqnorm(sala$YearsExperience)
qqline(sala$YearsExperience)
plot(cr$Salary_hike,cr$Churn_out_rate)
cor(sala$YearsExperience,sala$Salary)
#97% is a good coorelation score
#performing liner model
model <- lm(sala$Salary~sala$YearsExperience)
model
summary(model)
#getting the ADJUSTED  R^2 Value of 96%
#transformation of independent variable
#1>log()
model1 <- lm(sala$Salary~log10(sala$YearsExperience))
summary(model1)
#getting the ADJUSTED  R^2 Value of 84%
#2>square()
model2 <- lm((sala$Salary)~(sala$YearsExperience)^2)
summary(model2)
#GETTING THE ADJUSTED R^2 VALUE OF 96%
#3>SQRT()
model3 <- lm(sala$Salary~sqrt(sala$YearsExperience))
summary(model3)
#GETTING THE ADJUSTED R^2 VALUE OF 93%
#4>log()
model4 <- lm(log(sala$Salary)~sala$YearsExperience)
summary(model4)
#getting an adjusted r^value of 93%
confint(model)
predict(model,interval="confidence")
par(mfrow=c(2,2))
plot(model)
library(mvinfluence)
influenceIndexPlot(model)
model5 <- lm(sala$Salary~sala$YearsExperience,data=sala[c(-2,-24),])
summary(model5)
confint(model5)
predict(model5,interval="predict")
