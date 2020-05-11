cr <- read.csv(file.choose())
View(cr)
summary(cr)
var(cr$Salary_hike)
sd(cr$Salary_hike)
var(cr$Churn_out_rate)
sd(cr$Churn_out_rate)
ggplot(cr,aes(x=cr$Salary_hike))+geom_dotplot(color="blue")
plot(cr$Salary_hike)
ggplot(cr,aes(cr$Salary_hike))+geom_boxplot(color="black",fill="blue")+coord_flip()
ggplot(cr,aes(cr$Salary_hike))+geom_histogram(color="black",fill="green")
qqplot(cr$Salary_hike,cr$Churn_out_rate)
qqnorm(cr$Salary_hike)
qqline(cr$Salary_hike)
plot(cr$Salary_hike,cr$Churn_out_rate)
cor(cr$Salary_hike,cr$Churn_out_rate)
#92% is a good coorelation score
#performing liner model
model <- lm(cr$Churn_out_rate~cr$Salary_hike)
model
summary(model)
#getting the ADJUSTED  R^2 Value of 82%
#transformation of independent variable
#1>log()
model1 <- lm(cr$Churn_out_rate~log10(cr$Salary_hike))
summary(model1)
#getting the ADJUSTED  R^2 Value of 83%
#2>square()
model2 <- lm((cr$Churn_out_rate)~(cr$Salary_hike)^2)
summary(model2)
#GETTING THE ADJUSTED R^2 VALUE OF 81%
#3>SQRT()
model3 <- lm(cr$Churn_out_rate~sqrt(cr$Salary_hike))
summary(model3)
#GETTING THE ADJUSTED R^2 VALUE OF 82%
#4>log()
model4 <- lm(log(cr$Churn_out_rate)~cr$Salary_hike)
summary(model4)
#getting an adjusted r^value of 86%
confint(model4)
predict(model4,interval="predict")
plot(model4)
library(mvinfluence)
influenceIndexPlot(model4)
model5 <- lm(log(cr$Churn_out_rate)~cr$Salary_hike,data=cr[c(-1,-10),])
summary(model5)
confint(model5)
predict(model5,interval="predict")