library(klaR)
library(ISLR)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(cowplot)
attach(df)

summary(df)
plot(df)
head(df)

##An Overview of Classification
p1 <- ggplot(df, aes(x=balance, y=income, color=default)) + geom_point() + labs(x="Balance", y="Income")+theme_minimal()
p2 <- ggplot(df, aes(x=default, y=balance, color=default))+geom_boxplot() + labs(x="Default", y="Balance")+theme_minimal()
p3 <- ggplot(df, aes(x=default, y=income, color=default))+geom_boxplot() + labs(x="Default", y="Income")+theme_minimal()
plot_grid(p1,p2, p3)

##Logistic Regression
glm.balance <- glm(default ~ balance, data = Default, family = "binomial")
summary(glm.balance)

###Making Predictions
new <- data.frame(balance=c(1000))
predict(glm.balance, newdata = new, type="response")

glm.student <- glm(default ~ student, data = Default, family = "binomial")
summary(glm.student)

new2 <- data.frame(student=c("Yes", "No"))
new2_predict <- predict(glm.student, newdata = new2, type="response")
names(new2_predict)[names(new2_predict) == "1"] <- "Yes"
names(new2_predict)[names(new2_predict) == "2"] <- "No"
new2_predict

##Multiple Logistic Regression
glm.fits <- glm(default ~ balance + income + student, data = Default, family = "binomial")
summary(glm.fits)

predict.student = predict(glm(default~student+balance, data=Default, family = "binomial"), type = "response")

plot1 <- ggplot(df, aes(x = balance,y = predict.student,color = student)) +geom_line()  + labs(x = "Credit Card Balance", y="Default Rate") +theme_minimal()
plot2 <- ggplot(data = df, aes(x = student, y = balance,color = student)) + geom_boxplot() + labs(x = "Student Status", y = "Credit Card Balance")+theme_minimal()

plot_grid(plot1, plot2)

## Linear Discriminant Analysis
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
train <- df[ind==1,]
dim(train)
test <- df[ind==2,]

library(MASS)
lda.fit <- lda(default ~., train)
lda.fit
plot(lda.fit)
ggplot(train,  aes(x=income, fill=student)) + 
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') + 
  scale_fill_manual(values=c("#69b3a2", "#404080"))

## Predict and get the error of the model for testing observation
lda.pred <- predict(lda.fit, test, type="response")
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, test$default, dnn = c("Pridected default status","True default status"))
(1-mean(lda.class == test$default))*100

pred.02 = ifelse(lda.pred$posterior[, "Yes"] >= .2, "Yes", "No")
table(pred.02, test$default, dnn = c("Pridected default status","True default status"))

## Quadratic Discriminant Analysis
qda.fit <- qda(default ~. , data = train)
qda.fit
qda.pred <- predict(qda.fit, test, type="response")
names(qda.pred)
qda.class <- qda.pred$class
table(qda.class, test$default, dnn = c("Pridected default status","True default status"))
(1-mean(qda.class == test$default))*100

## Naive Bayes
library(e1071)
nb.fit <- naiveBayes(default ~. , data = train)
nb.fit
nb.pred <- predict(nb.fit, test, type = c("class", "raw"))
names(nb.pred)
nb.class <- nb.pred
table(nb.class, test$default, dnn = c("Pridected default status","True default status"))
(1-mean(nb.class == test$default))*100
