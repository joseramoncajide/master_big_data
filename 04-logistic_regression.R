##########################################################################
# Jose Cajide - @jrcajide
# Master Big Data: Supervised Classification
##########################################################################

# Packages
library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs
library(ROCR)
library(caret)
library(ISLR)

# Load data 
(default <- as_tibble(ISLR::Default))


default %>% 
  ggplot(aes(x=balance, y=income, color=default, shape=default)) + geom_point() +
  theme_bw() +
  ggtitle("Datos de deuda de clientes") +
  xlab("Balance") +
  ylab("Income")


default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "lm") +
  ggtitle("Linear regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default") +
  theme_bw()

set.seed(161)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.8,0.2))
train <- default[sample, ]
test <- default[!sample, ]


# Simple Logistic Regression ----------------------------------------------

model1 <- glm(default ~ balance, family = "binomial", data = train)

default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default") +
  theme_bw()

# Assessing Coefficients
summary(model1)
# for every one dollar increase in monthly balance carried, the odds of the customer defaulting increases by a factor of 1.0057.
exp(coef(model1))

# Making Predictions
predict(model1, data.frame(balance = c(1000, 2000)), type = "response")


# Multiple Logistic Regression --------------------------------------------

model2 <- glm(default ~ balance + income + student, family = "binomial", data = train)
tidy(model2)

new.df <- data.frame(balance = 1500, income = 40, student = c("Yes", "No"))
predict(model2, new.df, type = "response")


# Validation of Predicted Values ------------------------------------------

predicted_value <- predict(model2,test,type = "response")
predicted_class <- ifelse(predicted_value>0.5, "Yes","No")
performance_data<-data.frame(observed=test$default,
                             predicted= predicted_class)


prediction(predicted_value, test$default) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(colorize=T, main="Curva ROC")

positive <- sum(performance_data$observed=="Yes")
negative <- sum(performance_data$observed=="No")
predicted_positive <- sum(performance_data$predicted=="Yes")
predicted_negative <- sum(performance_data$predicted=="No")
total <- nrow(performance_data)
data.frame(positive, negative,predicted_positive,predicted_negative)

tp<-sum(performance_data$observed=="Yes" & performance_data$predicted=="Yes")
tn<-sum(performance_data$observed=="No" & performance_data$predicted=="No")
fp<-sum(performance_data$observed=="No" & performance_data$predicted=="Yes")
fn<-sum(performance_data$observed=="Yes" & performance_data$predicted=="No")
data.frame(tp,tn,fp,fn)

accuracy <- (tp+tn)/total
error_rate <- (fp+fn)/total
sensitivity <- tp/positive
especificity <- tn/negative
precision <- tp/predicted_positive
npv <- tn / predicted_negative
data.frame(accuracy,error_rate,sensitivity,especificity,precision,npv)


caret::confusionMatrix(predicted_class, test$default, mode="everything",positive="Yes")


# Deploy model ------------------------------------------------------------

saveRDS(model2, file = "model.Rds", compress = TRUE)

