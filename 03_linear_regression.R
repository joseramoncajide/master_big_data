##########################################################################
# Jose Cajide - @jrcajide
# Master Big Data: Supervised Regression
##########################################################################

# Packages
list.of.packages <- c("R.utils", "tidyverse", "modelr", "broom", "ISLR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(tidyverse)  # data manipulation and visualization
library(modelr)     # provides easy pipeline modeling functions
library(broom)      # helps to tidy up model outputs

# Load data 
advertising <- read_csv('data/advertising.csv')
advertising

# Reemplazar con dplyr
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(advertising), replace = T, prob = c(0.6,0.4))
train <- advertising[sample, ]
test <- advertising[!sample, ]

# Y = β0 + β1X +ϵ

model1 <- lm(sales ~ TV, data = train)
summary(model1)
# Assessing Coefficients
tidy(model1) %>% select(estimate) %>% mutate(estimate = estimate * 1000)
# Assessing Our Model Visually
ggplot(train, aes(TV, sales)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(se = FALSE, color = "red")

model1_results <- augment(model1, train)

ggplot(model1_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")

# Making Predictions
(test <- test %>% 
    add_predictions(model1))


# out-of-sample mean squared error (MSE)
# test MSE
test %>% 
  add_predictions(model1) %>%
  summarise(MSE = mean((sales - pred)^2))

# training MSE
train %>% 
  add_predictions(model1) %>%
  summarise(MSE = mean((sales - pred)^2))


# Multiple Regression -----------------------------------------------------

model2 <- lm(sales ~ TV + radio + newspaper, data = train)
summary(model2)
tidy(model2) %>% select(estimate) %>% mutate(estimate = estimate * 1000)
# Assessing Model Accuracy
list(model1 = broom::glance(model1), model2 = broom::glance(model2))
# Assessing Our Model Visually

# add model diagnostics to our training data
model1_results <- model1_results %>%
  mutate(Model = "Model 1")

model2_results <- augment(model2, train) %>%
  mutate(Model = "Model 2") %>%
  rbind(model1_results)

ggplot(model2_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Model) +
  ggtitle("Residuals vs Fitted")

# Making Predictions

test %>%
  gather_predictions(model1, model2) %>%
  group_by(model) %>%
  summarise(MSE = mean((sales-pred)^2))


# Incorporating Interactions ----------------------------------------------

# option A
model3 <- lm(sales ~ TV + radio + TV * radio, data = train)

# option B
model3 <- lm(sales ~ TV * radio, data = train)

summary(model3)
tidy(model3) %>% select(estimate) %>% mutate(estimate = estimate * 1000)

# Assessing Model Accuracy

list(model1 = broom::glance(model1), 
     model2 = broom::glance(model2),
     model3 = broom::glance(model3))

# Assessing Our Model Visually
model3_results <- augment(model3, train) %>%
  mutate(Model = "Model 3") %>%
  rbind(model2_results)

ggplot(model3_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Model) +
  ggtitle("Residuals vs Fitted")

# Making Predictions

test %>%
  gather_predictions(model1, model2, model3) %>%
  group_by(model) %>%
  summarise(MSE = mean((sales-pred)^2))

# Additional Considerations
# Linear regression models assume a linear relationship between the response and predictors

# Transformations


auto <- ISLR::Auto

ggplot(auto, aes(horsepower, mpg)) +
  geom_point() +
  geom_smooth(method = "lm")

model4 <- lm(mpg ~ horsepower + I(horsepower^2), data = auto)
summary(model4)

ggplot(auto, aes(horsepower, mpg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

auto <- auto %>% 
  mutate(log_mpg=log(mpg))

model5 <- lm(log_mpg ~ horsepower, data = auto)
summary(model5)
 

