# model.R

#* @get /predict
default <- function(student, balance, income){
  student <- as.character(student)
  balance <- as.numeric(balance)
  income <- as.numeric(income)
  new.df <- data.frame(balance = balance, income = income, student = student)
  trained_model <- readRDS('model.Rds') 
  predict(trained_model, new.df, type = "response")
}

