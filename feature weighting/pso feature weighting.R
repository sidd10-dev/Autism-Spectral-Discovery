#install.packages('pso')
#install.packages('parsnip')
#install.packages('ranger')
#install.packages('caTools')
#install.packages('e1071')
library(e1071)
library('parsnip')
library('ranger')
library('tidyverse')
library('pso')

df <- read.csv("./Data/adult_cleaned.csv")
df %>% ncol()
df <- df %>% 
  mutate(Class.ASD = as.factor(Class.ASD))

#PSO Optimisation

#Defining fitness function
fit_function <- function(x){
  a <- x[1]
  b <- x[2]
  
  #define model
  model <- svm(formula = Class.ASD ~ .,
               data = df,
               type = 'C-classification', 
               cost = a,
               gamma = b)
  
  pred_test <- predict(model, new_data = select(df, -'Class.ASD'))
  cm <- table(pred_test, pull(df, 'Class.ASD'))
  acc <- (cm[1] + cm[4])/nrow(df)
  return(acc)
}

psoptim(par = rep(NA,2), fn = fit_function, lower = c(0.01,0.01), upper = c(10, 10), 
        control = list(maxit = 10, vectorize = T, s = 1))


#c = 1.352863, gamma = 9.898703
model <- svm(formula = Class.ASD ~ .,
             data = df,
             type = 'C-classification', 
             cost = 1.352863,
             gamma = 9.898703)

pred_test <- predict(model, new_data = select(df, -'Class.ASD'))
cm <- table(pred_test, pull(df, 'Class.ASD'))
acc <- (cm[1] + cm[4])/nrow(df)
acc
