library(readr)
library(caret)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dataset_updated <- read_csv("./../data/dataset_updated.csv")
View(dataset_updated)


nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
# normalisation ? faire des colonnes 5 ? 26 gr?ce ? la fonction nor
dataset_updated[,5:26] <-  as.data.frame(lapply(dataset_updated[,5:26], nor))



ran <- sample(1:nrow(dataset_updated), 0.75 * nrow(dataset_updated)) 
train <- dataset_updated[ran,] 
##extract testing set
test <- dataset_updated[-ran,]
test

set.seed(1)
model3 <- train(
  total_deaths ~ reproduction_rate+stringency_index+population+
    median_age+gdp_per_capita+cardiovasc_death_rate+diabetes_prevalence,
  data = train,
  method = 'knn',
  preProcess = c("center", "scale")
)
summary(model3)

### Predictions of the model on the data set Test ###

predTest <- predict(model3, test)

head(predTest$class, 5)

### R-Squared, MSE, ... ###

mse=mean((test$total_deaths-predTest)^2)# 0.0001795573

rmse=sqrt(mse) # 0.0133999

mae = mae(test$total_deaths, predTest)# 0.002119819

saveRDS(model3, file = "total_deaths_KNN.rds")


model3 <- train(
  total_cases ~ reproduction_rate+stringency_index+population+
    median_age+gdp_per_capita+cardiovasc_death_rate+diabetes_prevalence,
  data = train,
  method = 'knn',
  preProcess = c("center", "scale")
)


### Predictions of the model on the data set Test ###

predTest <- predict(model3, test)

head(predTest$class, 5)

### R-Squared, MSE, ... ###

mse=mean((test$total_cases-predTest)^2)# 0.0001432582

rmse=sqrt(mse) # 0.01196905

mae = mae(test$total_cases, predTest)# 0.001744544

saveRDS(model3, file = "total_cases_KNN.rds")
