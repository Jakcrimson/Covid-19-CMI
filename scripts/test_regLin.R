df <- read.csv(file.choose())
df$date = as.Date(df$date)


summary(df$total_cases)

# suppression des valeurs manquantes pour des colonnes donn?es :

rm_na = function(df,list_col){
  for (col in list_col){
    df = df[is.na(df[[col]]) == F,]
  }
  return(df)
}

list_col = c("total_cases", "population", "people_vaccinated", "total_tests", "icu_patients", "reproduction_rate", "total_deaths_per_million" )

df = rm_na(df,list_col)


# normalisation des donn?es pour des colonnes donn?es :

df_norm = as.data.frame(scale(df[,list_col]))

df_norm$total_cases = df$total_cases # rajout de la colonne total_case (variable ? pr?dire)

# s?paration des donn?es en train et test split (0.75 - 0.25)

smp_size <- floor(0.75 * nrow(df_norm))

set.seed(123)
train_ind <- sample(seq_len(nrow(df_norm)), size = smp_size)

train <- df_norm[train_ind, ]
test <- df_norm[-train_ind, ]


# mod?lisation avec les donn?es d'entrainnement
model = lm(total_cases ~ population + people_vaccinated + total_tests +  icu_patients  + reproduction_rate  + total_deaths_per_million , train   )

summary(model)


# pr?diction sur les donn?es de test
pred = predict(model, test)

# calcul du RMSE 
rmse = sqrt(mean((test$total_cases - pred)^2))
rmse

# plot des prediction du mod?le pour les donn?es de test et des valeurs r?elles

plot(pred)
points(test$total_cases,col="red")


saveRDS(model, file = "rds/LR.rds")
