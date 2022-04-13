##########  ########    #######    #########
    #       #           #              #
    #       ####        #######        #
    #       #                 #        #
    #       ########    #######        #

# creating training data set
TKS=c(20,10,30,20,80,30)
CSS=c(90,20,40,50,50,80)
Placed=c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df=data.frame(TKS,CSS,Placed)

# load library
library(neuralnet)

# fit neural network
nn=neuralnet(Placed~TKS+CSS,data=df, hidden=3,act.fct = "logistic",
             linear.output = FALSE)

# plot neural network
# plot(nn)

# creating test set
TKS=c(30,40,85)
CSS=c(85,50,40)
test=data.frame(TKS,CSS)

## Prediction using neural network
Predict=compute(nn,test)
# Predict$net.result

# Converting probabilities into binary classes setting threshold level 0.5
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)


#############
# CASE DATA #
#############

covid = read.csv('D:/2021-2022-UBS/COVID-19-STUDY/dataset_updated.csv')
head(covid)

Samples<-sample(seq(1,3),size=nrow(covid),replace=TRUE,prob=c(0.8,0.2,0.2))
Train<-covid[Samples==1,]
Test<-covid[Samples==2,]
Validate<-covid[Samples==3,]

nn=neuralnet(total_deaths~reproduction_rate+stringency_index+population+median_age+gdp_per_capita+cardiovasc_death_rate+diabetes_prevalence            
             ,data=Train, hidden=5,
             linear.output = FALSE,
             lifesign = 'full',
             rep = 2,
             algorithm = "rprop+",
             stepmax = 100000)
# plot our neural network 
plot(nn, rep = 1)

# error
nn$result.matrix

# Prediction
output <- compute(nn, rep = 1, Test)
head(output$net.result)
