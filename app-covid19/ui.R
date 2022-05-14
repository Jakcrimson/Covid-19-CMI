# ================
# Librairies
# ================
library(shiny)
library(dplyr)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(purrr)
library(htmlTable)
library(psych)
library(data.table)
library(shinyjs)
library(shinydashboard)
library(zoo)


# ====================================
# prepa analyse
# ====================================

#df_page1 = read.csv('./data/owid-covid-data.csv')
df_page1 = read.csv('./data/dataset_updated.csv')
#df =read.csv('./data/dataset_updated.csv')
df_page1$date = as.Date(df_page1$date)

varNames = names(df_page1)

varNames = varNames[varNames!="iso_code" & varNames!="continent" & varNames!="location" & varNames!="date" & varNames!="continent"]


# ====================================
# prepa modelisation
# ====================================

# =============
# donnees
# =============

#df <- read.csv("Documents/FAC/2021_2022/PROJET_CMI/datasets/owid-covid-data.csv")
df = read.csv('./data/owid-covid-data.csv')
# x = RCurl::getURL("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv")
# df = read.csv(text = x, sep = )
# 
# urlfile<-'https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv'
# dsin<-read.csv(url(urlfile))



# function to remove rows with NA values
rm_na = function(df,cols){
  
  for (i in 1:length(cols)){
    df = df[!is.na(df[,cols[i]]),]
  }
  return(df)
}

# function to clean df and keep cleaned columns
clean_df = function(df){
  # date format
  df$date = as.Date(df$date)
  
  # columns to remove NA values
  list_col = c('date','location', "population", "people_vaccinated", "total_tests", "icu_patients", "reproduction_rate", "total_deaths_per_million", "total_cases" )
  #list_col = c('date','location', "population", "people_vaccinated", "total_tests", "reproduction_rate", "total_deaths_per_million", "total_cases" )
  # removing rows with NA values
  df = rm_na(df,list_col)
  
  return(df[,c(list_col)])
}

# function to encode categorical variable
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  return(x)
}

# function to perform feature engineering
new_features_df = function(df){
  
  # time variables
  df$year = as.numeric(format(df$date, '%Y'))
  df$month = as.numeric(format(df$date, '%m'))
  df$day = as.numeric(format(df$date, '%d'))
  
  # encoding location variable
  df$location_c = encode_ordinal(df$location)
  
  return(df)
}

# function to normalize a dataframe
normalisation = function(df,list_col){
  for(i in 1:length(list_col)){
    df[,c(list_col[i])] = (df[,c(list_col[i])] - mean(df.orig_new_features[,c(list_col[i])]))/sd(df.orig_new_features[,c(list_col[i])])
  }
  return(df)
}

# function to get df with a subset of normalized columns
get_df_norm = function(df){
  # columns to normalize
  list_col = c('year','month','day','location_c', "population", "people_vaccinated", "total_tests", "icu_patients", "reproduction_rate", "total_deaths_per_million" )
  
  df_norm = normalisation(df, list_col)
  
  
  return(df_norm)
}


df = clean_df(df)
df.orig = df # used as not normalized data for the extend prediction

df = new_features_df(df)

df.orig_new_features = df # used as not normalized data for the extend prediction

df_norm = get_df_norm(df)


# function to extrapolate new NA values in dataframe
extrapolation = function(x){
  na.spline(x)
}

# function to calculate the RSME
get_RMSE = function(df_norm, model){
  
  # train/test split
  smp_size <- floor(0.75 * nrow(df_norm))
  set.seed(123)
  train_ind <- sample(seq_len(nrow(df_norm)), size = smp_size)
  
  train <- df_norm[train_ind, ]
  test <- df_norm[-train_ind, ]
  
  # predictions on test set
  pred = predict(model, test[,c("year","month", "day", "location_c", "population", "people_vaccinated", "total_tests",  "icu_patients", "reproduction_rate", "total_deaths_per_million")])
  
  # calculation of RMSE 
  rmse = sqrt(mean((test$total_cases - pred)^2))
  return(rmse)
}



target_variables = c('total_cases','total_deaths')
LR <- readRDS('./data/LR.rds')
models = c('LR','SVM','RF')

model = LR


# ========
# ui
# ========
ui <- dashboardPage(skin = 'blue',
                    
                    dashboardHeader(
                      title = "Covid-19",titleWidth = 250
                    ),
                    
                    dashboardSidebar(sidebarMenu(
                                                 disable = FALSE,
                                                 menuItem("Descriptive Analysis", tabName = "analyse", icon = icon('list'), selected = 1 ),
                                                 menuItem("Modelisation", tabName = "model", icon = icon('table')),
                                                 menuItem("Our Project", tabName = 'project', icon = icon('signature'))
                    )), 
                    
                    
                    body = dashboardBody(
                      tabItems(
                        # first tabItem for analysis
                        tabItem(tabName = "analyse",
                                # Sidebar layout
                                sidebarLayout(
                                  sidebarPanel(
                                    dateRangeInput(
                                      'dateRange',
                                      label = paste('Date range'),
                                      start = "2020-01-01", end = "2022-02-02",
                                      min = "2020-01-01", max = "2022-02-02",
                                      separator = " to ", format = "yyyy-mm-dd",
                                      startview = 'month', weekstart = 1
                                    ),
                                    selectInput(
                                      'variable_options', 
                                      'variable options', 
                                      choices = varNames, 
                                      selected = "total_cases"
                                    ),
                                    
                                    selectInput(
                                      'location_options', 
                                      'location options', 
                                      choices = unique(df_page1$location),
                                      selected = 'France',
                                      multiple = T
                                    ),
                                    selectInput(
                                      'plot_choice', 
                                      'plot choice', 
                                      choices = c("line","histogram","boxplot","violin"),
                                      selected = "line"
                                    )
                                    
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel('Plot',uiOutput(outputId = "plot")),
                                      tabPanel('Table',DT::DTOutput('table_timeSeries')),
                                      tabPanel('Summary',tableOutput('summary_var'))
                                    ),
                                    
                                    plotly::plotlyOutput(outputId = "map"),
                                    div(
                                      
                                      sliderInput("date_map",
                                                  "Date:",
                                                  min = as.Date("2020-01-01","%Y-%m-%d"),
                                                  max = as.Date("2022-02-02","%Y-%m-%d"),
                                                  value=as.Date("2022-01-01"),
                                                  timeFormat="%Y-%m-%d"))
                                  )
                                  #div(plotlyOutput(outputId = "histogram"), style = "margin-top: 50px"),
                                  #div(DT::dataTableOutput(outputId = "dataTable"), style = "margin-top: 50px")
                                  
                                )
                                
                                ),
                        
                        
                        
                        # 2nd tabitem for modeling
                        tabItem(tabName = 'model',
                                titlePanel(
                                  h1("Modeling and predicting the evolution of COVID-19", style = "padding-bottom: 20px")
                                ),
                                selectInput(
                                  'target', 
                                  'target variable', 
                                  choices = target_variables, 
                                  selected = "total_cases"
                                ),
                                # Sidebar layout
                                uiOutput(
                                  outputId = 'InputUI'
                                ) 
                        ),
                        
                        tabItem(tabName = 'project',
                                h1('Machine Learning for modeling and predicting the evolution 
                                of the COVID-19 epidemic at the global level')
                                )
                      )
                    ),
)
