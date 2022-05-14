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

server <- function(input, output, session) {
  
  # =========================
  # analyse
  # =========================
  
  timeSeries_values <-reactive({
    df_page1 %>% 
      filter(
        between(
          date, 
          as.Date(as.character(input$dateRange[1])), 
          as.Date(as.character(input$dateRange[2]))
        )
        &
          location == input$location_options
        
      ) %>% 
      select(c("location","date",input$variable_options))
  })
  
  
  
  
  output$plot = renderUI({
    if(length(input$location_options)>0)
    {
      df_page1_plot = timeSeries_values()
      
      if(input$plot_choice=="line"){
        output$line <- plotly::renderPlotly({
          plot_ly(x=df_page1_plot[,c(2)],y=df_page1_plot[,c(3)], type = 'scatter', mode = 'lines', color = df_page1_plot$location)
          #%>% layout(title = input$variable_options)
        })
      }
      
      else if(input$plot_choice=="histogram"){
        output$hist <- plotly::renderPlotly({
          plot_ly(x=df_page1_plot[,c(3)], type = 'histogram', color = df_page1_plot$location)
          #%>% layout(title = input$variable_options)
        })
      }   
      
      else if(input$plot_choice=="boxplot"){
        output$box <- plotly::renderPlotly({
          plot_ly(x=df_page1_plot[,c(3)], type = 'box', color = df_page1_plot$location)
          #%>% layout(title = input$variable_options)
        })
      } 
      
      else if(input$plot_choice=="violin"){
        output$violin <- plotly::renderPlotly({
          plot_ly(x=df_page1_plot[,c(1)],y=df_page1_plot[,c(3)],split=df_page1_plot[,c(1)], type = 'violin', color = df_page1_plot$location,
                  box = list(
                    visible = T
                  ),
                  meanline = list(
                    visible = T
                  ))
          #%>% layout(title = input$variable_options)
        })
      } 
    }
    
    
  })
  
  
  
  output$table_timeSeries <- DT::renderDT({
    timeSeries_values()
  })
  
  output$map <- plotly::renderPlotly({
    
    df_page12 = df_page1 %>% filter(date == as.Date(as.character(input$date_map))) %>% select(c("location","iso_code",input$variable_options))
    
    plot_ly(
      df_page12, 
      type = 'choropleth',
      locations = df_page12[,c(2)][df_page12[,c(3)]>0],
      z = log(df_page12[,c(3)][df_page12[,c(3)]>0]),
      text = df_page12[,c(1)][df_page12[,c(3)]>0],
      autocolorscale=F,
      reversescale=F,
      colorscale = "cividis") #%>% layout(title = input$variable_options)
    
  })
  
  output$summary_var = renderTable({
    
    var_name = input$variable_options
    #ddply(timeSeries_values(),.(location),summarise, mean = mean(total_cases))
    df_page1_summary = timeSeries_values() %>% select(c("location",input$variable_options)) 
    df_page1_summary = df_page1_summary[!(is.na(df_page1_summary[[var_name]])),]
    
    #assign(var_name, df_page1_summary[[var_name]])
    
    #describeBy(df_page1_summary[[var_name]], df_page1_summary$location, mat = T) 
    
    df_page1_summary %>% group_by(location) %>% summarise_all(funs(mean,sd,min, median, max))
    #tapply(df_page1_summary[[var_name]], df_page1_summary$location, summary)
    #df_page1_summary %>% group_by(location) %>%
    #summarise(mean = mean(as.name(var_name)))
    #summarise(mean = mean(var_name))
    
    #split(.$location) %>% 
    #map(summary)
    
    
  })
  
  
  
  
  
  
  # =========================
  # modelisation
  # =========================
  
  df.orig_curr <-reactive({
    df.orig %>% 
      filter(
        
        location == input$location
        
      )
  })
  
  observeEvent(input$target, ignoreNULL = TRUE, {
    
    ######### ui for the prediction of total_cases
    
    if (input$target == 'total_cases'){
      
      output$InputUI <- renderUI({
        sidebarLayout(
          sidebarPanel(
            selectInput(
              'model', 
              'model', 
              choices = models,
              selected = 'LR'
            ),
            selectInput(
              'location', 
              'location', 
              choices = unique(df_norm$location),
              selected = 'France'
            ),
            numericInput("population", label = 'population', value = 67422000),
            numericInput("people_vaccinated", label = 'people_vaccinated', value = 1240),
            numericInput("total_tests", label = "total_tests", value = 34791440),
            numericInput("icu_patients", label = "icu_patients", value = 2694),
            numericInput("reproduction_rate", label = "reproduction_rate", value = 1.05),
            numericInput("total_deaths_per_million", label = 'total_deaths_per_million', value = 936.208),
            dateInput("date", "date", value = "2021-01-01")
            
          ),
          
          mainPanel(
            div(style="text-align:center;
                                  width:1000px;
                                  height:100px;",
                #strong(textOutput("res_pred_total_cases"))),
                infoBoxOutput("res_pred_total_cases")),
                
                
            selectInput(
              'config_pred_extend_total_cases', 
              'Extend prediction of total_cases', 
              choices = c('1 week', '15 days', '1 month'),
              selected = '1 week'
            ),
            plotly::plotlyOutput(outputId = "plot_pred_extend_total_cases", width = "100%",
                                 height = "500px"),
            plotly::plotlyOutput(outputId = "plot_fit_total_cases", width = "100%",
                                 height = "500px"),
            div(style="text-align:center;
                                  width:1000px;
                                  height:100px;",
                strong(textOutput("RMSE")))
            
          ) 
        )
      }) 
      
    }
    
    ######### ui for the prediction of total_deaths
    
    else if(input$target == 'total_deaths'){
      output$InputUI <- renderUI({
        sidebarLayout(
          sidebarPanel(
            selectInput(
              'model', 
              'model', 
              choices = models,
              selected = 'LR'
            ),
            selectInput(
              'location', 
              'location', 
              choices = unique(df_norm$location),
              selected = 'France'
            ),
            numericInput("population", label = 'population', value = 67422000),
            numericInput("people_vaccinated", label = 'people_vaccinated', value = 1240),
            numericInput("total_tests", label = "total_tests", value = 34791440),
            numericInput("total_deaths_per_million", label = 'total_deaths_per_million', value = 936.208),
            dateInput("date", "date", value = as.Date("2021-01-01"),min = as.Date(min(df_norm[df_norm$location == input$location,]$date)),
                      max = as.Date(max(df_norm[df_norm$location == input$location,]$date)) )
            
            
          ), mainPanel(
            #textOutput("res_pred_total_cases"),
            #plotly::plotlyOutput(outputId = "plot_fit_total_death","Model fitting")
          )
        )
      })
    }
  })
  
  
  
  output$plot_pred_extend_total_cases =  plotly::renderPlotly({
    
    df.orig_curr2 = subset(df.orig_curr(), select = -c(total_cases))
    # we use df.orig_curr2 (not normalized data) to extrapolate the values of the futur inputs :
    # 1) we add the empty rows associated to the length of the extend prediction chosed
    # 2) we add the features for the model
    # 3) we normalize the added rows to use them as inputs for the extend prediction
    if (input$target == 'total_cases'){
      
      if (input$config_pred_extend_total_cases == '1 week'){
        
        df.orig_curr_extend = rbind(df.orig_curr2, data.frame(date = seq(as.Date(input$date),as.Date(input$date)+6,by ="day" ), location = input$location, population = NA, people_vaccinated = NA, total_tests = NA, icu_patients = NA, reproduction_rate = NA, total_deaths_per_million = NA ))
        df.orig_curr_extend = new_features_df(df.orig_curr_extend)
        df.orig_curr_extend <- df.orig_curr_extend %>%mutate_at(.vars = c("population", "people_vaccinated", "total_tests", "icu_patients", "reproduction_rate", "total_deaths_per_million"),extrapolation)
        #df.orig_curr_extend <- df.orig_curr_extend %>%mutate(population = na.approx(population, rule = 2), people_vaccinated = na.approx(people_vaccinated, rule = 2), total_tests = na.approx(total_tests, rule = 2), icu_patients= na.approx(icu_patients, rule = 2), reproduction_rate = na.approx(reproduction_rate, rule = 2), total_deaths_per_million = na.approx(total_deaths_per_million, rule = 2))
        
        #print(tail(df.orig_curr_extend,20))
        df.orig_curr_extend_norm = get_df_norm(df.orig_curr_extend)
        #print(df.orig_curr_extend_norm)
        inputs_pred = tail(df.orig_curr_extend_norm, n = 7)
      }
      else if (input$config_pred_extend_total_cases == '15 days'){
        df.orig_curr_extend = rbind(df.orig_curr2, data.frame(date = seq(as.Date(input$date),as.Date(input$date)+14,by ="day" ), location = input$location, population = NA, people_vaccinated = NA, total_tests = NA, icu_patients = NA, reproduction_rate = NA, total_deaths_per_million = NA ))
        df.orig_curr_extend = new_features_df(df.orig_curr_extend)
        df.orig_curr_extend <- df.orig_curr_extend %>%mutate_at(.vars = c("population", "people_vaccinated", "total_tests", "icu_patients", "reproduction_rate", "total_deaths_per_million"),extrapolation)
        #df.orig_curr_extend <- df.orig_curr_extend %>%mutate(population = na.approx(population, rule = 2), people_vaccinated = na.approx(people_vaccinated, rule = 2), total_tests = na.approx(total_tests, rule = 2), icu_patients= na.approx(icu_patients, rule = 2), reproduction_rate = na.approx(reproduction_rate, rule = 2), total_deaths_per_million = na.approx(total_deaths_per_million, rule = 2))
        
        df.orig_curr_extend_norm = get_df_norm(df.orig_curr_extend)
        inputs_pred = tail(df.orig_curr_extend_norm, n = 15)
        
      }
      else if (input$config_pred_extend_total_cases == '1 month'){
        df.orig_curr_extend = rbind(df.orig_curr2, data.frame(date = seq(as.Date(input$date),as.Date(input$date)+29,by ="day" ), location = input$location, population = NA, people_vaccinated = NA, total_tests = NA, icu_patients = NA, reproduction_rate = NA, total_deaths_per_million = NA ))
        df.orig_curr_extend = new_features_df(df.orig_curr_extend)
        df.orig_curr_extend <- df.orig_curr_extend %>%mutate_at(.vars = c("population", "people_vaccinated", "total_tests", "icu_patients", "reproduction_rate", "total_deaths_per_million"),extrapolation)
        #df.orig_curr_extend <- df.orig_curr_extend %>%mutate(population = na.approx(population, rule = 2), people_vaccinated = na.approx(people_vaccinated, rule = 2), total_tests = na.approx(total_tests, rule = 2), icu_patients= na.approx(icu_patients, rule = 2), reproduction_rate = na.approx(reproduction_rate, rule = 2), total_deaths_per_million = na.approx(total_deaths_per_million, rule = 2))
        
        df.orig_curr_extend_norm = get_df_norm(df.orig_curr_extend)
        inputs_pred = tail(df.orig_curr_extend_norm, n = 30)
      }
      
      
      pred_extend = predict(model, inputs_pred[,])
      
      fig = plot_ly(x=inputs_pred$date, y=pred_extend, type = 'scatter', mode = 'lines+markers')%>%layout(title = 'Extend prediction', plot_bgcolor = "#e5ecf6")
      fig
    }
  })
  
  # Plotting the fit of the model to the data
  output$plot_fit_total_cases =  plotly::renderPlotly({
    if (input$target == 'total_cases'){
      
      pred = predict(model, df_norm[df_norm$location == input$location,])
      
      fig = plot_ly(x=df.orig_curr()$date, y=df_norm[df_norm$location == input$location,]$total_cases, type = 'scatter', mode = 'line',name = "observation")%>%layout(title = 'Fit of the model', plot_bgcolor = "#e5ecf6")
      fig <- fig %>% add_trace(y = ~pred, name = 'model',mode = 'lines') 
      fig
    }
  })
  
  
  output$res_pred_total_cases = renderInfoBox({
    if (input$target == 'total_cases'){
      location_input = df[df$location == input$location,]$location_c[1]
      year = as.numeric(format(as.Date(input$date), '%Y'))
      month = as.numeric(format(as.Date(input$date), '%m'))
      day = as.numeric(format(as.Date(input$date), '%d'))
      
      new_input = data.frame(matrix(data = c(year,month, day, location_input, input$population, input$people_vaccinated, input$total_tests,  input$icu_patients, input$reproduction_rate, input$total_deaths_per_million), nrow = 1, ncol = 10)) 
      colnames(new_input) = c("year","month", "day", "location_c", "population", "people_vaccinated", "total_tests",  "icu_patients", "reproduction_rate", "total_deaths_per_million")
      
      for(i in 1:ncol(new_input)){
        new_input[,i] = (new_input[,i] - mean(df[,c(colnames(new_input)[i])]))/sd(df[,c(colnames(new_input)[i])])
      }
      
      pred = predict(model, newdata = new_input)
      #paste0("The predicted value is: ", as.integer(pred))
      infoBox("The predicted value is: ", paste0(30 + as.integer(pred)),
              color = 'blue', fill = TRUE)
    }
    
  })
  
  # Printing the predicted value of the model based on the choice of the user in the ui
  # output$res_pred_total_cases <- renderText({
  #   if (input$target == 'total_cases'){
  #     location_input = df[df$location == input$location,]$location_c[1]
  #     year = as.numeric(format(as.Date(input$date), '%Y'))
  #     month = as.numeric(format(as.Date(input$date), '%m'))
  #     day = as.numeric(format(as.Date(input$date), '%d'))
  #     
  #     new_input = data.frame(matrix(data = c(year,month, day, location_input, input$population, input$people_vaccinated, input$total_tests,  input$icu_patients, input$reproduction_rate, input$total_deaths_per_million), nrow = 1, ncol = 10)) 
  #     colnames(new_input) = c("year","month", "day", "location_c", "population", "people_vaccinated", "total_tests",  "icu_patients", "reproduction_rate", "total_deaths_per_million")
  #     
  #     for(i in 1:ncol(new_input)){
  #       new_input[,i] = (new_input[,i] - mean(df[,c(colnames(new_input)[i])]))/sd(df[,c(colnames(new_input)[i])])
  #     }
  #     
  #     pred = predict(model, newdata = new_input)
  #     paste0("The predicted value is: ", as.integer(pred))
  #   }
  # })
  
  # printing the RMSE of the model 
  output$RMSE <- renderText({
    rmse = get_RMSE(df_norm, model)
    paste0("LR ","RMSE : ", rmse)
    
  })
  
}

