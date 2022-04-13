df <- read.csv("C:/Users/USER/Downloads/owid-covid-data.csv")
df$date = as.Date(df$date)

varNames = names(df)

varNames = varNames[varNames!="iso_code" & varNames!="continent" & varNames!="location" & varNames!="date" & varNames!="continent"]

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

##########################
##### User interface #####
##########################
ui <- fluidPage(
  
  # Title 
  titlePanel(
    h1("EDA covid-19", style = "padding-bottom: 20px")
  ),
  
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
        choices = unique(df$location),
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

)


###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  
  timeSeries_values <-reactive({
    df %>% 
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
          df_plot = timeSeries_values()
    
        if(input$plot_choice=="line"){
            output$line <- plotly::renderPlotly({
                plot_ly(x=df_plot[,c(2)],y=df_plot[,c(3)], type = 'scatter', mode = 'lines', color = df_plot$location)
   #%>% layout(title = input$variable_options)
                })
          }
      
          else if(input$plot_choice=="histogram"){
            output$hist <- plotly::renderPlotly({
              plot_ly(x=df_plot[,c(3)], type = 'histogram', color = df_plot$location)
              #%>% layout(title = input$variable_options)
            })
          }   
          
          else if(input$plot_choice=="boxplot"){
            output$box <- plotly::renderPlotly({
              plot_ly(x=df_plot[,c(3)], type = 'box', color = df_plot$location)
              #%>% layout(title = input$variable_options)
            })
          } 
          
          else if(input$plot_choice=="violin"){
            output$violin <- plotly::renderPlotly({
              plot_ly(x=df_plot[,c(1)],y=df_plot[,c(3)],split=df_plot[,c(1)], type = 'violin', color = df_plot$location,
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
   
   df2 = df %>% filter(date == as.Date(as.character(input$date_map))) %>% select(c("location","iso_code",input$variable_options))
   
   plot_ly(
     df2, 
     type = 'choropleth',
     locations = df2[,c(2)][df2[,c(3)]>0],
     z = log(df2[,c(3)][df2[,c(3)]>0]),
     text = df2[,c(1)][df2[,c(3)]>0],
     autocolorscale=F,
     reversescale=F,
     colorscale = "cividis") #%>% layout(title = input$variable_options)
   
 })
 
 output$summary_var = renderTable({
   
   var_name = input$variable_options
   #ddply(timeSeries_values(),.(location),summarise, mean = mean(total_cases))
   df_summary = timeSeries_values() %>% select(c("location",input$variable_options)) 
   df_summary = df_summary[!(is.na(df_summary[[var_name]])),]
   
   #assign(var_name, df_summary[[var_name]])
   
   #describeBy(df_summary[[var_name]], df_summary$location, mat = T) 
   
   df_summary %>% group_by(location) %>% summarise_all(funs(mean,sd,min, median, max))
    #tapply(df_summary[[var_name]], df_summary$location, summary)
   #df_summary %>% group_by(location) %>%
   #summarise(mean = mean(as.name(var_name)))
   #summarise(mean = mean(var_name))
   
   #split(.$location) %>% 
   #map(summary)
  
   
 })
}


##################################
##### ShinyApp function #####
##################################
shinyApp(ui = ui, server = server)
