################################################################################
## DATA QUALITY, MERGING DATASET, AND IMPORTING MODEL
################################################################################

#Required Libraries
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate)
library(tree)

#Read in the Client and Loan Data
client <- read_csv("C:/Users/tilin/Desktop/BetterFi/Copy of DataLab-clientdata-anon.csv")
loan <- read_csv("C:/Users/tilin/Desktop/BetterFi/Copy of DataLab-loandata-anon.csv")

#Cleaning Client Data
clientClean <- client %>% 
  mutate(employer = ifelse(is.na(employer), "UNEMPLOYED", client$employer)) %>% 
  select(custID, streetzip, incomeamt, income_source, employer, referenceperson, incomelevel)

#Cleaning Loan Data
loanClean <- loan %>% 
  mutate(status_code = ifelse(is.na(status_code), "active", loan$status_code)) %>% 
  select(primary_borrower_name, status_code, purpose, amount_approved)

#Creating One Data set
df <- merge(clientClean, loanClean, by.x = 'custID', by.y = 'primary_borrower_name')

#Predictive Model
load('model.RData')

################################################################################
## SHINY DASHBOARD
################################################################################

#User Interface
ui <- dashboardPage(
  
  #Dashboard Color 
  skin = 'green',
  
  #Dashboard Title
  dashboardHeader(title = "BetterFi Data Analytics", 
                  titleWidth = 250),
  
  #Dashboard Side Panels
  dashboardSidebar(
    
    #Design
    width = 250,
    
    #Panel Setup
    sidebarMenu(
      menuItem("Predictive Model", tabName = "model", icon = icon("puzzle-piece")), 
      menuItem("Data Analytics", tabName = "analysis", icon = icon("bar-chart"))
     )),
  
  dashboardBody(
    
    #Predictive Model Panel
    tabItems(
      tabItem(tabName = "model",
              #Inputs - Based Off Feature Selection
              fluidRow(width = 12,
                       column(3, numericInput(inputId = 'amount', 
                                              h5("Amount Approved:"), 
                                              value = 0, min = 0, 
                                              width = '100%')), 
                       
                       column(3, selectInput(inputId = 'employer', 
                                             h5("Employer:"), 
                                             choices = sort(unique(df$employer)), 
                                             width = '100%')),
                       
                       column(3, selectInput(inputId = 'streetzip', 
                                             h5("Street Zip:"), 
                                             choices = sort(unique(df$streetzip)), 
                                             width = '100%')), 
                       
                       column(3, selectInput(inputId = 'referenceperson', 
                                             h5("Reference Person:"), 
                                             choices = sort(unique(df$referenceperson)), 
                                             width = '100%'))
                       ),
              
              #Inputs - Based Off Feature Selection
              fluidRow(width = 12,
                       column(3, selectInput(inputId = 'incomesource', 
                                             h5("Income Source:"), 
                                             choices = sort(unique(df$income_source)), 
                                             width = '100%')),
                       
                       column(3, numericInput(inputId = 'incomeamt', 
                                              h5("Income Amount:"), 
                                              value = 0, min = 0, 
                                              width = '100%')),
                       
                       column(3, selectInput(inputId = 'incomelevel', 
                                             h5("Income Level:"), 
                                             choices = sort(unique(df$incomelevel)), 
                                             width = '100%')),
                       
                       column(3, selectInput(inputId = 'purpose', 
                                             h5("Purpose:"), 
                                             choices = sort(unique(df$purpose)), 
                                             width = '100%')) 
                       ), 
 
              #Submit the Data
              fluidRow(width = 12, actionButton('submit', 
                                    label = h4("Generate Prediction"), 
                                    width = '100%')),
              br(),
              
              #Show the Probability
              fluidRow(width = 12, infoBoxOutput("paidInFull"), 
                       infoBoxOutput("refinanced"),
                       infoBoxOutput("chargedOff"))
      )
    ),
      
    #Exploratory Data Analysis
    tabItems(
    tabItem(tabName = "analysis"))
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  rv <- reactiveValues()
  
  #Takes in Data from Client and Makes a Data Frame
  observeEvent(input$submit, {
    rv$df <- data.frame()
    rv$predict <- data.frame()
    #new_data <- c(input$incomelevel, input$amount, input$employer, input$streetzip, input$incomesource, 
    #              input$incomeamt, input$purpose, input$referenceperson)
    #new_data <- paste(new_data, collapse = ',')
    #new_data <- paste0(new_data, '\n')
    
    #Data Frame with Applicant Data
    rv$df <- data.frame(incomelevel = input$incomelevel,
                        amount_approved = input$amount,
                        employer = input$employer, 
                        streetzip = input$streetzip, 
                        income_source = input$incomesource, 
                        incomeamt = input$incomeamt, 
                        purpose = input$purpose, 
                        referenceperson = input$referenceperson)
    
    customerData <- data.frame(incomelevel = 'HIGH',
                        amount_approved = 200,
                        employer = 'BANK', 
                        streetzip = 37375, 
                        income_source = 'DI', 
                        incomeamt = 10000, 
                        purpose = 'busi', 
                        referenceperson = 'BANK')
    
    customerData <- rv$df
    #print(customerData)
    #Run Data into the Model
    if(nrow(customerData)>0){
      customerData$employer <- as.factor(customerData$employer)
      customerData$streetzip <- as.factor(customerData$streetzip)
      customerData$income_source <- as.factor(customerData$income_source)
      customerData$purpose <- as.factor(customerData$purpose)
      customerData$referenceperson <- as.factor(customerData$referenceperson)
      customerData$incomelevel <- as.factor(customerData$incomelevel)
      
      (pred <- predict(model2, customerData))
     rv$predict <- pred
      
      #print(customerData)
      print(pred)
      print(rv$predict)
    }
    
    #Probability of Charged Off
    output$chargedOff <- renderInfoBox({
      print(rv$predict)
      infoBox(
        "Charged Off", paste0(round(rv$predict[,1]*100, 2), "%"), color = "red"
      )
    })
    
    #Probability of Paid in Full
    output$paidInFull <- renderInfoBox({
      infoBox(
        "Paid in Full", paste0(round(rv$predict[,2]*100, 2), "%"), color = "olive"
      )
    })
    
    #Probability of PIF - Refinanced
    output$refinanced <- renderInfoBox({
      infoBox(
        "Refinanced", paste0(round(rv$predict[,3]*100, 2), "%"), color = "yellow"
      )
    })
  })
  
}

# Run the application 
shinyApp(ui, server)
