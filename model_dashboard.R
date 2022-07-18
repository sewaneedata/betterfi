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
library(scales)

#Read in the Client and Loan Data
client <- read_csv("/Users/user/Desktop/DataLab/BetterFi New Data/new_client.csv")
loan <- read_csv("/Users/user/Desktop/DataLab/BetterFi Data/Loan Data.csv")
loan2 <- read_csv("/Users/user/Desktop/DataLab/BetterFi New Data/new_loan.csv")

################################################################################
## Client Dataset for Decision Tree 
################################################################################
dt_client <- client %>% 
  mutate(streetcity = case_when(streetcity == 'Traciy City' | streetcity == 'TRACY CITY' ~ 'Tracy City', 
                                streetcity == 'TULLAHOMA' ~ 'Tullahoma',
                                TRUE ~ streetcity)) %>%  
  mutate(employer = ifelse(is.na(employer), "UNEMPLOYED", client$employer)) %>% 
  mutate(incomeamt = case_when(incomefreq == 'MO' || incomefreq == 'mo' ~ incomeamt, 
                               incomefreq == 'biwk' || incomefreq == 'BI' ~ incomeamt * 2, 
                               incomefreq == 'WE' ~ incomeamt * 4)) %>% 
  mutate(totalExpense = rowSums(client[,c(54, 55, 56, 57, 59)], na.rm = TRUE)) %>% 
  select(custID,
         streetcity, 
         streetzip, 
         income_source, 
         employer,
         incomeamt, 
         annualincome, 
         incomelevel, 
         totalExpense, 
         referenceperson)
################################################################################
## Loan Dataset for Decision Tree
################################################################################
dt_loan1 <- loan %>% 
  mutate(status_code = case_when(is.na(status_code) ~ "active", 
                                 status_code == 'pif_refinanced' ~ 'pif', 
                                 TRUE ~ loan$status_code)) %>% 
  select(primary_borrower_name, status_code, purpose, amount_approved, full_identifier) 

dt_loan2 <- loan2 %>% 
  select(payment_periods, primary_borrower_name, loan_identifier) %>% 
  mutate(payment_periods = gsub("mo","", as.character(payment_periods))) 

dt_loan <- merge(dt_loan1, dt_loan2, by.x = 'full_identifier', by.y = 'loan_identifier') %>% 
  mutate(monthlyPayment = round(amount_approved/as.numeric(payment_periods), 2))
dt_loan <- dt_loan[-7]
################################################################################
## Merging Dataset for Decision Tree 
################################################################################
df_model <- merge(dt_client, dt_loan, by.x = 'custID', by.y = 'primary_borrower_name.x')
df_model <- df_model[df$status_code != 'active',] 
df_model <- df_model[-c(1, 11)]
################################################################################
## Categorical Factors 
################################################################################
cat <- c('streetcity', 'streetzip', 'income_source', 'employer', 'referenceperson', 'incomelevel', 
         'status_code', 'purpose',  'payment_periods')

for(x in cat) {
  df_model[, x] <- as.factor(df_model[, x])
}
str(df_model)
################################################################################
## Loading Model
################################################################################
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
    width = 250,
    sidebarMenu(
      menuItem("Predictive Model", tabName = "model", icon = icon("puzzle-piece"))
      )
    ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "model",
              
              h3('CLIENT'),
              fluidRow(width = 12,
                       column(3, textInput(inputId = 'lastName', 
                                             h5("Last Name:"),   
                                             width = '100%')), 
                       column(3, textInput(inputId = 'firstName', 
                                           h5("First Name:"),   
                                           width = '100%')),
                       column(3, selectInput(inputId = 'referenceperson', 
                                             h5("Reference Person:"), 
                                             choices = (sort(unique(df$referenceperson))),
                                             width = '100%'))),
              br(),
              h3('LOCATION'),
              fluidRow(width = 12,
                       column(3, selectInput(inputId = 'streetcity', 
                                             h5("Street City:"), 
                                             choices = sort(unique(df$streetcity)),  
                                             width = '100%')), 
                       
                       column(3, selectInput(inputId = 'streetzip', 
                                             h5("Street Zip:"), 
                                             choices = sort(unique(df$streetzip)), 
                                             width = '100%'))), 
              br(),
              h3('INCOME AND EXPENSES'),
              fluidRow(width = 12, 
                       column(3, selectInput(inputId = 'income_source', 
                                             h5("Income Source:"), 
                                             choices = sort(unique(df$income_source)), 
                                             width = '100%')), 
                       column(3, selectInput(inputId = 'employer', 
                                             h5("Employer:"), 
                                             choices = sort(unique(df$employer)), 
                                             width = '100%')),
                       column(3, selectInput(inputId = 'incomelevel', 
                                             h5("Income Level:"),
                                             choices = sort(unique(df$incomelevel)),
                                             width = '100%'))),
              fluidRow(width = 12,
                       column(3, numericInput(inputId = 'incomeamt', 
                                             h5("Monthly Income:"), 
                                             value = 0, min = 0, 
                                             width = '100%')),
                       column(3, numericInput(inputId = 'annualincome', 
                                              h5("Annual Income:"), 
                                              value = 0, min = 0, 
                                              width = '100%')),
                       column(3, numericInput(inputId = 'totalExpenses', 
                                              h5("Total Expenses:"),
                                              value = 0, min = 0, 
                                              width = '100%'))), 
              br(),
              h3('LOAN INFORMATION'),
              fluidRow(width = 12,
                       column(3, selectInput(inputId = 'purpose', 
                                              h5("Purpose:"), 
                                              choices = sort(unique(df$purpose)), 
                                              width = '100%')),
                       column(3, numericInput(inputId = 'amount_approved', 
                                             h5("Amount Approved:"), 
                                             value = 0, min = 0,  
                                             width = '100%')),
                       column(3, numericInput(inputId = 'monthlyPayment', 
                                              h5("Monthly Payment:"), 
                                              value = 0, min = 0, 
                                              width = '100%')),
                       column(3, selectInput(inputId = 'payment_periods', 
                                              h5("Payment Periods:"), 
                                              choices = sort(unique(df$payment_periods)),  
                                              width = '100%'))),
              
              #Submit the Data
              fluidRow(width = 12, actionButton('submit', 
                                                label = h4("Generate Prediction"), 
                                                width = '100%')),
              br(),
              
              #Show the Probability
              fluidRow(width = 12, infoBoxOutput("paidInFull"), 
                       infoBoxOutput("chargedOff"))
      )
    
  )
  
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues()
  
  #Takes in Data from Client and Makes a Data Frame
  observeEvent(input$submit, {
    rv$df_model <- data.frame()
    rv$predict <- data.frame()
    
    #Data Frame with Applicant Data
    rv$df <- data.frame(streetcity = input$streetcity,
                        streetzip = input$streetzip,
                        income_source = input$income_source, 
                        employer = input$employer, 
                        incomeamt = input$incomeamt, 
                        annualincome = input$annualincome, 
                        incomelevel = input$incomelevel, 
                        totalExpense = input$totalExpenses, 
                        referenceperson = input$referenceperson, 
                        purpose = input$purpose, 
                        amount_approved = input$amount_approved,
                        monthlyPayment = input$monthlyPayment, 
                        payment_periods = input$payment_periods)
    
    customerData <- rv$df
    
    #Run Data into the Model
    if(nrow(customerData)>0){
      customerData$streetcity <- as.factor(customerData$streetcity)
      customerData$streetzip <- as.factor(customerData$streetzip)
      customerData$income_source <- as.factor(customerData$income_source)
      customerData$employer <- as.factor(customerData$employer)
      customerData$incomelevel <- as.factor(customerData$incomelevel)
      customerData$referenceperson <- as.factor(customerData$referenceperson)
      customerData$purpose <- as.factor(customerData$purpose)
      customerData$payment_periods <- as.factor(customerData$payment_periods)
    
      
      pred <- predict(model0, customerData, type = "prob")
      rv$predict <- pred
      
      new_data <- c(input$lastName, input$firstName, input$streetcity, input$streetzip, input$income_source, input$employer, input$incomeamt,
                    input$annualincome, input$incomelevel, input$totalExpenses, input$referenceperson, input$purpose,
                    input$amount_approved, input$monthlyPayment, input$payment_periods, pred[,1], pred[,2])
      new_data <- paste(new_data, collapse = ',')
      new_data <- paste0(new_data, '\n')
      cat(new_data, file = 'newClientData.csv', append = TRUE)
      
      print(customerData)
      print(rv$predict)
      
    }
    
    #Probability of Charged Off
    output$chargedOff <- renderInfoBox({
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
    
  })

}

# Run the application 
shinyApp(ui, server)
