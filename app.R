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
client <- read_csv("/Users/user/Desktop/DataLab/BetterFi New Data/new_client.csv")
loan <- read_csv("/Users/user/Desktop/DataLab/BetterFi Data/Loan Data.csv")
loan2 <- read_csv("/Users/user/Desktop/DataLab/BetterFi New Data/new_loan.csv")

######## Cleaning Client Data #######
clientClean <- client %>% 
  mutate(age = year(Sys.Date()) - dob) %>%
  mutate(streetcity = case_when(streetcity == 'Traciy City' | streetcity == 'TRACY CITY' ~ 'Tracy City', 
                                streetcity == 'TULLAHOMA' ~ 'Tullahoma',
                                TRUE ~ streetcity))  
clientClean <- clientClean %>% 
  mutate(employer = ifelse(is.na(employer), "UNEMPLOYED", client$employer)) %>% 
  mutate(incomeamt = case_when(incomefreq == 'MO' || incomefreq == 'mo' ~ incomeamt, 
                               incomefreq == 'biwk' || incomefreq == 'BI' ~ incomeamt * 2, 
                               incomefreq == 'WE' ~ incomeamt * 4)) %>% 
  mutate(residenceyear= floor(as.numeric(residenceperiod)/12)) %>% 
  mutate(carsinhh = ifelse(is.na(carsinhh), 0, carsinhh)) %>% 
  mutate(monthlyincome= ifelse(clientClean$incomefreq %in% c("BI","biwk"), clientClean$incomeamt*2,
                               ifelse(clientClean$incomefreq%in% c("WE", "wk"), clientClean$incomeamt*4,
                                      clientClean$incomeamt)))  

clinetClean<- clientClean %>% 
  select(custID,
         sex, 
         age,
         ethnicity,
         cellphone,
         licensestate,
         County,
         streetcity, 
         streetzip,
         residenceyear,
         employer,
         income_source,
         incomeamt,
         incomefreq,
         monthlyincome,
         bankaccount,
         housingexpense, 
         phoneplanexpense, 
         elecexpense,
         utilitystatus,
         carsinhh,
         carinsexpense,
         denied,
         bankruptcy,
         WHO,
         referenceperson,
         ogborrower)



# If residence year NA, assume average residence year
clientClean$residenceyear[is.na(clientClean$residenceyear)]<-(mean(clientClean$residenceyear, na.rm=TRUE))
clientClean <- clientClean %>% mutate(residenceyear = floor(residenceyear))

# If licensestate NA, assume TN
clientClean<-clientClean %>% 
  mutate(licensestate = ifelse(is.na(licensestate), "TN",
                               clientClean$licensestate))

# If employer NA, they are Unemployed
clientClean<-clientClean %>% 
  mutate(employer = ifelse(is.na(employer), "Unemployed",
                           clientClean$employer))


# If incomefreq biwk, Bi, if mo, MO
clientClean<-clientClean %>% 
  mutate(incomefreq= ifelse(incomefreq== "biwk", "BI",
                            ifelse(incomefreq== "mo", "MO",
                                   incomefreq)))


# If housingexpense NA, take average
clientClean$housingexpense[is.na(clientClean$housingexpense)]<-(mean(clientClean$housingexpense, na.rm=TRUE))  
clientClean <- clientClean %>% mutate(housingexpense = floor(housingexpense))


# If phoneplanexpense NA, take average
clientClean$phoneplanexpense[is.na(clientClean$phoneplanexpense)]<-(mean(clientClean$phoneplanexpense, na.rm=TRUE))  
clientClean <- clientClean %>% mutate(phoneplanexpense = floor(phoneplanexpense))




# if elecexpenxe NA, take average
clientClean$elecexpense[is.na(clientClean$elecexpense)]<-(mean(clientClean$elecexpense, na.rm=TRUE)) 
clientClean <- clientClean %>% mutate(elecexpense = floor(elecexpense))




# If carsinhh NA, assume 0
clientClean<-clientClean %>% 
  mutate(carsinhh= ifelse(is.na(carsinhh), 0,
                          carsinhh))




# if carsinhh is 0 then carinsexpense 0
clientClean<- clientClean %>% 
  mutate(carinsexpense= ifelse(carsinhh== 0, 0, carinsexpense))

# if carinsexpense NA, take average
clientClean$carinsexpense[is.na(clientClean$carinsexpense)]<-(mean(clientClean$carinsexpense, na.rm=TRUE)) 
clientClean <- clientClean %>% mutate(carinsexpense = floor(carinsexpense))


# Manually filling missing value for County
clientClean<-clientClean %>% 
  mutate(County= ifelse(streetzip== "02128", "Suffolk", County))

# if ogborrower is NA, none
clientClean<- clientClean %>% 
  mutate(ogborrower= ifelse(is.na(ogborrower), "None",
                            ogborrower) )


# if denied is NA, Loan accepted
clientClean<- clientClean %>% 
  mutate(denied= ifelse(is.na(denied), "Loan Accepted",
                        denied) )



############## Cleaning Loan Data ##############

loan1Clean <- loan %>% 
  #  mutate(status_code = ifelse(is.na(status_code), "active", loan$status_code)) %>% 
  select(primary_borrower_name, full_identifier, purpose) 
#mutate(primary_borrower_name= tolower(primary_borrower_name))

loan2Clean <- loan2 %>% 
  mutate(pop = ifelse(primary_person_population_classification == "non_urban", 'rural', primary_person_population_classification)) %>% 
  select(primary_person_population_classification,
         status_code, 
         amount_approved, 
         max_delinquency_period,
         number_of_missed_days,
         delinquency_total,
         line_of_credit,
         first_payment_due_date,
         maturity_date,
         status_date,
         payment_periods, 
         pop, 
         risk_rating,
         reserve_percentage,
         reserve_amount,
         loan_identifier)

loanClean <- merge(loan1Clean, loan2Clean, by.x = 'full_identifier', by.y = 'loan_identifier') %>% 
  unique()




######## Creating One Data set #######
df <- merge(clientClean, loanClean, by.x = 'custID', by.y = 'primary_borrower_name')







######## Predictive Model ######
#load('../model.RData')

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
      menuItem("Data Analytics", tabName = "analysis", icon = icon("bar-chart"),
               
               #Submenu Setup
               menuItem('Interactive Analytics', 
                        tabName = 'interactive_analytics',
                        icon= icon('bar-chart')),
               menuItem('Demographics',
                        tabName = 'demographics',
                        icon = icon("bar-chart"),
                        menuItem('Customer ID', 
                                 tabName = 'custID',
                                 icon= icon('bar-chart')),
                        menuItem('Age', 
                                 tabName = 'age',
                                 icon= icon('bar-chart')),
                        menuItem('Sex', 
                                 tabName = 'sex',
                                 icon= icon('bar-chart')),
                        menuItem('State', 
                                 tabName = 'state',
                                 icon= icon('bar-chart')),
                        menuItem('County', 
                                 tabName = 'county',
                                 icon= icon('bar-chart')),
                        menuItem('City', 
                                 tabName = 'city',
                                 icon= icon('bar-chart')),
                        menuItem('Zip Code', 
                                 tabName = 'zip',
                                 icon= icon('bar-chart')),
                        menuItem('Residence Year', 
                                 tabName = 'residenceyear',
                                 icon= icon('bar-chart'))
               ),
               menuItem('Income and Expenses', 
                        tabName = 'employer-income',
                        icon= icon('bar-chart'),
                        menuItem('Employer', 
                                 tabName = 'employer',
                                 icon= icon('bar-chart')),
                        menuItem('Monthly Income', 
                                 tabName = 'monthlyincome',
                                 icon= icon('bar-chart')),
                        menuItem('Income Frequency', 
                                 tabName = 'incomefreq',
                                 icon= icon('bar-chart')),
                        menuItem('Cars in Household', 
                                 tabName = 'carsinhh',
                                 icon= icon('bar-chart')),
                        menuItem('Expenses', 
                                 tabName = 'expenses',
                                 icon= icon('bar-chart'))
               ),
               menuItem('Loan Details', 
                        tabName = 'loan',
                        icon= icon('bar-chart'),
                        menuItem('Loan Amount', 
                                 tabName = 'amount_approved',
                                 icon= icon('bar-chart')),
                        menuItem('Purpose of Loan', 
                                 tabName = 'purpose',
                                 icon= icon('bar-chart')),
                        menuItem('Reference Person', 
                                 tabName = 'referenceperson',
                                 icon= icon('bar-chart'))
               )
               
      )
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
      ),
      
      
      
      ################ Data Analytics #########################
      
      #### Interactive Analytics ####
      
      tabItem(tabName = "interactive_analytics",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      #### Demographics ####
      
      tabItem(tabName = "custID",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),    
      
      
      # age
      tabItem(tabName = "age",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      
      # sex
      tabItem(tabName = "sex",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # state
      tabItem(tabName = "state",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # county
      tabItem(tabName = "county",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # city
      tabItem(tabName = "city",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # zip
      tabItem(tabName = "zip",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # residenceyear
      tabItem(tabName = "residenceyear",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      
      
      #### Income and Expenses ####
      
      # employer
      tabItem(tabName = "employer",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # monthlyincome
      tabItem(tabName = "monthlyincome",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # incomefreq
      tabItem(tabName = "incomefreq",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # carsinhh
      tabItem(tabName = "carsinhh",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # expenses
      tabItem(tabName = "expenses",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      
      #### Loan Details ####
      
      # amount_approved
      tabItem(tabName = "amount_approved",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # purpose
      tabItem(tabName = "purpose",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      # referenceperson
      tabItem(tabName = "referenceperson",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      )
    )
    
    
  )
  
)




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  rv <- reactiveValues()
  
  #Takes in Data from Client and Makes a Data Frame
  observeEvent(input$submit, {
    rv$df <- data.frame()
    rv$predict <- data.frame()
    new_data <- c(input$incomelevel, input$amount, input$employer, input$streetzip, input$incomesource, 
                  input$incomeamt, input$purpose, input$referenceperson)
    new_data <- paste(new_data, collapse = ',')
    new_data <- paste0(new_data, '\n')
    
    #Data Frame with Applicant Data
    rv$df <- data.frame(incomelevel = input$incomelevel,
                        amount_approved = input$amount,
                        employer = input$employer, 
                        streetzip = input$streetzip, 
                        income_source = input$incomesource, 
                        incomeamt = input$incomeamt, 
                        purpose = input$purpose, 
                        referenceperson = input$referenceperson)
    
    customerData <- rv$df
    
    #Run Data into the Model
    if(nrow(customerData)>0){
      customerData$employer <- as.factor(customerData$employer)
      customerData$streetzip <- as.factor(customerData$streetzip)
      customerData$income_source <- as.factor(customerData$income_source)
      customerData$purpose <- as.factor(customerData$purpose)
      customerData$referenceperson <- as.factor(customerData$referenceperson)
      customerData$incomelevel <- as.factor(customerData$incomelevel)
      
      pred <- predict(model2, customerData)
      rv$predict <- pred
      
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

