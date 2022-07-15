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
client <- read_csv("C:/Users/tilin/Desktop/BetterFi/Copy of DataLab-clientdata-anon.csv")
loan <- read_csv("C:/Users/tilin/Desktop/BetterFi/Copy of DataLab-loandata-anon.csv")
loan2 <- read_csv("C:/Users/tilin/Downloads/LOAN-DATA-070122.csv")

<<<<<<< HEAD
################################################################################
## Client Dataset for Decision Tree 
################################################################################
dt_client <- client %>% 
=======

#Read in income data for county, city, zip

#countyincome <- https://docs.google.com/spreadsheets/d/1_190bfKW-Ni933rcDBcbB89vYIKKd-G6StwUcNHGF88/edit#gid=0
#zipincome <- https://docs.google.com/spreadsheets/d/1_YXgrvFUQbLlRgHCAKzVLOtZE7X8XQrqTAHPkfuJ7Mg/edit#gid=0
#cityincome <- https://docs.google.com/spreadsheets/d/1mbcHz0QlaeW2rZZZR63cXCHupk08ykJ6iZPLf5l3Ydk/edit#gid=0

countyincome <- read_csv('/Users/user/Desktop/DataLab/BetterFi New Data/County Median Income.csv')
zipincome <- read_csv('/Users/user/Desktop/DataLab/BetterFi New Data/Zip Median Income.csv')
cityincome <- read_csv('/Users/user/Desktop/DataLab/BetterFi New Data/City Median Income.csv')


######## Cleaning Client Data #######
clientClean <- client %>% 
  mutate(age = year(Sys.Date()) - dob) %>%
>>>>>>> bb01748559892547be1e6555bd8448d6c4d16888
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
<<<<<<< HEAD
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
=======
         income_source,
         incomeamt,
         incomefreq,
         monthlyincome,
         bankaccount,
         housingexpense, 
         phoneplanexpense, 
         elecexpense,
         waterexpense,
         utilitystatus,
         carsinhh,
         carinsexpense,
         denied,
         bankruptcy,
         WHO,
         referenceperson,
         ogborrower)



# if ogborrower is NA, none
clientClean<- clientClean %>% 
  mutate(ogborrower= ifelse(is.na(ogborrower), "None",
                            ogborrower) )

# If carsinhh NA, assume 0
clientClean<-clientClean %>% 
  mutate(carsinhh= ifelse(is.na(carsinhh), 0,
                          carsinhh))

# if carsinhh is 0 then carinsexpense 0
clientClean<- clientClean %>% 
  mutate(carinsexpense= ifelse(carsinhh== 0, 0, carinsexpense))

# if denied is NA, Loan accepted
clientClean<- clientClean %>% 
  mutate(denied= ifelse(is.na(denied), "Loan Accepted",
                        denied) )

## Application Method dataset
whomodel3<-clientClean


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
                                   ifelse(incomefreq== 'wk', "WE",
                                          incomefreq))))


# If housingexpense NA, take average
clientClean$housingexpense[is.na(clientClean$housingexpense)]<-(mean(clientClean$housingexpense, na.rm=TRUE))  
clientClean <- clientClean %>% mutate(housingexpense = floor(housingexpense))


# If phoneplanexpense NA, take average
clientClean$phoneplanexpense[is.na(clientClean$phoneplanexpense)]<-(mean(clientClean$phoneplanexpense, na.rm=TRUE))  
clientClean <- clientClean %>% mutate(phoneplanexpense = floor(phoneplanexpense))




# if elecexpenxe NA, take average
clientClean$elecexpense[is.na(clientClean$elecexpense)]<-(mean(clientClean$elecexpense, na.rm=TRUE)) 
clientClean <- clientClean %>% mutate(elecexpense = floor(elecexpense))









# if carinsexpense NA, take average
clientClean$carinsexpense[is.na(clientClean$carinsexpense)]<-(mean(clientClean$carinsexpense, na.rm=TRUE)) 
clientClean <- clientClean %>% mutate(carinsexpense = floor(carinsexpense))


# Manually filling missing value for County
clientClean<-clientClean %>% 
  mutate(County= ifelse(streetzip== "02128", "Suffolk", County))








# cleaning waterexpense

clientClean<-clientClean %>% 
  mutate(waterexpense= abs(waterexpense))


clientClean$waterexpense[is.na(clientClean$waterexpense)]<-(mean(clientClean$waterexpense, na.rm=TRUE)) 
clientClean <- clientClean %>% mutate(waterexpense = floor(waterexpense))


# if utilitystatus is na, FALSE
clientClean$utilitystatus[is.na(clientClean$utilitystatus)]<-("FALSE") 


# total expense

clientClean<- clientClean %>% 
  mutate(totalexpense = housingexpense + phoneplanexpense + elecexpense + waterexpense + carinsexpense)


# Cleaning Bank Account

clientClean<- clientClean %>% 
  mutate(bankaccount= ifelse(is.na(bankaccount), FALSE,
                             ifelse(bankaccount== "Checking", TRUE,
                                    ifelse(bankaccount== "Savings", TRUE,
                                           bankaccount))))

# if ethnicity wh, WH
clientClean<- clientClean %>% 
  mutate(ethnicity= ifelse(ethnicity== "Wh", "WH",
                           ethnicity))



############## Cleaning Loan Data ##############

loan1Clean <- loan %>% 
  #  mutate(status_code = ifelse(is.na(status_code), "active", loan$status_code)) %>% 
  select(primary_borrower_name, full_identifier, purpose) 
#mutate(primary_borrower_name= tolower(primary_borrower_name))

loan2Clean <- loan2 %>% 
  mutate(pop = ifelse(primary_person_population_classification == "non_urban", 'rural', primary_person_population_classification)) %>%
  mutate(status=status_code) %>% 
  select(primary_person_population_classification,
         status, 
         amount_approved, 
         first_payment_due_date,
         maturity_date,
         status_date,
         payment_periods, 
         pop,
         loan_identifier)

loanClean <- merge(loan1Clean, loan2Clean, by.x = 'full_identifier', by.y = 'loan_identifier') %>% 
  unique()

loanClean<- loanClean %>% 
  mutate(status= ifelse(status== "pif", "Paid in Full",
                        ifelse(status== "pif_refinanced", "Refinanced",
                               ifelse(status== "active", "Active",
                                      ifelse(status== "charged_off", "Charged Off", status)))))



# if primary_person_population_classification non_urban, rural

loanClean<- loanClean %>% 
  mutate(primary_person_population_classification= ifelse(is.na(primary_person_population_classification), "rural",
                                                          ifelse(primary_person_population_classification== "non_urban", "rural",
                                                                 primary_person_population_classification)))


######## Creating One Data set #######
df <- merge(clientClean, loanClean, by.x = 'custID', by.y = 'primary_borrower_name')






######## Predictive Model ######
#load('../model.RData')
>>>>>>> bb01748559892547be1e6555bd8448d6c4d16888

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
<<<<<<< HEAD
      menuItem("Predictive Model", tabName = "model", icon = icon("puzzle-piece"))
=======
      menuItem("Predictive Model", tabName = "model", icon = icon("puzzle-piece")), 
      menuItem("Data Analytics", tabName = "analysis", icon = icon("bar-chart"),
               
               #Submenu Setup
               menuItem('Demographics',
                        tabName = 'demographics',
                        icon = icon("bar-chart"),
                        menuItem('Application Method', 
                                 tabName = 'WHO',
                                 icon= icon('bar-chart')),
                        #menuItem('Customer ID', 
                        #        tabName = 'custID',
                        #       icon= icon('bar-chart')),
                        menuItem('Age', 
                                 tabName = 'age',
                                 icon= icon('bar-chart')),
                        menuItem('Sex', 
                                 tabName = 'sex',
                                 icon= icon('bar-chart')),
                        menuItem('Ethnicity', 
                                 tabName = 'ethnicity',
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
                        #menuItem('Zip Code', 
                        #        tabName = 'zip',
                        #       icon= icon('bar-chart')),
                        menuItem('Population Classification', 
                                 tabName = 'ruralurban',
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
                        menuItem('Bank Account', 
                                 tabName = 'bankaccount',
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
                                 icon= icon('bar-chart')),
                        menuItem('Loan Denied',
                                 tabName= 'denied',
                                 icon=icon('bar-chart'))
               )
               
>>>>>>> bb01748559892547be1e6555bd8448d6c4d16888
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
<<<<<<< HEAD
=======
      ),
      
      
      
      ################ Data Analytics #########################
      
      #### Interactive Analytics ####
      
      tabItem(tabName = "interactive_analytics",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),
      
      #### Demographics ####
      
      # WHO
      tabItem(tabName = "WHO", 
              plotOutput("whobarplot"),
              plotOutput("whobarplot2"),
              plotOutput("whobarplot3")
      ),
      
      # custID
      #tabItem(tabName = "custID",
      #        selectInput(inputId = "demo",
      #                   label= 'Select:',
      #                  choices = c("hello", "bye"))
      #),    
      
      
      # age
      tabItem(tabName = "age",
              plotOutput("agebarplot"),
              plotOutput("ageprobcurve")
      ),
      
      
      # sex
      tabItem(tabName = "sex",
              plotOutput("sexbarplot"),
              plotOutput("sexbarplot2"),
              plotOutput("sexbarplot3"),
              plotOutput("agesexprobability")
      ),
      # ethnicity
      tabItem(tabName = "ethnicity",
              #plotOutput("ethnictybarplot"),
              plotOutput("ethnictybarplot2"),
              plotOutput("ethnictybarplot3")
      ),
      
      # state
      tabItem(tabName = "state",
              plotOutput("statebarplot"),
              plotOutput("statebarplot2")
      ),
      
      # county
      tabItem(tabName = "county",
              plotOutput("countybarplot"),
              plotOutput("countybarplot2"),
              plotOutput("countymedian")
      ),
      
      # city
      tabItem(tabName = "city",
              plotOutput("citybarplot"),
              plotOutput("citybarplot2"),
              plotOutput("citymedian")
      ),
      
      # rural or urban
      tabItem(tabName = "ruralurban",
              plotOutput("ruralurbanplot"),
              plotOutput("ruralurbanplot2")
      ),
      
      
      # residenceyear
      tabItem(tabName = "residenceyear",
              plotOutput("residencebarplot"),
              plotOutput("residencebarplot2")
      ),
      
      
      
      #### Income and Expenses ####
      
      # employer
      tabItem(tabName = "employer",
              plotOutput('employerbarplot'),
              plotOutput("employerbarplot3"),
              plotOutput("employerbarplot2"),
              plotOutput("employerbarplot4"),
              plotOutput("employerbarplot5")            
      ),
      
      # monthlyincome
      tabItem(tabName = "monthlyincome",
              plotOutput("incomebarplot0"),
              plotOutput("incomebarplot"),
              plotOutput("incomeprob"),
              plotOutput("incomecounty")
      ),
      
      # incomefreq
      tabItem(tabName = "incomefreq",
              plotOutput("incomefreqbarplot"),
              plotOutput("incomefreqbarplot2")
      ),
      # bank account
      tabItem(tabName = "bankaccount",
              plotOutput("bankbarplot2"),
              plotOutput("bankbarplot")
      ),
      
      # carsinhh
      tabItem(tabName = "carsinhh",
              plotOutput("carsinhhbarplot"),
              plotOutput("carsinhhbarplot2")
      ),
      
      # expenses
      tabItem(tabName = "expenses",
              plotOutput("totalexpensebarplot"),
              plotOutput("totalexpensebarplot2"),
              plotOutput("totalexpensebarplot3")
      ),
      
      
      #### Loan Details ####
      
      # amount_approved
      tabItem(tabName = "amount_approved",
              plotOutput("loanamountbarplot"),
              plotOutput("probcurveloanamt"),
              plotOutput("loanamountbarplot2"),
              plotOutput("loanamountbarplot3")
      ),
      
      # purpose
      tabItem(tabName = "purpose",
              plotOutput("purposebarplot"),
              plotOutput("purposebarplot2"),
              plotOutput("purposeprobcurve")
      ),
      
      # referenceperson
      tabItem(tabName = "referenceperson",
              plotOutput("referencebarplot"),
              plotOutput("ogborrowerbarplot"),
              plotOutput("referencebarplot2")
      ),
      
      # denied
      tabItem(tabName = "denied",
              plotOutput("deniedincome"),
              plotOutput("deniedemployer"),
              plotOutput("deniedreference")
              
>>>>>>> bb01748559892547be1e6555bd8448d6c4d16888
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
<<<<<<< HEAD

=======
  
  #######################################################
  
  #Bar Chart Showing WHO
  
  output$whobarplot<- renderPlot({
    
    whomodel1<- clientClean %>% 
      group_by(WHO) %>% 
      summarise(number=n(),
      ) %>% 
      unique()
    
    ggplot(data<- whomodel1,
           aes(x= WHO,
               y= number,
               fill= WHO))+
      geom_col()+
      labs(x= "Application Method",
           y= "Number of Clients")
  })
  
  
  #Bar Chart Showing WHO and Status
  
  output$whobarplot2<- renderPlot({
    
    whomodel2<- clientClean %>% 
      group_by(WHO,
               streetcity) %>% 
      summarise(number=n(),
                streetcity) %>% 
      unique()
    
    ggplot(data<- whomodel2,
           aes(x= streetcity,
               y= number,
               fill= WHO))+
      geom_col()+
      coord_flip()+
      labs(x= "City",
           y= "Number of Clients")
  })
  
  # Bar Chart showing percentage of missing values
  
  output$whobarplot3<- renderPlot({
    
    whomodel3$missing <- rowSums(is.na(whomodel3))
    
    
    why<- whomodel3 %>%
      mutate(mis= ifelse(missing==0, "0 Missing Values",
                         ifelse(missing>0 & missing<=2, "0-2 Missing Values",
                                ifelse(missing>2 & missing<=5, "2-5 Missing Values", 
                                       ifelse(missing>5, ">5 Missing Values",  
                                              missing)
                                )))) %>% 
      group_by(WHO, mis) %>%  
      summarise(number= n())
    
    
    
    
    why2<- whomodel3 %>% 
      mutate(mis= ifelse(missing==0, "0 Missing Values",
                         ifelse(missing>0 & missing<=2, "0-2 Missing Values",
                                ifelse(missing>2 & missing<=5, "2-5 Missing Values",
                                       ifelse(missing>5, ">5 Missing Values",
                                              missing)
                                )))) %>% 
      group_by(WHO) %>% 
      summarise(num= n())
    
    
    
    why3<- merge(why, why2)
    why3<- why3 %>% 
      mutate(percentage= (number/num*100))
    
    
    
    
    ggplot(data<- why3,
           aes(x= WHO,
               y= percentage,
               fill= mis))+
      geom_col()+
      labs(x= "Application Method",
           y= "Percentage of Data Collected")
    
    
  })
  
  
  
  #Bar Chart Showing age for the age submenu
  
  output$agebarplot<- renderPlot({
    
    agemodel1<- df %>% 
      filter(age<100) %>% 
      group_by(age, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- agemodel1,
           aes(x= age,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Age",
           y= "Number of Loans")
  })
  
  # Elizabeth 
  
  output$ageplace <- renderPlot({
    
    ages <- clientClean %>%
      filter(age<100) %>%
      group_by(age, County) %>%
      mutate((agegroup = case_when(age >= 19  & age <= 29 ~ '20-30',
                                   age >= 30  & age <= 39 ~ '30-40',
                                   age >= 40  & age <= 49 ~ '40-50',
                                   age >= 50 & age <= 59 ~ '50-60',
                                   age >= 60 & age <= 69 ~ '60-70',
                                   age >= 70 & age <= 90 ~ '70+')))
    group_by(agegroup) %>%
      tally()
    
  })
  
  # Probability curve of age
  
  output$ageprobcurve<- renderPlot({  
    
    agemodel2<- df %>% 
      filter(status!= "Active") %>% 
      mutate(status= ifelse(status!= "Charged Off", TRUE,
                            FALSE)) %>% 
      filter(age<100) %>% 
      group_by(age) %>% 
      summarise(number=n(),
                average= mean(status))
    
    ggplot(data<- agemodel2,
           aes(x= age,
               y= average))+
      geom_point()+
      ylim(0,1)+
      geom_smooth(se= FALSE)+
      labs(x= "Age",
           y= "Probability of Repayment")
    
  })
  
  # Bar Chart Showing sex for the submenu
  
  output$sexbarplot<- renderPlot({
    
    sexmodel1<- df %>% 
      group_by(sex, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- sexmodel1,
           aes(x= sex,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Sex",
           y= "Number of Loans")
  })  
  
  # Bar Chart showing age and loans for females
  output$sexbarplot2<- renderPlot({
    
    sexmodel2<- df %>% 
      filter(age<100) %>% 
      filter(sex== "FE") %>% 
      group_by(age, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- sexmodel2,
           aes(x= age,
               y= number,
               fill= status))+
      geom_col()+
      labs(title = "Loans Dispersed to Females",
           x= "Age",
           y= "Number of Loans")
  })
  
  # Bar Chart showing age and loans for males
  output$sexbarplot3<- renderPlot({
    
    sexmodel3<- df %>% 
      filter(age<100) %>% 
      filter(sex== "MA") %>% 
      group_by(age, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- sexmodel3,
           aes(x= age,
               y= number,
               fill= status))+
      geom_col()+
      labs(title = "Loans Dispersed to Males",
           x= "Age",
           y= "Number of Loans")
  })
  
  
  # age probability by sex and age
  
  output$agesexprobability<- renderPlot({
    sexmodel2 <- df %>%
      filter(status!= "Active") %>% 
      mutate(status= ifelse(status!= "Charged Off", TRUE,
                            FALSE)) %>% 
      filter(age<100) %>%
      group_by(age, status, sex) %>%
      summarise(Number= n(), Average= mean(status), sex) %>%
      unique()
    
    ggplot(data = sexmodel2, 
           aes(x = age, y= Average))+
      geom_point() +
      facet_wrap(~sex)+
      geom_smooth(se= FALSE)+
      labs(x= "Age",
           y= "Probability of Repayment")
  })
  
  
  
  # Bar Chart Showing ethnicty
  
  #output$ethnictybarplot<- renderPlot({
  
  
  # ethnicitymodel1<- clientClean %>% 
  #  group_by(ethnicity) %>% 
  # summarise(number=n()) %>% 
  #unique()
  
  #ggplot(data<- ethnicitymodel1,
  #      aes(x= ethnicity,
  #         y= number,
  #        fill= ethnicity))+
  #geom_col()+
  #labs(y= "Number of Clients")
  #}) 
  
  # Bar Chart showing ethnicity denied
  
  output$ethnictybarplot2<- renderPlot({
    
    
    ethnicitymodel2<- clientClean %>% 
      group_by(ethnicity,
               denied) %>% 
      summarise(number=n()) %>% 
      unique()
    
    ggplot(data<- ethnicitymodel2,
           aes(x= ethnicity,
               y= number,
               fill= denied))+
      geom_col()+
      labs(y= "Number of Clients")
  })  
  
  # Bar Chart showing ethnicity loans
  
  output$ethnictybarplot3<- renderPlot({
    
    
    ethnicitymodel3<- df %>% 
      group_by(ethnicity,
               status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- ethnicitymodel3,
           aes(x= ethnicity,
               y= number,
               fill= status))+
      geom_col()+
      labs(y= "Number of Loans")
  }) 
  
  
  
  # Bar Chart Showing state and denied
  
  output$statebarplot<- renderPlot({
    
    statemodel1<- clientClean %>% 
      group_by(licensestate, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- statemodel1,
           aes(x= licensestate,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "License State",
           y= "Number of Clients")
  }) 
  
  # Bar Chart Showing state and status
  
  output$statebarplot2<- renderPlot({
    
    statemodel2<- df %>% 
      group_by(licensestate, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- statemodel2,
           aes(x= licensestate,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "License State",
           y= "Number of Loans")
  }) 
  
  
  # Bar Chart Showing county and denied
  
  output$countybarplot<- renderPlot({
    
    countymodel1<- clientClean %>% 
      group_by(County, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- countymodel1,
           aes(x= County,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "County",
           y= "Number of Clients")
  }) 
  
  # Bar Chart Showing county and status  
  output$countybarplot2<- renderPlot({
    
    countymodel2<- df %>% 
      group_by(County, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- countymodel2,
           aes(x= County,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "County",
           y= "Number of Loans")
  }) 
  
  
  # County median income
  output$countymedian <- renderPlot({
    ggplot(data = countyincome)+
      geom_col(aes(x = reorder(county, -countyincome), y = countyincome, fill = countypop))+
      labs(x = 'County', y = 'Median Houshold Income',caption = "From 2020 U.S. Census Data")+
      coord_flip()
  })
  
  
  
  # Bar Chart Showing city and denied
  
  output$citybarplot<- renderPlot({
    
    citymodel1<- clientClean %>% 
      group_by(streetcity, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- citymodel1,
           aes(x= streetcity,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "City",
           y= "Number of Clients")
  })
  
  
  # Bar Chart Showing city and status
  output$citybarplot2<- renderPlot({
    
    citymodel2<- df %>% 
      group_by(streetcity, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- citymodel2,
           aes(x= streetcity,
               y= number,
               fill= status))+
      geom_col()+
      coord_flip()+
      labs(x= "City",
           y= "Number of Loans")
  })  
  
  
  #Median income in cities
  output$citymedian<- renderPlot({
    ggplot(data = cityincome)+
      geom_col(aes(x= reorder(city, -cityincome), y = cityincome))+
      coord_flip()+
      labs(title = 'Median Household Income in Cities', x = 'City',
           y= 'Median income')
  })
  
  
  
  
  
  
  # Bar Chart Showing population classification and status
  output$ruralurbanplot<- renderPlot({
    
    ruralurbanplot<- df %>% 
      group_by(primary_person_population_classification, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- ruralurbanplot,
           aes(x= primary_person_population_classification,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Population Classification",
           y= "Number of Loans")
  }) 
  
  
  # Bar Chart showing residence year and denied
  output$residencebarplot<- renderPlot({
    
    residencemodel1<- clientClean %>% 
      mutate(residenceyear= ifelse(residenceyear>10, ">10",
                                   residenceyear)) %>% 
      group_by(residenceyear, denied) %>% 
      summarise(number= n())
    
    ggplot(data<- residencemodel1,
           aes(x= residenceyear,
               y= number,
               fill= denied))+
      geom_col()+
      scale_x_discrete(limits = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", ">10"))+
      coord_flip()+
      labs(x= "Residence Year",
           y= "Number of Clients")
    
    
  })
  
  # Bar Chart showing residence year and status
  output$residencebarplot2<- renderPlot({
    
    residencemodel2<- df %>% 
      mutate(residenceyear= ifelse(residenceyear>10, ">10",
                                   residenceyear)) %>% 
      group_by(residenceyear, status) %>% 
      summarise(number= n())
    
    ggplot(data<- residencemodel2,
           aes(x= residenceyear,
               y= number,
               fill= status))+
      geom_col()+
      #scale_x_discrete(limits = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", ">10"))+
      labs(x= "Residence Year",
           y= "Number of Loans")
    
    
  })
  
  
  # Bar Chart Showing employer and denied
  output$employerbarplot<- renderPlot({
    
    employermodel1<- clientClean %>% 
      group_by(employer, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- employermodel1,
           aes(x= employer,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "Employer",
           y= "Number of Clients")
    
  })
  
  
  # Bar Chart Showing employer for the submenu
  output$employerbarplot3<- renderPlot({
    
    employermodel3<- df %>% 
      group_by(employer, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- employermodel3,
           aes(x= employer,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Employer",
           y= "Number of Loans")
    
  })
  
  
  # Bar Chart Showing employer average incomes
  output$employerbarplot2<- renderPlot({
    
    employermodel2<- clientClean %>% 
      group_by(employer) %>%   
      summarise(avgincome= mean(monthlyincome),
                incomefreq) %>% 
      unique()
    
    ggplot(data<- employermodel2,
           aes(x= employer,
               y= avgincome,
               fill= incomefreq))+
      geom_col()+
      labs(x= "Employer",
           y= "Average Income of Clients")
    
  })  
  
  
  
  
  
  # Bar Chart Showing employer reference
  output$employerbarplot4<- renderPlot({
    
    employermodel4<- clientClean %>% 
      filter(referenceperson== "EMPLOYER") %>% 
      group_by(employer) %>%   
      summarise(number=n()) %>% 
      unique()
    
    ggplot(data<- employermodel4,
           aes(x= employer,
               y= number,
               fill= employer))+
      geom_col()+
      labs(x= "Employer",
           y= "Number of Clients Refered by Employer")
    
  })
  
  # Bar Chart Showing employer reference loans
  output$employerbarplot5<- renderPlot({
    
    employermodel5<- df %>% 
      filter(referenceperson== "EMPLOYER") %>% 
      group_by(employer,
               status) %>%   
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- employermodel5,
           aes(x= employer,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Employer",
           y= "Number of Loans Taken by Refered Clients")
    
  })  
  
  
  
  # Bar Chart Showing monthlyincome for the submenu
  output$incomebarplot0<- renderPlot({
    
    incomemodel0<- clientClean %>% 
      mutate(monthlyincome= ifelse(monthlyincome<1000, "<1000",
                                   ifelse(monthlyincome>=1000 & monthlyincome<2000, "<2000",
                                          ifelse(monthlyincome>=2000 & monthlyincome<3000, "<3000",
                                                 ifelse(monthlyincome>=3000 & monthlyincome<4000, "<4000",
                                                        ">4000"))))) %>% 
      group_by(monthlyincome, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- incomemodel0,
           aes(x= monthlyincome,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "Monthly Income",
           y= "Number of Clients")
    
  })
  
  
  # Bar Chart Showing monthlyincome for the submenu
  output$incomebarplot<- renderPlot({
    
    incomemodel1<- df %>% 
      mutate(monthlyincome= ifelse(monthlyincome<1000, "<1000",
                                   ifelse(monthlyincome>=1000 & monthlyincome<2000, "<2000",
                                          ifelse(monthlyincome>=2000 & monthlyincome<3000, "<3000",
                                                 ifelse(monthlyincome>=3000 & monthlyincome<4000, "<4000",
                                                        ">4000"))))) %>% 
      group_by(monthlyincome, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- incomemodel1,
           aes(x= monthlyincome,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Monthly Income",
           y= "Number of Loans Dispersed")
    
  })
  
  # Average income per per County
  output$incomecounty<- renderPlot({
    
    incomemodel2<- df %>% 
      mutate(monthlyincome= ifelse(monthlyincome<1000, "<1000",
                                   ifelse(monthlyincome>=1000 & monthlyincome<2000, "<2000",
                                          ifelse(monthlyincome>=2000 & monthlyincome<3000, "<3000",
                                                 ifelse(monthlyincome>=3000 & monthlyincome<4000, "<4000",
                                                        ">4000"))))) %>% 
      group_by(monthlyincome,
               County) %>% 
      summarise(number=n(),
                County) %>% 
      unique()
    
    ggplot(data<- incomemodel2,
           aes(x= monthlyincome,
               y= number,
               fill= County))+
      geom_col()+
      labs(x= "Monthly Income",
           y= "Number of Loans Dispersed")
    
  })
  
  # Probability of Repayment considering Monthly Income
  output$incomeprob<- renderPlot({
    incomemodel3<- df %>% 
      filter(status!= "Active") %>% 
      mutate(status= ifelse(status!= "Charged Off", TRUE,
                            FALSE)) %>% 
      group_by(monthlyincome) %>% 
      summarise(number=n(),
                probability= mean(status))
    
    ggplot(data<- incomemodel3,
           aes(x= monthlyincome,
               y= probability))+
      geom_point()+
      ylim(0,1)+
      geom_smooth( se= FALSE, alpha= 0.2)+
      
      labs(y= "Probability of Repayment",
           x= "Monthly Income")
    
    
  })
  
  
  # Probability of Repayment considering disposable income
  output$disposincomeprob<- renderPlot({
    incomemodel4<- df %>% 
      mutate(disposableincome= monthlyincome-totalexpense) %>% 
      group_by(disposableincome) %>% 
      summarise(number=n(),
                probability= mean(status))
    
    ggplot(data<- incomemodel4,
           aes(x= disposableincome,
               y= probability))+
      geom_point()+
      ylim(0,1)+
      geom_smooth( se= FALSE, alpha= 0.2)+
      
      labs(y= "Probability of Repayment",
           x= "Disposable Income")
    
    
  })
  
  
  # Bar Chart Showing incomefreq and denied
  output$incomefreqbarplot<- renderPlot({
    
    incomefreqmodel1<- clientClean %>% 
      group_by(incomefreq, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- incomefreqmodel1,
           aes(x= incomefreq,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "Income Frequency",
           y= "Number of Clients")
  })
  
  
  # Bar Chart Showing incomefreq and status
  output$incomefreqbarplot2<- renderPlot({
    
    incomefreqmodel2<- df %>% 
      group_by(incomefreq, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- incomefreqmodel2,
           aes(x= incomefreq,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Income Frequency",
           y= "Number of Loans")
  })
  
  
  # Bar Chart Showing bankaccount and denied
  output$bankbarplot2<- renderPlot({
    
    bankmodel2<- clientClean %>% 
      group_by(bankaccount, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- bankmodel2,
           aes(x= bankaccount,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "Bank Account",
           y= "Number of Clients")
  })
  
  
  # Bar Chart Showing bankaccount and status
  output$bankbarplot<- renderPlot({
    
    bankmodel<- df %>% 
      group_by(bankaccount, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- bankmodel,
           aes(x= bankaccount,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Bank Account",
           y= "Number of Loans")
  })
  
  
  
  
  
  # Bar Chart Showing carsinhh for the submenu
  output$carsinhhbarplot<- renderPlot({
    
    carsinhhmodel1<- clientClean %>% 
      group_by(carsinhh, denied) %>% 
      summarise(number=n(),
                denied) %>% 
      unique()
    
    ggplot(data<- carsinhhmodel1,
           aes(x= carsinhh,
               y= number,
               fill= denied))+
      geom_col()+
      coord_flip()+
      labs(x= "Cars in Household",
           y= "Number of Clients")
  })
  
  
  # Bar Chart Showing carsinhh for the submenu
  output$carsinhhbarplot2<- renderPlot({
    
    carsinhhmodel2<- df %>% 
      group_by(carsinhh, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- carsinhhmodel2,
           aes(x= carsinhh,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Cars in Household",
           y= "Number of Loans")
  })
  
  
  # line graph Showing Expenses for the submenu
  output$totalexpensebarplot<- renderPlot({
    
    totalexpensemodel<- clientClean %>% 
      arrange(monthlyincome) 
    
    ggplot(data= totalexpensemodel)+
      geom_point(aes(x= custID,
                     y= monthlyincome),
                 color= "Blue")+      
      geom_point(aes(x= custID,
                     y= totalexpense),
                 color= "red")+
      geom_line(aes(x=custID,
                    y=monthlyincome),
                color= "blue")+
      geom_line(aes(x= custID,
                    y= totalexpense),
                color= "red")+
      scale_y_continuous()+
      labs(title= "Monthly Income and Monthly Expenses for Clients",
           x= "Customers",
           y= "Monthly Income")
    
    
  })
  
  # Total expenses  breakdown
  output$totalexpensebarplot2<- renderPlot({
    
    totalexpensemodel2<- clientClean %>% 
      arrange(monthlyincome)
    
    ggplot(data= totalexpensemodel2)+
      geom_point(aes(x= custID,
                     y= totalexpense),
                 color= "red")+
      geom_point(aes(x= custID,
                     y= housingexpense),
                 color= "green")+
      geom_point(aes(x= custID,
                     y= phoneplanexpense),
                 color= "yellow")+
      geom_point(aes(x= custID,
                     y= elecexpense),
                 color= "orange")+
      geom_point(aes(x= custID,
                     y= waterexpense),
                 color= "black")+
      geom_point(aes(x= custID,
                     y= carinsexpense),
                 color= "purple")+
      geom_line(aes(x= custID,
                    y= totalexpense),
                color= "red")+
      scale_y_continuous()+
      labs(title= "Breakdown of Monthly Expenses for Clients",
           x= "Customers",
           y= "Monthly Expenses")
    
    
  })
  
  # Probability of Repayment considering disposable income
  output$totalexpensebarplot3<- renderPlot({
    totalexpensemodel3<- df %>% 
      filter(status!= "Active") %>% 
      mutate(status= ifelse(status!= "Charged Off", TRUE,
                            FALSE)) %>% 
      mutate(disposableincome= monthlyincome-totalexpense) %>% 
      group_by(disposableincome) %>% 
      summarise(number=n(),
                probability= mean(status))
    
    ggplot(data<- totalexpensemodel3,
           aes(x= disposableincome,
               y= probability))+
      geom_point()+
      ylim(0,1)+
      geom_smooth( se= FALSE, alpha= 0.2)+
      
      labs(title= "Disposable Income Probability Curve",
           y= "Probability of Repayment",
           x= "Disposable Income")
    
    
  })
  
  
  # Bar Chart Showing loanamount for the submenu
  output$loanamountbarplot<- renderPlot({
    
    loanamountmodel1<- df %>% 
      mutate(loanamount= ifelse(amount_approved<500, "<500",
                                ifelse(amount_approved>=500 & amount_approved<1000, "500-1000",
                                       ifelse(amount_approved>=1000 & amount_approved<2000, "1000-2000",
                                              ifelse(amount_approved>=2000 & amount_approved<3000, "2000-3000",
                                                     ">3000"))))) %>% 
      group_by(loanamount, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- loanamountmodel1,
           aes(x= loanamount,
               y= number,
               fill= status))+
      geom_col()+
      scale_x_discrete(limits = c("<500", "500-1000", "1000-2000", "2000-3000", ">3000"))+
      labs(x= "Loan Amount",
           y= "Number of Loans")
  })
  
  # Bar Chart Showing loanamount and loan duration
  output$loanamountbarplot2<- renderPlot({
    
    loanamountmodel2<- df %>% 
      mutate(loanamount= ifelse(amount_approved<500, "<500",
                                ifelse(amount_approved>=500 & amount_approved<1000, "500-1000",
                                       ifelse(amount_approved>=1000 & amount_approved<2000, "1000-2000",
                                              ifelse(amount_approved>=2000 & amount_approved<3000, "2000-3000",
                                                     ">3000"))))) %>%
      mutate(payment_periods= strsplit(payment_periods, " ")) %>% 
      mutate(payment_periods= as.integer(sapply(payment_periods, "[[", 1))) %>% 
      mutate(loan.duration= ifelse(payment_periods<=6, "<6",
                                   ifelse(payment_periods>6 & payment_periods<=12, "6-12",
                                          ifelse(payment_periods>12 & payment_periods<=24, "12-24",
                                                 ifelse(payment_periods>24 & payment_periods<=36, "24-36",
                                                        ifelse(payment_periods>36, ">36",
                                                               payment_periods)))))) %>% 
      group_by(loanamount, loan.duration) %>% 
      summarise(number=n()) %>% 
      unique()
    
    ggplot(data<- loanamountmodel2,
           aes(x= loanamount,
               y= number,
               fill= loan.duration))+
      geom_col()+
      scale_x_discrete(limits = c("<500", "500-1000", "1000-2000", "2000-3000", ">3000"))+
      labs(x= "Loan Amount",
           y= "Number of Loans")
  })
  
  
  
  # Bar Chart Showing loanamount and city
  output$loanamountbarplot3<- renderPlot({
    
    loanamountmodel3<- df %>% 
      mutate(loanamount= ifelse(amount_approved<500, "<500",
                                ifelse(amount_approved>=500 & amount_approved<1000, "500-1000",
                                       ifelse(amount_approved>=1000 & amount_approved<2000, "1000-2000",
                                              ifelse(amount_approved>=2000 & amount_approved<3000, "2000-3000",
                                                     ">3000"))))) %>%
      group_by(loanamount, streetcity) %>% 
      summarise(number=n()) %>% 
      unique()
    
    ggplot(data<- loanamountmodel3,
           aes(x= streetcity,
               y= number,
               fill= loanamount))+
      geom_col()+
      coord_flip()+
      labs(x= "City",
           y= "Number of Loans")
  })
  
  
  # Probability Curve of loan amount 
  output$probcurveloanamt<- renderPlot({
    
    loanamtprobcurve<- df %>% 
      filter(status!= "Active") %>% 
      mutate(status= ifelse(status!= "Charged Off", TRUE,
                            FALSE)) %>% 
      group_by(amount_approved) %>% 
      summarise(number=n(),
                probability= mean(status))
    
    ggplot(data<- loanamtprobcurve,
           aes(x= amount_approved,
               y= probability))+
      geom_point()+
      ylim(0,1)+
      geom_smooth(se= FALSE)+
      labs(x= "Loan Amount",
           y= "Probability of Repayment")
    
    
  })
  
  
  # Bar Chart Showing loan purpose for the submenu
  output$purposebarplot<- renderPlot({
    
    purposemodel1<- df %>% 
      group_by(purpose, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- purposemodel1,
           aes(x= purpose,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Purpose of Loan",
           y= "Number of Loans")
  })
  
  
  # Bar Chart Showing loan purpose and average loan amount
  output$purposebarplot2<- renderPlot({
    
    purposemodel2<- df %>% 
      group_by(purpose) %>% 
      summarise(avgloanamt= mean(amount_approved)) %>% 
      unique()
    
    ggplot(data<- purposemodel2,
           aes(x= purpose,
               y= avgloanamt,
               fill= purpose))+
      geom_col()+
      labs(x= "Purpose",
           y= "Average Loan Amount")
  })
  
  # Probability Curve of loan purpose 
  output$purposeprobcurve<- renderPlot({
    
    purposemodel3<- df %>% 
      filter(status!= "Active") %>% 
      mutate(status= ifelse(status!= "Charged Off", TRUE,
                            FALSE)) %>% 
      group_by(purpose) %>% 
      summarise(number=n(),
                probability= mean(status))
    
    ggplot(data<- purposemodel3,
           aes(x= purpose,
               y= probability))+
      geom_point()+
      ylim(0,1)+
      geom_smooth(se= FALSE)+
      labs(x= "Purpose of Loan",
           y= "Probability of Repayment")
    
    
  })
  
  
  # Bar Chart Showing reference person for the submenu
  output$referencebarplot<- renderPlot({
    
    referencemodel1<- df %>% 
      group_by(referenceperson, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- referencemodel1,
           aes(x= referenceperson,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Reference Person",
           y= "Number of Loans")
  })
  
  # Bar Chart Showing ogborrower for the submenu
  output$ogborrowerbarplot<- renderPlot({
    
    ogborrowermodel1<- df %>% 
      filter(ogborrower!= "None") %>% 
      group_by(ogborrower, status) %>%
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- ogborrowermodel1,
           aes(x= ogborrower,
               y= number,
               fill= status))+
      geom_col()+
      labs(x= "Client's Referrals",
           y= "Number of Loans")
  })
  
  # Bar Chart Showing reference person for the submenu
  output$referencebarplot2<- renderPlot({
    
    referencemodel2<- clientClean %>% 
      filter(referenceperson!= "NONE") %>% 
      group_by(referenceperson, streetcity) %>% 
      summarise(number=n()) %>% 
      unique()
    
    ggplot(data<- referencemodel2,
           aes(x= streetcity,
               y= number,
               fill= referenceperson))+
      geom_col()+
      coord_flip()+
      labs(x= "City",
           y= "Number of Loans")
  })
  
  
  # Loan Denied and income
  
  output$deniedincome<- renderPlot({
    
    deniedincome<-clientClean %>% 
      filter(denied== "denied") 
    
    ggplot(data<- deniedincome)+
      geom_point(aes(x= custID,
                     y= monthlyincome),
                 color= "blue")+
      geom_point(aes(x= custID,
                     y= totalexpense),
                 color= "red")+
      labs(y= "Monthly Income and Expenses")
    
  })
  
  # Loan Denied and employer
  
  output$deniedemployer<- renderPlot({
    
    deniedemployer<-clientClean %>% 
      filter(denied== "denied") %>% 
      group_by(employer) %>% 
      summarise(number= n())
    
    ggplot(data<- deniedemployer,
           aes(x= employer,
               y= number,
               fill= employer))+
      geom_col()+
      labs(x= "Employer",
           y= "Number of Clients")
    
  }) 
  
  # Loan Denied and referenceperson
  
  output$deniedreference<- renderPlot({
    
    deniedreference<- clientClean %>% 
      filter(denied== "denied") %>% 
      group_by(referenceperson) %>% 
      summarise(number= n())
    
    ggplot(data<- deniedreference,
           aes(x= referenceperson,
               y= number,
               fill= referenceperson))+
      geom_col()+
      labs(x= "Reference Person",
           y= "Number of Clients")    
    
    
  })
  
>>>>>>> bb01748559892547be1e6555bd8448d6c4d16888
}

# Run the application 
shinyApp(ui, server)