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

clientClean<- clientClean %>% 
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
         waterexpense,
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
                                    bankaccount)))



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
  filter(status!= "active")

loanClean<- loanClean %>% 
  mutate(status= ifelse(status== "charged_off", FALSE,
                        TRUE))


######## Creating One Data set #######
df <- merge(clientClean, loanClean, by.x = 'custID', by.y = 'primary_borrower_name')

<<<<<<< HEAD
#Predictive Model
load('model.RData')
=======





######## Predictive Model ######
#load('../model.RData')
>>>>>>> 8bf06f8cd5d14afd8e933fa59039772059339448

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
                        menuItem('Application Method', 
                                 tabName = 'WHO',
                                 icon= icon('bar-chart')),
                        menuItem('Customer ID', 
                                 tabName = 'custID',
                                 icon= icon('bar-chart')),
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
                                 icon= icon('bar-chart')),
                        menuItem('Loan Denied',
                                 tabName= 'denied',
                                 icon=icon('bar-chart'))
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
      
      # WHO
      tabItem(tabName = "WHO", 
              plotOutput("whobarplot")
      ),
      
      # custID
      tabItem(tabName = "custID",
              selectInput(inputId = "demo",
                          label= 'Select:',
                          choices = c("hello", "bye"))
      ),    
      
      
      # age
      tabItem(tabName = "age",
              plotOutput("agebarplot")
      ),
      
      
      # sex
      tabItem(tabName = "sex",
              plotOutput("sexbarplot")
      ),
      
      # state
      tabItem(tabName = "state",
              plotOutput("statebarplot")
      ),
      
      # county
      tabItem(tabName = "county",
              plotOutput("countybarplot")
      ),
      
      # city
      tabItem(tabName = "city",
              plotOutput("citybarplot")
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
              plotOutput('employerbarplot')
      ),
      
      # monthlyincome
      tabItem(tabName = "monthlyincome",
              plotOutput("incomebarplot")
      ),
      
      # incomefreq
      tabItem(tabName = "incomefreq",
              plotOutput("incomefreqbarplot")
      ),
      
      # carsinhh
      tabItem(tabName = "carsinhh",
              plotOutput("carsinhhbarplot")
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
              plotOutput("loanamountbarplot")
      ),
      
      # purpose
      tabItem(tabName = "purpose",
              plotOutput("purposebarplot")
      ),
      
      # referenceperson
      tabItem(tabName = "referenceperson",
              plotOutput("referencebarplot"),
              plotOutput("ogborrowerbarplot"),
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
      geom_col()
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
      geom_col()
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
      geom_col()
  })  
  
  # Bar Chart Showing state for the submenu
  
  output$statebarplot<- renderPlot({
    
    statemodel1<- df %>% 
      group_by(licensestate, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- statemodel1,
           aes(x= licensestate,
               y= number,
               fill= status))+
      geom_col()
  }) 
  
  # Bar Chart Showing county for the submenu  
  output$countybarplot<- renderPlot({
    
    countymodel1<- df %>% 
      group_by(County, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- countymodel1,
           aes(x= County,
               y= number,
               fill= status))+
      geom_col()
  }) 
  
  # Bar Chart Showing city for the submenu
  output$citybarplot<- renderPlot({
    
    citymodel1<- df %>% 
      group_by(streetcity, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- citymodel1,
           aes(x= streetcity,
               y= number,
               fill= status))+
      geom_col()+
      labs(y= "Number of Loans")
  })  
  
  # Bar Chart Showing employer for the submenu
  output$employerbarplot<- renderPlot({
    
    employermodel1<- df %>% 
      group_by(employer, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- employermodel1,
           aes(x= employer,
               y= number,
               fill= status))+
      geom_col()
    
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
      geom_col()
    
  })
  
  # Bar Chart Showing incomefreq for the submenu
  output$incomefreqbarplot<- renderPlot({
    
    incomefreqmodel1<- df %>% 
      group_by(incomefreq, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- incomefreqmodel1,
           aes(x= incomefreq,
               y= number,
               fill= status))+
      geom_col()
  })
  
  # Bar Chart Showing carsinhh for the submenu
  output$carsinhhbarplot<- renderPlot({
    
    carsinhhmodel1<- df %>% 
      group_by(carsinhh, status) %>% 
      summarise(number=n(),
                status) %>% 
      unique()
    
    ggplot(data<- carsinhhmodel1,
           aes(x= carsinhh,
               y= number,
               fill= status))+
      geom_col()
  })
  
  
  # Bar Chart Showing expenses for the submenu
  output$totalexpensebarplot<- renderPlot({
    
    totalexpensemodel<- clientClean %>% 
      mutate(monthlyincome= ifelse(monthlyincome<1000, "<1000",
                                   ifelse(monthlyincome>=1000 & monthlyincome<2000, "<2000",
                                          ifelse(monthlyincome>=2000 & monthlyincome<3000, "<3000",
                                                 ifelse(monthlyincome>=3000 & monthlyincome<4000, "<4000",
                                                        ">4000"))))) %>% 
      group_by(monthlyincome) %>% 
      summarise(monthlyexpense= mean(totalexpense)) %>% 
      unique()
    
    ggplot(data<- totalexpensemodel,
           aes(x= monthlyincome,
               y= 4000,
               fill= monthlyexpense))+
      geom_col()+
      scale_x_discrete(limits = c("<1000", "1000-2000", "2000-3000", "3000-4000", ">4000"))
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
      scale_x_discrete(limits = c("<500", "500-1000", "1000-2000", "2000-3000", ">3000"))
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
      geom_col()
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
      geom_col()
    
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
        geom_col()
    })
  })
  
  
}

# Run the application 
shinyApp(ui, server)
