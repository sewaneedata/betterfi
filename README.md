# Ending the Payday Loan Scam
## A *Better Fi*ght to Predatory Lending

For millions of Americans with bad credit, taking out a loan means paying **400% or higher APR to a lender**, trapping clients in debt for years.

### How Can BetterFi Help?
BetterFi is a 501(c)(3) non-profit corporation that promotes economic justice and offers low interest rates to help individuals get out of debt. They are looking for a data-driven method to evaluate and enhance their lending decisions. Based on historical BetterFi data, the DataLab team will create a risk assessment tool that can calculate default risk based on applicant data.

### Who Are We?
We are a new generation of data scientists who work on social impact projects, trained by the [Sewanee DataLab](https://new.sewanee.edu/sewanee-datalab/).

# Final Products

### The Internal Dashboard
The Predictive Model tab allows for our client to input new client's details to generate a prediction of whether or not an applicant will pay a loan off in full or default/charge off.

The Data Analytics tab is broken down into Demographics, Income and Expenses, and Loan Details. These visualizations shows significant trends that influence the probability of repayment, as well as the relationship between variables and loan repayment.

### The Map Dashboard
The map shows potential predatory lending locations and the Annual Percentage Rate with a hover-over feature. It also shows household median income and population of each county, as well as the amount of payday loan locations as compared to the number of McDonald's in Tennessee. 

# Predictive Model
To optimize interpretability and accuracy, a decision tree was implemented. The variables used to train the model were streetcity, streetzip, income_source, employer, annualincome, incomelevel, referenceperson, status_code, purpose, payment_periods and amount approved. Other variables used that were created by DataLab interns were incomeamt, which is the monthly income per customer, totalExpense, which is the sum of a client’s housing, gas, car insurance, electric, and water expenses, and monthlyPayments, the monthly payments towards the loan which is the quotient of amount_approved and payment_periods.

Our decision tree can correctly predict whether an applicant will pay in full or default on a loan in 86% of the cases. The operational cost of mistakenly giving a loan to someone who will default on it is higher than the cost of mistakenly denying a loan to someone would have repaid it, so we selected for models that minimized overall cost.

According to the model, five of the most important variables in the prediction process are the streetzip, annual income, employer, amount_approved, and the monthlyPayments. Overall, due to the limited training data, and especially defaulting data, the model does a better job at predicting loans that will be paid off compared to loans that will default.

# Running the Model
1. Open R Studio, and make sure to have the `app.R` file open 
2. Click on the `Run` dropdown, then select `Run All`
3. The dashboard will load, automatically showing the `Predictive Model` tab 
4. Input a client’s information by filling in the text boxes and selecting the appropriate choices (calculations are required for incomeamt, totalExpense and monthlyPayments)
5. Click on `Generate Prediction` to see the probabilities 
The information will be saved onto a csv file on your laptop called `newClientData.csv`

# Public Data
There is some data sets that is opensource to the public as it shows the 
