library(readxl)
TechSales <- read_excel("TechSales2.xlsx")
#FinalTesting <- read_excel("FinalTesting.xlsx")
#Remove Null 
which(!complete.cases(TechSales))

#Remove duplicates
which(duplicated(TechSales))

#Basic Visual Stats (I used the dataframe to check for outliers as aswell)
summary(techsales)

boxplot(techsales$Age)
boxplot(techsales$Salary)
boxplot(techsales$NPS)

#Create some conditions for our data to remove outliers 
age_condition <- TechSales$Age < 90 & TechSales$Age > 15
salary_condition <- TechSales$Salary < 180000
nps_condition <- TechSales$NPS > 0 & TechSales$NPS <= 10
TechSalesCleaned <- TechSales[age_condition & salary_condition & nps_condition,]
 
#Remove first column 
TechSalesCleaned <- TechSalesCleaned[, -1]

#Summary Stats clean data 
summary(TechSalesCleaned)

#optional
options(scipen = 999, digits = 2)

#Running practice first LR model (x,y)
TechSalesCleaned.lm <- lm(Salary ~ Age, data = TechSalesCleaned)
summary(TechSalesCleaned.lm)
#Running pracrtice second improved LR model (x,x2,y)
TechSalesCleaned.lm <- lm(Salary ~ Age + NPS, data = TechSalesCleaned)
summary(TechSalesCleaned.lm)

#get numrows from dim
numrows <- dim(TechSalesCleaned)[1]

#Split data 70/30
train.index <- sample(numrows, 0.7*numrows)

#Create dfs for both sets 
train.df <- TechSalesCleaned[train.index,]
validate.df <- TechSalesCleaned[-train.index,]

#Build first ML model 
Training.lm <- lm(Salary ~ ., data = train.df )
summary(Training.lm)
#Build improved ML model
Training2.lm <- lm(Salary ~ . -Years -Personality, data = train.df)
summary (Training2.lm)

# use model to predict price in validate df
ValidatePredicted <- predict(Training2.lm, validate.df)

# create a data frame to show actual price in validate df and predicted price
Predicted.df <- data.frame(validate.df$Salary, ValidatePredicted)
summary(Predicted.df)

colnames(Predicted.df) <- c("ActualSalary", "PredictedSalary")
Predicted.df$Error <- Predicted.df$ActualSalary - Predicted.df$PredictedSalary

mean(Predicted.df$Error)
mean(Predicted.df$ActualSalary)

#############################################
#use model to predict price in FinalTesting df
newcases.df <- read_excel("TechSales2.xlsx", sheet = 2)
#Inserting the data manually: Issues with accessing data 


ValidateFinalTesting <- predict(Training2.lm, newcases.df)
summary (Training2.lm)

newcases.df$PredictedSalary <- ValidateFinalTesting



