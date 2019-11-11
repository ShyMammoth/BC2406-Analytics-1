# ==============================================================================================================
# Part 3 : Attrition
# ==============================================================================================================
setwd('C:/Users/ooimi/Documents/BC2406')
library(data.table)
data<-fread("IBM HR Data.csv")
summary(data)

# ==============================================================================================================
# Data Cleaning
# ==============================================================================================================
# Removal of less meaningful data points
data<-data[!data$EmployeeNumber == "TESTING",]
data<-data[!data$EmployeeNumber == "TEST",]
data<-data[!data$EmployeeNumber == "Test",]
data<-data[!data$EmployeeNumber == "Test 456",]
data<-data[!data$`Application ID` == "TESTING",]
data<-data[!data$`Application ID` == "Test",]

# Remove rows with at least 25 of the columns blank
data<-data[!(apply(is.na(data), 1, sum)>25)] 

# Remove shifted rows
data<-data[!data$MaritalStatus=="4",]

# Remove duplicates
data<-data[!duplicated(data),]

# Anomaly
subset(data,TotalWorkingYears<YearsAtCompany) #6 data points
subset(data,NumCompaniesWorked==0 & TotalWorkingYears>YearsAtCompany) #2800 people are at their first company but has more working years than years in the company

# Remove the anomalies
data<-data[!(TotalWorkingYears<YearsAtCompany),]
data<-data[!(NumCompaniesWorked==0 & TotalWorkingYears>YearsAtCompany),]

# Change to categorical variables
data$Gender<-factor(data$Gender)
data$BusinessTravel<-factor(data$BusinessTravel)
data$Attrition<-factor(data$Attrition)
data$Department<-factor(data$Department)
data$EducationField<-factor(data$EducationField)
data$JobRole<-factor(data$JobRole)
data$MaritalStatus<-factor(data$MaritalStatus)
data$OverTime<-factor(data$OverTime)
data$Over18<-factor(data$Over18)
data$`Employee Source`<-factor(data$`Employee Source`)

# Change satisfactions to ordinal categorical variable
data$JobSatisfaction<-factor(data$JobSatisfaction,ordered=T,levels = c(1,2,3,4))
data$RelationshipSatisfaction<-factor(data$RelationshipSatisfaction,ordered=T,levels = c(1,2,3,4))
data$EnvironmentSatisfaction<-factor(data$EnvironmentSatisfaction,ordered=T,levels = c(1,2,3,4))

# Change to numeric
data$DistanceFromHome<-as.numeric(data$DistanceFromHome)
data$PercentSalaryHike<-as.numeric(data$PercentSalaryHike)
data$HourlyRate<-as.numeric(data$HourlyRate)
data$MonthlyIncome<-as.numeric(data$MonthlyIncome)
data$JobLevel<-as.numeric(data$JobLevel)

# Drop columns with no change in variable
data<-data[,Over18:=NULL]
data<-data[,StandardHours:=NULL]
data<-data[,EmployeeCount:=NULL]


# Comparing correlation of data related to salary and JobLevel
library(corrplot)
library(RColorBrewer)
salary<-data[, .(HourlyRate, DailyRate, MonthlyRate, MonthlyIncome, JobLevel)]
m<-cor(salary,use="complete.obs")
corrplot(m, method = "color", col =  brewer.pal(n = 8, name = "RdBu"),
         type = "upper", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

# Drop HourlyRate, DailyRate and MonthlyRate as illogical for correlation between these variables to be so low
data<-data[,HourlyRate:=NULL]
data<-data[,DailyRate:=NULL]
data<-data[,MonthlyRate:=NULL]

# Change blanks to NA in Attrition
data$Attrition[data$Attrition==" "|data$Attrition==""]<-NA
data<-data[, "Attrition":=ifelse(Attrition=="Current employee",1,0)]
data$Attrition<-factor(data$Attrition)

# ==============================================================================================================
# Logistic Regression
# All variables to predict "ATTRITION" 
# ==============================================================================================================

data1<-data.table(data)

#remove irrelevant variables
data1<-data1[,EmployeeNumber:=NULL]
data1<-data1[,`Application ID`:=NULL]

m1<-glm(Attrition ~., family = binomial, data=data1, na.action=na.omit)
summary(m1)

#refitted model
m2<-glm(Attrition ~ Age+DistanceFromHome+EnvironmentSatisfaction+JobInvolvement+JobLevel+JobSatisfaction+NumCompaniesWorked+PercentSalaryHike+RelationshipSatisfaction+StockOptionLevel+TrainingTimesLastYear+WorkLifeBalance+YearsInCurrentRole+YearsSinceLastPromotion+YearsWithCurrManager,family = binomial, data=data1, na.action=na.omit)
summary(m2)
OR<-exp(coef(m2))
OR
OR.CI<-exp(confint(m2))
OR.CI

# ==============================================================================================================
# CART
# Other input variables to predict "SATISFACTIONS
# ==============================================================================================================
# CART library
library(rpart)
library(rpart.plot)
set.seed(2014)
options(digits = 5)

#dataset
data2<-data.table(data)
#remove irrelavant variables
data2<-data2[,Attrition:=NULL]
data2<-data2[,EmployeeNumber:=NULL]
data2<-data2[,`Application ID`:=NULL]


#FOR ENVIRONMENT SATISFACTION
data.env<-data.table(data2)
data.env<-data.env[,JobSatisfaction:=NULL]
data.env<-data.env[,RelationshipSatisfaction:=NULL]
cart.env<- rpart(EnvironmentSatisfaction ~ ., data=data.env, method='class', cp=0)
#rpart.plot(cart.env, nn= T, main = "Maximal Tree in cart.env")
#print(cart.env)
printcp(cart.env)
plotcp(cart.env, main = "Prune Sequence CV Errors")

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap.env <- cart.env$cptable[which.min(cart.env$cptable[,"xerror"]),
                                    "xerror"] + cart.env$cptable[which.min(cart.env$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart.env$cptable[i,j] > CVerror.cap.env) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt.env = ifelse(i > 1, sqrt(cart.env$cptable[i,1] * cart.env$cptable[i-1,1]), 1)

cart.env.opt <- prune(cart.env, cp = cp.opt.env)
rpart.plot(cart.env.opt, nn= T, main = "EnvironmentSatisfaction Optimal Tree")

cart.env.opt$variable.importance

# Confusion Matrix
tree_predict.env <- predict(cart.env.opt, newdata = data.env, type = 'class')
tree_table.env <- table(data.env$EnvironmentSatisfaction, tree_predict.env)
tree_table.env
round(prop.table(tree_table.env),3)
cat("CART Overall Accuracy Rate (EnvironmentSatisfaction): ", round(0.161+0.161+0.264+0.259,3), "\n")



# FOR JOB SATISFACTION
data.job<-data.table(data2)
data.job<-data.job[,EnvironmentSatisfaction:=NULL]
data.job<-data.job[,RelationshipSatisfaction:=NULL]
cart.job<-rpart(JobSatisfaction ~ ., data=data.job, method='class', cp=0)
#rpart.plot(cart.job, nn= T, main = "Maximal Tree in cart.job")
#print(cart.job)
printcp(cart.job)
plotcp(cart.job, main = "Prune Sequence CV Errors")

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap.job <- cart.job$cptable[which.min(cart.job$cptable[,"xerror"]),
                                    "xerror"] + cart.job$cptable[which.min(cart.job$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart.job$cptable[i,j] > CVerror.cap.job) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt.job = ifelse(i > 1, sqrt(cart.job$cptable[i,1] * cart.job$cptable[i-1,1]), 1)

cart.job.opt <- prune(cart.job, cp = cp.opt.job)
rpart.plot(cart.job.opt, nn= T, main = "JobSatisfaction Optimal Tree")
cart.job.opt$variable.importance

# Confusion Matrix
tree_predict.job <- predict(cart.job.opt, newdata = data.job, type = 'class')
tree_table.job <- table(data.job$JobSatisfaction, tree_predict.job)
tree_table.job
round(prop.table(tree_table.job),3)
cat("CART Overall Accuracy Rate (JobSatisfaction): ", round(0.187+0.182+0.292+0.298,3), "\n")


# FOR RELATIONSHIP SATISFACTION
data.rns<-data.table(data2)
data.rns<-data.rns[,JobSatisfaction:=NULL]
data.rns<-data.rns[,EnvironmentSatisfaction:=NULL]
cart.rns<- rpart(RelationshipSatisfaction ~ ., data=data.rns, method='class', cp=0)
#rpart.plot(cart.job, nn= T, main = "Maximal Tree in cart.rns")
#print(cart.rns)
printcp(cart.rns)
plotcp(cart.rns, main = "Prune Sequence CV Errors")

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap.rns <- cart.rns$cptable[which.min(cart.rns$cptable[,"xerror"]),
                                    "xerror"] + cart.rns$cptable[which.min(cart.rns$cptable[,"xerror"]), "xstd"]
# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart.rns$cptable[i,j] > CVerror.cap.rns) {
  i <- i + 1
}
# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt.rns = ifelse(i > 1, sqrt(cart.rns$cptable[i,1] * cart.rns$cptable[i-1,1]), 1)

cart.rns.opt <- prune(cart.rns, cp = cp.opt.rns)
rpart.plot(cart.rns.opt, nn= T, main = "RelationshipSatisfaction Optimal Tree")
cart.rns.opt$variable.importance

# Confusion Matrix
tree_predict.rns <- predict(cart.rns.opt, newdata = data.rns, type = 'class')
tree_table.rns <- table(data.rns$RelationshipSatisfaction, tree_predict.rns)
tree_table.rns
round(prop.table(tree_table.rns),3)
cat("CART Overall Accuracy Rate (RelationshipSatisfaction): ", round(0.177+0.198+0.308+0.283,3), "\n")
