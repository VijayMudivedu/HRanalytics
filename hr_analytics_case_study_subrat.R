# ************************************************
# Business Understanding
# ************************************************
# A large company named XYZ, employs, at any given point of time, around 4000 employees. However, every year, around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market. The management believes that this level of attrition (employees leaving, either on their own or because they got fired) is bad for the company, because of the following reasons -
#   
# Hence, the management has contracted an HR analytics firm to understand what factors they should focus on, in order to curb attrition. In other words, they want to know what changes they should make to their workplace, in order to get most of their employees to stay. Also, they want to know which of these variables is most important and needs to be addressed right away.

# ********** Goal of the case study ************

# You are required to model the probability of attrition using a logistic regression. The results thus obtained will be used by the management to understand what changes they should make to their workplace, in order to get most of their employees to stay.
# 

# ************Results Expected ************

# Write all your code in one well-commented R file; briefly, mention the insights and observations from the analysis 
# Present the overall approach of the analysis in a presentation 
# Mention the problem statement and the analysis approach briefly 
# Explain the results in business terms
# Include visualisations and summarise the most important results in the presentation

# ************ You need to submit the following two components ************:
   
# R commented file: Should include detailed comments and should not contain unnecessary pieces of code 
# Presentation:  Make a presentation to present your analysis to the chief data scientist of your company (and thus you should include both technical and business aspects). The presentation should be concise, clear, and to the point. Submit the presentation after converting it into PDF format.

# ************Important Note:************ 
# You are supposed to code entirely in R. All your plots and tables must be created in R, though you may recreate the same in Tableau as well (for the presentation) for better aesthetics. Please submit the presentation in a PDF format. Please make sure to rename your R script as "Group_Facilitator_RollNo_main.R".

# install.packages("caret")
#dev.off()

#formatR::tidy_source()
library(lubridate)
library(MASS)
library(tidyverse)
library(stats)
library(caret)
library(e1071)
library(reshape2)
library(ggplot2)
library(car)
library(gains)
library(ROCR)

# READ ALL EXCEL FILES
#  setwd("~/OneDrive/OneDrive - Atimi Software Inc/Upgrad/case study/HR Analytics CaseStudy/HRAnalytics Case Study")

# read HR analytics files
employee_survey_data <- read.csv("PA-I_Case_Study_HR_Analytics/employee_survey_data.csv",stringsAsFactors = TRUE)
general_data <- read.csv("PA-I_Case_Study_HR_Analytics/general_data.csv",stringsAsFactors = TRUE)
in_time_data <- read.csv("PA-I_Case_Study_HR_Analytics/in_time.csv",stringsAsFactors = F,check.names = F)
manager_survey_data <- read.csv("PA-I_Case_Study_HR_Analytics/manager_survey_data.csv",stringsAsFactors = TRUE)
out_time <- read.csv("PA-I_Case_Study_HR_Analytics/out_time.csv",stringsAsFactors = F,check.names = F)


# viewing the dataframe
head(general_data)
head(in_time_data) 
head(out_time)
head(manager_survey_data)
head(employee_survey_data)

# ********************************************************
# DATA UNDERSTANDING - Data Preparation
# ********************************************************

# renaming the 1st column name missing of (in_time_date, out_time) with EmployeeID--Assumption as 4410 observations present--

names(in_time_data)[which(names(in_time_data) == "")] <- "EmployeeID"
names(out_time)[which(names(out_time) == "")] <- "EmployeeID"

# -----------------------
# CHECK DUPLICATES AND NA IN DIFFERENT DATASETS
# -----------------------
sapply(list(general_data,in_time_data,
            manager_survey_data,
            employee_survey_data,
            out_time), function(x) sum(!duplicated(x$EmployeeID)))

# Check for number of NA's in each data set 
sum(is.na(general_data)) # 28 
sum(is.na(in_time_data)) # 109080
sum(is.na(out_time)) # 109080
sum(is.na(manager_survey_data)) # 0
sum(is.na(employee_survey_data)) # 83

# COMMENTS:
# There are no duplicates. Employee ID column has 4410 unique records
# in_time and out_time Data.Frames has time in chars AND lot of NAs which will treated in future codes
# Employee ID to be converted to Factors.
# Thus the assumption that the EmployeeID holds true for in_time and out_time data
# general_data, employee_survey_data has few NAs which will be treated in future codes

# ------------------------------------------------
# ANALYSING of in_time_data and out_time dataframes
# ------------------------------------------------

# GOAL OF THIS ANALYSIS IS TO:
# 1.Find regularity of an employee
# 2.Vacations Taken by employee, 
# 3.Omit Statutory Holidays .Data with NAs is assumed as "statutory_holidays/Vacations"

# check if the headers of the data.frames are identifical.
sum(names(in_time_data) != names(out_time))

# COMMENTS: all columns are idenditcal in_time and out_time dataframe.

# ********************************************************
# Derived metrics from the in_time and out_time
# ********************************************************

# creating an empty matrix for calculating and storing the in_out_duration
in_out_dur <- matrix(data = 0,nrow = nrow(in_time_data), ncol = ncol(in_time_data))

# creating the in_out_duration matrix that stores the calculated difference of the work timings
for (i in 1:ncol(in_time_data)) {
  in_out_dur[,i] <- difftime(time1 = strptime(out_time[,i], format = "%Y-%m-%d %H:%M:%S"),
                           time2 = strptime(in_time_data[,i],format = "%Y-%m-%d %H:%M:%S"), units = "hours")
}

# removing the first column which is the employee id column and converting the in_out_dur matrix to a dataframe
in_out_dur <- data.frame(in_out_dur[,-which(names(in_time_data) == "EmployeeID")])

# replacing the NAs with 0s 
in_out_dur[is.na(in_out_dur)] <- 0

# converting the in_out_dur to data.frame
in_out_dur <- round(in_out_dur,2)

# copying the headers of data frame in_time data to in_out_dur
names(in_out_dur) <- c(names(in_time_data[,-which(names(in_time_data) == "EmployeeID")]))
head(in_out_dur)

#--------------------------------------------------
# OBJECTIVES OF STUDY OF in_out_dur data.frame
#--------------------------------------------------
# columns that have only zeros have a pattern. Finding with columns have the all columns as time_offs and 
# removing the data frame that has all Statutory Holidays
# This can be indentified by identifying the ColSums of the data and equating with nrows.
# Prepare a data frame without statutory holidays
# #--------------------------------------------------

# Computing the Zero Date Columns
all_zero_date_columns <- which(colSums(in_out_dur == 0) == nrow(in_out_dur))
all_zero_date_columns
# names of the columns that have zeros
names_of_columns_with_all_zeros <- names(in_out_dur[all_zero_date_columns])
names_of_columns_with_all_zeros

# Deleting the columns that have statutory holidays
in_out_data_without_stats <- in_out_dur[,-which(names(in_out_dur) %in% names_of_columns_with_all_zeros)]
# View(in_out_data_without_stats)

# Difference between the number of columns between two dataframes
ncol(in_out_dur) - ncol(in_out_data_without_stats)

# COMMENTS: Thus there are 12 Statutory Holidays

# ----Derived Metric #1: Vacations taken by each employee ----
vacations_taken_by_employee <- rowSums(in_out_data_without_stats == 0)
general_data$vacations <- vacations_taken_by_employee

#---- Derived Metric #2: Calculating the mean working hours of each excluding the vacations, stats, and weekends. ----
# Mean attendance without considering the vacations
general_data$mean_attendance <-  apply(in_out_data_without_stats, MARGIN = 1, 
                                       FUN = function(x) mean(x[1:ncol(in_out_data_without_stats)][x > 0],na.rm = TRUE))

# ----Derived Metric #3: Computing the regularity to work ----
# An employee is irregular if an employee works for less than 7 hours on a working data (not considering vacations and statutory holidays)
regularity_df <- data.frame(work_cnt = rowSums((in_out_data_without_stats > 0) & (in_out_data_without_stats < 7)))

# ---- Studying the distribution of data in the regularity_df. ----
table(regularity_df)
quantile(regularity_df$work_cnt,seq(0,1,0.25))

# Plotting histogram of regularity_df to visualize the distribution of data
cbind(regularity_df,atri = general_data$Attrition) %>% filter(work_cnt > 0) %>%  # non-zero work counts
  ggplot(aes(work_cnt,fill = atri)) + geom_histogram(binwidth = 2) +
  labs(title = "Regularity to Work of Employees in one Year") +
  xlab(label = "Daily Work distribution in one Year") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8,0.8))


# COMMENTS: 
# Thus 50% of data is less than 20 times in 265 working dates, 
# 75 % percentile of data is 205, 
# 100% percentile is 249

# Classifying the employees who are regular, irregular, chronic_irregular
# Categories defined below: 
# 0-"regular",
# 1-"irregular",
# 2-"chronic_irregular"

general_data$work_regularity <- cut(as.numeric(rowSums((in_out_data_without_stats > 0) & (in_out_data_without_stats < 7))), 
                                      breaks = c(0,20,205,245), 
                                      labels = c(0,1,2),
                                      include.lowest = T)

# ---- Derived Metric #4: WORKLOAD ----
# An employee is assumed to be overworked if an employe works for more than 9 hours intermittently, on-off continuously, and consistently

workload_df <- data.frame(work_cnt = rowSums((in_out_data_without_stats > 9)))

# Most of the data is accumlated in 0.
table(workload_df)
quantile(workload_df$work_cnt ,seq(0,1,0.25))

# Plotting the workload
cbind(workload_df,atri = general_data$Attrition) %>% filter(work_cnt > 0) %>% 
  ggplot(aes(work_cnt,fill = atri)) + geom_histogram(binwidth = 2) + 
  labs(title = "Workload Distribution of Employees in one Year") +
  xlab(label = "Daily Work distribution in one Year") +
  theme(axis.title.y = element_blank(),
        legend.position = c(0.8,0.8))

# WORKLOAD CLASSIFICATION: employees with heavy work load working overtime
# Classification of Overtime: is based on the aggregated number of overtime incident counts in a year.
# 0-"Normal", # count
# 1-"Heavy",  # count
# 2-"Severe"  # count

# evaulating the workload count for each employee
rowSums(in_out_data_without_stats > 9)

general_data$workLoad <- cut(as.numeric(rowSums(in_out_data_without_stats > 9)), 
                                   breaks = c(0,3,14,250), 
                                   labels = c(0,1,2),
                                   include.lowest = T)

#------------------------
# MERGING OF DATAFRAMES: general_data, manager_survey_data, employee_survey_data, derived metrics from in_time, Out_time
#------------------------

# checking if the EmployeeIDs in the general_data, manager_survey_data, employee_survey_data is unique
setdiff(x = manager_survey_data$EmployeeID, y = employee_survey_data$EmployeeID)
setdiff(x = general_data$EmployeeID, y = employee_survey_data$EmployeeID)
setdiff(x = in_time_data$EmployeeID, y = employee_survey_data$EmployeeID)
# COMMENTS: there are no outliers

# merge employee survey data and manager_survey_data
employee_survey_data <- merge(x = employee_survey_data,y = manager_survey_data,
                              by = intersect(names(employee_survey_data),names(manager_survey_data)))

# Removing unwanted fields , EmployeeCount, Over 18, StandardHours which do not change
general_data <- general_data[,-which(names(general_data) %in% c("Over18","StandardHours","EmployeeCount"))]

# merge general_data and employee_survey_data dataframes
employee_master <-  merge(x = general_data,y = employee_survey_data, 
                          by = intersect(names(general_data),names(employee_survey_data)))
#------------------------
# Data Quality Checks - checking Invalid Records
#------------------------
# Total Working years less than Year at Company
sum(employee_master$TotalWorkingYears < employee_master$YearsAtCompany,na.rm = T)
# Total WOrking years less than YearsWith CurrManager
sum(employee_master$TotalWorkingYears < employee_master$YearsWithCurrManager,na.rm = T)
# Years At Company Less than Years with Current Manager
sum(employee_master$YearsAtCompany < employee_master$YearsWithCurrManager,na.rm = T )
# Monthly Income is Zero
sum(employee_master$MonthlyIncome == 0)
# Total Years At Company is less than YearsSince Last Promotion
sum(employee_master$YearsAtCompany < employee_master$YearsSinceLastPromotion,na.rm = T )

#------------------------
# Identifying and Deleting the columns that have NAs
#------------------------
XempMaster <- sapply(employee_master, function(x) sum(is.na(x)))
XempMaster[which(XempMaster > 0)]

# Count of NAs in each columns
# NumCompaniesWorked: 19     TotalWorkingYears:9        EnvironmentSatisfaction: 25         JobSatisfaction: 20         WorkLifeBalance: 38

# NumCompaniesWorked, TotalWorkingYears, EnvironmentSatisfaction, Jobsatisfaction, WorklifeBallance of an employee is subjective. It doesn't make sense to replace the NA's with median or mean of the respective variable. Thus deleting the records of the employees that do not have enough data is one among the better solutions. But before attempting to remove the recors, using the Weight of Evidence and Information Values, method of transformation of variables validate the claim, let 

#-------------------------
# Using WOE and IV analysis
#-------------------------

# Attempting to approximate the "NA"s in NumCompaniesWorked,EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance using the Weight of Evidence (WOE) and Information Values (IV) methods

employee_master_cleaned <- employee_master

# Replacing the NA values as "missing" 
employee_master_cleaned$NumCompaniesWorked[which(is.na(employee_master_cleaned$NumCompaniesWorked))] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$EnvironmentSatisfaction)),"EnvironmentSatisfaction"] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$JobSatisfaction)),"JobSatisfaction"] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$WorkLifeBalance)),"WorkLifeBalance"] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$TotalWorkingYears)),"TotalWorkingYears"] <- "missing"
# employee_master_cleaned$NumCompaniesWorked[which(is.na(employee_master_cleaned$NumCompaniesWorked))] <- "missing"

# converting the attrition into 1's and 0's for WOE analysis
employee_master_cleaned$Attrition <- ifelse(employee_master_cleaned$Attrition == "Yes",1,0)

# convertying the variables of NumCompaniesWored to factors
employee_master_cleaned$NumCompaniesWorked <- factor(employee_master_cleaned$NumCompaniesWorked)

#install.packages("Information")
library(Information) # WOE analysis Begins ----

# creating the information values of the "employee_master_cleaned" data frame to see if the missing values can be imputed with the nearest numbers.
IV_employee_master <- create_infotables(data = employee_master_cleaned[which(names(employee_master_cleaned) %in% c("Attrition","NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","TotalWorkingYears"))],
                                        y = "Attrition", 
                                        bins = 6,
                                        parallel = TRUE)

print(IV_employee_master)

# WOE of Missing values = 0.39 of TotalworkingYears lie between and 2 and 3 years, closer to 3 years (0.35). 
# Thus imputing the missing values with years and calculating the WOE and IV independently for TotalWorkingYears

employee_master_cleaned$TotalWorkingYears[which(employee_master_cleaned$TotalWorkingYears == "missing")] <- 3

# converting the character data to numeric data..
employee_master_cleaned$TotalWorkingYears <- as.numeric(employee_master_cleaned$TotalWorkingYears)

# creating WOE bins and checking the monontonic increase in the solution..
IV_TotalWorkingYears <- create_infotables(data = employee_master_cleaned[which(names(employee_master_cleaned) %in% c("Attrition","TotalWorkingYears"))],y = "Attrition", bins = 10,parallel = TRUE)

print(IV_TotalWorkingYears)

# COMMENTS: Thus for the data IV values - 0.42 is a strong predictor for the variable Total working years of experince.
# For other variables, predictors range from intermediate to Weak IV Values

xNumCompnaiesWorked <- IV_employee_master$Tables["NumCompaniesWorked"]
xNumCompnaiesWorked_df <- as.data.frame(xNumCompnaiesWorked)
#View(xNumCompnaiesWorked_df)
str(xNumCompnaiesWorked_df)

# Plotting the variables with NAs and approximating the predictor analysis using GGPLOT. 
library(ggplot2)

# plotting for number of companies worked
ggplot(xNumCompnaiesWorked_df,aes(x = NumCompaniesWorked.NumCompaniesWorked,y = NumCompaniesWorked.IV)) + 
  geom_point() +
  xlab(label = "Num of Companies Worked") + ylab(label = "WOE")

# plotting the graph for environment Survey
ggplot(data.frame(IV_employee_master$Tables["EnvironmentSatisfaction"]),aes(x = (EnvironmentSatisfaction.EnvironmentSatisfaction),y = EnvironmentSatisfaction.IV)) +  geom_point() +
  xlab(label = "EnvironmentSatisfaction") + ylab(label = "WOE")

# plots for "JobSatisfaction"
ggplot(data.frame(IV_employee_master$Tables["JobSatisfaction"]),aes(x = JobSatisfaction.JobSatisfaction,y = JobSatisfaction.IV)) + geom_point() +
  xlab(label = "JobSatisfaction") + ylab(label = "WOE")

# plots for "WorkLifeBalance"
ggplot(data.frame(IV_employee_master$Tables["WorkLifeBalance"]),aes(x = WorkLifeBalance.WorkLifeBalance,y = WorkLifeBalance.IV)) + geom_point() + geom_line() +
  xlab(label = "WorkLifeBalance") + ylab(label = "WOE")


# plotting the graph for Total Working Years
ggplot(data.frame(IV_TotalWorkingYears$Tables["TotalWorkingYears"]),aes(x = reorder(factor(TotalWorkingYears.TotalWorkingYears),TotalWorkingYears.IV),y = TotalWorkingYears.IV)) + geom_point() +
  xlab(label = "TotalWorkingYears") + ylab(label = "WOE")

# WOE of missing values aren't any closer to the nearest WOE segment. Thus it makes sense to remove the records for the below reasons:
# 1. It is difficult to emperically add missing values using Weights of Evidences of these variables
# 2. Adding mean and median values to the missing values of  "NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance"  isn't logical as each of these variables are subjective each to each individual. 

# Imputing the rows that have NAs and that attribute to not so significant predictors

employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$NumCompaniesWorked == "missing"),]
employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$EnvironmentSatisfaction == "missing"),]
employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$JobSatisfaction == "missing"),]
employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$WorkLifeBalance == "missing"),]
#employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$TotalWorkingYears == "missing"),]
head(employee_master_cleaned)

# # calculating the data loss as a result of missing
 paste(100 - (round(nrow(employee_master_cleaned)*100/nrow(employee_master),2)),"%")
# # COMMENTS: Thus approximately 2.3% of data is lost as result of removing the "NAs"

# Converting categorical variables to factors
colnames(employee_master_cleaned)
cols_factors <- c("EmployeeID","Attrition","BusinessTravel","Department","Education","EducationField","Gender","JobLevel","JobRole","MaritalStatus", "StockOptionLevel","JobInvolvement","PerformanceRating","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance","work_regularity","workLoad")

# converting integer columns to factors
employee_master_cleaned[cols_factors] <- lapply(employee_master_cleaned[cols_factors], factor)

#employee_master_cleaned$YearsSinceLastPromotion <- as.integer(employee_master_cleaned$YearsSinceLastPromotion)
 employee_master_cleaned$NumCompaniesWorked <- as.integer(employee_master_cleaned$NumCompaniesWorked)
 employee_master_cleaned$TrainingTimesLastYear <- as.integer(employee_master_cleaned$TrainingTimesLastYear)
 
 # temp dataframe
temp_employee_df_missing <- employee_master_cleaned
#employee_master_cleaned <- temp_employee_df_missing

library(reshape2)

# Checking unique factors in the dataframe
sapply(employee_master_cleaned, unique)
# COMMENTS: 
# 1. there are no duplicates in the categories
# 2. there are no spelling mistakes

# Renaming the categories:
# Business Travel
# Replacing the long values with shorter ones.
levels(employee_master_cleaned$BusinessTravel) <- c("no_travel","frequently","rarely")

# Department
levels(employee_master_cleaned$Department) <-  c("HR","RnD","sales")

# EducationField
# xlevel <- employee_master_cleaned$EducationField
# xlevel %>% head(10)
# levels(xlevel) <- c("HumRes","LifSci","Mrkt","Med","Oth","TecDeg")
#employee_master_cleaned$EducationField <- temp_employee_df_missing$EducationField
levels(employee_master_cleaned$EducationField) <- c("HumRes","LifSci","Mrkt","Med","Oth","TecDeg")

# JobRole
# xlevel <- employee_master_cleaned$JobRole
# xlevel %>% head(10)
# levels(xlevel) <- c("HlthCreRep","HumRes","LabTech","Mngr","Manf_Dir","Res_Dir","ResSci","SalesExec","SalesRep")
levels(employee_master_cleaned$JobRole) <- c("HlthCreRep","HumRes","LabTech","Mngr","Manf_Dir","Res_Dir","ResSci","SalesExec","SalesRep")


#########################################
#### EXPLORATORY DATA ANALYSIS
#########################################

# Checking for outliers in Continuous variables
#---------------------------------------------------
# OUTLIERS IN THE DATASETS
#---------------------------------------------------
str(employee_master_cleaned)
# separating continuous variables for outlier treatment
cont_vars <- c("EmployeeID","MonthlyIncome","Age","DistanceFromHome","PercentSalaryHike","TotalWorkingYears","YearsAtCompany","NumCompaniesWorked", "TrainingTimesLastYear","YearsSinceLastPromotion","YearsWithCurrManager","vacations","mean_attendance")

# Visualizing the outliers of continuous variables using boxplots
melt(data = employee_master_cleaned[cont_vars],id.vars = "EmployeeID") %>% 
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free")
# COMMENTS: From the plots it can be inferred that Monthly Income, Yars At Company, Total WOrking years, YearsSince last Promotion, are plagued with outliers

# ---------- checking the outliers across all the variables in the df dataset ----------
sapply(employee_master_cleaned[cont_vars][,-which(names(employee_master_cleaned) == "EmployeeID")], function(x) quantile(x,seq(0,1,0.02))) 

# ---------- checking the summary of outliers ----------
sapply(employee_master_cleaned[cont_vars][,-which(names(employee_master_cleaned) == "EmployeeID")], summary)

# Keeping the data within the limit of IQR of 1.58.

# ---------- MONTHLY INCOME OUTLIERS: Removing the outliers in the Monthly Income
boxplot.stats(x = employee_master_cleaned[cont_vars]$MonthlyIncome,coef = 1.58)$out
# removing the outliers
employee_master_cleaned <- employee_master_cleaned[!(employee_master_cleaned$MonthlyIncome %in% 
                                                       boxplot.stats(x = employee_master_cleaned$MonthlyIncome,coef = 1.58)$out),]

# verifing the employee_master_after removing outliers 
sapply(employee_master_cleaned[cont_vars][,-which(names(employee_master_cleaned) == "EmployeeID")],summary)

# ---------- AGE outliers: removing outliers in age
boxplot.stats(x = employee_master_cleaned$Age,coef = 1.58)$out
# No outliers found.

# ---------- DistanceFromHome outliers. 
boxplot.stats(x = employee_master_cleaned$DistanceFromHome,coef = 1.58)$out
# Appartently There are no outliers

# ---------- PercentSalaryHike Outliers
boxplot.stats(x = employee_master_cleaned$PercentSalaryHike,coef = 1.58)$out
# No Outliers found

# ---------- TotalWorkingYears Outliers
boxplot.stats(x = employee_master_cleaned[cont_vars]$TotalWorkingYears,coef = 1.58)$out
# outliers found from TotalWorkingYears are removed
employee_master_cleaned <- employee_master_cleaned[!(employee_master_cleaned$TotalWorkingYears %in% 
                                                       boxplot.stats(x = employee_master_cleaned$TotalWorkingYears,coef = 1.58)$out),]
# verifying the dataframe after removing the outliers
sapply(employee_master_cleaned[cont_vars][,-which(names(employee_master_cleaned) == "EmployeeID")],summary)

# ---------- Outliers in "YearsAtCompany"
boxplot.stats(employee_master_cleaned[cont_vars]$YearsAtCompany,coef = 1.58)$out
#  verifying the count of outliers before removing them
table(boxplot.stats(employee_master_cleaned[cont_vars]$YearsAtCompany,coef = 1.58)$out)
#  count of "YearsAtCompany and Attrition
table(employee_master_cleaned$YearsAtCompany,employee_master_cleaned$Attrition)
# COMMENTS: thus the outliers in YearsAtCompany are miniscule and can be removed.
# removing the outliers found YearsAtCompnay
employee_master_cleaned <- employee_master_cleaned[!(employee_master_cleaned$YearsAtCompany %in% 
                                                       boxplot.stats(x = employee_master_cleaned$YearsAtCompany,coef = 1.58)$out),]

# ---------- Outliers in "NumCompaniesWorked"
boxplot.stats(employee_master_cleaned$NumCompaniesWorked,coef = 1.58)$out
# removing the outliers in the NumofCompaniesWorked
employee_master_cleaned <- employee_master_cleaned[!(employee_master_cleaned$NumCompaniesWorked %in% 
                                                       boxplot.stats(x = employee_master_cleaned$NumCompaniesWorked,coef = 1.58)$out),]

# ---------- Outliers in "YearsSinceLastPromotion"
# Suspecting that this variable is highly likely impact attrition, outliers aren't is being uptaken up for the below reason:
boxplot.stats(employee_master_cleaned$YearsSinceLastPromotion,coef = 3.58)$out
table(boxplot.stats(employee_master_cleaned$YearsSinceLastPromotion,coef = 3.58)$out)
table(employee_master_cleaned$YearsSinceLastPromotion,employee_master_cleaned$Attrition)
# COMMENTS:Although employees with 10 years of experiences have 9/40 ~ 25% of attrition which is a considerable, they are miniscule number of outliers
# Hence ignoring the Outliers in YearsSinceLast Promotion

employee_master_cleaned <- employee_master_cleaned[!(employee_master_cleaned$YearsSinceLastPromotion %in% 
                                                       boxplot.stats(x = employee_master_cleaned$YearsSinceLastPromotion,coef = 3.58)$out),]

# ---------- Outliers in "YearsWithCurrManager"
boxplot.stats(employee_master_cleaned$YearsWithCurrManager,coef = 1.58)$out
# COMMENTs: there are very small set of outliers in "YearsWithCurrManager" ignoring them. 

# ---------- TrainingTimesLastYear outliers 
table(boxplot.stats(employee_master_cleaned$TrainingTimesLastYear,coef = 1.58)$out)
# COMMENTS: this variable is not highly likely impact attrition AND the outliers can be ignored.

# table(employee_master_cleaned$TrainingTimesLastYear,employee_master_cleaned$Attrition)
# # employees with 15 years of experiences have 9/40 ~ 25% of attrition which is a considerable, hence ignoring the Outliers in YearsSinceLast Promotion
# employee_master_cleaned <- employee_master_cleaned[!(employee_master_cleaned$TrainingTimesLastYear %in% 
#                                                        boxplot.stats(x = employee_master_cleaned$TrainingTimesLastYear,coef = 3.58)$out),]

# summary before cleaning the outliers
summary(employee_master[cont_vars][,-which(names(employee_master_cleaned) == "EmployeeID")])
# summary of the data_after cleaning the outliers
summary(employee_master_cleaned[cont_vars][,-which(names(employee_master_cleaned) == "EmployeeID")])

# boxplots of continuous variables devoid of outliers
melt(data = employee_master_cleaned[cont_vars],id.vars = "EmployeeID") %>%
  ggplot(aes(x = variable, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = expression(paste(bold("After removing outliers"))),
       x = expression(paste(italic("Continuous variables in data set"))),
       y = expression(paste(italic("Values")))) + 
  theme(text = element_text(size = 11),
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(linetype = "dotted",fill = NA))

# COMMENTS: Thus, removing the outliers that signficiantly impacted the mean median values of the variables.

temp_employee_df <- employee_master_cleaned
# employee_master_cleaned <- temp_employee_df

# reverting the Attrition to "Yes", "No"
employee_master_cleaned$Attrition <- ifelse(test = employee_master_cleaned$Attrition == 1, "Yes","No")
employee_master_cleaned$Attrition <- as.factor(employee_master_cleaned$Attrition)

#--------------------------------------------------------------------------
# UNIVARATE ANALYSIS - NOMINAL CATEGORIES:"BusinessTravel", "Department" "JobRole" "MaritalStatus" "EducationField" "Gender" 
#--------------------------------------------------------------------------

str(employee_master_cleaned)
library(reshape2)
library(ggthemes)

 temp_df1 <- as.data.frame(melt(data = subset(employee_master_cleaned,
                   select = c("Attrition","BusinessTravel", 
                              "Department","JobRole", 
                              "MaritalStatus","EducationField", "Gender")),
     id.vars = "Attrition") %>% 
  group_by(variable,value, Attrition) %>% 
  summarise(value_count = n())  %>%
  mutate( per_cnt = paste0(round(value_count*100/sum(value_count)),"%")) )

# Plotting the temp_df1 data
 temp_df1 %>% #filter(Attrition == "Yes" ) %>% # filter "Attrition" for "Yes"
  ggplot(aes(x = factor(reorder(value,-value_count)),y = value_count, fill = Attrition)) +
  geom_bar(position = "fill",stat = "identity") + 
   geom_text(aes(label = per_cnt),position = position_fill(vjust = 0.5),size = 3) +
  coord_flip() +
  facet_wrap(facets = ~variable,scales = "free",ncol = 3) +
    labs(title = expression(paste(bold("% Attrition by each Nominal category")))) +
    theme(#axis.text.x = element_text(angle = 90,size = 10),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          panel.grid.major = element_line(colour = "grey80"),
          panel.border = element_rect(linetype = "dotted",fill = NA)) + 
  scale_fill_manual(values = c("snow","darksalmon")) 
 
# COMMENTS: BUSINESS TRAVEL-  TRAVEL_FREQUENTLY, TRAVEL_RARELY ; 
# ** DEPARTMENT: HRD, R&D, Sales ; 
# ** JOBROLE - Reserach Director, Research Scientist, Sales Rep and Sales Execs, Lab Technicians; 
# ** MARITAL STATUS: Single and Married have highest Attrition; 
# ** EDUCATION FIELD - Human Resources, Life sciences, Medical Professionals have  
#  Have strong correlation

#---------------------------------------------------
# PLOTS BY - ORDINAL CATEGORIES
#---------------------------------------------------

# "Education", "StockOptionLevel","work_regularity","workLoad",
# "JobLevel","EnvironmentSatisfaction",
# "JobSatisfaction","WorkLifeBalance","JobInvolvement","PerformanceRating",

temp_df6 <- as.data.frame(melt(data = subset(employee_master_cleaned,
                   select = c("Education", "StockOptionLevel","work_regularity","workLoad",
                              "JobLevel","Attrition")),id.vars = "Attrition") %>% 
                     group_by(variable,value, Attrition) %>% 
                     summarise(value_count = n())  %>%
                     mutate( per_cnt = paste0(round(value_count*100/sum(value_count)),"%")))

 # Plotting the temp_df1 data
 temp_df6 %>%     # filter "Attrition" for "Yes"
  ggplot(aes(x = factor(reorder(value,-value_count)),y = value_count, fill = Attrition)) +
  geom_bar(position = "fill",stat = "identity") + geom_text(aes(label = per_cnt),position = position_fill(vjust = 0.5),size = 3) +
  facet_wrap(facets = ~variable,scales = "free",ncol = 2) +
  labs(title = expression(paste(bold("% Attrition by each Ordinal category")))) +
    theme(axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    panel.grid.major = element_line(colour = "grey80"),
    panel.border = element_rect(linetype = "dotted",fill = NA)) + 
  scale_fill_manual(values = c("grey69","darksalmon"))

 
 
 temp_df4 <- as.data.frame(melt(data = subset(employee_master_cleaned,
                                              select = c("JobSatisfaction","WorkLifeBalance","EnvironmentSatisfaction",
                                                          "JobInvolvement","PerformanceRating","Attrition")),id.vars = "Attrition") %>% 
                             group_by(variable,value, Attrition) %>% 
                             summarise(value_count = n())  %>%
                             mutate( per_cnt = paste0(round(value_count*100/sum(value_count)),"%")))
 head(temp_df4)
 
 # Plotting the temp_df1 data
 temp_df4 %>%     # filter "Attrition" for "Yes"
   ggplot(aes(x = factor(reorder(value,-value_count)),y = value_count, fill = Attrition)) +
   geom_bar(position = "fill",stat = "identity") + geom_text(aes(label = per_cnt),position = position_fill(vjust = 0.5),size = 3) +
   facet_wrap(facets = ~variable,scales = "free",ncol = 2) +
   labs(title = expression(paste(bold("% Attrition by each Ordinal category")))) +
   theme(axis.ticks.x = element_blank(),
         axis.text.y = element_blank(),
         axis.ticks.y = element_blank(),
         axis.title.x = element_blank(),
         axis.title.y = element_blank(),
         plot.title = element_text(hjust = 0.5),
         legend.position = "bottom",
         panel.grid.major = element_line(colour = "grey80"),
         panel.border = element_rect(linetype = "dotted",fill = NA)) + 
   scale_fill_manual(values = c("grey69","darksalmon"))
 

# COMMENTS: Performance Rating-4,3, JobInvovlment = 1,2, EnvironmentSatisifaction-1, JobSatisifaction -1, WorkLifeBalance 1-4,
# Education levels: 2(college),3(Bachelor),4(Master); StockOption Levels 0,1,2; JobLevel - 0,1 - are strong indicators of Attrition
# Additionally, Workload: Heavily Worked Individuals, and less work individuals are more likely to be on Attrition
# Regularity: From the data, employees regular to work shows higher attrition over other factors indicators.

#---------------------------------------------------
# PLOTS BY - INTERVAL CONTINUOUS VARIABLES 
#---------------------------------------------------
# employee_master_cleaned <- temp_employee_df
library(RColorBrewer)

melt(data = subset(employee_master_cleaned,
                   select = c("DistanceFromHome","Age","Attrition","vacations", "mean_attendance")),
     id.vars = "Attrition") %>% 
  ggplot(aes(x = value, fill = Attrition)) +
  geom_histogram(binwidth = 2, aes(y = ..density..)) + 
  scale_fill_manual(values = c("grey69","seagreen3")) +
  geom_density(aes(alpha = .001,col = Attrition)) + 
  scale_color_brewer(palette = "BrBG",direction = 1) +
  scale_alpha_identity(guide = "none") + 
  facet_wrap(facets = ~variable,scales = "free",ncol = 2) +
  labs(title = expression(paste(bold("% Attrition of Interval variables"))),
       y = expression(paste(italic("Density distribution")))) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        # panel.background = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_rect(linetype = "dotted",fill = NA)) 


# NOTE: Change the attrtion attribute for Yes in the density plot.
# COMMENTS: Some Strong indicators of Attrition from Density plot
# Distnace From 0-10 KM, 
# Age: 25-35 
# Vacations: More Spiked Vacations between 20 and 30
# Attendance: Spiked 7 hours or 10 hours are some of the good indicators from data with Attrition


# PLOTTING ORDINAL AND INTERVAL VARIABLES TOGETHER
# gridExtra::grid.arrange(p1,p2,nrow = 1,widths = 2:1)

#---------------------------------------------------
# PLOTS FOR RATIO VARIABLES:
#---------------------------------------------------
# str(employee_master_cleaned$TrainingTimesLastYear)
# [13] "MonthlyIncome" "PercentSalaryHike"
# [17] "TotalWorkingYears" "YearsAtCompany" "YearsSinceLastPromotion"
# [21] "YearsWithCurrManager"    

# subsetting the data that contain the continuous variables
df_Ratio_variables <- subset(employee_master_cleaned,
                             select = c("MonthlyIncome","PercentSalaryHike","TotalWorkingYears",
                                        "YearsAtCompany","YearsSinceLastPromotion","YearsWithCurrManager",
                                        "TrainingTimesLastYear"))

library(GGally)
library(ggplot2)
# creating the continuous variable plots using ggpairs
ggpairs(df_Ratio_variables) + 
  labs(title = expression(paste(bold("Plotting correlations between variables")))) +
  theme(text = element_text(size = 8),
        panel.grid.major = element_line(colour = "grey80"))

# Comments:
# There appears strong Positive correlation betwen the variables: 
# a. YearsAtCompany and YearsWithCurrManager; YearsAtCompnay and YearsSinceLastPromotion)
# c. YearsAtCompany and TotalWorkingYears
# d. YearSinceLastPromotion and TotalWorkingYears
# e. YearsWithCurrManager and TimeSinceLastPromotion.

# Thus the variables: "YearsAtCompany" "YearsSinceLastPromotion","TotalWorkingYears","YearsAtCompany"are strongly correlated. 

################################################################
# ********** MODEL BUILDING *********
################################################################

# Reseting the Attrition variable to "yes" "no"
employee_master_cleaned$Attrition <- temp_employee_df$Attrition
employee_master_cleaned$Attrition <- factor(ifelse(employee_master_cleaned$Attrition == 1, "Yes","No"))

# Scaling the continous Variables: Age, DistanceFromHome,MonthlyIncome,PercentSalaryHike, Vacations, mean_attendance:
scale_var_emp <- c("Age", "DistanceFromHome","MonthlyIncome","PercentSalaryHike", "vacations", "mean_attendance")

# scaling the variables and assigning to the employee_master_cleaned
employee_master_cleaned[scale_var_emp] <-  lapply(employee_master_cleaned[scale_var_emp], scale) 

#------------------------------------------------
# DUMMY VARIABLES
#------------------------------------------------

# Copying the details of the cleaned and merged data frame without the EmployeeID column:
employee_dummy_var_df <- employee_master_cleaned[,-which(names(employee_master_cleaned) == "EmployeeID")]

library(caret) # USED TO CREATE DUMMY VARIABLES -----

# using the dummyVars function, from the caret Library, for transforming the categorical variables in "employee_master_cleaned" data to dummy variables.
# This would eliminate several lines of code compared to model.matrix(), instead of repeatedly applying on each of the categorical variables.

xDummy <- dummyVars(formula = ~ .,data = employee_dummy_var_df,sep = "_", levelsOnly = FALSE, fullRank = TRUE)

employee_dummy_var_df <- data.frame(predict(xDummy,employee_dummy_var_df))

# Final Dataset
emp_master_final <- employee_dummy_var_df
head(emp_master_final)
dim(emp_master_final)

#------------------------------------------------
# SAMPLING OF datasets - test and training dataset of employee
#------------------------------------------------
set.seed(100)
# creating a test and 
# index of randowm numbers
index <- sample(x = 1:nrow(emp_master_final),size = 0.7 * nrow(emp_master_final))

# Test and training Datasets
train <- emp_master_final[index,]
test <- emp_master_final[-index,]

#------------------------------------------------
# MODEL CREATION
#------------------------------------------------
# creating the initial model
hr_model_01 <- glm(formula = Attrition_Yes ~ .,family = "binomial",data = train)

# optimizing the hr_model_1 using StepAIC
hr_model_02 <- stepAIC(hr_model_01,direction = "both")
# 61 variables were reduced to 30 variables after regressing through Akaike's Information Criterion 

# Summarizing the hr_model_02 
summary(hr_model_02)

# checking the collinearility
library(car)
vif(hr_model_02)

# VIF of the variables is relatively low. Prioritizing the p-Value over VIF
# JobInvolvement_2	-0.29301	0.20096	-1.458	p-value= 0.1448 qualify for removal

hr_model_03 <-  glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_2 + Education_5 + 
    EducationField_Mrkt + EducationField_Oth + EducationField_TecDeg + 
    JobLevel_4 + JobRole_HumRes + JobRole_LabTech + JobRole_Manf_Dir + 
    JobRole_Res_Dir + JobRole_ResSci + JobRole_SalesExec + MaritalStatus_Married + 
    MaritalStatus_Single + NumCompaniesWorked + StockOptionLevel_1 + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + mean_attendance + work_regularity_1 + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + 
    EnvironmentSatisfaction_2 + EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + 
    JobSatisfaction_2 + JobSatisfaction_3 + JobSatisfaction_4 + 
    WorkLifeBalance_2 + WorkLifeBalance_3 + WorkLifeBalance_4, 
    family = "binomial", data = train)

summary(hr_model_03)
# checking the collinearity of model_03
vif(hr_model_03)

# JobRole_Manf_Dir	p-value:	0.148346		JobRole_Manf_Dir	1.329517777
# eliminating EducationField.Medical

hr_model_04 <-  glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_2 + Education_5 + 
    EducationField_Mrkt + EducationField_Oth + EducationField_TecDeg + 
    JobLevel_4 + JobRole_HumRes + JobRole_LabTech + 
    JobRole_Res_Dir + JobRole_ResSci + JobRole_SalesExec + MaritalStatus_Married + 
    MaritalStatus_Single + NumCompaniesWorked + StockOptionLevel_1 + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + mean_attendance + work_regularity_1 + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)


summary(hr_model_04)
# checking the collinearity of model_04
vif(hr_model_04)

# work_regularity_1	p-value =	0.1203850	has very low significance. removing it.
hr_model_05 <-  glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_2 + Education_5 + 
    EducationField_Mrkt + EducationField_Oth + EducationField_TecDeg + 
    JobLevel_4 + JobRole_HumRes + JobRole_LabTech + JobRole_Res_Dir + 
    JobRole_ResSci + JobRole_SalesExec + MaritalStatus_Married + 
    MaritalStatus_Single + NumCompaniesWorked + StockOptionLevel_1 + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_05)
# checking the collinearity of model_05
vif(hr_model_05)

# EducationField_Mrkt	-0.42573	0.26072	-1.633 p-value:	0.1024920 that has low significance p-value

hr_model_06 <-  glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_2 + Education_5 + 
    EducationField_Oth + EducationField_TecDeg + 
    JobLevel_4 + JobRole_HumRes + JobRole_LabTech + JobRole_Res_Dir + 
    JobRole_ResSci + JobRole_SalesExec + MaritalStatus_Married + 
    MaritalStatus_Single + NumCompaniesWorked + StockOptionLevel_1 + 
    TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
    YearsWithCurrManager + mean_attendance + workLoad_1 + workLoad_2 + 
    JobInvolvement_3 + EnvironmentSatisfaction_2 + EnvironmentSatisfaction_3 + 
    EnvironmentSatisfaction_4 + JobSatisfaction_2 + JobSatisfaction_3 + 
    JobSatisfaction_4 + WorkLifeBalance_2 + WorkLifeBalance_3 + 
    WorkLifeBalance_4, family = "binomial", data = train)

summary(hr_model_06)
# checking the collinearity of model_03
vif(hr_model_06)

# Eliminating StockOptionLevel_1	p-value:0.1139930 with low value

hr_model_07 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_2 + Education_5 + 
    EducationField_Oth + EducationField_TecDeg + JobLevel_4 + 
    JobRole_HumRes + JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + 
    TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
    mean_attendance + workLoad_1 + workLoad_2 + JobInvolvement_3 + 
    EnvironmentSatisfaction_2 + EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + 
    JobSatisfaction_2 + JobSatisfaction_3 + JobSatisfaction_4 + 
    WorkLifeBalance_2 + WorkLifeBalance_3 + WorkLifeBalance_4, 
    family = "binomial", data = train)


summary(hr_model_07)
# checking the collinearity of model_03
vif(hr_model_07)

# Eliminating Education_2	p-value:	0.0957610

hr_model_08 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_5 + 
    EducationField_Oth + EducationField_TecDeg + JobLevel_4 + 
    JobRole_HumRes + JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_08)
# checking the collinearity of model_08
vif(hr_model_08)

# Eliminating: JobRole_HumRes	p-value:	0.0778510 that has lower insignificant

hr_model_09 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + Education_5 + 
    EducationField_Oth + EducationField_TecDeg + JobLevel_4 + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_09)
# checking the collinearity of model_09
vif(hr_model_09)

# Eliminating - Education_5 p-value: 0.0562350
hr_model_10 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    EducationField_Oth + EducationField_TecDeg + JobLevel_4 + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_10)
# checking the collinearity of model_10
vif(hr_model_10)

# Eliminating - EducationField_Oth(er) p-value: 	0.0553650
hr_model_11 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    EducationField_TecDeg + JobLevel_4 + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_11)
# checking the collinearity of model_11
vif(hr_model_11)


# Eliminating - EducationField_TecDeg	p-value:	0.0478790	that has lower p-value
hr_model_12 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    JobLevel_4 + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_12)
# checking the collinearity of model_12
vif(hr_model_12)

## Eliminating - JobLevel_4	p-value:	0.0309330	that has lower p-value
hr_model_13 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Married + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_13)
# checking the collinearity of model_13
vif(hr_model_13)


# MaritalStatus_Married	0.47635	0.20376	2.338	0.0193980	*	MaritalStatus_Married	2.416876822
# Eliminating - MaritalStatus_Married		p-value:	0.0193980	that has lower p-value
hr_model_14 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_14)
# checking the collinearity of model_14
vif(hr_model_14)

## Eliminating - TrainingTimesLastYear	-0.12822	0.05406	-2.372	p-value:	0.0177010	that has lower p-value
hr_model_15 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + JobSatisfaction_2 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_15)
# checking the collinearity of model_15
vif(hr_model_15)


# MODELS FROM HERE ARE FOR VERIFICATION-- verifying the models

## Eliminating - 	JobSatisfaction_2	p-value: 0.0095090 	that has lower p-value
hr_model_16 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + 
    JobSatisfaction_3 + JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_16)
# checking the collinearity of model_16
vif(hr_model_16)

## Eliminating - 	JobSatisfaction_3	p-value: 0.0095090 	that has lower p-value
hr_model_17 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
    Department_RnD + Department_sales + 
    JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
    JobRole_SalesExec + MaritalStatus_Single + 
    NumCompaniesWorked + TotalWorkingYears + 
    YearsSinceLastPromotion + YearsWithCurrManager + mean_attendance + 
    workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
    EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + 
    JobSatisfaction_4 + WorkLifeBalance_2 + 
    WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
    data = train)

summary(hr_model_17)
# checking the collinearity of model_17
vif(hr_model_17)

## Eliminating - 	mean_attendance	0.35537	0.1303	2.727	0.0063860	**		4.730408944 lower p-value
hr_model_18 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
                     Department_RnD + Department_sales + 
                     JobRole_LabTech + JobRole_Res_Dir + JobRole_ResSci + 
                     JobRole_SalesExec + MaritalStatus_Single + 
                     NumCompaniesWorked + TotalWorkingYears + 
                     YearsSinceLastPromotion + YearsWithCurrManager + 
                     workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
                     EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + 
                     JobSatisfaction_4 + WorkLifeBalance_2 + 
                     WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
                   data = train)

summary(hr_model_18)
# checking the collinearity of model_18
vif(hr_model_18)

## Eliminating - 	 JobRole_LabTech	that has lower p-value
hr_model_19 <- glm(formula = Attrition_Yes ~ Age + BusinessTravel_frequently + 
                     Department_RnD + Department_sales + 
                     JobRole_Res_Dir + JobRole_ResSci + 
                     JobRole_SalesExec + MaritalStatus_Single + 
                     NumCompaniesWorked + TotalWorkingYears + 
                     YearsSinceLastPromotion + YearsWithCurrManager + 
                     workLoad_1 + workLoad_2 + JobInvolvement_3 + EnvironmentSatisfaction_2 + 
                     EnvironmentSatisfaction_3 + EnvironmentSatisfaction_4 + 
                     JobSatisfaction_4 + WorkLifeBalance_2 + 
                     WorkLifeBalance_3 + WorkLifeBalance_4, family = "binomial", 
                   data = train)

summary(hr_model_19)
# checking the collinearity of model_19
vif(hr_model_19)


# the AIC of the model changes significantly when the number GRADUALLY from model 11, further AIC increases from Model 15.

#------------------------------------------------
# MODEL EVALUATION
#------------------------------------------------
  
final_model <- hr_model_15

# Predicting the probabilites of the model
hr_attri_prob_pred <- predict(object = final_model,type = "response",newdata = test) 
# using type="response" predict function returns odds
test$Predicted_probability <- hr_attri_prob_pred

# Actual Attrition
test_actual_attrition <- factor(ifelse(test$Attrition == 1, "Yes","No"))

# ---- COMPUTING OPTIMUM VALUE OF CUTOFF WHEN SENSITIVITY, SPECIFIVITY AND ACCURACY MAXIMIZE  ----

# Function to evaluate Cutoff
perform_fn <- function(cutoff) 
{
  hr_attri_pred <- factor(ifelse(hr_attri_prob_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(hr_attri_pred, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  #colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

summary(hr_attri_prob_pred)
# Sequence and Empty Matrix to Store thte Probabilities, Sensitivity, Specificity and Accouracy
s = seq(0.01,0.96,length = 100)
output_matrix = matrix(0,100,3)

# Iterate th for loop to compute Sens, Spec, and Accuracy ---
for (i in 1:100) {
  output_matrix[i,] = perform_fn(s[i])
} 
# create an output matrix data.frame
output_matrix <- as.data.frame(output_matrix)
colnames(output_matrix) <- c("sensitivity", "specificity", "accuracy")
head(output_matrix)

output_matrix$probability <- s

# COMPUTING THE CUTOFF_MAX and CUTOFF VALUE accoracy and specificiy ----
cutoff_max <- s[which.max(abs(output_matrix$sensitivity + output_matrix$specificity + output_matrix$accuracy))]

cut_off <- s[which.min(abs(output_matrix$sensitivity - output_matrix$specificity))] # CUTOFF VALUE ----
cut_off

# COMMENTS: cut-off OF sensitiviy, specificity and accuracy reaches optimum at: s:0.1642393

# PLOTTING THE SENSITIVITY, SPECIFICITY, AND ACCURACY
library(ggplot2)
ggplot(output_matrix) +
  geom_line(aes(x = s,y = output_matrix$sensitivity,col = "black")) + 
  geom_line(aes(x = s,y = output_matrix$specificity,col = "blue")) +
  geom_line(aes(x = s,y = output_matrix$accuracy,col = "green")) +
  geom_line(aes(x = s,y = abs(output_matrix$sensitivity + output_matrix$specificity + output_matrix$accuracy))) +
  scale_color_discrete(labels = c("Sensitivity","Specificity","Accuracy")) +
  geom_vline(xintercept = c(cut_off,cutoff_max),linetype = "dotted") + 
  xlab(label = "Predicted Probability") + ylab(label = "Sensitiviy, Specificity, and Accuracy")

# CHARACTERISITCS OF THE MODEL AT CUTOFF

# cut-off OF sensitiviy, specificity and accuracy reaches optimum at: s:0.1642393
predicted_attrition_cutoff <- factor(ifelse(test$Predicted_probability > cut_off,"Yes","No"))
actual_attrition <- factor(ifelse(test$Attrition_Yes == 1, "Yes","No"))

# confusion Matrix at Cutoff
conf_cutoff <- confusionMatrix(predicted_attrition_cutoff,actual_attrition,positive = "Yes")

# confusion matrix element
conf_cutoff$table

# Accuracy
conf_cutoff$overall[1]
# sensitivity
conf_cutoff$byClass[1]
# specificity
conf_cutoff$byClass[2]

# ADDIING THE CUTOFF PREDICTED ATTRITION TO TEST 
test$predicted_attr_cutoff <- ifelse(predicted_attrition_cutoff == "Yes",1,0) 

# PLOTTING THE predicted_attrition and actual attrition
ggplot(test,aes(x = Predicted_probability,y = predicted_attr_cutoff)) + 
  geom_point(position = "dodge") + 
  geom_point(aes(x = Predicted_probability, Y = Attrition_Yes,col = "red"),alpha = 0.08) +
  geom_smooth(method = "glm",se = FALSE,method.args = list(family = "binomial")) +
  ylab("Attrition Rate and Predicted Probability") 


#-----------------------------------------------------------
# CROSS VALIDATION OF the MODEL using GAIN CHART LIFT CHART
#-----------------------------------------------------------
library(ROCR) # USED TO MEASURE CROSS VALIDATION OF BUILT MODEL ----

# COMPUTING THE PREDICTIVE POWER AND PERFORMANCE OF THE MODE TP, TN, FP, FN
predictHR <- prediction(hr_attri_prob_pred,test$Attrition_Yes)

#-------------
# GAIN CHART
#-------------
# Computing the performance of predictions
perfHR <- performance(predictHR,measure = "tpr",x.measure = "fpr")

# PLOTTING THE GAIN Chart ----
#plot(perfHR, col = "blue") + lines(x = c(0,1),y = c(0,1))
temp_df <- data.frame(x = perfHR@x.values, y = perfHR@y.values)
names(temp_df) <- c("x","y")
perfHR@x.name
perfHR@y.name

# PLOTTING THE GAIN CHART
ggplot(temp_df,aes(x = x, y = y)) + 
  geom_line(na.rm = T) + 
  geom_line(aes(x = y ,y = y),linetype = "dotted") +
  labs(title = expression(bold("Gain Chart"))) + xlab(label = "False positive rate") + ylab(label = "True positive rate") + 
  theme(plot.title = element_text(hjust = 0.5))

# COMMENTS: PLOTS GIVES INFORMATION ABOUT THE HOW WELL THE MODEL IS SCALING AGAINST THE RANDOM MODEL.

#-------------
# LIFT CHART
#-------------

perf_lift_hr_analytics <- performance(prediction.obj = predictHR,measure = "lift",x.measure = "rpp")
 
temp_df3 <- data.frame(x = perf_lift_hr_analytics@x.values, y = perf_lift_hr_analytics@y.values)
summary(temp_df3)
names(temp_df3) <- c("x","y")

# PLOTTING THE LIFT CHART
ggplot(temp_df3,aes(x = x, y = y)) + geom_line(na.rm = T) + 
  labs(title = expression(bold("Lift Chart"))) + xlab(label = "Rate of positive predictions") + ylab(label = "Lift Value") + 
  theme(plot.title = element_text(hjust = 0.5))
# COMMENTS: PLOTS GIVES INFORMATION ABOUT THE HOW WELL THE MODEL IS SCALING AGAINST THE RANDOM MODEL.

# AREAD UNDER THE CURVE IS
area_under_curve <- performance(predictHR,"auc")
area_under_curve@y.values
# COMMENTS: WITH AREA UNDER THE CURVE CLOSER TO 1, MODEL IS SCALING WELL AGAINST THE DATA.

#--------------------------------------------------
# COMPUTING PREDICITVE POWER USING ks-statisitc
#--------------------------------------------------
ks_static_hr_anal = max(attr(perfHR,'y.values')[[1]] - attr(perfHR,'x.values')[[1]])
plot(perfHR, main = paste0(' KS_Statistic=',round(ks_static_hr_anal*100,1),'%'))

# KS-STATISITIC
print(ks_static_hr_anal)
# [1] 0.5412942


#--------------------------------------------------
# K-FOLD CROSS VALIDATION
#--------------------------------------------------

library(caret)

summary(hr_model_15)

# preparing the data for a repeated covariance
 crossValsettings <- trainControl(method = "repeatedcv",number = 30,savePredictions = TRUE)
# 
 hr_anal_crossVal <- train(as.factor(Attrition_Yes) ~ Age + BusinessTravel_frequently + 
                             Department_RnD + Department_sales + JobRole_LabTech + JobRole_Res_Dir + 
                             JobRole_ResSci + JobRole_SalesExec + MaritalStatus_Single + 
                             NumCompaniesWorked + TotalWorkingYears + YearsSinceLastPromotion + 
                             YearsWithCurrManager + mean_attendance + workLoad_1 + workLoad_2 + 
                             JobInvolvement_3 + EnvironmentSatisfaction_2 + EnvironmentSatisfaction_3 + 
                             EnvironmentSatisfaction_4 + JobSatisfaction_2 + JobSatisfaction_3 + 
                             JobSatisfaction_4 + WorkLifeBalance_2 + WorkLifeBalance_3 + 
                             WorkLifeBalance_4 ,data = train,family = "binomial",method = "glmStepAIC",                 trControl = crossValsettings)
 hr_anal_crossVal

# using gains package
# install.packages("gains")
library(gains)

length(test$Attrition_Yes)
length(test$predicted_attr_cutoff)

hr_analyt_gains <- gains(actual = test$Attrition_Yes,predicted = test$Predicted_probability,
      groups = 10,ties.method = c("max"),
      conf = c("normal"),conf.level = 0.95,
      optimal = TRUE)
print.gains(hr_analyt_gains)

library(ggplot2)
# Plotting Gain Chart
ggplot(data.frame(dec = hr_analyt_gains[[1]], cumm_gain = hr_analyt_gains[[6]]),aes(x = dec,y = cumm_gain)) + 
  geom_point() + geom_text(aes(label = paste0(round(cumm_gain*100,1),"%")),nudge_y = 0.05) +
  geom_line(linetype = "dotted") + 
  geom_line(aes(x = dec,y = seq(0.1,1,0.1)),linetype = "dashed") +
  xlab(label = "decile") + ylab(label = "Cummulative Gain") +
  labs(title = expression(bold("Gain Chart"))) +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting the lift

ggplot(data.frame(dec = hr_analyt_gains[[1]], lift = hr_analyt_gains[[6]]*100/hr_analyt_gains[[1]]),aes(x = dec,y = lift)) + 
  geom_point() +
  geom_line(linetype = "dotted") + 
  xlab(label = "decile") + ylab(label = "Lift")



# coefficients values in the model
hr_model_15$coefficients

# (Intercept)                     Age 
# 1.2055018                -0.3486585 
# BusinessTravel_frequently  Department_RnD 
# 1.1853131                -0.9538096 
# Department_sales           JobRole_LabTech 
# -1.1825023                 0.5910383 
# JobRole_Res_Dir            JobRole_ResSci 
# 0.9587616                 0.5207675 
# JobRole_SalesExec      MaritalStatus_Single 
# 0.6998034                 1.1194237 
# NumCompaniesWorked         TotalWorkingYears 
# 0.1549323                -0.1077533 
# YearsSinceLastPromotion      YearsWithCurrManager 
# 0.1884030                -0.1486597 
# mean_attendance                workLoad_1 
# 0.3500662                 1.9915155 
# workLoad_2          JobInvolvement_3 
# 0.8896388                -0.4260892 
# EnvironmentSatisfaction_2 EnvironmentSatisfaction_3 
# -0.8286463                -0.8667352 
# EnvironmentSatisfaction_4 JobSatisfaction_2 
# -1.3368172                -0.5098920 
# JobSatisfaction_3         JobSatisfaction_4 
# -0.4853897                -1.4299662 
# WorkLifeBalance_2         WorkLifeBalance_3 
# -1.1616639                -1.4883921 
# WorkLifeBalance_4 
# -1.1651051 












# UNUSED CODE

# employees irregular to work/heavily worked and taking vacations.. This is not a useful metric, as the resigned date is not known... SO COMMENTING THE BELOW CODE
# apply(in_out_data_without_stats, MARGIN = 1, FUN = function(x) sum(x[1:ncol(in_out_data_without_stats)][(x > 9) & (x == 0)],na.rm = TRUE)) 
# sum(rowSums((in_out_data_without_stats > 9) & (in_out_data_without_stats == 0)))



# table(boxplot.stats(df_no_outliers$YearsWithCurrManager,coef = 1.58)$out)
# table of outliers with current manager and Attrition
# table(df_no_outliers$YearsWithCurrManager,employee_master_cleaned$Attrition)
# # Thus removing the outliers
# df_no_outliers <- df_no_outliers[!(df_no_outliers$YearsWithCurrManager %in% boxplot.stats(x = df_no_outliers$YearsWithCurrManager,coef = 1.58)$out),]


# # MONTHLY INCOME: CHECKING OUTLIERS IN "MONTLYINCOME"
# quantile(df$MonthlyIncome,seq(0,1,0.01)) %>% tail()
# summary(df$MonthlyIncome)
# 
# # Impact of removing the outliers in the monthly income that lie above 99% percentile of monthly_income data and checking the values. 
# summary(df[-which(df$MonthlyIncome >= quantile(df$MonthlyIncome, 0.95)),])
# # COMMENTS: no significant change in the mean value
# 
# 
# # Impact of removing Outliers in "YearsAtCompany"
# quantile(df$YearsAtCompany,seq(0,1,0.01)) %>% tail()
# summary(df[-which(df$YearsAtCompany >= quantile(df$YearsAtCompany, 0.95)),])
# # Comment: small change in the mean value
# 
# # Impact of "TotalWorkingYears"
# quantile(df$TotalWorkingYears,seq(0,1,0.01)) %>% tail()
# summary(df[-which(df$TotalWorkingYears >= quantile(df$TotalWorkingYears, 0.95)),])
# # Comments: Small change in the mean values


# 
# # METHOD 2
# # Longer method of creating dummy variables 2 of creating 
# summary(as.factor(employee_dummy_var_df$TrainingTimesLastYear))
# 
# 
# # ATTRITION to 1 and 0
# employee_dummy_var_df$Attrition <- ifelse(employee_master_cleaned$Attrition == "Yes",1,0)
# 
# # BUSINESS_TRAVEL: Creating Dummy Variable for BUSINESS_TRAVEL 
#  dummy_var_BUSTRVL <- data.frame(model.matrix(~BusinessTravel,employee_master_cleaned))[,-1]
# #   adding dummry variable to the "employe_dummy_data_frame"
#  employee_dummy_var_df <- cbind(dummy_var_BUSTRVL,employee_dummy_var_d[,-which(names(employee_dummy_var_df) == "BusinessTravel")])
#  names(dummy_var_BUSTRVL)
#  names(employee_dummy_var_df)
# 
# sum(employee_dummy_var_df[c("BusinessTravel_frequently")] != dummy_var_BUSTRVL["BusinessTravelfrequently"])
# sum(employee_dummy_var_df[c("BusinessTravel_rarely")] != dummy_var_BUSTRVL["BusinessTravelrarely"])
 
# 
# # DEPARTMENT: creating Dummy variable for DEPARETMENT variable. 
# employee_dummy_var_df$Department <- employee_master_cleaned$Department
# employee_dummy_var_df$Department <- str_replace(string = employee_dummy_var_df$Department,pattern = "Research & Development",replacement = "R_and_D")
# 
# #creating a Dummy variable using model.matrix
# dumm_var_DEPT <- data.frame(model.matrix(object = ~Department,data = employee_dummy_var_df))[,-1]
# employee_dummy_var_df <- cbind(dumm_var_DEPT,employee_dummy_var_df[,-which(names(employee_dummy_var_df) == "Department")]) 
# 
# # EDUCATION_FIELD: Converting education to factors to Short Forms.
# levels(x = employee_dummy_var_df$EducationField) <- list(HR_LS = c("Human Resources", "Life Sciences"),
#                                                          Mkt_Medi = c("Marketing","Medical"),
#                                                          Othr_TechDeg = c("Other","Technical Degree"))
# 
# # Creating the Dummy variable for Education Field using model.matrix
# dummy_var_EDU <- data.frame(model.matrix(object = ~EducationField,data = employee_dummy_var_df))[,-1]
# employee_dummy_var_df <- cbind(dummy_var_EDU,employee_dummy_var_df[,-which(names(employee_dummy_var_df) == "EducationField")])
# 
# # GENDER: Converting Gender to factors of 1 and 0
# employee_dummy_var_df$Gender <- ifelse(test = employee_dummy_var_df$Gender == "Female",0,1)
# 
# # JOBROLE: creating dummy variables creating levels
# # [1] "Healthcare Representative" "Human Resources"           "Laboratory Technician"     "Manager"                   "Manufacturing Director"   
# # [6] "Research Director"         "Research Scientist"        "Sales Executive"           "Sales Representative"     
# 
# # converting mulitple levels to simpler three levels
# levels(employee_dummy_var_df$JobRole) <- list(HlthRep_LabTech_Mngr = c("Healthcare Representative","Laboratory Technician","Manager"),
#                                               ManfDir_ResDir_ResSci = c("Manufacturing Director", "Research Director","Research Scientist"),
#                                               SlsRep_SlsExe_HR = c("Sales Representative","Sales Executive","Human Resources"))
# 
# # Using Model matrix to convert the variable into Dummy Variables
# dummy_var_JBROLE <- data.frame(model.matrix(object = ~JobRole, data = employee_dummy_var_df))[,-1] #%>%head()
# employee_dummy_var_df <- cbind(dummy_var_JBROLE,employee_dummy_var_df[,-which(names(employee_dummy_var_df) == "JobRole")]) 
# 
# # MARITAL_STATUS - creating dummy variables for marital_Status
# dummy_var_MARSTUS <- data.frame(model.matrix(object = ~MaritalStatus,data = employee_dummy_var_df))[,-1]
# 
# 
# library(dummies)
# dummy.data.frame()
# 
# # creating sample test and training sample sets
# 
# # changing factors to levels
# xBusinessTravel <-  employee_master_cleaned$BusinessTravel
# levels(xBusinessTravel) <- c(0,1,2)
# xBusinessTravel
# 

# 
# 
# # plotting the "monthly_income" variable to check the outliers, after removing the employee id
# melt(data = data.frame(df_mnth_incm[,-1])) %>% 
#   ggplot(aes(x = variable, y = value)) + 
#   geom_boxplot() + 
#   facet_wrap(~variable, scales = "free")
# 
# # comparing the values of mean and median can
# summary(df$MonthlyIncome)
# summary(df_mnth_incm$MonthlyIncome)
# 
# # YEARSATCOMPANY: measuring the outliers in "YearsAtCompany"
# sapply(df_mnth_incm[,-1], function(x) quantile(x, seq(0,1,0.01))) %>% tail()
# 
# # quantile from 0 to 1 in steps of 1.
# quantile(df$YearsAtCompany,seq(0,1,0.01)) %>% tail()
# 
# # identifying the outliers in the "YearsAtCompany" variable,
# df_mnth_incm$YearsAtCompany[which(df_mnth_incm$YearsAtCompany > quantile(df_mnth_incm$YearsAtCompany, 0.98))]
# 
# df_YrsAtComp <-  df_mnth_incm[-which(df_mnth_incm$YearsAtCompany > quantile(df_mnth_incm$YearsAtCompany, 0.98)),]
# 
# # checking the Outliers in the Years at Company
# sapply(df_YrsAtComp[,-1], function(x) quantile(x, seq(0,1,0.01))) %>% tail()
# 
# # checking the summaries of monthly_income and YearsAtCompany, and original dataframe
# summary(df)
# summary(df_mnth_incm)
# summary(df_YrsAtComp)
# 
# 
# # TOTAL WORKING YEARS: removing the records with outliers in: "TotalWorkingYears" 
# quantile(df_YrsAtComp$TotalWorkingYears, seq(0,1, 0.01)) #%>% tail(n=10)
# 
# # identifying the values of the records that display and rapid change in the variations
# df_YrsAtComp$TotalWorkingYears[which(df_YrsAtComp$TotalWorkingYears > quantile(df_YrsAtComp$TotalWorkingYears, 0.975))]
# 
# # removing the outlier records from "TotalWorkingYears"
# df_TotalWorkingYears <- df_YrsAtComp[-which(df_YrsAtComp$TotalWorkingYears > quantile(df_YrsAtComp$TotalWorkingYears,probs = 0.975)),] #tail()
# 
# # checking the outliers 
# sapply(df_TotalWorkingYears[,-1], function(x) quantile(x, seq(0,1,0.01))) %>% tail()
# 
# # YearsAtCompany: Removing the outliers in YearsAtCompany
# 
# # printing the field in the dataframe employee_master_cleaned
# names(employee_master_cleaned)
# 
# 
# # Plotting theoutliers in the "YearsAtCompany" and "TotalWokingYears"
# melt(data = subset(employee_master_cleaned,select = c("Attrition","MonthlyIncome","PercentSalaryHike","TotalWorkingYears",
#                                                       "TrainingTimesLastYear","YearsAtCompany","YearsSinceLastPromotion",
#                                                       "YearsWithCurrManager","Age","DistanceFromHome","vacations","irregular_to_work",
#                                                       "heavy_workLoad","mean_attendance")),id.vars = "Attrition") %>% 
#   ggplot(aes(x = variable, y = value, fill = Attrition)) + 
#   geom_boxplot() + 
#   facet_wrap(~variable, scales = "free")
# 
# # plots after removing the outliers from df_YersAtComp
# 
# melt(data = data.frame(df_YrsAtComp[,-1])) %>% 
#   ggplot(aes(x = variable, y = value)) + 
#   geom_boxplot() + 
#   facet_wrap(~variable, scales = "free")
# 
# 
# # plots after removing the outliers: df_TotalWorkingYears
# 
# melt(data = data.frame(df_TotalWorkingYears[,-1])) %>% 
#   ggplot(aes(x = variable, y = value)) + 
#   geom_boxplot() + 
#   facet_wrap(~variable, scales = "free")
# 
# 
# names(employee_master_cleaned)
# View(df_TotalWorkingYears)
# View(employee_master_cleaned)
# 
# 
# # identifying the position of the column indices of final Outlier treated dataframe "df_TotalWorkingYears" with "employe_master_cleaned"
# which(x = names(employee_master_cleaned) %in% names(df_TotalWorkingYears[,-1]))
# 
# employee_master_cleaned_merged <- merge.data.frame(x = employee_master_cleaned[,-which(names(employee_master_cleaned) %in% names(df_TotalWorkingYears[,-1]))],
#                                                    y = df_TotalWorkingYears,
#                                                    by = "EmployeeID",
#                                                    all.y = TRUE)
# 
# 
# 
# 
# 
# # ROUGH CODE USED IN DEVELOPMENT.
# 
# # xRowMeans_all <- rowMeans(df)
# # View(data.frame(xRowMeans_apply,xRowMeans_all))
# # # column means
# # #sapply(x160, function(x) mean(x))
# # colMeans(in_out_data_without_stats)
# # str(in_out_data_without_stats)
# 
# # calculating the row means
# # row means
# # rowMeans(in_out_data_without_stats)
# # data.frame(ID = c(1:nrow(in_out_data_without_stats)), Row_means = rowMeans(in_out_data_without_stats))
# 
# 
# 
# 
# # install.packages("woe")
# # library(woe)
# # woe(Data = employee_master_cleaned_merged,Independent = "employee_master.NumCompaniesWorked",Continuous = FALSE,Dependent = "employee_master.Attrition",C_Bin = 1,Good = 1,Bad = 0)
# # 
# 
# 
# 
# # ******** finding the patterns of holidays ***********
# # list_freq <- as.matrix(apply(X = in_out_data_without_stats, MARGIN = c(1,2), FUN =  function(x) which(x == 0)))
# # 
# # list_character <- matrix(freq, ncol = ncol(in_out_data_without_stats), byrow = TRUE)
# # 
# # 
# # # there appears to be no change in the way the data looks.
# # 
# # # ********************************************************
# # # library(purrr)
# # # map_chr(freq,1)
# # 
# # list_character <- matrix(data = 0, nrow = nrow(in_out_data_without_stats),ncol = 3)
# # 
# # # list_vector_num <- matrix(data = 0,nrow = nrow(in_out_data_without_stats),ncol=)
# # 
# # for (i in 1:nrow(in_out_data_without_stats)) {
# #   list_character[i,] <- freq[[i]]
# # }
# 
# 
# 
# # ********************************************************
# # USING THE TEST DATAFRAMNES
# # ********************************************************
# 
# # test date difference between mulitple columns
# test_in <- in_time_data[1:10,1:31]
# 
# test_out <- out_time[1:10,1:31]
# #str(test_in)
# #str(test_out)
# 
# # replace NAs with "time_off" since there are no punches Assuming the NA as 0 
# which(is.na(test_out))
# test_in[is.na(test_in)] <- 0
# test_out[is.na(test_out)] <- 0
# 
# 
# # converting the data to parse_date_time format one column
# # x12 <- parse_date_time(test_in$`2015-01-02`, orders = "Y-m-d H:M:S",tz = "GMT")
# # x13 <- parse_date_time(test_out$`2015-01-02`,orders = "Y-m-d H:M:S",tz = "GMT")
# # x12
# # x13
# 
# # trying to convert the complete dataframe in parse_date_time converting
# # parse_date_time(test_in,orders = "Y-m-d H:M:S")
# # doesn't seam to work
# 
# 
# #format(as.Date(test_in$`2015-01-02`[1:5],format = "%Y-%m-%d"),"%Y")
# 
# # creating an empty matrix for storing the test duration
# test_dur <- matrix(data = 0,nrow = nrow(test_in), ncol = ncol(test_in))
# 
# # time difference calculation
# # difftime(time1 = x13,time2 = x12, units = "hours")
# 
# # creating the tet_duration matrix that calcuates the difference of the work timings
# 
# for(i in 1:ncol(test_in)) {
#   test_dur[,i] <- difftime(time1 = parse_date_time(test_out[,i],orders = "Y-m-d H:M:S"),
#                            time2 = parse_date_time(test_in[,i],orders = "Y-m-d H:M:S"), units = "hours")
# }
# 
# # creating the headers of data frames
# colnames(test_dur) <- c(names(test_in))
# 
# # replacing the NA with time_offs being calcualted
# test_dur[is.na(test_dur)] <- 0
# 
# # removing the first column which is the employee id column
# test_dur <- test_dur[,-1]
# # converting the test_dur to data.frame
# test_dur <- data.frame(test_dur)
# test_dur <- round(test_dur,2)
# 
# # finding with columns have the all columns as time_offs 
# 
# #sum(test_dur == "time_off")
# #which(sapply(X = test_dur, FUN = function(x) length(which(x == "time_off"))) == 10 )
# 
# # removing the data frame that has all Statutory Holidays Remove
# x160 <- test_dur[,-which(names(test_dur) %in% names(test_dur[which(colSums(test_dur == 0) == nrow(test_dur))]))]
# 
# # getting the count of Vacations of each employee
# rowSums(test_dur == 0 )
# # employees who are irregular
# rowSums(test_dur < 7)
# # average work hours spent at work
# mean(x160,na.rm = T)
# 
# # column means
# sapply(x160, function(x) mean(x))
# colMeans(x160)
# 
# str(x160)
# # row means
# rowMeans(x160)
# 
# 
# 
# # calculating the row means
# data.frame(ID = c(1:nrow(z160)), Row_means = rowMeans(x160))
# 
# 
# 
# # check if the headers of the data.frames are identifical.
# sum(names(in_time_data) == names(out_time))
# 
# # aver
# # there appears to be no change in the way the data looks.
# 
# # ********************************************************
# 
