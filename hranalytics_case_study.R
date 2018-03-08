# Predictive Analytics Ikeyboard_arrow_rightGroup Project 1keyboard_arrow_rightSession 1

# ************************************************
# Business Understanding
# ************************************************

# ************Problem Statement ************

# A large company named XYZ, employs, at any given point of time, around 4000 employees. However, every year, around 15% of its employees leave the company and need to be replaced with the talent pool available in the job market. The management believes that this level of attrition (employees leaving, either on their own or because they got fired) is bad for the company, because of the following reasons -
#   
#   The former employees’ projects get delayed, which makes it difficult to meet timelines, resulting in a reputation loss among consumers and partners
# 
# A sizeable department has to be maintained, for the purposes of recruiting new talent
# 
# More often than not, the new employees have to be trained for the job and/or given time to acclimatise themselves to the company
# 
# Hence, the management has contracted an HR analytics firm to understand what factors they should focus on, in order to curb attrition. In other words, they want to know what changes they should make to their workplace, in order to get most of their employees to stay. Also, they want to know which of these variables is most important and needs to be addressed right away.
# 
# Since you are one of the star analysts at the firm, this project has been given to you.
# 

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
# Downloads
# All the files required for this case study are given in the following zip file.
# 
# ************Important Note:************ 
# You are supposed to code entirely in R. All your plots and tables must be created in R, though you may recreate the same in Tableau as well (for the presentation) for better aesthetics. Please submit the presentation in a PDF format. Please make sure to rename your R script as "Group_Facilitator_RollNo_main.R".


library(lubridate)

# READ ALL EXCEL FILES


# read HR analytics files
employee_survey_data <- read.csv("PA-I_Case_Study_HR_Analytics/employee_survey_data.csv")
general_data <- read.csv("PA-I_Case_Study_HR_Analytics/general_data.csv")
in_time_data <- read.csv("PA-I_Case_Study_HR_Analytics/in_time.csv",stringsAsFactors = F,check.names = F)
manager_survey_data <- read.csv("PA-I_Case_Study_HR_Analytics/manager_survey_data.csv")
out_time <- read.csv("PA-I_Case_Study_HR_Analytics/out_time.csv",stringsAsFactors = F,check.names = F)



data_dictionary <- readxl::read_xlsx(path = "PA-I_Case_Study_HR_Analytics/data_dictionary.xlsx")#(file = "PA-I_Case_Study_HR_Analytics/data_dictionary.xlsx")

#hr_data_dictionary <- xlsx::read.xlsx(file = "PA-I_Case_Study_HR_Analytics/data_dictionary.xlsx",sheetName = "data_dictionary",as.data.frame = TRUE)
View(data_dictionary)


# ********************************************************
# DATA UNDERSTANDING
# ********************************************************



# ********************************************************
# Data Preparation
# ********************************************************

# **********
# check duplicates
# **********
sum(duplicated(x = employee_survey_data$EmployeeID))

# checking the characterisitics of the data frame
sapply(list(general_data,in_time_data,manager_survey_data,out_time,employee_survey_data),str)

# COMMENTS: THERE ARE NO DUPLCIATES


# **********
# analysis of in_time_data and out_time dataframes
# **********

# GOAL OF THIS ANALYSIS IS TO FIND THE REGULARITY OF AN EMPLOYEE, VACATIONS TAKEN BY THE EMPLOYEE, Omit Statutory Holidays

# DATA with NAs is assumed as "statutory_holidays/Vacations"

# check if the headers of the data.frames are identifical.
sum(names(in_time_data) == names(out_time))

# renaming the blank field of in_time_date, out_time with EmployeedID of the
names(in_time_data)[which(names(in_time_data) == "")] <- "EmployeeID"
names(out_time)[which(names(out_time) == "")] <- "EmployeeID"


# ********************************************************
# Derieving metrics from the in_time and out_time
# ********************************************************

# creating an empty matrix for storing the  in_out_duration
in_out_dur <- matrix(data = 0,nrow = nrow(in_time_data), ncol = ncol(in_time_data))

# creating the in_out_duration matrix that stores the calculated difference of the work timings
for (i in 1:ncol(in_time_data)) {
  in_out_dur[,i] <- difftime(time1 = parse_date_time(out_time[,i],orders = "Y-m-d H:M:S"),
                           time2 = parse_date_time(in_time_data[,i],orders = "Y-m-d H:M:S"), units = "hours")
}

# copying the headers of data frames
colnames(in_out_dur) <- c(names(in_time_data))

# replacing the NAs with 0 being calcualted
in_out_dur[is.na(in_out_dur)] <- 0

# removing the first column which is the employee id column
in_out_dur <- in_out_dur[,-1]

# converting the in_out_dur to data.frame
in_out_dur <- data.frame(in_out_dur)
in_out_dur <- round(in_out_dur,2)

# columns that have only zeros have a pattern. Finding with columns have the all columns as time_offs and 
# removing the data frame that has all Statutory Holidays Remove
# data frame without statutory holidays
in_out_data_without_stats <- in_out_dur[,-which(names(in_out_dur) %in% names(in_out_dur[which(colSums(in_out_dur == 0) == nrow(in_out_dur))]))]

# write.csv(in_out_dur,"in_out_dur.csv")

# difference between the number of columns betwen two dataframes
ncol(in_out_dur) - ncol(in_out_data_without_stats)
# Thus there are 12 Statutory Holidays

# getting the count of Vacations taken by each employee adding to the general_data
general_data$vacations <- rowSums(in_out_dur == 0)

# employees who are irregular
general_data$irregular_to_work <- rowSums((in_out_data_without_stats > 0) & (in_out_data_without_stats < 7))

# employees with heavy work load working overtime
general_data$heavy_workLoad <- rowSums(in_out_data_without_stats > 9)

# # employees irregular to work/heavily worked and taking vacations
# rowSums((in_out_data_without_stats > 8) & (in_out_data_without_stats == 0))

# column means
#sapply(x160, function(x) mean(x))
colMeans(in_out_data_without_stats)
str(in_out_data_without_stats)

# calculating the row means
# row means
rowMeans(in_out_data_without_stats)
data.frame(ID = c(1:nrow(in_out_data_without_stats)), Row_means = rowMeans(in_out_data_without_stats))


# checking if the EmployeeIDs in the general_data, manager_survey_data, Employee Survey data is unique
setdiff(x = manager_survey_data$EmployeeID, y = employee_survey_data$EmployeeID)
setdiff(x = general_data$EmployeeID, y = employee_survey_data$EmployeeID)
setdiff(x = in_time_data$EmployeeID, y = employee_survey_data$EmployeeID)


# merge employee survey data and manager_survey_data
employee_survey_data <- merge(x = employee_survey_data,y = manager_survey_data,by = intersect(names(employee_survey_data),names(manager_survey_data)))

# # check for NAs in the employee Survey Data

# Removing unwanted fields , Employee Count, Over 18, StandardHours, 
general_data <- general_data[,-which(names(general_data) %in% c("Over18","StandardHours","EmployeeCount"))]


# merge general_data and employee_survey_data dataframes
employee_master <-  merge(x = general_data,y = employee_survey_data, by = intersect(names(general_data),names(employee_survey_data)))

# identifying and Deleting the columns that have NAs 
# identifying the columns that have NAs
sapply(employee_master, function(x) sum(is.na(x)))

# Count of NAs in each columns
# NumCompaniesWorked: 19        EnvironmentSatisfaction: 25         JobSatisfaction: 20         WorkLifeBalance: 38

# NumCompaniesWorked by an employee is subjective. It doesn't make sense to replace the NA's with median or mean as the NumOfCompanies a candidate. Thus delete the records of the employees that do not have enough data is one among the better solutions. 
# EnvironmentSatisfaction: 

# writing csv of employe_master
write.csv(x = employee_master,file = "employee_master.csv")



#----------------------$$$$$$$$$$$$$
# Attempt to approximate the "NA"s in NumCompaniesWorked,EnvironmentSatisfaction,JobSatisfaction,WorkLifeBalance
#----------------------$$$$$$$$$$$$$


employee_master_cleaned <- employee_master

# employee_master_cleaned <- data.frame(employee_master$Attrition,employee_master$NumCompaniesWorked,
#                      employee_master$EnvironmentSatisfaction,employee_master$JobSatisfaction,
#                      employee_master$WorkLifeBalance)
#View(employee_master_cleaned)
str(employee_master_cleaned)

# Imputing the missing values as "missing" 

employee_master_cleaned$NumCompaniesWorked[which(is.na(employee_master_cleaned$NumCompaniesWorked))] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$EnvironmentSatisfaction)),"EnvironmentSatisfaction"] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$JobSatisfaction)),"JobSatisfaction"] <- "missing"
employee_master_cleaned[which(is.na(employee_master_cleaned$WorkLifeBalance)),"WorkLifeBalance"] <- "missing"

# employee_master_cleaned$NumCompaniesWorked[which(is.na(employee_master_cleaned$NumCompaniesWorked))] <- "missing"



# converting the attrition in 1 and 0
employee_master_cleaned$Attrition <- ifelse(employee_master_cleaned$Attrition == "Yes",1,0)

# convertying the variables of NumCompaniesWored to factors
employee_master_cleaned$NumCompaniesWorked <- factor(employee_master_cleaned$NumCompaniesWorked)

#install.packages("Information")
library(Information)
# creating the information values of the "employee_master_cleaned" data frame to see if the missing values can be imputed with the nearest numbers.

IV_employee_master <- create_infotables(data = employee_master_cleaned[which(names(employee_master_cleaned) %in% c("Attrition","NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance"))],
                                        y = "Attrition", 
                                        bins = 4,
                                        parallel = TRUE)
print(IV_employee_master)

xNumCompnaiesWorked <- IV_employee_master$Tables["NumCompaniesWorked"]
xNumCompnaiesWorked_df <- as.data.frame(xNumCompnaiesWorked)
View(xNumCompnaiesWorked_df)
str(xNumCompnaiesWorked_df)

library(ggplot2)

# plotting for number of companies worked
ggplot(xNumCompnaiesWorked_df,aes(x = factor(NumCompaniesWorked.NumCompaniesWorked),y = NumCompaniesWorked.IV)) + 
  geom_point() + geom_line()

# plotting the graph for environment Survey
ggplot(data.frame(IV_employee_master$Tables["EnvironmentSatisfaction"]),aes(x = factor(EnvironmentSatisfaction.EnvironmentSatisfaction),y = EnvironmentSatisfaction.IV)) + 
  geom_point() + geom_line()

# plots for "JobSatisfaction"
ggplot(data.frame(IV_employee_master$Tables["JobSatisfaction"]),aes(x = JobSatisfaction.JobSatisfaction,y = JobSatisfaction.IV)) + geom_point()

# plots for "WorkLifeBalance"
ggplot(data.frame(IV_employee_master$Tables["WorkLifeBalance"]),aes(x = WorkLifeBalance.WorkLifeBalance,y = WorkLifeBalance.IV)) + geom_point()



# missing values aren't any closer to the nearest WOE. Thus it makes sense to remove the records for the below reasons:
# 1. It is difficult to  emperically add missing values using Weights of Evidences of other variables
# 2. Adding mean and median values to the missing values of  "NumCompaniesWorked","EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance"  isn't logical as each of these variables are highly subjective each to each individual. 


employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$NumCompaniesWorked == "missing"),]
employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$EnvironmentSatisfaction == "missing"),]
employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$JobSatisfaction == "missing"),]
employee_master_cleaned <- employee_master_cleaned[-which(employee_master_cleaned$WorkLifeBalance == "missing"),]


View(employee_master_cleaned)

# calculating the data loss
paste(100-(round(nrow(employee_master_cleaned)*100/nrow(employee_master),2)),"%")

# Thus approximately 2.3% of data is lost as result of removing the "NAs"

str(employee_master_cleaned)

# changing factors to levels
xBusinessTravel <-  employee_master_cleaned$BusinessTravel
levels(xBusinessTravel) <- c(0,1,2)
xBusinessTravel

# "Non-Travel", "Travel_Frequently", "Travel_Rarely" are encoded as c(0,1,2)

#levels(employee_master_cleaned$BusinessTravel) <- c(0,1,2)
write.csv(employee_master_cleaned,"employee_master_cleaned.csv")






# EXPLORATORY DATA ANALYSIS

























# install.packages("woe")
# library(woe)
# woe(Data = employee_master_cleaned,Independent = "employee_master.NumCompaniesWorked",Continuous = FALSE,Dependent = "employee_master.Attrition",C_Bin = 1,Good = 1,Bad = 0)
# 



# ******** finding the patterns of holidays ***********
# list_freq <- as.matrix(apply(X = in_out_data_without_stats, MARGIN = c(1,2), FUN =  function(x) which(x == 0)))
# 
# list_character <- matrix(freq, ncol = ncol(in_out_data_without_stats), byrow = TRUE)
# 
# 
# # there appears to be no change in the way the data looks.
# 
# # ********************************************************
# # library(purrr)
# # map_chr(freq,1)
# 
# list_character <- matrix(data = 0, nrow = nrow(in_out_data_without_stats),ncol = 3)
# 
# # list_vector_num <- matrix(data = 0,nrow = nrow(in_out_data_without_stats),ncol=)
# 
# for (i in 1:nrow(in_out_data_without_stats)) {
#   list_character[i,] <- freq[[i]]
# }



# ********************************************************
# USING THE TEST DATAFRAMNES
# ********************************************************

# test date difference between mulitple columns
test_in <- in_time_data[1:10,1:31]

test_out <- out_time[1:10,1:31]
#str(test_in)
#str(test_out)

# replace NAs with "time_off" since there are no punches Assuming the NA as 0 
which(is.na(test_out))
test_in[is.na(test_in)] <- 0
test_out[is.na(test_out)] <- 0


# converting the data to parse_date_time format one column
# x12 <- parse_date_time(test_in$`2015-01-02`, orders = "Y-m-d H:M:S",tz = "GMT")
# x13 <- parse_date_time(test_out$`2015-01-02`,orders = "Y-m-d H:M:S",tz = "GMT")
# x12
# x13

# trying to convert the complete dataframe in parse_date_time converting
# parse_date_time(test_in,orders = "Y-m-d H:M:S")
# doesn't seam to work


#format(as.Date(test_in$`2015-01-02`[1:5],format = "%Y-%m-%d"),"%Y")

# creating an empty matrix for storing the test duration
test_dur <- matrix(data = 0,nrow = nrow(test_in), ncol = ncol(test_in))

# time difference calculation
# difftime(time1 = x13,time2 = x12, units = "hours")

# creating the tet_duration matrix that calcuates the difference of the work timings

for(i in 1:ncol(test_in)) {
  test_dur[,i] <- difftime(time1 = parse_date_time(test_out[,i],orders = "Y-m-d H:M:S"),
                           time2 = parse_date_time(test_in[,i],orders = "Y-m-d H:M:S"), units = "hours")
}

# creating the headers of data frames
colnames(test_dur) <- c(names(test_in))

# replacing the NA with time_offs being calcualted
test_dur[is.na(test_dur)] <- 0

# removing the first column which is the employee id column
test_dur <- test_dur[,-1]
# converting the test_dur to data.frame
test_dur <- data.frame(test_dur)
test_dur <- round(test_dur,2)

# finding with columns have the all columns as time_offs 

#sum(test_dur == "time_off")
#which(sapply(X = test_dur, FUN = function(x) length(which(x == "time_off"))) == 10 )

# removing the data frame that has all Statutory Holidays Remove
x160 <- test_dur[,-which(names(test_dur) %in% names(test_dur[which(colSums(test_dur == 0) == nrow(test_dur))]))]

# getting the count of Vacations of each employee
rowSums(test_dur == 0 )
# employees who are irregular
rowSums(test_dur < 7)
# average work hours spent at work
mean(x160,na.rm = T)

# column means
sapply(x160, function(x) mean(x))
colMeans(x160)

str(x160)
# row means
rowMeans(x160)



# calculating the row means
data.frame(ID = c(1:nrow(z160)), Row_means = rowMeans(x160))



# check if the headers of the data.frames are identifical.
sum(names(in_time_data) == names(out_time))

# aver
# there appears to be no change in the way the data looks.

# ********************************************************

