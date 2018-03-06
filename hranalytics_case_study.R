# Predictive Analytics Ikeyboard_arrow_rightGroup Project 1keyboard_arrow_rightSession 1

# ************************************************
# ************ HR Analytics Case Study************
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


##################################
# DATA PREPARATION
##################################

# checking for duplicates

summary(employee_survey_data)

# applying the str
# checking the characterisitics of the data frame
sapply(list(general_data,in_time_data,manager_survey_data,out_time,employee_survey_data),str)

# CHECKING DUPLICATES
sum(duplicated(x = employee_survey_data$EmployeeID))

# COMMENTS: THERE ARE NO DUPLCIATES

# substracting in and outtimes of the employess of two dataframes.

library(lubridate)

# IN_TIME AND OUT_TIME DATA_FRAME ANALYSIS

# GOAL OF THIS ANALYSIS IS TO FIND THE REGULARITY OF AN EMPLOYEE, VACATIONS TAKEN BY THE EMPLOYEE

# MISSING DATA IS ASSUMED AS "VACATION/TIME_OFF"

# LET US CREATE A SUBSET DATAFRAMES WITH LIMITED DATA TO TEST THE DATE TIME DIFFERENCE

# ********************************************************
# USING THE TEST DATAFRAMNES
# ********************************************************

# test date difference between mulitple columns
test_in <- in_time_data[1:20,1:5]

test_out <- out_time[1:20,1:5]
#str(test_in)
#str(test_out)

# replace NAs with "time_off" text
which(is.na(test_out))
test_in[is.na(test_in)] <- "time_off"
test_out[is.na(test_out)] <- "time_off"


# converting the data to parse_date_time format one column
x12 <- parse_date_time(test_in$`2015-01-02`, orders = "Y-m-d H:M:S",tz = "GMT")
x13 <- parse_date_time(test_out$`2015-01-02`,orders = "Y-m-d H:M:S",tz = "GMT")
x12
x13

# trying to convert the complete dataframe in parse_date_time converting
# parse_date_time(test_in,orders = "Y-m-d H:M:S")
# doesn't seam to work


#format(as.Date(test_in$`2015-01-02`[1:5],format = "%Y-%m-%d"),"%Y")

test_dur <- matrix(data = 0,nrow = nrow(test_in), ncol = ncol(test_in))


for(i in 1:length(test_in)) {

test_dur[,i] <- difftime(time1 = parse_date_time(test_out[,i],orders = "Y-m-d H:M:S"),
         time2 = parse_date_time(test_in[,i],orders = "Y-m-d H:M:S"), 
         units = "hours")
      }

colnames(test_dur) <- c(names(test_in))

test_dur[is.na(test_dur)] <- "time_off"

test_dur <- test_dur[,-1]
test_dur <- data.frame(test_dur)
round(test_dur,2)


#difftime(time1 = x13,time2 = x12,units = "hours")


# difftime(time1 = parse_date_time(test_in[,-c(1,2)],orders = "Y-m-d H:M:S"),
#         time2 = parse_date_time(test_out[,-c(1,2)],orders = "Y-m-d H:M:S"),tz = "GMT",units = c("auto"))
#View(test_out

#
# check if the headers of the data.frames are identifical.
sum(names(in_time_data) == names(out_time))

# there appears to be no change in the way the data looks.

# binding the two datasets
test_time_in_time_out <- rbind(test_in,test_out)
test_time_in_time_out[1:20,] - test_time_in_time_out[21:40,]

?diff.difftime()

















#































# EXPLORATORY DATA ANALYSIS










