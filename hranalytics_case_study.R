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


# read HR analytics files
employee_survey_data <- read.csv("PA-I_Case_Study_HR_Analytics/employee_survey_data.csv")
general_data <- read.csv("PA-I_Case_Study_HR_Analytics/general_data.csv")
in_time_data <- read.csv("PA-I_Case_Study_HR_Analytics/in_time.csv",check.names = T)
manager_survey_data <- read.csv("PA-I_Case_Study_HR_Analytics/manager_survey_data.csv")
out_time <- read.csv("PA-I_Case_Study_HR_Analytics/out_time.csv")

install.packages("xlsx")
data_dictionary <- xlsx::read.xlsx(file = "PA-I_Case_Study_HR_Analytics/data_dictionary.xlsx")

hr_data_dictionary <- xlsx::read.xlsx(file = "PA-I_Case_Study_HR_Analytics/data_dictionary.xlsx",sheetName = "data_dictionary",as.data.frame = TRUE)
View(hr_data_dictionary)

summary(employee_survey_data)

# DATA PREPARATION
# checking for duplicates
sum(duplicated(x = employee_survey_data$EmployeeID))





# EXPLORATORY DATA ANALYSIS










