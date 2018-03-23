myData <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(myData)

myData[myData$admit == 1,3] <- myData[myData$admit == 1,3] + 1

ggplot(myData,aes(gpa+1,admit)) + geom_point() +
  geom_smooth(method = "lm",se = FALSE) +
  coord_cartesian(ylim = c(0,1))

ggplot(myData,aes(gpa+1,admit)) + geom_point() +
  geom_smooth(method = "glm",se = FALSE,method.args = list(family = "binomial"))

# Model admin by GPA --- 
gpaModel <- glm(admit ~ gpa,data = myData,family ="binomial")

summary(gpaModel)

# gpa            8.708      1.205   7.227 4.93e-13 *** # log odds.. odds ratio is more interpretable
gpaModel$coefficients
options(scipen = 999)

exp(gpaModel$coefficients)
exp(confint(gpaModel))

# Model admint by GPA, ADMIT, GRE AND RANK
myData$rank <- factor(myData$rank)

fullModel <- glm(formula = admit ~ gpa + gre + rank,data = myData, family = "binomial")
summary(fullModel)

fullModel$coefficients
exp(fullModel$coefficients)
exp(confint(fullModel))

cbind(odds_ratio = exp(fullModel$coefficients),exp(confint(fullModel)))

fullModel <- glm(formula = admit ~ gpa + log2(gre) + rank,data = myData, family = "binomial")

cbind(odds_ratio = exp(fullModel$coefficients),exp(confint(fullModel)))

install.packages("aod")

library(aod)

wald.test(b = fullModel$coefficients,Sigma = vcov(fullModel),Terms = 4:6)

# p-value is much greater than 0.16
summary(fullModel)
# rank doesn't contribute much to the much to the fullModel, therefore removing it

fullModel <- glm(formula = admit ~ gpa + log2(gre) ,data = myData, family = "binomial")
summary(fullModel)

# k-fold cross validation. 
# test data and training set
library(caret)
crossValsettings <- trainControl(method = "repeatedcv",number = 10,savePredictions = TRUE)
crossVal <- train(as.factor(admit) ~ gpa + log2(gre),data = myData,family = "binomial",method = "glm",
                  trControl = crossValsettings)
crossVal



















