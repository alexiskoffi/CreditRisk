# Load library

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

# Data Exploration
# The data used comes from Kaggle, available [here](https://www.kaggle.com/c/GiveMeSomeCredit/data).

# /!\ Automatic download doesn't work with the code below, due to kaggle login + password

# download.file("https://www.kaggle.com/c/2551/download-all", "GiveMeSomeCredit.zip")
# unzip("GiveMeSomeCredit.zip", exdir = "data")
# file.remove("GiveMeSomeCredit.zip")

# So, I manually download the zip file and unzip it in a directory called "data" before working with

training <- read.csv("data/cs-training.csv")
test <- read.csv("data/cs-test.csv")
str(training)

# List of variables

# *SeriousDlqin2yrs* : indicates default behaviour, with 1 representing default (loan repayment is overdue by 90 days or more) and 0 representing non-default.
# SeriousDlqin2yrs Proportion
prop.table(table(training$SeriousDlqin2yrs))

# Number of customers that have defaulted on a loan
counts <- table(training$SeriousDlqin2yrs)
barplot(counts, main = "Number of customers that have defaulted on a loan", ylab = "Frequency")

# Histogram of age
hist(training$age, main = "Histogram of age", xlab = "Age")

# Age boxplot
boxplot(training$age, ylab = "Age")

# Histogram of monthly income
hist(training$MonthlyIncome, main = "Histogram of monthly income", xlab = "Monthly income")

# Monthly income
plot(training$MonthlyIncome, ylab = "Monthly income")

# Histogram of monthly income
index_highincome <- which(training$MonthlyIncome > 50000)
new_data <- training[-index_highincome, ]
hist(new_data$MonthlyIncome, main = "Histogram of monthly income", xlab = "Monthly income")

# Bar plot of the number of dependents
counts <- table(training$NumberOfDependents)
barplot(counts, main = "Bar plot of the number of dependents", xlab = "Number of dependents")

# Number of dependents
hist(training$NumberOfDependents, ylab = "Number of dependents")

# Training summary
training_summary <- training[-1]
summary(training_summary)

# MonthlyIncome* and *NumberOfDependents* have some `NA`s.
# One method for dealing with missing values is by replacing them with the median.

replace_NA <- function(x){
  index_NA <- which(is.na(x))
  x[index_NA] <- median(x, na.rm = TRUE)
  x
}

training$MonthlyIncome <- replace_NA(training$MonthlyIncome)
training$NumberOfDependents <- replace_NA(training$NumberOfDependents)

# Methods

# We are interested in predicting the probability of default SeriousDlqin2yrs in the `test` dataset.

# SeriousDlqin2yrs view in the test set
head(test$SeriousDlqin2yrs)

# Note *SeriousDlqin2yrs* is `NA` for each observation.

# We can predict the probability of default by using logistic regression, which has an output between 0 and 1.

log_model <- glm(SeriousDlqin2yrs ~ age, family = "binomial", data = training)
summary(log_model)

# Multiple variables

log_model_multi <- glm(SeriousDlqin2yrs ~ . - X, family = "binomial", data = training)
summary(log_model_multi)

# *RevolvingUtilizationOfUnsecuredLines* should be removed from the model because it is highly non-significant (p = 0.45885). I decided to keep *NumberOfOpenCreditLinesAndLoans* because it is only slightly above 0.05.

log_model_multi <- glm(SeriousDlqin2yrs ~ . - X - RevolvingUtilizationOfUnsecuredLines, family = "binomial", data = training)

# Predicting the probability of default

str(test)
summary(test)

# There are many `NA`s in the **test** dataset. We need to fix this because missing values are contagious (i.e., the model will predict `NA` for any row that has atleast 1 `NA` for any predictor variable). As seen above in the descriptive statistics, *MonthlyIncome* and *NumberOfDependents* have missing values.

test_replace <- test
test_replace$MonthlyIncome <- replace_NA(test_replace$MonthlyIncome)
test_replace$NumberOfDependents <- replace_NA(test_replace$NumberOfDependents)


predictions <- predict(log_model_multi, newdata = test_replace, type = "response")
range(predictions)


test_replace$SeriousDlqin2yrs <- predictions
test_prob <- data.frame(test_replace$X, test_replace$SeriousDlqin2yrs)
names(test_prob)[names(test_prob) == "test_replace.X"] <- "Id"
names(test_prob)[names(test_prob) == "test_replace.SeriousDlqin2yrs"] <- "Probability"

# Results

# We can see the top ten of the predictions : 

test_prob %>% 
  arrange(desc(.$Probability)) %>%
  head(10)
