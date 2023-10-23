title: "Alzheimer's Assessment: Predicting and Diagnosing Alzheimer's Disease"

## r setup, include = FALSE 
knitr::opts_chunk$set(echo = TRUE)

## r Install/Load packages, echo=FALSE, message=FALSE, warning=FALSE 
if(!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, skimr, nycflights13, gapminder, ggthemes, ggpubr, data.table, plotly, colorspace, tree, rpart, randomForest, ranger, rattle, pROC, partykit, ggplot2, glmnet, lda, data.table, ISLR, car)


## Goal of the Study:
## identify the main factors in an Alzheimer’s diagnosis and predict the probability of whether one is at risk of or has Alzheimer’s given certain factors

## Data:
## 7 explanatory variables: Age, Years of Education (EDU), Socioeconomic Status (SES), Mini Mental State Examination Scores (MMSE), Clinical Dementia Rating (CDR), Estimated Total Intracranial Volume (eTIV), Normalize Whole Brain Volume (nWBV), Atlas Scaling Factor (ASF) 
## 2 dependent variables: Clinical Dementia Rating (CDR), Dementia status
## 164 patients with dementia and 190 without. 206 people had a CDR of 0 (no dementia), 110 had a CDR of 0.5 (very mild dementia), 35 had a CDR of 1 (mild dementia), and 3 had a CDR of 2 (moderate dementia). Through our exploratory data analysis, we found that the age range was from 60-96 yrs old, with a mean of 77.03 years, a standard deviation of 7.81 years, and the oldest patients being female. EDU ranged from six to 23 years of education, with a mean of 14.70 years, a standard deviation of 2.9 years, and with the modes being 12 and 16 years. For the socioeconomic statuses, we found the median to be 2 on a scale of 1 through 5 with 1 being the highest socioeconomic status and 5 being lowest. The nWBV for those without Alzheimer’s had a mean of 0.741, while those who have Alzheimer’s had a mean nWBV of 0.717. Likewise, for eTIV, those without Alzheimer’s on average had a greater eTIV than those who do, being 1496 and 1484, respectively. Meanwhile, we found that the ASF is inversely proportional to the eTIV.


## Read both CSV files
## https://github.com/multivacplatform/multivac-dl/blob/master/data/mri-and-alzheimers/oasis_longitudinal.csv?plain=1
data <- read.csv("/Users/caedenkim/Desktop/WDS/oasis_longitudinal.csv")

## r Change "Converted" to "Demented"
data$Group[data$Group == 'Converted'] <- 'Demented' # Converted patients were patients who were diagnosed in the middle of the study

## r assign values of 0 and 1 to demented patients
data$Group[data$Group == 'Demented'] <- as.numeric(1)
data$Group[data$Group == 'Nondemented'] <- as.numeric(0)
data$Group <- as.numeric(data$Group)

## r clean data
data <- data %>% 
  select(Subject.ID, Group, Visit, M.F, Age, EDUC, SES, MMSE, CDR, eTIV, nWBV, ASF) %>%
  rename(dementia = Group, visit = Visit, sex = M.F, age = Age, education = EDUC)

## r check incomplete rows
data_na <- data[!complete.cases(data), ] 
summary(data_na)
# 19 incomplete rows, all missing SES data, 2 missing MMSE data. All 19 rows are positive for dementia 

## r removing incomplete rows
clean_data <- data[complete.cases(data), ]

## r changing var types
## Turning dementia, visit, SES, and CDR into categorical variables
clean_data$dementia <- as.factor(clean_data$dementia)
clean_data$visit <- as.factor(clean_data$visit)
clean_data$CDR <- as.factor(clean_data$CDR)

## Key Terms

# Dementia: A term loosely used to describe patients with Alzheimer's Disease, which is related to the broader condition of dementia.

# CDR: Clinical Dementia Rating - How severe the dementia of a patient is on a scale of 0-3 (0 = No Dementia, 3 = Severe Dementia)

# Mini Mental State Exam: A 30 question test administered to test the mental capabilities of a patient. 

# eTIV: estimated Total Intracranial Volume - How much space there is within the skull (measured in cubic centimeters)

# nWBV: normalized Whole Brain Volume - The percentage of white or gray matter inside the brain

# ASF: Atlas Scaling Factor - A scaling factor used to compare the eTIV of a patient to the volume of the average calculated by a healthy young brain, and healthy aged brain. In this case, the average volume is 1755 cubic centimeters. 

## r separate cleaned data into two dataframes 
clean_data_ndem <- clean_data %>% filter(dementia == 0) # Patients who do not have dementia
clean_data_dem <- clean_data %>% filter(dementia == 1) # Patients who have dementia 


## r summary of cleaned data
summary(clean_data)
summary(clean_data_ndem)
summary(clean_data_dem)

## r, include = FALSE}
summary(clean_data_ndem$nWBV)
summary(clean_data_dem$nWBV)
```

## r box plot of nWBV in demented/nondemented patients
clean_data %>% 
  ggplot(aes(y = nWBV)) + 
  geom_boxplot(aes(fill = dementia)) + 
  facet_wrap(~dementia)

## r pie chart for dementia vs non-dementia
clean_data %>%
  ggplot(aes(x = factor(1), fill = dementia)) + 
  geom_bar(width = 1) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("#ff416b","#ff8e6d"))

## r CDR pie chart
clean_data %>%
  ggplot(aes(x = factor(1), fill = CDR)) + 
  geom_bar(width = 1) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("#ff416b","#ff8e6d", "#ec8da2", "#ffd0dd"))

## r pie chart for sex
clean_data %>%
  ggplot(aes(x = factor(1), fill = sex)) + 
  geom_bar(width = 1) + 
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("#ff416b","#ff8e6d"))

## r gender distribution
gender_table <- table(clean_data$sex)
gender_table
gender_table_d <- table(clean_data_dem$sex)
gender_table_d
gender_table_nd <- table(clean_data_ndem$sex)
gender_table_nd

## r age distribution of data set
clean_data %>%
  ggplot(aes(x = age)) + 
  geom_bar(position = 'stack', aes(fill = sex)) + 
  scale_fill_manual(values = c("#ff416b","#ff8e6d"))+ 
  ggtitle('Age of Patients')

## r eTIV distribution
clean_data %>%
  ggplot(aes(x = eTIV)) + 
  geom_histogram(bins = 10, fill = '#00243b') +
  ggtitle('Distribution of "estimated Total Intracranial Volume"') + 
  facet_wrap(~dementia)

## r nWBV distribution
clean_data %>%
  ggplot(aes(x = nWBV)) + 
  geom_histogram(bins = 10, fill = '#00243b') + 
  ggtitle('Distribution of "normal Whole-Brain Volume"') + 
  facet_wrap(~dementia)

## r MMSE distribution
clean_data %>%
  ggplot(aes(x = MMSE)) + 
  geom_histogram(bins = 10, fill = '#00243b') + 
  ggtitle('Distribution of "normal Whole-Brain Volume"') + 
  facet_wrap(~dementia)

## r Bar Graph of education
clean_data %>%
  ggplot(aes(x = education)) + 
  geom_bar(aes(fill = sex)) + 
  scale_fill_manual(values = c("#ff416b","#ff8e6d"))  + 
  ggtitle('Years of Education')

## r SES distribution
clean_data %>%
  ggplot(aes(x = SES)) + 
  geom_bar(fill = '#00243b') + 
  scale_x_reverse() + 
  ggtitle('Distribution of "Socio-Economic Standing"')
```

##### MODELS #####

# 25% of data went into the testing data, 75% of our dataset went into training data
# Each model uses backwards selection to determine which variables were significant

## r changing converted to demented
set.seed(1)
n <- nrow(clean_data)
index.1 <- sample(n,n*3/4, replace = FALSE)
clean_train <- clean_data[index.1, ]
testdata <- clean_data[-index.1, ]
clean_data

alzlm <- clean_data %>%
  select(-visit, -CDR)

alzlm$sex <- as.factor(alzlm$sex)
alzlm

##### Multiple Regression Model #####

## Used backwards elimination to remove least significant variables: Age, Atlas Scaling Factor, MR delay.

## r linear model
fit1 <- lm(dementia ~., data = alzlm)

## r backwards selection linear model
fit1.1 <- update(fit1, .~. -age)

fit1.2 <- update(fit1.1, .~. -ASF)

fit1.3 <- update(fit1.2, .~. -mrdelay)

# Anova(fit1.3)

## r exploring linear model MCE
predictions <- predict(fit1.3, select(alzlm, c(-age, -ASF)), response = "confidence")

predictionsbinary <- ifelse(predictions>1/3, "1","0")

MCElm <- mean(predictionsbinary != alzlm$dementia)
MCElm # 69.1% accuracy

##### Full Logistic #####

##  Used backwards elimination to select for variables. Removed Atlas Scaling Factor and MR delay

## r logistic regression and backwards elimination
fit2 <- glm(dementia~., data = alzlm, family=binomial)
fit2.1 <- update(fit2, .~.)
fit2.2 <- update(fit2.1, .~. -ASF)
summary(fit2.2)

## r exploring logistic model MCE
predictionslog <- predict(fit2.2, select(testdata, c(-ASF)), response = "confidence")

predictionsbinarylog <- ifelse(predictionslog>1/3, "1","0")

MCElogm <- mean(predictionsbinarylog != testdata$dementia)
MCElogm

## r
fit1nme <- update(fit1, .~. -MMSE)

fit1nme.1 <- update(fit1nme, .~. -ASF)
fit1nme.2 <- update(fit1nme.1, .~. -SES)

# Anova(fit1nme.2)

fit2nme <- update(fit2, .~. -MMSE)

fit2nme.1 <- update(fit2nme, .~. -ASF)

Anova(fit2nme.1)

## r exploring simple logistic model
fit3 <- glm(dementia~MMSE + sex, data = alzlm, family=binomial)
summary(fit3)
predictionslog2 <- predict(fit3, select(testdata, c(-ASF)), response = "confidence")

predictionsbinarylog2 <- ifelse(predictionslog2>1/3, "1","0")

MCElogm2 <- mean(predictionsbinarylog2 != testdata$dementia)
MCElogm2

## r exploring simple logistic model ASF
fit4 <- glm(dementia~ASF, data = alzlm, family=binomial)
summary(fit4)
predictionslog2 <- predict(fit4, select(testdata, c(ASF)), response = "confidence")

predictionsbinarylog2 <- ifelse(predictionslog2>1/3, "1","0")

MCElogm2 <- mean(predictionsbinarylog2 != testdata$dementia)
MCElogm2 # 82.8% accuracy 

##### Decision tree #####

## All variables included in the final model excluding identifiers and clinical dementia rating. No backwards selection
## Built from 75% of the data (randomly selected). Tested the other 25% of the data, received a misclassification error of 0.202. 

n <- nrow(clean_data)
set.seed(1)
train.index <- sample(n, n*3/4) # we use about 3/4 of the subjects as the training data.
train.index
data.train <- clean_data[train.index, ]
data.test <- clean_data[-train.index, ]

data.traintr <- data.train %>% select(-CDR, -visit)
data.traintr$dementia <- data.traintr$dementia
fit1.single.full <- tree(dementia~., data=data.traintr)  
fit1.single.full
#fit2.single.full <- update(fit1.single.full, .~. -CDR)

plot(fit1.single.full)
#fancyRpartPlot(fit1.single.full)
title(main =  "final single tree")
text(fit1.single.full, pretty=0)

fit1.single.full.s <- summary(fit1.single.full)
names(fit1.single.full.s)
names(fit1.single.full) 

sum((fit1.single.full.s$residuals)^2) # to check the dev is the RSS

fit1.single.full.s$used # Var's included
nvar <- length(fit1.single.full.s$used )

test.error.tree <- predict(fit1.single.full, data.test)
predictionsb <- ifelse(test.error.tree > 1/3, "1", "0")
mce <- mean(predictionsb != data.test$dementia)

print(mce)


##### FINDINGS #####

* Mini Mental State Exam:
  Mini Mental State Examination is one of the most accurate predictors of dementia. Dementia definitely becomes more common as Age increases
* Atlas Scaling Factor:
  ASF is not a good predictor of Alzheimer’s disease
* Age:
  As age increases, dementia becomes more common.
* Sex:
  Sex is a significant variable, with males having a higher chance of dementia when accounting for other variables.

## Citations:

Marcus, D. S., Fotenos, A. F., Csernansky, J. G., Morris, J. C., & Buckner, R. L. (2010, December). Open access series of imaging studies: Longitudinal MRI data in nondemented and demented older adults. Retrieved from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2895005/
  
  Buckner, R. L., Head, D., Parker, J., Fotenos, A. F., Marcus, D., Morris, J. C., & Snyder, A. Z. (2004, September 09). A unified approach for morphometric and functional data analysis in young, old, and demented adults using automated atlas-based head size normalization: Reliability and validation against manual measurement of total intracranial volume. Retrieved from https://www.sciencedirect.com/science/article/abs/pii/S1053811904003271

