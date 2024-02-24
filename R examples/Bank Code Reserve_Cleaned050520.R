#https://www.machinelearningplus.com/machine-learning/logistic-regression-tutorial-examples-r/
#Bank Card Acceptance Rate - https://archive.ics.uci.edu/ml/datasets/Bank+Marketing##
## https://www.kaggle.com/chintanchitroda/bank-marketing/comments Look at this one
##
###'7. Attribute information:
#
#https://juliasilge.com/blog/hotels-recipes/''
#There are four datasets:
#1) bank-additional-full.csv with all examples (41188) and 20 inputs, ordered by date (from May 2008 to November 2010), very close to the data analyzed in [Moro et al., 2014]
#2) bank-additional.csv with 10% of the examples (4119), randomly selected from 1), and 20 inputs.
#3) bank-full.csv with all examples and 17 inputs, ordered by date (older version of this dataset with less inputs).
#4) bank.csv with 10% of the examples and 17 inputs, randomly selected from 3 (older version of this dataset with less inputs)
# Input variables:
#   # bank client data:
#   1 - age (numeric)
# 2 - job : type of job (categorical: "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")
# 3 - marital : marital status (categorical: "divorced","married","single","unknown"; note: "divorced" means divorced or widowed)
# 4 - education (categorical: "basic.4y","basic.6y","basic.9y","high.school","illiterate","professional.course","university.degree","unknown")
# 5 - default: has credit in default? (categorical: "no","yes","unknown")
# 6 - housing: has housing loan? (categorical: "no","yes","unknown")
# 7 - loan: has personal loan? (categorical: "no","yes","unknown")
# related with the last contact of the current campaign:
# 8 - contact: contact communication type (categorical: "cellular","telephone") 
# 9 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
# 10 - day_of_week: last contact day of the week (categorical: "mon","tue","wed","thu","fri")
# 11 - duration: last contact duration, in seconds (numeric). Important note:  
#this attribute highly affects the output target (e.g., if duration=0 then y="no")
#. Yet, the duration is not known before a call is performed. 
#Also, after the end of the call y is obviously known. Thus, this input should only be included
#for benchmark purposes and should be discarded if the intention is to have a realistic
#predictive model.
# other attributes:
# 12 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# 13 - pdays: number of days that passed by after the client was last contacted from a previous
#campaign (numeric; 999 means client was not previously contacted)
# 14 - previous: number of contacts performed before this campaign and for this client (numeric)
# 15 - poutcome: outcome of the previous marketing campaign (categorical: "failure","nonexistent","success")
# social and economic context attributes
# 16 - emp.var.rate: employment variation rate - quarterly indicator (numeric)
# 17 - cons.price.idx: consumer price index - monthly indicator (numeric)     
# 18 - cons.conf.idx: consumer confidence index - monthly indicator (numeric)     
# 19 - euribor3m: euribor 3 month rate - daily indicator (numeric)
# 20 - nr.employed: number of employees - quarterly indicator (numeric)
# Output variable (desired target):
# 21 - y - has the client subscribed a term deposit? (binary: "yes","no")
# 8. Missing Attribute Values: There are several missing values in some categorical attributes, all coded with the "unknown" label. These missing values can be treated as a possible class label or using deletion or imputation techniques. 

### Prep###
###

bank.additional.full <- read.csv("~/CCSU/R Programming Class/bank-additional-full.csv", sep=";")
View(bank.additional.full)

#bank.additional.full <- read.csv("~/CCSU2020/Data 476 - R Programming
#                                 /bank/bank-additional/bank-additional/
#                                 bank-additional-full.csv", sep=";")
View(bank.additional.full)


### 1 Descriptive Stats
### Do some desriptive statistics and EDA
banksum <-summary(bank.additional.full)
View(banksum)
#install.packages("fBasics")
#install.packages("skimr")

### 2 EDA
#2 - job : type of job (categorical: "admin.","blue-collar","entrepreneur","housemaid","management","retired","self-employed","services","student","technician","unemployed","unknown")
library(tibble)
library(ggplot2)
library(tidyr)
library(dplyr)
#install.packages("e1071")
library(e1071)

#https://community.rstudio.com/t/create-a-percentage-stacked-bar-chart/26223/5
library("RColorBrewer")
display.brewer.all()

###1 age
response <- bank.additional.full$y
var <- bank.additional.full$age
#install.packages("wesanderson")
#library(wesanderson)
#display.wesanderson.all()

ggplot(bank.additional.full, 
aes(var))  +
scale_fill_brewer(palette = "Set1") + 
geom_bar(aes(fill = response), position = "fill")+
xlab("Age") + ylab("% of 100") +
ggtitle("Age vs Response")

summary(var)
table(var)

### RECODE AGE ####
age_factor <- cut(x = var, breaks = c(0, 25, 35, 50, 60,70, 100))

table(age_factor)  
bank.additional.full$age_factor <- age_factor

###2 Job
response <- bank.additional.full$y
var <- bank.additional.full$job

ggplot(bank.additional.full, 
       aes(bank.additional.full$job))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("Job") + ylab("% of 100") +
  ggtitle("Job vs Response")
summary(var)

###3 marital
response <- bank.additional.full$y
var <- bank.additional.full$marital

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("Status") + ylab("% of 100") +
  ggtitle("Marital Status vs Response")
summary(var)

###4 education
response <- bank.additional.full$y
var <- bank.additional.full$education

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("Education") + ylab("% of 100") +
  ggtitle("Education vs Response")
summary(var)

###5 default
response <- bank.additional.full$y
var <- bank.additional.full$default

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("Default") + ylab("% of 100") +
  ggtitle("Default vs Response")
summary(var)
## Recode yes to unknown

###6 housing
response <- bank.additional.full$y
var <- bank.additional.full$housing
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("housing") + ylab("% of 100") +
  ggtitle("Housing vs Response")
summary(var)

###7 loan
response <- bank.additional.full$y
var <- bank.additional.full$loan

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("loan") + ylab("% of 100") +
  ggtitle("Loan vs Response")
summary(var)

###8 contact
response <- bank.additional.full$y
var <- bank.additional.full$contact

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("contact") + ylab("% of 100") +
  ggtitle("Contact vs Response")
summary(var)

###9 month
response <- bank.additional.full$y
var <- bank.additional.full$month

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("month") + ylab("% of 100") +
  ggtitle("Month vs Response")
summary(var)

###10 Day of the Week
response <- bank.additional.full$y
var <- bank.additional.full$day_of_week

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("month") + ylab("% of 100") +
  ggtitle("Day of Week vs Response")
summary(var)

###11 Duration
response <- bank.additional.full$y
var <- bank.additional.full$duration

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("duration") + ylab("% of 100") +
  ggtitle("Duration vs Response")
### Not known before call, drop it

###12 Campaign
response <- bank.additional.full$y
var <- bank.additional.full$campaign

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("campaign") + ylab("% of 100") +
  ggtitle("Campaign vs Response")
summary(var)
class(var)

x <-data.frame(table(var))
View(x)
#https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/
#install.packages("epiDisplay")
library(epiDisplay)
tab1(var, sort.group = "decreasing", cum.percent = TRUE)

### RECODE IT, count n of times and bucket
var2 <- ifelse(var >= 7, '7 more', var)
var2
tab1(var2, sort.group = "decreasing", cum.percent = TRUE)
bank.additional.full$camp_recode <- var2
var<- bank.additional.full$camp_recode

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("campaign recode") + ylab("% of 100") +
  ggtitle("Campaign Recode vs Response")

###13 Pdays
response <- bank.additional.full$y
var <- bank.additional.full$pdays

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("pdays") + ylab("% of 100") +
  ggtitle("PDays vs Response")
summary(var)
### RECODE IT, get rid of 999s, look at counts
x <-data.frame(table(var))
View(x)
var2 <- replace(var, var == 999, 'g. none')
tab1(var2, sort.group = "decreasing", cum.percent = TRUE)
class(var2)
### Collapse days into one week two weeks three weeks or more
var2 <- ifelse(var >= 0 & var <= 7, 'a lt one week', var2)
var2 <- ifelse(var >7 & var <= 14, 'b one to two weeks', var2)
var2 <- ifelse(var > 14 & var <= 21, 'c two to three weeks', var2)
var2 <- ifelse(var > 21 & var <= 40, 'd three pl weeks', var2)

  x <-data.frame(table(var2))
  View(x)

bank.additional.full$pday_recode <- var2


var<- bank.additional.full$pday_recode

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("pdays") + ylab("% of 100") +
  ggtitle("PDays Recode vs Response")

x <-data.frame(table(var))
View(x)

###14 Previous
response <- bank.additional.full$y
var <- bank.additional.full$previous
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("previous") + ylab("% of 100") +
  ggtitle("Previous vs Response")
summary(var)
x <-data.frame(table(var))
View(x)
### RECODE IT, get rid of 9999s, look at counts
var2 <- ifelse(var >= 3, 3, var)
x <-data.frame(table(var2))
View(x)
bank.additional.full$previous_recode <- var2

var <- bank.additional.full$previous_recode
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("previous") + ylab("% of 100") +
  ggtitle("Previous Recode vs Response")


###15 P Outcome
response <- bank.additional.full$y
var <- bank.additional.full$poutcome

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("poutcome") + ylab("% of 100") +
  ggtitle("Poutcome vs Response")
summary(var)

###16 emp.var.rate
response <- bank.additional.full$y
var <- bank.additional.full$emp.var.rate
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("emp.var.rate") + ylab("% of 100") +
  ggtitle("emp.var.rate vs Response")
summary(var)
tab1(var, sort.group = "decreasing", cum.percent = TRUE)
table(var)

library(dplyr)

var2 <- cut(var, breaks=c(-Inf, -3, -1.5, 0, 1.3, Inf),
                     labels=c("a_lowest","b_lower","c_low","d_medium","e_highest"))
table(var2)

bank.additional.full$emp.var.rate_recode <- var2
var <- bank.additional.full$emp.var.rate_recode
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("emp.var.rate") + ylab("% of 100") +
  ggtitle("emp.var.rate Recode vs Response")

###17 cons.price_indx
response <- bank.additional.full$y
var <- bank.additional.full$cons.price.idx
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("cons.price_indx") + ylab("% of 100") +
  ggtitle("cons.price_indx vs Response")
table(var)

var2 <- cut(var, breaks=c( 92, 92.5, 93, 93.5, 94,94.5, 95),
            labels=c("a","b","c","d","e","f"))
table(var2)

bank.additional.full$cons.price.idx_recode <- var2
var <- bank.additional.full$cons.price.idx_recode

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("cons.price_indx_recode") + ylab("% of 100") +
  ggtitle("cons.price_indx_recode vs Response")
table(var)

###18 cons.conf.indx
response <- bank.additional.full$y
var <- bank.additional.full$cons.conf.idx
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("cons.conf.indx") + ylab("% of 100") +
  ggtitle("cons.conf.indx vs Response")
table(var)


var2 <- cut(var, breaks=c(-Inf, -50, -40, -35, -30, Inf),
            labels=c("a_cconf","b_cconf","c_cconf","d_cconf","e_cconf"))
table(var2)

bank.additional.full$cons.conf.idx_recode <- var2
var <- bank.additional.full$cons.conf.idx_recode
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("cons.conf.indx_recode") + ylab("% of 100") +
  ggtitle("cons.conf.indx_recode vs Response")

###19 euribor3m
response <- bank.additional.full$y
var <- bank.additional.full$euribor3m
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("euribor3m") + ylab("% of 100") +
  ggtitle("euribor3m vs Response")
table(var)
var2 <- cut(var, breaks=c(-Inf, 1, 2, 3, 6, Inf),
            labels=c("a_euribor3m","b_euribor3m","c_euribor3m","d_euribor3m","e_euribor3m"))
table(var2)

bank.additional.full$euribor3m_recode <- var2
var <- bank.additional.full$euribor3m_recode

ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("euribor3m_recode") + ylab("% of 100") +
  ggtitle("euribor3m_recode vs Response")

###20 nr.employed
response <- bank.additional.full$y
var <- bank.additional.full$nr.employed
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("nr.employed") + ylab("% of 100") +
  ggtitle("nr.employed vs Response")
table(var)

var2 <- cut(var, breaks=c(-Inf, 5000, 5100,  5500),
            labels=c("a_nr.employed","b_nr.employed","c_nr.employed"))
table(var2)

bank.additional.full$nr.employed_recode <- var2
var <- bank.additional.full$nr.employed_recode
ggplot(bank.additional.full, 
       aes(var))  +
  scale_fill_brewer(palette = "Set1") + 
  geom_bar(aes(fill = response), position = "fill")+
  xlab("nr.employed_recode") + ylab("% of 100") +
  ggtitle("nr.employed_recode vs Response")
table(var)


### 3 - Linear Model
for_LM = subset(bank.additional.full,
select = c('age','age_factor','job','marital','education','default','housing','loan','contact','month','day_of_week',
           'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
           'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode','y'))

for_LM$target <- ifelse(for_LM$y == 'yes', 1,0)


# Prep Training and Test data.
#install.packages('mlbench')
set.seed(100)

library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.
set.seed(100)
trainDataIndex <- createDataPartition(for_LM$target, p=0.6, list = F)
trainData <- for_LM[trainDataIndex, ]
testData <- for_LM[-trainDataIndex, ]

# Class distribution of train data
table(trainData$y)
table(testData$y)

class(trainData$y)
## Make it a class variable
trainData$yclass <- as.character(trainData$y)
class(trainData$yclass)


# Build Logistic Model
logitmod <- glm(target ~ age + job + marital ,
                family = "binomial", data=down_train)

summary(logitmod)

#https://thestatsgeek.com/2014/02/08/r-squared-in-logistic-regression/
#  https://stats.idre.ucla.edu/r/dae/logit-regression/
# https://www.r-bloggers.com/evaluating-logistic-regression-models/

## STEPWISE how to
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4842399/
### Replace data_balanced_over with trainData to not oversample
#### IMPORTANT###########################

for_LM_xs = subset(trainData,
                   select = c('age', 'job','marital','education','default','housing','loan','contact','month',
                              'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
                              'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode'))

Z = as.data.frame(cbind(trainData$target,for_LM_xs))
View(Z)

logitmod2a <- glm(trainData$target ~ age+job+marital+education + default + housing
                  + contact, family = "binomial", data=Z)
summary(logitmod2a)

logitmodfull <- glm(trainData$target ~ ., family = "binomial", data=for_LM_xs)
summary(logitmodfull)

library(MASS)
stepmod <- stepAIC(logitmodfull, direction = 'backward', trace = TRUE)

summary(stepmod)
stepmod$anova

logitmod2 <- stepmod


### Variable Importance
#https://www.r-bloggers.com/variable-importance-plot-and-variable-selection/
library(caret)
Varimplist <- varImp(stepmod, importance = TRUE)

pred <- predict(logitmod2, newdata = testData, type = "response")
pred

# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$target

# Accuracy
###https://rpubs.com/kmanalo/235576
mean(y_pred == y_act)  # 94%

confusionMatrix(y_pred, y_act)
xtab =table(y_pred, y_act)

print(confusionMatrix(xtab[2:1,2:1]))

### Rename Target
names(Z)[1] <- "termdep_Y"

#  Add prediction to the training set
Z <- cbind(Z, fitted = fitted(logitmod2))
Z$reccount = 1
#  Add decile to the training set
decLocations <- quantile(Z$fitted, probs = seq(0.1,0.9,by=0.1))
Z$decile <- findInterval(Z$fitted,c(-Inf,decLocations, Inf))
+
## Same for Test Data
for_LM_xs_test = subset(testData,
                        select = c('age', 'job','marital','education','default','housing','loan','contact','month',
                                   'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
                                   'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode'))

Z_test = as.data.frame(cbind(testData$target,for_LM_xs_test))
### Rename Target
names(Z_test)[1] <- "termdep_Y"


#  Add prediction to the test set
Z_test <- cbind(Z_test, fitted = pred)
Z_test$reccount = 1

#  Add decile to the test set
decLocations <- quantile(Z_test$fitted, probs = seq(0.1,0.9,by=0.1))
Z_test$decile <- findInterval(Z_test$fitted,c(-Inf,decLocations, Inf))


#### Make Deciles Table Training
#https://www.datavedas.com/model-evaluation-in-r/
library(dplyr)
class(Z$termdep_Y)
decile_grp<-group_by(Z,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=sum(reccount), min_prob=min(p=fitted),
                             max_prob=max(fitted),
                             resp_cnt=sum(termdep_Y),
                             non_resp_cnt=total_cnt -resp_cnt,
                             rep_rate=(resp_cnt/total_cnt)*100)
View(decile_summ_train)

decile_summ_train<-arrange(decile_summ_train, desc(decile))
sum1 <- sum(decile_summ_train$resp_cnt)
sum1

sum2 <- sum(decile_summ_train$non_resp_cnt)
sum2

decile_summ_train$resp_pct <- ((decile_summ_train$resp_cnt)/sum1)*100
decile_summ_train$non_resp_pct <- ((decile_summ_train$non_resp_cnt)/sum2)*100
decile_summ_train$cum_resp_pct <- cumsum(decile_summ_train$resp_pct)
decile_summ_train$cum_non_resp_pct <- cumsum(decile_summ_train$non_resp_pct)
decile_summ_train$ks_stats <- abs(decile_summ_train$cum_resp_pct-decile_summ_train$cum_non_resp_pct)
View(decile_summ_train)


### DECILES FOR TEST
#############################
decile_grp<-group_by(Z_test,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=sum(reccount), min_prob=min(p=fitted),
                             max_prob=max(fitted),
                             resp_cnt=sum(termdep_Y),
                             non_resp_cnt=total_cnt -resp_cnt,
                             rep_rate=(resp_cnt/total_cnt)*100)
View(decile_summ_test)

decile_summ_test<-arrange(decile_summ_test, desc(decile))
sum1 <- sum(decile_summ_test$resp_cnt)
sum1

sum2 <- sum(decile_summ_test$non_resp_cnt)
sum2

decile_summ_test$resp_pct <- ((decile_summ_test$resp_cnt)/sum1)*100
decile_summ_test$non_resp_pct <- ((decile_summ_test$non_resp_cnt)/sum2)*100
decile_summ_test$cum_resp_pct <- cumsum(decile_summ_test$resp_pct)
decile_summ_test$cum_non_resp_pct <- cumsum(decile_summ_test$non_resp_pct)
decile_summ_test$ks_stats <- abs(decile_summ_test$cum_resp_pct-decile_summ_test$cum_non_resp_pct)
View(decile_summ_test)


###### Gains Chart

df_gain_train <- subset(decile_summ_train,select = c(1,10))

df_gain_train <- rename(df_gain_train,'cum_resp_pct_train'='cum_resp_pct')
df_gain_test <- subset(decile_summ_test,select = c(10))
df_gain_test <- rename(df_gain_test,'cum_resp_pct_test'='cum_resp_pct')
df_gain_chart <- cbind(df_gain_train,df_gain_test)
df_gain_chart <- cbind(df_gain_train,df_gain_test)
df_gain_chart$cum_resp_pct_train <- round(df_gain_chart$cum_resp_pct_train,2)
df_gain_chart$cum_resp_pct_test <- round(df_gain_chart$cum_resp_pct_test,2)

base_pct <- c(10,20,30,40,50,60,70,80,90,100)
df_gain_chart <- cbind(df_gain_chart,base_pct)

#@install.packages("plotly")
library(plotly)
plot_ly(df_gain_chart,x=~decile,y=~cum_resp_pct_train,type = 'scatter',name = 'train_pct',mode='lines')%>%
  add_trace(y = ~cum_resp_pct_test, name = 'test_pct', mode = 'lines') %>%
  add_trace(y = ~base_pct, name = 'base_pct', mode = 'lines')%>%
  layout(title = "Gains Chart",
         xaxis = list(title = "Decile",range=c(10,1)),
         yaxis = list (title = "Percenatge"))

###### Lift Chart

df_gain_chart$lift_train <-df_gain_chart$cum_resp_pct_train/df_gain_chart$base_pct
df_gain_chart$lift_test <-df_gain_chart$cum_resp_pct_test/df_gain_chart$base_pct
df_gain_chart$base_line <- c(1,1,1,1,1,1,1,1,1,1)

lift_chart <- subset(df_gain_chart,select = c(1,5,6,7))

plot_ly(lift_chart,x=~decile,y=~lift_train,type = 'scatter',name = 'lift_train',mode='lines')%>%
  add_trace(y = ~lift_test, name = 'lift_test', mode = 'lines') %>%
  add_trace(y = ~base_line, name = 'base_line', mode = 'lines')%>%
  layout(title = "Lift Chart",
         xaxis = list(title = "Decile",range=c(10,1)),
         yaxis = list (title = "Percenatge",range=c(0,5)))


# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$target

# Accuracy
###https://rpubs.com/kmanalo/235576
mean(y_pred == y_act)  # 94%

confusionMatrix(y_pred, y_act)
xtab =table(y_pred, y_act)

print(confusionMatrix(xtab[2:1,2:1]))


### 4 - SVM
#https://www.datacamp.com/community/tutorials/support-vector-machines-r
set.seed(10111)
#install.packages(('e1071'))
library(e1071)
stepmod$anova
#trainData$target ~ age + job + default + contact + month + camp_recode + 
#  pday_recode + poutcome + emp.var.rate_recode + cons.conf.idx_recode + 
#  cons.price.idx_recode + euribor3m_recode + nr.employed_recode

for_SVM_xs = subset(trainData,
    select = c('age', 'job','default','contact','month',
    'camp_recode','pday_recode','poutcome','emp.var.rate_recode',
    'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode',
    'nr.employed_recode'))
Z2 = as.data.frame(cbind(trainData$target,for_SVM_xs))
View(Z2)
### Rename Target
names(Z2)[1] <- "termdep_Y"
View(Z2)
#LINEAR KERNAL, Tried Others Performance was about the same
svmfit = svm(termdep_Y ~ ., data = Z2,type = 'C-classification', kernel = 'linear')
print(svmfit)
summary(svmfit)
pred_svm <- predict(svmfit, newdata = testData, type = "response")
pred_svm
y_act_svm <- factor(testData$target)

# Accuracy
###https://rpubs.com/kmanalo/235576
mean(pred_svm == y_act)  # 94%

class(y_act_svm)
class(pred_svm)

svmconf <- confusionMatrix(pred_svm, y_act_svm)
xtab =table(pred_svm, y_act_svm)

print(confusionMatrix(xtab[2:1,2:1]))

##### Decile for SVM Model######################################################################
#  Add prediction to the training set

Z2_SVM <- cbind(Z2, fitted = fitted(svmfit))
Z2_SVM$reccount = 1
class(Z2$fitted)
Z2_SVM <- Z2_SVM[ order(Z2_SVM$fitted, decreasing=FALSE), ]  # Use built-in R functions

## Make a running count
Z2_SVM$running = cumsum(Z2_SVM$reccount)
Z2_SVM$totN <- max(Z2_SVM$running)
## Create the decile off the running count
Z2_SVM$perrank <- Z2_SVM$running/Z2_SVM$totN
View(Z2_SVM)

#  Add decile to the training set
Z2_SVM$fitted_ct <- as.numeric(Z2_SVM$fitted)
table(Z2_SVM$fitted)
table(Z2_SVM$fitted_ct)
class(Z2_SVM$fitted_ct)
Z2_SVM$fitted_ct <- ifelse(Z2_SVM$fitted_ct == 2 , 1, 0)
table(Z2_SVM$fitted_ct)

#  Add decile to the training set
decLocations_SVM <- quantile(Z2_SVM$running, probs = seq(0.1,0.9,by=0.1))
Z2_SVM$decile <- findInterval(Z2_SVM$running,c(-Inf,decLocations_SVM, Inf))


#### Make Deciles Table Training
#https://www.datavedas.com/model-evaluation-in-r/
library(dplyr)
class(Z2_SVM$termdep_Y)

decile_grp<-group_by(Z2_SVM,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=sum(reccount), min_prob=min(p=fitted_ct),
                             max_prob=max(fitted_ct),
                             resp_cnt=sum(termdep_Y),
                             non_resp_cnt=total_cnt -resp_cnt,
                             rep_rate=(resp_cnt/total_cnt)*100)
View(decile_summ_train)

decile_summ_train<-arrange(decile_summ_train, desc(decile))
sum1 <- sum(decile_summ_train$resp_cnt)
sum1

sum2 <- sum(decile_summ_train$non_resp_cnt)
sum2

decile_summ_train$resp_pct <- ((decile_summ_train$resp_cnt)/sum1)*100
decile_summ_train$non_resp_pct <- ((decile_summ_train$non_resp_cnt)/sum2)*100
decile_summ_train$cum_resp_pct <- cumsum(decile_summ_train$resp_pct)
decile_summ_train$cum_non_resp_pct <- cumsum(decile_summ_train$non_resp_pct)
decile_summ_train$ks_stats <- abs(decile_summ_train$cum_resp_pct-decile_summ_train$cum_non_resp_pct)
View(decile_summ_train)

### To HERE 4/14/20############

### DECILES FOR TEST
#############################
## Same for Test Data
for_LM_xs_test = subset(testData,
                        select = c('age_factor', 'job','marital','education','default','housing','loan','contact','month',
                                   'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
                                   'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode'))

Z2_testSVM = as.data.frame(cbind(testData$target,for_LM_xs_test))
### Rename Target
names(Z2_testSVM)[1] <- "termdep_Y"


#  Add prediction to the test set
Z2_testSVM <- cbind(Z2_testSVM, fitted = pred_svm)
Z2_testSVM$reccount = 1
View(Z2_testSVM)

#  Add decile to the test set
class(Z2_testSVM$fitted)
Z2_testSVM <- Z2_testSVM[ order(Z2_testSVM$fitted, decreasing=FALSE), ]  # Use built-in R functions

## Make a running count
Z2_testSVM$running = cumsum(Z2_testSVM$reccount)
Z2_testSVM$totN <- max(Z2_testSVM$running)
## Create the decile off the running count
Z2_testSVM$perrank <- Z2_testSVM$running/Z2_testSVM$totN
View(Z2_testSVM)

table(Z2_testSVM$fitted)
541/15934

#  Add decile to the test set
Z2_testSVM$fitted_ct <- as.numeric(Z2_testSVM$fitted)
table(Z2_testSVM$fitted)
table(Z2_testSVM$fitted_ct)
class(Z2_testSVM$fitted_ct)
Z2_testSVM$fitted_ct <- ifelse(Z2_testSVM$fitted_ct == 2 , 1, 0)
table(Z2_testSVM$fitted_ct)
View(Z2_testSVM)

decLocations_SVMtest <- quantile(Z2_testSVM$running, probs = seq(0.1,0.9,by=0.1))
Z2_testSVM$decile <- findInterval(Z2_testSVM$running,c(-Inf,decLocations_SVMtest, Inf))

table(Z2_testSVM$decile)

decile_grp<-group_by(Z2_testSVM,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=sum(reccount), min_prob=min(p=fitted_ct),
                            max_prob=max(fitted_ct),
                            resp_cnt=sum(termdep_Y),
                            non_resp_cnt=total_cnt -resp_cnt,
                            rep_rate=(resp_cnt/total_cnt)*100)
View(decile_summ_test)

decile_summ_test<-arrange(decile_summ_test, desc(decile))
sum1 <- sum(decile_summ_test$resp_cnt)
sum1

sum2 <- sum(decile_summ_test$non_resp_cnt)
sum2

decile_summ_test$resp_pct <- ((decile_summ_test$resp_cnt)/sum1)*100
decile_summ_test$non_resp_pct <- ((decile_summ_test$non_resp_cnt)/sum2)*100
decile_summ_test$cum_resp_pct <- cumsum(decile_summ_test$resp_pct)
decile_summ_test$cum_non_resp_pct <- cumsum(decile_summ_test$non_resp_pct)
decile_summ_test$ks_stats <- abs(decile_summ_test$cum_resp_pct-decile_summ_test$cum_non_resp_pct)
View(decile_summ_test)


###### Gains Chart

df_gain_train <- subset(decile_summ_train,select = c(1,10))

df_gain_train <- rename(df_gain_train,'cum_resp_pct_train'='cum_resp_pct')
df_gain_test <- subset(decile_summ_test,select = c(10))
df_gain_test <- rename(df_gain_test,'cum_resp_pct_test'='cum_resp_pct')
df_gain_chart <- cbind(df_gain_train,df_gain_test)
df_gain_chart <- cbind(df_gain_train,df_gain_test)
df_gain_chart$cum_resp_pct_train <- round(df_gain_chart$cum_resp_pct_train,2)
df_gain_chart$cum_resp_pct_test <- round(df_gain_chart$cum_resp_pct_test,2)

base_pct <- c(10,20,30,40,50,60,70,80,90,100)
df_gain_chart <- cbind(df_gain_chart,base_pct)

#install.packages("plotly")
library(plotly)
plot_ly(df_gain_chart,x=~decile,y=~cum_resp_pct_train,type = 'scatter',name = 'train_pct',mode='lines')%>%
  add_trace(y = ~cum_resp_pct_test, name = 'test_pct', mode = 'lines') %>%
  add_trace(y = ~base_pct, name = 'base_pct', mode = 'lines')%>%
  layout(title = "Gains Chart",
         xaxis = list(title = "Decile",range=c(10,1)),
         yaxis = list (title = "Percenatge"))

###### Lift Chart

df_gain_chart$lift_train <-df_gain_chart$cum_resp_pct_train/df_gain_chart$base_pct
df_gain_chart$lift_test <-df_gain_chart$cum_resp_pct_test/df_gain_chart$base_pct
df_gain_chart$base_line <- c(1,1,1,1,1,1,1,1,1,1)

lift_chart <- subset(df_gain_chart,select = c(1,5,6,7))

plot_ly(lift_chart,x=~decile,y=~lift_train,type = 'scatter',name = 'lift_train',mode='lines')%>%
  add_trace(y = ~lift_test, name = 'lift_test', mode = 'lines') %>%
  add_trace(y = ~base_line, name = 'base_line', mode = 'lines')%>%
  layout(title = "Lift Chart",
         xaxis = list(title = "Decile",range=c(10,1)),
         yaxis = list (title = "Percenatge",range=c(0,5)))


library(rpart)
library(e1071)
#https://stackoverflow.com/questions/34781495/how-to-find-important-factors-in-support-vector-machine
cat('SVM model case:\n')
fit2 <- svmfit
w <- t(fit2$coefs) %*% fit2$SV                 # weight vectors
w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
w <- sort(w, decreasing = T)
print(w)

SVMimport<- as.data.frame(w)

#####################################################################################
#######################################################################################
################################################ END END END##########################
#### OVERSAMPLE MODEL JUST TO SEE ###################################################################
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
# Down Sample
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "y"],
                         y = trainData$y)
table(down_train$y)
# Up Sample (optional)
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "y"],
                     y = trainData$y)
table(up_train$y)


### Misclass cost so this thing works
#https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/
#install.packages("ROSE")
library(ROSE)
OverSamp <- +2773*5+21940
data_balanced_over <- ovun.sample(y ~ ., data = trainData, method = "over",N = (+2773*10+21940))$data
table(trainData$y)
table(testData$y)
table(data_balanced_over$y)


for_LM_xs = subset(data_balanced_over,
                   select = c('age', 'job','marital','education','default','housing','loan','contact','month',
                              'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
                              'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode'))

OverSampModel <- glm(data_balanced_over$target ~ ., family = "binomial", data=for_LM_xs)
summary(OverSampModel)

library(MASS)
OverSamp_stepmod <- stepAIC(OverSampModel, direction = 'backward', trace = TRUE)
summary(OverSamp_stepmod)

pred_OS <- predict(OverSamp_stepmod, newdata = testData, type = "response")

pred_OStrain <- predict(OverSamp_stepmod, newdata = trainData, type = "response")



# Recode factors
y_pred_num <- ifelse(pred_OS > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$target

# Accuracy
###https://rpubs.com/kmanalo/235576
mean(y_pred == y_act)  # 94%
#??confusionMatrix
library(caret)
confusionMatrix(y_pred, y_act)
xtab =table(y_pred, y_act)

print(confusionMatrix(xtab[2:1,2:1]))

for_LM_xs = subset(trainData,
                   select = c('age', 'job','marital','education','default','housing','loan','contact','month',
                              'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
                              'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode'))

### Rename Target
#  Add prediction to the Training set to Look at Performance
Zover = as.data.frame(cbind(trainData$target,for_LM_xs))

Zover <- cbind(Zover, fitted = pred_OStrain)
Zover$reccount = 1
names(Zover)[1] <- "termdep_Y"

#  Add decile to the training set
decLocations <- quantile(Zover$fitted, probs = seq(0.1,0.9,by=0.1))
Zover$decile <- findInterval(Zover$fitted,c(-Inf,decLocations, Inf))
View(Zover)
  ## Same for Test Data
  for_LM_xs_test = subset(testData,
                          select = c('age', 'job','marital','education','default','housing','loan','contact','month',
                                     'camp_recode','pday_recode','previous_recode','poutcome','emp.var.rate_recode',
                                     'cons.conf.idx_recode','cons.price.idx_recode','euribor3m_recode','nr.employed_recode'))

Z_test = as.data.frame(cbind(testData$target,for_LM_xs_test))
### Rename Target
names(Z_test)[1] <- "termdep_Y"


#  Add prediction to the test set
Z_test <- cbind(Z_test, fitted = pred)
Z_test$reccount = 1

#  Add decile to the test set
decLocations <- quantile(Z_test$fitted, probs = seq(0.1,0.9,by=0.1))
Z_test$decile <- findInterval(Z_test$fitted,c(-Inf,decLocations, Inf))


#### Make Deciles Table Training
#https://www.datavedas.com/model-evaluation-in-r/
library(dplyr)
class(Zover$termdep_Y)

decile_grp<-group_by(Zover,decile)
decile_summ_train<-summarize(decile_grp, total_cnt=sum(reccount), min_prob=min(p=fitted),
                             max_prob=max(fitted),
                             resp_cnt=sum(termdep_Y),
                             non_resp_cnt=total_cnt -resp_cnt,
                             rep_rate=(resp_cnt/total_cnt)*100)
View(decile_summ_train)

decile_summ_train<-arrange(decile_summ_train, desc(decile))
sum1 <- sum(decile_summ_train$resp_cnt)
sum1

sum2 <- sum(decile_summ_train$non_resp_cnt)
sum2

decile_summ_train$resp_pct <- ((decile_summ_train$resp_cnt)/sum1)*100
decile_summ_train$non_resp_pct <- ((decile_summ_train$non_resp_cnt)/sum2)*100
decile_summ_train$cum_resp_pct <- cumsum(decile_summ_train$resp_pct)
decile_summ_train$cum_non_resp_pct <- cumsum(decile_summ_train$non_resp_pct)
decile_summ_train$ks_stats <- abs(decile_summ_train$cum_resp_pct-decile_summ_train$cum_non_resp_pct)
View(decile_summ_train)


### DECILES FOR TEST
#############################
decile_grp<-group_by(Z_test,decile)
decile_summ_test<-summarize(decile_grp, total_cnt=sum(reccount), min_prob=min(p=fitted),
                            max_prob=max(fitted),
                            resp_cnt=sum(termdep_Y),
                            non_resp_cnt=total_cnt -resp_cnt,
                            rep_rate=(resp_cnt/total_cnt)*100)
View(decile_summ_test)

decile_summ_test<-arrange(decile_summ_test, desc(decile))
sum1 <- sum(decile_summ_test$resp_cnt)
sum1

sum2 <- sum(decile_summ_test$non_resp_cnt)
sum2

decile_summ_test$resp_pct <- ((decile_summ_test$resp_cnt)/sum1)*100
decile_summ_test$non_resp_pct <- ((decile_summ_test$non_resp_cnt)/sum2)*100
decile_summ_test$cum_resp_pct <- cumsum(decile_summ_test$resp_pct)
decile_summ_test$cum_non_resp_pct <- cumsum(decile_summ_test$non_resp_pct)
decile_summ_test$ks_stats <- abs(decile_summ_test$cum_resp_pct-decile_summ_test$cum_non_resp_pct)
View(decile_summ_test)


###### Gains Chart

df_gain_train <- subset(decile_summ_train,select = c(1,10))

df_gain_train <- rename(df_gain_train,'cum_resp_pct_train'='cum_resp_pct')
df_gain_test <- subset(decile_summ_test,select = c(10))
df_gain_test <- rename(df_gain_test,'cum_resp_pct_test'='cum_resp_pct')
df_gain_chart <- cbind(df_gain_train,df_gain_test)
df_gain_chart <- cbind(df_gain_train,df_gain_test)
df_gain_chart$cum_resp_pct_train <- round(df_gain_chart$cum_resp_pct_train,2)
df_gain_chart$cum_resp_pct_test <- round(df_gain_chart$cum_resp_pct_test,2)

base_pct <- c(10,20,30,40,50,60,70,80,90,100)
df_gain_chart <- cbind(df_gain_chart,base_pct)

#@install.packages("plotly")
library(plotly)
plot_ly(df_gain_chart,x=~decile,y=~cum_resp_pct_train,type = 'scatter',name = 'train_pct',mode='lines')%>%
  add_trace(y = ~cum_resp_pct_test, name = 'test_pct', mode = 'lines') %>%
  add_trace(y = ~base_pct, name = 'base_pct', mode = 'lines')%>%
  layout(title = "Gains Chart",
         xaxis = list(title = "Decile",range=c(10,1)),
         yaxis = list (title = "Percenatge"))

###### Lift Chart

df_gain_chart$lift_train <-df_gain_chart$cum_resp_pct_train/df_gain_chart$base_pct
df_gain_chart$lift_test <-df_gain_chart$cum_resp_pct_test/df_gain_chart$base_pct
df_gain_chart$base_line <- c(1,1,1,1,1,1,1,1,1,1)

lift_chart <- subset(df_gain_chart,select = c(1,5,6,7))

plot_ly(lift_chart,x=~decile,y=~lift_train,type = 'scatter',name = 'lift_train',mode='lines')%>%
  add_trace(y = ~lift_test, name = 'lift_test', mode = 'lines') %>%
  add_trace(y = ~base_line, name = 'base_line', mode = 'lines')%>%
  layout(title = "Lift Chart",
         xaxis = list(title = "Decile",range=c(10,1)),
         yaxis = list (title = "Percenatge",range=c(0,5)))


# Recode factors
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$target

# Accuracy
###https://rpubs.com/kmanalo/235576
mean(y_pred == y_act)  # 94%

confusionMatrix(y_pred, y_act)
xtab =table(y_pred, y_act)

print(confusionMatrix(xtab[2:1,2:1]))


#https://www.geeksforgeeks.org/classifying-data-using-support-vector-machinessvms-in-r/
