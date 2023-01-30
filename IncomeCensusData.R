update.packages("rlang")
library(ggplot2)
library(plyr)
library(gridExtra)
library(gmodels)
library(grid)
library(vcd)
library(scales)
library(ggthemes)
library(EDAWR)
library(tidyr)
#preprocessing train data
setwd("/Users/alurubindu/Documents/Income_data ")
ad_data<-read.csv(file = "Adult_data.csv",header = T,sep = ",")
head(ad_data)
tail(ad_data)
nrow(ad_data)  
names(ad_data)
str(ad_data)
sapply(ad_data,function(x) sum(is.na(x)))
table(ad_data$workclass)
table(ad_data$occupation)
table(ad_data$native.country)

ad_data[ad_data  == " ?"] <- NA
sapply(ad_data,function(x) sum(is.na(x)))


str(ad_data)
summary(ad_data)




# now dealing with Outliers
par(mar = c(1,1,1,1))
boxplot(ad_data$age, main = "Age")
boxplot(ad_data$fnlwgt, main = "fnlwgt")
boxplot(ad_data$education.num,main = "education.num")
boxplot(ad_data$hours.per.week,main = "hours.per.week")


levels_factors <- function(ad_data) {
  col_names <- names(ad_data)
  for (i in 1:length(col_names)) {
    if (is.factor(ad_data[, col_names[i]])) {
      message(noquote(paste("Covariate ", "*", 
                            col_names[i], "*", 
                            " with factor levels:", 
                            sep = "")))
      print(levels(ad_data[, col_names[i]]))
    }
  }
}



#transformation of hours.per.week
ggplot(aes(x = factor(0), y = hours.per.week),
       data = ad_data) + 
  geom_boxplot() +
  stat_summary(fun = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  xlab(label = "") +
  ylab(label = "Working hours per week") +
  ggtitle("Box Plot of Working Hours per Week")



ad_data<-na.omit(ad_data)
row.names(ad_data)<-1:nrow(ad_data)
sapply(ad_data,function(x) sum(is.na(x)))

summary(ad_data$hours.per.week)

ad_data$hours_w[ad_data$hours.per.week < 40] <- "less_than_40"
ad_data$hours_w[ad_data$hours.per.week >= 40 & 
                     ad_data$hours.per.week <= 45] <- "between_40_and_45"
ad_data$hours_w[ad_data$hours.per.week > 45 &
                     ad_data$hours.per.week <= 60  ] <- "between_45_and_60"
ad_data$hours_w[ad_data$hours.per.week > 60 &
                     ad_data$hours.per.week <= 80  ] <- "between_60_and_80"
ad_data$hours_w[ad_data$hours.per.week > 80] <- "more_than_80"

ad_data$hours_w <- factor(ad_data$hours_w,
                           ordered = FALSE,
                           levels = c("less_than_40", 
                                      "between_40_and_45", 
                                      "between_45_and_60",
                                      "between_60_and_80",
                                      "more_than_80"))
summary(ad_data$hours_w)
for(i in 1:length(summary(ad_data$hours_w))){
  
  print(round(100*summary(ad_data$hours_w)[i]/sum(!is.na(ad_data$hours_w)), 2)) 
}

#transformation of native country
levels(ad_data$native.country)

Asia_East <- c("Cambodia", "China", "Hong", "Laos", "Thailand",
               "Japan", "Taiwan", "Vietnam")

Asia_Central <- c("India", "Iran")

Central_America <- c("Cuba", "Guatemala", "Jamaica", "Nicaragua", 
                     "Puerto-Rico",  "Dominican-Republic", "El-Salvador", 
                     "Haiti", "Honduras", "Mexico", "Trinadad&Tobago")

South_America <- c("Ecuador", "Peru", "Columbia")


Europe_West <- c("England", "Germany", "Holand-Netherlands", "Ireland", 
                 "France", "Greece", "Italy", "Portugal", "Scotland")

Europe_East <- c("Poland", "Yugoslavia", "Hungary")

#Then we modify the data frame by adding an additional column named "native_region". 
#We do this with the help of the "mutate" function form the "plyr" package
ad_data <- mutate(ad_data, 
                   native_region = ifelse(native.country %in% Asia_East, "East-Asia",
                                   ifelse(native.country%in% Asia_Central, "Central-Asia",
                                   ifelse(native.country%in% Central_America, "Central-America",
                                   ifelse(native.country %in% South_America, "South-America",
                                   ifelse(native.country %in% Europe_West, "Europe-West",
                                   ifelse(native.country%in% Europe_East, "Europe-East",
                                   ifelse(native.country == "United-States", "United-States","Outlying-US" ))))))))
ad_data$native_region <- factor(ad_data$native_region, ordered = FALSE)

#transformation of capital_gain and capitalloss
summary(ad_data$capital.gain)
summary(ad_data$capital.loss)
(nrow(subset(ad_data, ad_data$capital.gain == 0))/nrow(ad_data))*100
#More precisely, the percentage of zeros in "capital_gain" is very big - 91.59%:

(nrow(subset(ad_data, ad_data$capital.loss == 0))/nrow(ad_data))*100
#the percentage of zeros in "capital_loss" is also very high - 95.27%:

mean.gain <- mean(ad_data$capital.gain)

mean.loss <- mean(ad_data$capital.loss)
library(knitr)
kable(data.frame(Mean_Capital_Gain = mean.gain, Mean_Capital_Loss = mean.loss),
      caption = "Mean Capital with Zero Values Included")
#the mean values of "capital_gain" and "capital_loss" with the zero values included

#We give the mean capital gain and loss also in the case of all the zero values removed
mean.gain <- mean(subset(ad_data$capital.gain, ad_data$capital.gain > 0))

mean.loss <- mean(subset(ad_data$capital.loss, ad_data$capital.loss > 0))

kable(data.frame(Mean_Capital_Gain = mean.gain, Mean_Capital_Loss = mean.loss),
      caption = "Mean Capital Only for Nonzero Values")

#
iqr.gain <- IQR(subset(ad_data$capital.gain, ad_data$capital.gain > 0))
iqr.loss <- IQR(subset(ad_data$capital.loss, ad_data$capital.loss > 0))



q.gain <- quantile(x = subset(ad_data$capital.gain, ad_data$capital.gain > 0), 
                   probs = seq(0, 1, 0.25))

q.loss <- quantile(x = subset(ad_data$capital.loss, ad_data$capital.loss > 0),
                   probs = seq(0, 1, 0.25))


kable(x = data.frame(Capital_Gain = q.gain, Capital_Loss = q.loss),
      caption = "Quantiles of the Nonzero Capital")
#IQR of the Nonzero Capital
kable(x = data.frame(IQR_Capital_Gain = iqr.gain, IQR_Capital_Loss = iqr.loss),
      caption = "IQR of the Nonzero Capital")

#box plot 
ggplot(aes(x = factor(0), y = capital.gain),
       data = subset(ad_data, ad_data$capital.gain > 0)) + 
  geom_boxplot() +
  stat_summary(fun = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100000, 5000)) +
  ylab("Capital Gain") +
  xlab("") +  
  ggtitle("Box plot of Nonzero Capital Gain") 
#From the box plot we see that, indeed, the bulk of the data is between 3,000 and 15,000 dollars, and there are a few outliers. 
#Next we show a histogram of the nonzero capital gain
df <- ad_data[ad_data$capital.gain > 0, ]

ggplot(data = df, 
       aes(x = df$capital.gain)) +
  geom_histogram(binwidth = 5000,
                 colour = "black",
                 fill = "lightblue",
                 alpha = 0.4) +
  scale_y_continuous(breaks = seq(0, 4000, 100)) +
  labs(x = "Capital gain", y = "Count") +
  ggtitle("Histogram of Nonzero Capital Gain")
#The histogram confirms once more what we have already established
#that the majority of people with positive capital gain have a capital gain between 0 and 25,000 dollars
# there are also about 150 people with capital gain of around 100,000 dollars.
#We also note that the biggest number of people with positive capital gain are those with about 5,000 dollars.

#boxplot of non zero capital loss
ggplot(aes(x = factor(0), y = capital.loss),
       data = subset(ad_data, ad_data$capital.loss > 0)) + 
  geom_boxplot() +
  stat_summary(fun = mean, 
               geom = 'point', 
               shape = 19,
               color = "red",
               cex = 2) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  ylab("Capital Loss") +
  xlab("") +  
  ggtitle("Box plot of Nonzero Capital Loss")
#From the box plot we observe that most values are between 1,700 and 2,000 dollars
#creating a nw column cap gain to mention low, mwdium and highfor capital gain
ad_data <- mutate(ad_data, 
                  cap_gain = ifelse(ad_data$capital.gain < 3464, "Low",
                                    ifelse(ad_data$capital.gain >= 3464 & 
                                             ad_data$capital.gain <= 14080, "Medium", "High")))
ad_data$cap_gain <- factor(ad_data$cap_gain,
                           ordered = TRUE,
                           levels = c("Low", "Medium", "High"))


#creating a new column cap loss to mention low, medium, high for capital loss
ad_data <- mutate(ad_data, 
                  cap_loss = ifelse(ad_data$capital.loss < 1672, "Low",
                                    ifelse(ad_data$capital.loss >= 1672 & 
                                             ad_data$capital.loss <= 1977, "Medium", "High")))


ad_data$cap_loss <- factor(ad_data$cap_loss,
                           ordered = TRUE,
                           levels = c("Low", "Medium", "High"))


#transformation of work class

summary(ad_data$workclass)
ad_data$workclass<- as.factor(ad_data$workclass)

ad_data$workclass<-droplevels(ad_data$workclass)
levels(ad_data$workclass)


#
#preprocessing test data

ad_data_test<-read.csv("adult.test",sep = ",",header = FALSE,na.strings = " ?")
colnames(ad_data_test) <- c("age", "workclass", "fnlwgt", "education",
                            "education.num", "marital.status", "occupation",
                            "relationship", "race", "sex", "capital.gain",
                            "capital.loss", "hours.per.week",
                            "native.country", "income")

sum(is.na(ad_data_test))
ad_data_test[ad_data_test==" ?"]<-NA
sapply(ad_data_test,function(x) sum(is.na(x)))
ad_data_test<-na.omit(ad_data_test)
row.names(ad_data_test) <- 1:nrow(ad_data_test)
sapply(ad_data_test,function(x) sum(is.na(x)))
head(ad_data_test)
str(ad_data_test)
ad_data_test$income<-as.factor(ad_data_test$income)
ad_data_test$income<- droplevels(ad_data_test$income)
levels(ad_data_test$income)[1] <- "<=50K"
levels(ad_data_test$income)[2] <- ">50K"

levels(ad_data_test$income)
#creating hours_w
ad_data_test$hours_w<- as.factor(ad_data_test$hours_w)
ad_data_test$hours_w[ad_data_test$hours.per.week < 40] <- "less_than_40"
ad_data_test$hours_w[ad_data_test$hours.per.week >= 40 & 
                       ad_data_test$hours.per.week <= 45] <- "between_40_and_45"
ad_data_test$hours_w[ad_data_test$hours.per.week > 45 &
                       ad_data_test$hours.per.week <= 60  ] <- "between_45_and_60"
ad_data_test$hours_w[ad_data_test$hours.per.week > 60 &
                       ad_data_test$hours.per.week <= 80  ] <- "between_60_and_80"
ad_data_test$hours_w[ad_data_test$hours.per.week > 80] <- "more_than_80"


ad_data_test$hours_w <- factor(ad_data_test$hours_w,
                          ordered = FALSE,
                          levels = c("less_than_40", 
                                     "between_40_and_45", 
                                     "between_45_and_60",
                                     "between_60_and_80",
                                     "more_than_80"))

#creating 
Asia_East <- c("Cambodia", "China", "Hong", "Laos", "Thailand",
               "Japan", "Taiwan", "Vietnam")

Asia_Central <- c("India", "Iran")

Central_America <- c("Cuba", "Guatemala", "Jamaica", "Nicaragua", 
                     "Puerto-Rico",  "Dominican-Republic", "El-Salvador", 
                     "Haiti", "Honduras", "Mexico", "Trinadad&Tobago")

South_America <- c("Ecuador", "Peru", "Columbia")


Europe_West <- c("England", "Germany", "Holand-Netherlands", "Ireland", 
                 "France", "Greece", "Italy", "Portugal", "Scotland")

Europe_East <- c("Poland", "Yugoslavia", "Hungary")
ad_data_test$native_region<- as.factor(ad_data_test$native_region)
ad_data_test <- mutate(ad_data_test, 
                     native_region = ifelse(native.country %in% Asia_East, "East-Asia",
                                     ifelse(native.country%in% Asia_Central, "Central-Asia",
                                     ifelse(native.country%in% Central_America, "Central-America",
                                     ifelse(native.country %in% South_America, "South-America",
                                     ifelse(native.country %in% Europe_West, "Europe-West",
                                     ifelse(native.country%in% Europe_East, "Europe-East",
                                     ifelse(native.country == "United-States", "United-States","Outlying-US" ))))))))
ad_data_test$native_region<-as.factor(ad_data_test$native_region)
ad_data_test$native_region<- droplevels(ad_data_test$native_region)
levels(ad_data_test$native_region)<-c("United-States","Central-America","Europe-West","Outlying-US","East-Asia",      
                                        "Central-Asia","South-America","Europe-East")
levels(ad_data_test$native_region) 
##creating capital loss and capital gain columns
ad_data_test <- mutate(ad_data_test, 
                       cap_gain = ifelse(ad_data_test$capital.gain < 3464, "Low",
                                         ifelse(ad_data_test$capital.gain >= 3464 & 
                                                  ad_data_test$capital.gain <= 14080, "Medium", "High")))

ad_data_test$cap_gain <- factor(ad_data_test$cap_gain,
                                ordered = FALSE,
                                levels = c("Low", "Medium", "High"))
##capital loss
ad_data_test<- mutate(ad_data_test, 
                      cap_loss = ifelse(ad_data_test$capital.loss < 1672, "Low",
                                        ifelse(ad_data_test$capital.loss >= 1672 & 
                                                 ad_data_test$capital.loss <= 1977, "Medium", "High")))


ad_data_test$cap_loss <- factor(ad_data_test$cap_loss,
                                ordered = FALSE,
                                levels = c("Low", "Medium", "High"))
ad_data_test$workclass<- as.factor(ad_data_test$workclass)
ad_data_test$workclass<-droplevels(ad_data_test$workclass)
levels(ad_data_test$workclass)
levels(ad_data_test$workclass)<-c("Federal-gov","Local-gov","Private","Self-emp-inc","Self-emp-not-inc","State-gov","Without-pay")
levels(ad_data_test$workclass)

ad_data_test$marital.status<-as.factor(ad_data_test$marital.status)
ad_data_test$marital.status<- droplevels(ad_data_test$marital.status)
levels(ad_data_test$marital.status)

levels(ad_data_test$marital.status)<-c("Divorced","Married-AF-spouse","Married-civ-spouse","Married-spouse-absent",
                                       "Never-married","Separated","Widowed")

ad_data_test$occupation<-as.factor(ad_data_test$occupation)
ad_data_test$occupation<-droplevels(ad_data_test$occupation)
levels(ad_data_test$occupation)
levels(ad_data_test$occupation)<-c("Prof-specialty","Craft-repair","Exec-managerial","Adm-clerical",     
                                   "Sales","Other-service","Machine-op-inspct","Transport-moving", 
                                   "Handlers-cleaners","Farming-fishing","Tech-support","Protective-serv",  
                                   "Priv-house-serv","Armed-Forces")

ad_data_test$relationship<-as.factor(ad_data_test$relationship)
ad_data_test$relationship<- droplevels(ad_data_test$relationship)
levels(ad_data_test$relationship)
levels(ad_data_test$relationship)<-c("Husband","Not-in-family","Own-child","Unmarried","Wife",          
                                     "Other-relative")
ad_data_test$race<-as.factor(ad_data_test$race)
ad_data_test$race<-droplevels(ad_data_test$race)
levels(ad_data_test$race)
levels(ad_data_test$race)<-c("Amer-Indian-Eskimo","Asian-Pac-Islander","Black","Other","White")

ad_data_test$sex<-as.factor(ad_data_test$sex)
ad_data_test$sex<- droplevels(ad_data_test$sex)
levels(ad_data_test$sex)
levels(ad_data_test$sex)<-c("Female","Male")

ad_data_test$native.country<- as.factor(ad_data_test$native.country)
ad_data_test$native.country<- droplevels(ad_data_test$native.country)
levels(ad_data_test$native.country)
levels(ad_data_test$native.country)<-c("Cambodia","Canada","China","Columbia","Cuba","Dominican-Republic",
                                       "Ecuador","El-Salvador","England","France","Germany","Greece","Guatemala",
                                       "Haiti","Holand-Netherlands","Honduras","Hong","Hungary","India","Iran",
                                       "Ireland","Italy","Jamaica","Japan","Laos","Mexico","Nicaragua","Outlying-US(Guam-USVI-etc)",
                                       "Peru","Philippines","Poland","Portugal","Puerto-Rico","Scotland","South",
                                       "Taiwan","Thailand","Trinadad&Tobago","United-States","Vietnam","Yugoslavia")


levels(ad_data_test$cap_gain)
levels(ad_data_test$cap_gain)<-c("Low","Medium","High")
levels(ad_data_test$cap_loss)<-c("Low","Medium","High")
str(ad_data)
str(ad_data_test)
ad_data_test$education<-as.factor(ad_data_test$education)
levels(ad_data_test$education)
levels(ad_data_test$education)<-c("10th","11th","12th","1st-4th","5th-6th",
                                  "7th-8th","9th","Assoc-acdm","Assoc-voc","Bachelors",
                                  "Doctorate","HS-grad","Masters","Preschool","Prof-school","Some-college")
levels(ad_data_test$education)
str(ad_data_test)
write.csv(ad_data, "ad_train_df.csv", row.names = FALSE)

write.csv(ad_data_test, "ad_test_df.csv", row.names = FALSE)



######################################################################################################################################################



########################################################################################################################

### Independence test
##Test for indepedence variables
# Two-way contingency table with Pearson's chi-square 
# test of independence for the variables "sex" and "income":
ad_train_df<- read.csv("ad_train_df.csv", sep =",")
CrossTable(ad_train_df$sex, ad_train_df$income, 
           prop.chisq = TRUE,
           chisq = TRUE)
# Pearson's chi-square test of independence for the variables
# "sex" and "income"

##The p-value is less than 0.05, which means that at the 0.05 significance level we fail to accept the null hypothesis 
##that the two categorical variables are independent.

##independence test for "race" and "income"


CrossTable(ad_train_df$race, ad_train_df$income, 
           prop.chisq = TRUE,
           chisq = TRUE)
#We reject the null hypothesis at the 0.05 significance level, because the p-values is less than 0.05.
##This means there is strong indication that "race" and "income" are correlated.

##independence test for "workclass" and "income"
##After we removed the cells with zero expected counts, we perform again the Pearson's chi-square test
CrossTable(ad_train_df$workclass, ad_train_df$income, 
           prop.chisq = TRUE,
           chisq = TRUE)
##The p-value is very small, which means that we reject the null hypothesis at the 0.05 significance level.
###########################################################################################################################################



#############################model building##############################

library(ggplot2)
library(scales)
library(plyr)
library(vcd)
library(ggthemes)
library(caret)
library(GoodmanKruskal)
library(ResourceSelection)
library(randomForest)
library(e1071)
library(nnet)
set.seed(1234)
ad_train<-read.csv("ad_train_df.csv")
ad_test<-read.csv("ad_test_df.csv")
ad_train$workclass<- as.factor(ad_train$workclass)
ad_train$education<- as.factor(ad_train$education)
ad_train$marital.status<- as.factor(ad_train$marital.status)
ad_train$occupation<- as.factor(ad_train$occupation)
ad_train$relationship<- as.factor(ad_train$relationship)
ad_train$race<- as.factor(ad_train$race)
ad_train$sex<- as.factor(ad_train$sex)
ad_train$native.country<- as.factor(ad_train$native.country)
ad_train$income<- as.factor(ad_train$income)
ad_train$hours_w<- as.factor(ad_train$hours_w)
ad_train$native_region<- as.factor(ad_train$native_region)
ad_train$cap_gain<- as.factor(ad_train$cap_gain)
ad_train$cap_loss<- as.factor(ad_train$cap_loss)
str(ad_train)
ad_test$workclass<- as.factor(ad_test$workclass)
ad_test$education<- as.factor(ad_test$education)
ad_test$marital.status<- as.factor(ad_test$marital.status)
ad_test$occupation<- as.factor(ad_test$occupation)
ad_test$relationship<- as.factor(ad_test$relationship)
ad_test$race<- as.factor(ad_test$race)
ad_test$sex<- as.factor(ad_test$sex)
ad_test$native.country<- as.factor(ad_test$native.country)
ad_test$income<- as.factor(ad_test$income)
ad_test$hours_w<- as.factor(ad_test$hours_w)
ad_test$native_region<- as.factor(ad_test$native_region)
ad_test$cap_gain<- as.factor(ad_test$cap_gain)
ad_test$cap_loss<- as.factor(ad_test$cap_loss)
str(ad_test)

names(ad_train)
levels(ad_train$education)
covariates<-paste("age","workclass","fnlwgt","education",
                  "marital.status","occupation","relationship","race","sex","capital.gain",
                  "capital.loss","hours.per.week","native.country","income","hours_w","cap_gain","cap_loss",sep = "+" )


form <- as.formula(paste("income ~", covariates))

start_time <- proc.time()
glm.model <- glm(formula = form,
                 data = ad_train, 
                 family = binomial(link ="logit"),
                 x = TRUE)

# The option "x=TRUE" returns the design matrix
time.logistic <- proc.time() - start_time
time.logistic
summary(glm.model)
 summary(glm.model)$coefficients[, 1:2]

library(GoodmanKruskal)
GKmatrix <- GKtauDataframe(ad_train[, c("age", "workclass", 
                                        "education", "education.num",
                                        "marital.status", 
                                        "occupation", "relationship",
                                        "race", "sex", "hours_w",
                                        "native_region", "cap_gain",
                                        "cap_loss")])

plot(GKmatrix)
##from the graph usiing tau value we can say say that education and education.num is collinear
##All other ?? values are close to zero, except for T(relationship, marital_status), T(relationship, sex), and T(marital_status, relationship)
# The T value of 0.42 suggests that being a female or male can determine the type of relationship that an individial is in.

tab <- xtabs(~ sex + relationship, data = ad_train)

ptab <- prop.table(tab, 1)

print(format(round(ptab, 2), scientific = FALSE))
library("vcd")
assocstats(tab)$cramer

tab1 <- xtabs(~ marital.status + relationship, data = ad_train)

assocstats(tab1)$cramer

#generalised variance inflation factor
library(car)
vif(glm.model)
##all GVIF(1/(2???Df)) are less than the treshold value of 5-???=2.23601
##there is no collinearity among the variables


###goodness of fit

summary(glm.model)$null.deviance

summary(glm.model)$deviance




##knowing to approve or reject the null hypothesis
k <- length(glm.model$coefficients)
D_M <- glm.model$deviance
D_0 <- glm.model$null.deviance

1 - pchisq(q = D_0 - D_M, df = k - 1)
#The p-value is approximately 0 and, hence, smaller than 0.05. 
#This means that we reject the null hypothesis that there is no significant difference between the grand mean model (intercept-only model) and the model "lm.model.wld 



head(glm.model$fitted.values)


predicted.probs <- predict(glm.model, type = "response")

# predicted.probs <- glm.model$fitted.values

head(predicted.probs) # returns probabilities

#
observed.values <- ifelse(ad_train$income== ">50K",1,0)

predicted.probs <- predict(glm.model, type = "response")

predicted.response <- ifelse(predicted.probs > 0.5, 1, 0)


head(predicted.response, 20)

head(observed.values,20)

mean(observed.values == predicted.response)
##there is a 85.1% match between observed and predicted values of the dependent variable.

##Overall significance of the categorical covariates in the model

An<-anova(glm.model, test = "LRT")

## Significance of the estimated model parameters
length(glm.model$coefficients)

summary(glm.model)

##
levels(ad_train$education)
summary(ad_train$education)
summary(ad_train$workclass)
confint.default(glm.model)
summary(ad_train$education)

##performance of fitted line
attributes(ad_train$income)
##prediction of the train
predicted.income.train <- ifelse(predicted.probs >0.5, ">50K", "<=50K")
mean(predicted.income.train == ad_train$income)
predicted.income.train<-as.factor(predicted.income.train)

tab_train<- table(predicted.income.train, ad_train$income)
tab_train
##for test data
names(ad_test)
levels(ad_test$education)
levels(ad_test$marital.status)
predicted.income.test <- predict(glm.model, 
                                 newdata = ad_test, 
                                 type ="response") 

predicted.income.test <- ifelse(predicted.income.test >0.5, ">50K", "<=50K")
mean(predicted.income.test == ad_test$income)

predicted.income.test<-as.factor(predicted.income.test)

tb<- table(predicted.income.test,ad_test$income)
tb
##random forest

set.seed(1234)

covariates <- paste("age", "workclass", "education", 
                    "marital.status", "occupation", "relationship",
                    "race", "sex", "native_region", "hours_w",
                    "cap_gain", "cap_loss", sep = "+")

form <- as.formula(paste("income ~", covariates))

start_time <- proc.time()
ModRF <- randomForest(formula = form,
                      data = ad_train)
time.rf.over <- proc.time() - start_time
time.rf.over
ModRF
mean(predict(ModRF, newdata = ad_train) == ad_train$income)
mean(predict(ModRF, newdata = ad_data_test) == ad_data_test$income)
plot(ModRF)


print(ModRF)

###
set.seed(1234)

start_time <- proc.time()
ModRF.small <- randomForest(formula = form, 
                            data = ad_train, 
                            ntree = 200)
time.rf.small.over <- proc.time() - start_time
time.rf.small.over
print(ModRF.small)
mean(predict(ModRF.small, newdata = ad_train) == ad_train$income)

mean(predict(ModRF.small, newdata = ad_data_test) == ad_data_test$income)


  