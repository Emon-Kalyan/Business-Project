library(psych)
library(tidyr)
library(ggplot2)
library(dplyr)
library(GPArotation)
install.packages("ROCR")
library(ROCR)
online_intention_data <- read.csv("online_shoppers_intention.csv")
View(online_intention_data)
##Informational - Informational - This is the number of pages of this type
##(informational) that the user visited.
##Informational_Duration - This is the amount of time spent in this 
##category of pages.
##ProductRelated - This is the number of pages of this type (product related) that the user visited.
##ProductRelatedDuration - This is the amount of time spent in this category of pages.
##BounceRates - The percentage of visitors who enter the website through that 
##page and exit without triggering any additional tasks.
##ExitRates-The percentage of pageviews on the website that end at that specific page.
##PageValues - The average value of the page averaged over the value of the target page 
##and/or the completion of an eCommerce
##SpecialDay - This value represents the closeness of the browsing date to
##special days or holidays (eg Mother's Day or Valentine's day) in
ncol(online_intention_data)
colnames(online_intention_data)
summary(online_intention_data)
str(online_intention_data)
levels(as.factor(online_intention_data$Revenue))
table(as.factor(online_intention_data$Revenue))
install.packages("gmodels")
library(gmodels)
CrossTable(online_intention_data$Revenue)
CrossTable(online_intention_data$Month, online_intention_data$Revenue, 
           prop.r = TRUE, prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE)
###----Let's start with the Exploratory Data Analysis----###
online_intention_data$Revenue_cat <- ifelse(online_intention_data$Revenue == TRUE, 1,0)
online_intention_data$Revenue_cat <- as.factor(online_intention_data$Revenue_cat)
###---Plotting the histograms of different variables in the data-set,skew---##
hist(online_intention_data$Informational)
hist(online_intention_data$Informational_Duration)
hist(online_intention_data$ProductRelated)
hist(online_intention_data$ProductRelated_Duration)
hist(online_intention_data$BounceRates)
hist(online_intention_data$ExitRates)
hist(online_intention_data$PageValues)
plot(online_intention_data$Informational)
###---Converting categorical variables into factors---###
y <- c('Month', 'OperatingSystems', 'Browser', 'Region', 'TrafficType', 'VisitorType',
       'Weekend')
onlinedata1 <- sapply(online_intention_data[y], as.factor)
x <- c('Administrative', 'Administrative_Duration', 'Informational', 'Informational_Duration', 'ProductRelated',
       'ProductRelated_Duration', 'BounceRates', 'ExitRates', 'PageValues',
       'SpecialDay')

#class(onlinedata1)
onlinedata1 <- data.frame(onlinedata1)
onlinedata2 <- online_intention_data[x]
class(onlinedata2)
summary(onlinedata2)
##First we make a function to understand the continous numeric variables of the data set
num_vars <- sapply(online_intention_data, is.numeric)
cat_vars <- sapply(onlinedata1, is.factor)
numvarsumm <- function(x){n = length(x)
average = mean(x, na.rm = TRUE)
nmiss = sum(is.na(x)|x == "")
skewness = skew(x)
s <- sd(x,na.rm=T)
min <- min(x,na.rm=T)
pctl <- quantile(x, na.rm=T, p=c(0.01,0.05,0.1,0.25,0.5,0.75,0.9, 0.95,0.99))
max <- max(x,na.rm=T)
UC <- average+3*s
LC <- average-3*s
return(c(n=n, nmiss = nmiss , skewness = skewness, mean = average, stdev=s,min = min, pctl=pctl, max=max, UC=UC, LC=LC))
}
catvarssumm <- function(x){
  n = length(x)
  nmiss <- sum(is.na(x)|x=="")
  return(c(n =n, nmiss = nmiss))
}
onlinenumsumm <- t(apply(onlinedata2[x], 2, numvarsumm))
View(onlinenumsumm)
onlinecatsumm <- t(apply(onlinedata1[y],2,catvarssumm))
View(onlinecatsumm)
###---We can see that there is no missing values in any of the variables---###
###---There is some skewness in the numeric variables---###
###---Thus we need to do outlier treatment---###
uppercaptreat <- function(x) { x <- replace(x, which(x > quantile(x, probs = c(0.99), na.rm = TRUE)), quantile(x, probs = c(0.99), na.rm = TRUE)) }
lowercaptreat <- function(x) { x <- replace(x, which(x < quantile(x, probs = c(0.01), na.rm = TRUE)), quantile(x, probs = c(0.01), na.rm = TRUE)) }
onlinedata2 <- apply(onlinedata2[x], 2, FUN = uppercaptreat)
onlinedata2 <- data.frame(onlinedata2)
onlinedata2 <- apply(onlinedata2[x], 2, FUN = lowercaptreat)
onlinedata2 <- data.frame(onlinedata2)
onlinenumsumm <- t(apply(onlinedata2[x], 2, numvarsumm))
View(onlinenumsumm)
hist(onlinedata2$Administrative)
hist(onlinedata2$BounceRates)
plot(onlinedata2$BounceRates)
###---As the data set is treated for outliers and some amount of skewness is reduced
###---we can merge the data-sets into 1
onlinedata3 <- cbind(onlinedata1, onlinedata2)
class(onlinedata3)
str(onlinedata3)
onlinedata3$Revenue_cat <- online_intention_data$Revenue_cat
##The next step is to find the significance of association of the dependent variables with the independent variables.
##For the association check with the continous variables we have to do anova tests and for association check with the categorical variables we have to do chi-square tests.
##So, we create user defined functions which will extract the significantly associated variables.
##First we create an anova function to extract the continous variables significantly associated with dependent variable churn.
onlinedata3$Month <- as.factor(onlinedata3$Month)
onlinedata3$OperatingSystems <- as.factor(onlinedata3$OperatingSystems)
onlinedata3$Browser <- as.factor(onlinedata3$Browser)
onlinedata3$Region <- as.factor(onlinedata3$Region)
onlinedata3$TrafficType <- as.factor(onlinedata3$TrafficType)
onlinedata3$VisitorType <- as.factor(onlinedata3$VisitorType)
onlinedata3$Weekend <- as.factor(onlinedata3$Weekend)
numvars <- sapply(onlinedata3, is.numeric)
numvars
j = vector()
x = which(numvars == TRUE)
x
for(i in 1:length(x) ){ 
  j[i] =  summary(aov(onlinedata3[,x[i]] ~ onlinedata3$Revenue_cat))
  print(j[[i]][[5]][1])
  print(colnames(onlinedata3[x[i]]))}
###---We get that all the numeric variables are significantly associated with
###---the categorical variable of revenue_cat---###
###---In the next step we check the multicollinearity
###---of the significantly associated continuous variable by reducing them into factors.
corrm <- cor(onlinedata3[numvars])
View(corrm)
write.csv(corrm, "corr_data1.csv")
require(GPArotation)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))
View(eigen_values)
###---From the eigen values we get that 5 factors explain about 82% of the, we can do the
###---Factor analysis by choosing 5 factors
scree(corrm, factors=T, pc=T, main="scree plot", 
      hline=NULL, add=FALSE) ###Warnings are thrown
eigen(corrm)$value
FA<-fa(r=corrm, 6, rotate="varimax", fm="ml")
print(FA)
FA_sort <- fa.sort(FA)
FAload <-  data.frame(FA_sort$loadings[1:ncol(onlinedata3[numvars]),])
FAload
write.csv(FAload, "FactorLoadings.csv")
###---The variables obtained significant from the factor loadings are
###---ExitRates, ProductRelated, Informational, Administrative
a <- c('ExitRates', 'ProductRelated', 'Informational', 'Administrative')
corrm1 <- cor(onlinedata3[a])
View(corrm1)
##Next we create a user defined function to extract the categorical variables which are significantly associated with the dependent variable.
chisq.test( onlinedata3$OperatingSystems, onlinedata3$Revenue_cat, simulate.p.value = TRUE)
chisq.test( onlinedata3$Browser, onlinedata3$Revenue_cat, simulate.p.value = TRUE)
chisq.test( onlinedata3$Region, onlinedata3$Revenue_cat, simulate.p.value = TRUE)
##----Region is not significantly associated
chisq.test( onlinedata3$Month, onlinedata3$Revenue_cat, simulate.p.value = TRUE)
chisq.test( onlinedata3$TrafficType, onlinedata3$Revenue_cat, simulate.p.value = TRUE)
chisq.test( onlinedata3$VisitorType, onlinedata3$Revenue_cat, simulate.p.value = TRUE)
###---The categorical variables which are significantly associated are---
###---OperatingSystems, Browser, Month, TrafficType, VisitorType
set.seed(123)
trainind <- sample(1:nrow(onlinedata3),size = floor(0.70*nrow(onlinedata3)))
trainlog <- onlinedata3[trainind,]
testlog <- onlinedata3[-trainind,]
View(trainlog)




fit1 <- glm(Revenue_cat~OperatingSystems+Browser+Month+VisitorType+TrafficType+
              ExitRates+ProductRelated+Informational+Administrative, data = trainlog, family=binomial(logit))
summary(fit1)
levels(trainlog$TrafficType)
step1 <- step(fit1)
fit2 <- glm(Revenue_cat ~ OperatingSystems + Month + VisitorType + TrafficType + 
              ExitRates + ProductRelated + Informational + Administrative, data = trainlog, family = binomial(logit))
df <- summary(fit2)$coefficients
levels(as.factor(onlinedata3$Month))
##Next we try to validate the fit3 model on trainlog
trainlog <- cbind(trainlog, prob = predict(fit2, type = "response"))
write.csv(df, "summary.csv")
##Doing decile analysis for trainlog validation
delocations <- quantile(trainlog$prob, probs = seq(0.1,0.9, by = 0.1))
trainlog$decile <- findInterval(trainlog$prob, c(-Inf, delocations, Inf))
trainlog$decile = as.factor(trainlog$decile)
View(trainlog)
#trainlog <- trainlog[,-20]
trainlog1 <- group_by(trainlog, decile ) %>% summarise(total_count = n(), revenue_count = sum(Revenue_cat ==1), maxprob = max(prob), minprob = min(prob))
View(trainlog1)
##Next we  draw the ROCR curve to check the accuracy of the model.
require(ROCR)
pred1 <- prediction(trainlog$prob, trainlog$Revenue_cat )
perf_fit2 <- performance(pred1, "tpr", "fpr")
plot(perf_fit2)
abline(0,1)
#trainlog <- trainlog[,-21]
trainlog$predrevenue_cat <- ifelse(trainlog$prob > 0.20, 1, 0)
actual <- data.frame(trainlog$Revenue_cat)
predicted <- data.frame(trainlog$predrevenue_cat)
traintab <- cbind(actual, predicted)
View(traintab)
traintab$chk <- ifelse((traintab$trainlog.Revenue_cat == 1 & traintab$trainlog.predrevenue_cat == 1), "TP", ifelse((traintab$trainlog.Revenue_cat == 1
                                                                                                    & traintab$trainlog.predrevenue_cat==0), "FN", 
                                                                                                   ifelse((traintab$trainlog.Revenue_cat == 0 
                                                                                                           & traintab$trainlog.predrevenue_cat == 1), "FP", "TN" )) ) 
sum(traintab$chk == "TP")
sum(traintab$chk == "TN")
sum(traintab$chk == "FP")
sum(traintab$chk == "FN")                                                                                                
sum(traintab$chk == "TP")/(sum(traintab$chk == "TP")+sum(traintab$chk == "FN") )
sum(traintab$chk == "TN")/(sum(traintab$chk == "TN")+sum(traintab$chk == "FP"))
(sum(traintab$chk == "TP")+sum(traintab$chk == "TN"))/(sum(traintab$chk == "TP")+
                                                         sum(traintab$chk == "TN")+sum(traintab$chk == "FP")+sum(traintab$chk == "FN"))
###---Testing the model on testlog---
testlog <- cbind(testlog, prob = predict(fit2, type = "response", newdata = testlog))
View(testlog)
##Doing decile analysis for testlog
declocationstest <- quantile(testlog$prob, probs = seq(0.1,0.9, by = 0.1))
testlog$decile <- findInterval(testlog$prob, c(-Inf, declocationstest, Inf))
testlog$decile = as.factor(testlog$decile)
testlog1 <- group_by(testlog, decile) %>% summarise(total_count = n(), Revenue_count = sum(Revenue_cat == 1), max_prob = max(prob),
                                                    min_prob = min(prob))
View(testlog1)
pred2 <- prediction(testlog$prob, testlog$Revenue_cat )
perf_fit3 <- performance(pred1, "tpr", "fpr")
plot(perf_fit3)
abline(0,1)
testlog$predrevenue_cat <- ifelse(testlog$prob > 0.20, 1, 0)
actual1 <- data.frame(testlog$Revenue_cat)
predicted1 <- data.frame(testlog$predrevenue_cat)
testtab1 <- cbind(actual1, predicted1)
View(testtab1)
testtab1$chk <- ifelse((testtab1$testlog.Revenue_cat == 1 & testtab1$testlog.predrevenue_cat == 1), "TP", ifelse((testtab1$testlog.Revenue_cat == 1
                       & testtab1$testlog.predrevenue_cat==0), "FN", 
                                    ifelse((testtab1$testlog.Revenue_cat == 0 & testtab1$testlog.predrevenue_cat == 1), "FP", "TN" )) )
sum(testtab1$chk == "TP")
sum(testtab1$chk == "TN")
sum(testtab1$chk == "FP")
sum(testtab1$chk == "FN")  
Sensitivity <- sum(testtab1$chk == "TP")/(sum(testtab1$chk == "TP")+sum(testtab1$chk == "FN"))
Sensitivity
Specificity <- sum(testtab1$chk == "TN")/(sum(testtab1$chk == "TN")+sum(testtab1$chk == "FP"))
Specificity
(sum(testtab1$chk == "TP")+sum(testtab1$chk == "TN"))/(sum(testtab1$chk == "TP")+sum(testtab1$chk == "TN")+sum(testtab1$chk == "FP")+sum(testtab1$chk == "FN"))
#install.packages("pROC")
library(pROC)
auc(testlog$Revenue_cat, testlog$predrevenue_cat)
traintab$cordance <- ifelse((trainlog$Revenue_cat == 1 & trainlog$prob > 0.20), "concordance", ifelse((trainlog$Revenue_cat == 1 & trainlog$prob == 0.20), "Ties", ifelse((trainlog$Revenue_cat == 0 & trainlog$prob < 0.20), "concordance", "ties")))
sum(traintab$cordance == "concordance")
pct_concordance <- (sum(traintab$cordance == "concordance"))/length(traintab$cordance)
pct_ties <- (sum(traintab$cordance == "Ties"))/length(traintab$cordance)
pct_concordance
pct_ties
testtab1$cordance <- ifelse((testlog$Revenue_cat == 1 & testlog$prob > 0.20), "concordance", ifelse((testlog$Revenue_cat == 1 & testlog$prob == 0.20), "Ties", ifelse((testlog$Revenue_cat == 0 & testlog$prob < 0.20), "concordance", "ties")))
sum(testtab1$cordance == "concordance")
pct_concordance <- (sum(testtab1$cordance == "concordance"))/length(testtab1$cordance)
pct_ties <- (sum(testtab1$cordance == "Ties"))/length(testtab1$cordance)
pct_concordance
pct_ties
###---We are choosing 0.20 as the cut off since it is giving us decent level
###---of sensitivity, specificity and accuracy.


















































































































































































































































