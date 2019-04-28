#install.packages('funModeling')
#install.packages('GGally')
library(funModeling)
#library(GGally)
library(ggplot2)
#install.packages('corrplot')
library(corrplot)
library(scales)
#install.packages("psych")
library(psych)
#install.packages("GPArotation")
library(GPArotation)
#install.packages('randomForest')
#install.packages('rfUtilities')
library(rfUtilities)
library(randomForest)
#install.packages("gbm")
library(gbm)
#install.packages("pROC")
library(pROC)
#install.packages("xgboost")
#library(xgboost)




#import data
print("Import Data")
data <- read.csv("D:/NYU/Curriculum/CapstoneProject/UCI_Credit_Card.csv",header=TRUE)
data_default <- data
#do not show expenentials
options(scipen=999)

head(data_default)
sum(is.na(data_default))

dim(data_default)

#summary(data_default)

status <- df_status(data_default)

# Converting target column to factor
data_default$default.payment.next.month <- 
  as.factor(data_default$default.payment.next.month)


# For further analysis, we need features names to demonstrate themselves
# So we need to convert some feature from numeric to factor to 
# make them more descriptive

# Convert Sex data 1,2 to Male and Female
data_default$SEX <- as.factor(data_default$SEX)
levels(data_default$SEX) <- c("Male","Female")

# Convert Education level data
data_default$EDUCATION <- as.factor(data_default$EDUCATION)
levels(data_default$EDUCATION) <- c("Others",
                                    "Graduate School",
                                    "Unversity",
                                    "High School",
                                    "Others",
                                    "Others",
                                    "Others")
#ai:can we make others and unknown into one level? 

# Convert Marriage level data
data_default$MARRIAGE <- as.factor(data_default$MARRIAGE)
levels(data_default$MARRIAGE) <- c("Others","Married","Single","Others")

# Convert Repayment Status columns to Factors
data_default$PAY_0 <-as.factor(data_default$PAY_0)
data_default$PAY_2 <- as.factor(data_default$PAY_2)
data_default$PAY_3 <- as.factor(data_default$PAY_3)
data_default$PAY_4 <- as.factor(data_default$PAY_4)
data_default$PAY_5 <- as.factor(data_default$PAY_5)
data_default$PAY_6 <- as.factor(data_default$PAY_6)

# Convert default.payment.next.month to Factors 
data_default$default.payment.next.month <- as.factor(data_default$default.payment.next.month)
levels(data_default$default.payment.next.month) <- c("No" , "Yes")

#default.payment.next.month is too long 
colnames(data_default)[25] <- "default_flag"

# Check the status of features
status <- df_status(data_default)

# Now begin the analysis of some typical features via plots
# First begin with SEX and AGE because these are two most typical 
# Physilogical features for human beings

#First,find the relationship between independent variable and the dependent variable

x11(width=6, height=5)
# [about sex factor]
#1
#percent_male <- sum(data_default$SEX=="Male")/dim(data_default)[1]
#percent_female <- sum(data_default$SEX=="Female")/dim(data_default)[1]
ggplot(aes(x=SEX,fill=default_flag), data=data_default)+ geom_bar()

ggplot(data_default, aes(x=SEX, fill=default_flag)) + 
  geom_bar(position="dodge") + 
  stat_count(aes(label=paste0(sprintf("%1.1f", ..count../sum(..count..)*100),
                              "%\n", ..count..), y=0.5*..count..), 
             geom="text", colour="white", size=4, position=position_dodge(width=1)) 


#conculsion:sex has an effect

# [about education backgrond]
# 1
ggplot(aes(x=EDUCATION,fill=default_flag), data=data_default)+ geom_bar()
# 2
ggplot(aes(x=default_flag,fill=default_flag),data=data_default) + 
  geom_histogram(stat="count") +
  facet_wrap(~EDUCATION)
#3
ggplot(aes(x=default_flag,fill=EDUCATION),data=data_default) + 
  geom_bar() +
  facet_wrap(~SEX)


ggplot(aes(x=default_flag,fill=SEX),data=data_default) + 
  geom_bar()+facet_wrap(~EDUCATION)
#conclusion:education has little impact on default

#[about marriage situation]
#1
ggplot(aes(x=MARRIAGE,fill=default_flag), data=data_default)+ geom_bar()
#2
ggplot(aes(x=default_flag,fill=default_flag),data=data_default) + 
  geom_histogram(stat="count") +
  facet_wrap(~MARRIAGE)
#conculsion:marriage has an effect

#[about age]
#1
ggplot(aes(x=AGE,fill=default_flag), data=data_default)+ geom_bar()
#2
ggplot(aes(x=default_flag,y=AGE,fill=default_flag), data=data_default)+ geom_boxplot()
#could have an impact, but not sure, so divide into age group
data_default$AGE.group<-cut(data_default$AGE,c(20,40,60,80))
#3
ggplot(aes(x=default_flag,fill=default_flag),data=data_default) + 
  geom_histogram(stat="count") +
  facet_wrap(~AGE.group)
#4
ggplot(aes(x=default_flag,fill=AGE.group), data=data_default) + 
  geom_bar() +facet_wrap(~SEX)
#conclusion:have some impact

#[about LIMIT_BAL]
#1
ggplot(aes(x=default_flag,y=LIMIT_BAL,fill=default_flag), data=data_default)+ geom_boxplot()
#2
ggplot(aes(x=LIMIT_BAL,fill=default_flag), data=data_default)+ geom_histogram(bins = 20)

def_rate <- function(x){
  sum <- sum(data_default$LIMIT_BAL==x)
  def <- sum(data_default$LIMIT_BAL==x & data_default$default_flag=="Yes")
  return (def/sum)}
#x <- data_default$LIMIT_BAL
ggplot(aes(x=LIMIT_BAL,y=sapply(x,FUN=def_rate)),data=data_default)+geom_line(color="red")+
  ylab("Default_rate")
#conclusion:have some impact
head(data_default)
str(data_default)

#Next, find relationship among the independent variables 
# the relationships between limit_bal and other variables could have some meaning
#1
ggplot(aes(x=SEX,y=LIMIT_BAL,fill=EDUCATION), data=data_default)+ geom_boxplot()
ggplot(aes(x=SEX,y=LIMIT_BAL,fill=EDUCATION), data=data_default)+ geom_violin()
#2
ggplot(aes(x=EDUCATION,y=LIMIT_BAL,fill=SEX), data=data_default)+ geom_boxplot()
ggplot(aes(x=EDUCATION,y=LIMIT_BAL,fill=SEX), data=data_default)+ geom_violin()
#we saw that gender has no effects on balance limit  
#while the education level is has a positive effect on this process
#3
ggplot(aes(x=AGE.group,y=LIMIT_BAL,fill=EDUCATION), data=data_default)+ geom_boxplot()
ggplot(aes(x=AGE.group,y=LIMIT_BAL,fill=EDUCATION), data=data_default)+ geom_violin()
#education's positive effect on balance limits for clients is increasing by later ages 


x11()
#Correlations Between Limit Balance, Bill Amounts & Payments
M <- cor(subset(data_default, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(M, method="number")



#Result: When we reflect the correlations between limit balances,
#bill amounts and payments amounts; 
#it presents us that there is a low correlation 
#between the limit balances and payments and bill amounts. 
#Howevet it can be seen that bill amounts has high correlation between each other as expected since the bills a reflecting the cumulative amounts.


# Principal component anlysis
data_pca <- data
data_pca <- data_pca[,1:24]
df_status(data_pca)
pca_default <- prcomp(data_pca,center = TRUE,scale. = TRUE)
summary(pca_default)

str(pca_default)
biplot(pca_default,var.axes=TRUE)

screeplot(pca_default)
screeplot(pca_default,type="lines")
predict(pca_default)

principal(data_pca[,-1],nfactors=5)

# parallel analysis
fa.parallel(data_pca)

fa(data_pca, nfactors=8,rotate="none", fm='ml')
#Tucker Lewis Index of factoring reliability =  0.897
#The first 7 factors explains 54% of the variance of data

#Now rotate the factors
fa_model_varimax <- fa(data_pca,nfactors = 8,rotate = 'varimax',fm='ml',scores = TRUE)
fa_model_varimax
fa_model_varimax$scores
factor.plot(fa_model_varimax)
fa.diagram(fa_model_varimax,simple = FALSE)

fa_model_quartimax <- fa(data_pca,nfactors = 8,rotate = 'quartimax' ,fm='ml',scores = TRUE)
fa_model_quartimax
head(fa_model_quartimax$scores,3)
factor.plot(fa_model_quartimax)
fa.diagram(fa_model_quartimax,simple = FALSE)


# the random forest prediction

# seperating the training and testing samples,we want to 
# use 80% of the data to train the model

data_rf <- data_default[,-26]
data_rf <- data_rf[,colnames(data_rf)!="ID"]
n_train <- 0.8*nrow(data_rf)
set.seed(1121)
t_rain <- sample(1:nrow(data_rf),n_train)

data_train_rf <- data_rf[t_rain,]
data_test_rf <- data_rf[-t_rain,]



oob_err <- matrix(nrow=5,ncol=23) # the out-of-bag error
test_err <- matrix(nrow=5,ncol=23) # test_error, which is the mean_squared error
auc_rf <- matrix(nrow = 5,ncol = 23)
# mtry is the variable number that each tree will split 


n_tree <- c(10,50,100,150,200)
# for tree number equals 10
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf,
                                subset=t_rain,mtry=mtry,ntree=n_tree[1])
  oob_err[1,mtry] <- random_forest$err.rate[n_tree[1]]
  class_rf <- predict(random_forest, data_rf[-t_rain,],type = "prob")
  pred <- predict(random_forest, data_rf[-t_rain,])
  test_err[1,mtry] <- with(data_rf[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf[1,mtry] <- auc(roc(data_test_rf$default_flag,class_rf[,2]))
  
}

# for tree number equals 50  
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf,
                                subset=t_rain,mtry=mtry,ntree=n_tree[2])
  oob_err[2,mtry] <- random_forest$err.rate[n_tree[2]]
  class_rf <- predict(random_forest, data_rf[-t_rain,],type = "prob")
  pred <- predict(random_forest, data_rf[-t_rain,])
  test_err[2,mtry] <- with(data_rf[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf[2,mtry] <- auc(roc(data_test_rf$default_flag,class_rf[,2]))
  
}


# for tree number equals 100
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf,
                                subset=t_rain,mtry=mtry,ntree=n_tree[3])
  oob_err[3,mtry] <- random_forest$err.rate[n_tree[3]]
  class_rf <- predict(random_forest, data_rf[-t_rain,],type = "prob")
  pred <- predict(random_forest, data_rf[-t_rain,])
  test_err[3,mtry] <- with(data_rf[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf[3,mtry] <- auc(roc(data_test_rf$default_flag,class_rf[,2]))
}

# for tree number equals 150
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf,
                                subset=t_rain,mtry=mtry,ntree=n_tree[4])
  oob_err[4,mtry] <- random_forest$err.rate[n_tree[1]]
  class_rf <- predict(random_forest, data_rf[-t_rain,],type = "prob")
  pred <- predict(random_forest, data_rf[-t_rain,])
  test_err[4,mtry] <- with(data_rf[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf[4,mtry] <- auc(roc(data_test_rf$default_flag,class_rf[,2]))
}

# for tree number equals 200
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf,
                                subset=t_rain,mtry=mtry,ntree=n_tree[5])
  oob_err[5,mtry] <- random_forest$err.rate[n_tree[5]]
  class_rf <- predict(random_forest, data_rf[-t_rain,],type = "prob")
  pred <- predict(random_forest, data_rf[-t_rain,])
  test_err[5,mtry] <- with(data_rf[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf[5,mtry] <- auc(roc(data_test_rf$default_flag,class_rf[,2]))
}

write.csv(oob_err,file = "oob_err.csv")
write.csv(test_err,file = "test_err.csv")
write.csv(auc_rf,file = "auc_rf.csv")

auc_rf
test_err
oob_err
apply(test_err,1,min)
apply(oob_err,1,min)

oob_err <- read.csv("D:/NYU/Curriculum/CapstoneProject/oob_err.csv")
test_err <- read.csv("D:/NYU/Curriculum/CapstoneProject/test_err.csv")
auc_rf <- read.csv("D:/NYU/Curriculum/CapstoneProject/auc_rf.csv")

matplot(1:mtry, t(test_err), pch=23,col = c("red","orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree,pch=23,col = c("red","orange","green","blue","black") )

matplot(1:mtry, t(oob_err), pch = 23, col = c("red","orange","green","blue","black") ,type = "b", ylab="OOB Error")
legend("topright",legend=n_tree,pch=23,col = c("red","orange","green","blue","black") )

matplot(1:mtry, t(auc_rf), pch=23,col = c("red","orange","green","blue","black") ,type = "b", ylab="AUC")
legend("topright",legend=n_tree,pch=23,col = c("red","orange","green","blue","black") )


matplot(3:mtry, t(test_err[2:5,3:23]), pch=23,col = c("orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree[2:5],pch=23,col = c("orange","green","blue","black") )

matplot(3:mtry, t(oob_err[2:5,3:23]), pch=23,col = c("orange","green","blue","black") ,type = "b", ylab="OOB Error")
legend("topright",legend=n_tree[2:5],pch=23,col = c("orange","green","blue","black") )

matplot(3:mtry, t(auc_rf[2:5,3:23]), pch=23,col = c("orange","green","blue","black") ,type = "b", ylab="AUC")
legend("topright",legend=n_tree[2:5],pch=23,col = c("orange","green","blue","black") )




table(apply(test_err,2,which.min))
which.min(test_err)
which.max(auc_rf)
# 55 200,11
max(auc_rf)
#0.766736
min(test_err)
oob_err[5,4]
which.min(oob_err[5,])
which.min(test_err[5,])

# the test accuracy comes to converge to a level at the 150 trees
# and the oob error of 200 trees and 150 trees has little diffrence
# so we just stop at 200 trees and choose the optimal mtry 4 to predict

# # so we just choose the model of 100 trees and the mtry number of 3
# # and we have a smaller oob error
# random_forest_150 <- randomForest(formula=default_flag ~ .,data=data_default,
#                               subset=t_rain,mtry=3,ntree=150)
# random_forest_150
# which.min(test_err[3,])
# min(test_err[3,])
# which.min(oob_err[3,])
# oob_err[3,3]
# # when tree number equals 100
# # choose the mtry equals 16
# random_forest_100 <- randomForest(formula=default_flag ~ .,data=data_default,
#                                   subset = t_rain,mtry=3,ntree=100)
# random_forest_100

# try to adjust the age into age groups
data_rf_2 <- data_rf
data_rf_2$AGE.group<-cut(data_rf_2$AGE,c(20,40,60,80))
#data_rf$AGE.group
data_rf_2 <- data_rf_2[,colnames(data_rf_2)!="AGE"]
data_rf_2 <- data_rf_2[,colnames(data_rf_2)!="ID"]


n_train_2 <- 0.8*nrow(data_rf_2)
set.seed(1121)
t_rain_2 <- sample(1:nrow(data_rf_2),n_train_2)

data_train_rf_2 <- data_rf_2[t_rain_2,]
data_test_rf_2 <- data_rf_2[-t_rain_2,]
auc_rf_2 <- matrix(nrow = 5,ncol = 23)
oob_err_2 <- matrix(nrow=5,ncol=23) # the out-of-bag error
test_err_2 <- matrix(nrow=5,ncol=23) # test_error, which is the mean_squared error
# mtry is the variable number that each tree will split 
n_tree <- c(10,50,100,150,200)
# for tree number equals 10
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[1])
  oob_err_2[1,mtry] <- random_forest$err.rate[n_tree[1]]
  class_rf <- predict(random_forest, data_rf_2[-t_rain_2,],type = "prob")
  pred <- predict(random_forest, data_rf_2[-t_rain_2,])
  test_err_2[1,mtry] <- with(data_rf_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf_2[1,mtry] <- auc(roc(data_test_rf_2$default_flag,class_rf[,2]))
  }

# for tree number equals 50  
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[2])
  oob_err_2[2,mtry] <- random_forest$err.rate[n_tree[2]]
  class_rf <- predict(random_forest, data_rf_2[-t_rain_2,],type = "prob")
  pred <- predict(random_forest, data_rf_2[-t_rain_2,])
  test_err_2[2,mtry] <- with(data_rf_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf_2[2,mtry] <- auc(roc(data_test_rf_2$default_flag,class_rf[,2]))
}

# for tree number equals 100
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[3])
  oob_err_2[3,mtry] <- random_forest$err.rate[n_tree[3]]
  class_rf <- predict(random_forest, data_rf_2[-t_rain_2,],type = "prob")
  pred <- predict(random_forest, data_rf_2[-t_rain_2,])
  test_err_2[3,mtry] <- with(data_rf_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf_2[3,mtry] <- auc(roc(data_test_rf_2$default_flag,class_rf[,2]))
}

# for tree number equals 150
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[4])
  oob_err_2[4,mtry] <- random_forest$err.rate[n_tree[4]]
  class_rf <- predict(random_forest, data_rf_2[-t_rain_2,],type = "prob")
  pred <- predict(random_forest, data_rf_2[-t_rain_2,])
  test_err_2[4,mtry] <- with(data_rf_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf_2[4,mtry] <- auc(roc(data_test_rf_2$default_flag,class_rf[,2]))
}

# for tree number equals 200
for (mtry in 1:23){
  set.seed(1121)
  random_forest <- randomForest(formula=default_flag ~ .,data=data_rf_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[5])
  oob_err_2[5,mtry] <- random_forest$err.rate[n_tree[5]]
  class_rf <- predict(random_forest, data_rf_2[-t_rain_2,],type = "prob")
  pred <- predict(random_forest, data_rf_2[-t_rain_2,])
  test_err_2[5,mtry] <- with(data_rf_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
  auc_rf_2[5,mtry] <- auc(roc(data_test_rf_2$default_flag,class_rf[,2]))
}


write.csv(oob_err_2,file = "oob_err_2.csv")
write.csv(test_err_2,file = "test_err_2.csv")
write.csv(auc_rf_2,file = "auc_rf_2.csv")

max(auc_rf_2)>max(auc_rf)
which.min(test_err_2)
min(oob_err_2)
min(test_err_2)<min(test_err)

matplot(1:mtry, t(test_err_2), pch =23,col = c("red","orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree,pch = 23,col = c("red","orange","green","blue","black") )

matplot(1:mtry, t(oob_err_2), pch = 23, col = c("red","orange","green","blue","black") ,type = "b", ylab="OOB Error")
legend("topright",legend=n_tree,pch = 23,col = c("red","orange","green","blue","black") )

matplot(3:mtry, t(test_err_2[2:5,3:23]), pch = 23,col = c("orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree[2:5],pch = 23,col = c("orange","green","blue","black") )

matplot(3:mtry, t(oob_err_2[2:5,3:23]), pch = 23,col = c("orange","green","blue","black") ,type = "b", ylab="OOB Error")
legend("topright",legend=n_tree[2:5],pch = 23,col = c("orange","green","blue","black") )

matplot(1:mtry,cbind(apply(test_err,2,min),apply(test_err_2,2,min)),pch = 23,col = c('red','green'),type = 'b',ylab = "Test Error")
legend("topright",legend=c("Original","Age Group"),pch = 23,col = c('red','green') )

sum(test_err<test_err_2)/(23*5)





# do a bootstrap of random forest
boot_strap_rf <- sapply(1:100, function(x) {
  train_set <- sample(1:30000,24000)
  train <- data_rf[train_set,]
  test <- data_rf[-train_set,]
  boot_sample <- randomForest(formula=default_flag ~ .,data=data_rf,
                                              subset=train_set,mtry=11,ntree=200)
  rf_test <-  predict(boot_sample, test)
  test_err <- with(test, cumsum( default_flag!=rf_test)[length(rf_test)]/(length(rf_test)))
  class_rf <- predict(boot_sample,test,type = "prob")
  auc <- auc(roc(test$default_flag,class_rf[,2]))
  c(auc=auc,test_err=test_err)
  
})

boot_strap_rf <- t(boot_strap_rf)
mean(boot_strap_rf[,1])
re_sult_rf <- apply(boot_strap_rf,2,quantile)
write.csv(re_sult_rf,file = "result of random forest bootstrap.csv")
re_sult_rf
# the mean of test error of random forest is 0.1484833

# gradient boosting machine model
# first try with the original data

data_gbm <- data_default[,-26]
data_gbm$default_flag <-ifelse(data_gbm$default_flag=="Yes",1,0)
data_gbm <- data_gbm[,colnames(data_gbm)!="ID"]
#data_gbm$default_flag
data_train_gbm <- data_gbm[t_rain,]
data_test_gbm <- data_gbm[-t_rain,]


iter_auc <- matrix(nrow = 2,ncol = 10)
for(dep_th in 1:10){
  set.seed(1121)
  gbm_model <- gbm(default_flag ~ .,
      data = data_train_gbm,
      n.trees = 5000,
      distribution = "bernoulli",
      interaction.depth = dep_th,
      shrinkage = 0.01,
      bag.fraction = 0.5,
      train.fraction = 0.8,
      cv.folds = 3)
  best_iter <- gbm.perf(gbm_model, method = "cv")
  iter_auc[1,dep_th] <- best_iter
  #gbm_improve <-  summary(gbm_model, n.trees = best_iter)
  gbm_test <-  predict(gbm_model, newdata = data_test_gbm, n.trees = best_iter)
  auc_gbm <-  roc(data_test_gbm$default_flag, gbm_test, plot = FALSE)
  iter_auc[2,dep_th] <- auc_gbm$auc
}

iter_auc
write.csv(iter_auc,file = "iter_auc.csv")
which.max(iter_auc[2,])
# we choose the interaction.depth of 9
set.seed(1121)
gbm_model <- gbm(default_flag ~ .,
                 data = data_train_gbm,
                 n.trees = 5000,
                 distribution = "bernoulli",
                 interaction.depth = 9,
                 shrinkage = 0.01,
                 bag.fraction = 0.5,
                 train.fraction = 0.8,
                 cv.folds = 3)


summary(gbm_model)

best_iter <- gbm.perf(gbm_model, method = "cv")
best_iter
# the best ntree is 847
gbm_improve <-  summary(gbm_model, n.trees = best_iter)

gbm_test <-  predict(gbm_model, newdata = data_test_gbm, n.trees = best_iter)

auc_gbm <-  roc(data_test_gbm$default_flag, gbm_test, plot = TRUE, col = "red")
print(auc_gbm)

#Call:
# roc.default(response = data_test_gbm$default_flag, predictor = gbm_test,     plot = TRUE, col = "red")
# 
# Data: gbm_test in 4666 controls (data_test_gbm$default_flag 0) < 1334 cases (data_test_gbm$default_flag 1).
# Area under the curve: 0.781


# to see the threshold that used to check whether a outcome is default or not
coords(auc_gbm,"best")

# threshold specificity sensitivity 
# -1.223       0.819       0.626
predict_class <- ifelse(gbm_test > coords(auc_gbm,"best")["threshold"],1,0)

#Area under the curve: 0.7814


# now add the age group into the model
data_gbm_2 <- data_gbm
data_gbm_2$AGE.group<-cut(data_gbm_2$AGE,c(20,40,60,80))
#data_gbm$AGE.group
data_gbm_2 <- data_gbm_2[,colnames(data_gbm_2)!="AGE"]
data_gbm_2 <- data_gbm_2[,colnames(data_gbm_2)!="ID"]
data_train_gbm_2 <- data_gbm_2[t_rain,]
data_test_gbm_2 <- data_gbm_2[-t_rain,]


iter_auc_2 <- matrix(nrow = 2,ncol = 10)
for(dep_th in 1:10){
  set.seed(1121)
  gbm_model <- gbm(default_flag ~ .,
                   data = data_train_gbm_2,
                   n.trees = 5000,
                   distribution = "bernoulli",
                   interaction.depth = dep_th,
                   shrinkage = 0.01,
                   bag.fraction = 0.5,
                   train.fraction = 0.8,
                   cv.folds = 3)
  best_iter <- gbm.perf(gbm_model, method = "cv")
  iter_auc_2[1,dep_th] <- best_iter
  #gbm_improve <-  summary(gbm_model, n.trees = best_iter)
  gbm_test <-  predict(gbm_model, newdata = data_test_gbm_2, n.trees = best_iter)
  auc_gbm <-  roc(data_test_gbm_2$default_flag, gbm_test, plot = FALSE)
  iter_auc_2[2,dep_th] <- auc_gbm$auc
}
iter_auc_2
write.csv(iter_auc_2,file = "iter_auc_2.csv")
which.max(iter_auc_2[2,-1])

matplot(1:10,cbind(t(iter_auc)[,2],t(iter_auc_2)[,2]),pch = 23,col = c('red','green'),type = 'b',ylab = "AUC")
legend("bottomright",legend=c("Original","Age Group"),pch = 23,col = c('red','green') )

# we can see that the performance of age group is a little bit better than original group

set.seed(1121)
gbm_model_2 <- gbm(default_flag ~ .,
                 data = data_train_gbm_2,
                 n.trees = 5000,
                 distribution = "bernoulli",
                 interaction.depth = 5,
                 shrinkage = 0.01,
                 bag.fraction = 0.5,
                 train.fraction = 0.8,
                 cv.folds = 3)


summary(gbm_model_2)

best_iter_2 <- gbm.perf(gbm_model_2, method = "cv")
best_iter_2

gbm_improve_2 <-  summary(gbm_model_2, n.trees = best_iter_2)

gbm_test_2 <-  predict(gbm_model_2, newdata = data_test_gbm_2, n.trees = best_iter_2)

auc_gbm_2 <-  roc(data_test_gbm_2$default_flag, gbm_test_2, plot = TRUE, col = "red")
print(auc_gbm_2)

coords(auc_gbm_2,"best")



#Call:
# roc.default(response = data_test_gbm_2$default_flag, predictor = gbm_test_2,     plot = TRUE, col = "red")
# 
# Data: gbm_test_2 in 4666 controls (data_test_gbm_2$default_flag 0) < 1334 cases (data_test_gbm_2$default_flag 1).
# Area under the curve: 0.782

# next we use bootstrap to validate the tested GBM model

# boot_strap_gbm <- sapply(1:10, function(x) {
#   train_set <- sample(1:30000,24000)
#   data_gbm_2 <- data_gbm
#   data_gbm_2$AGE.group<-cut(data_gbm_2$AGE,c(20,40,60,80))
#   data_gbm_2 <- data_gbm_2[,colnames(data_gbm_2)!="AGE"]
#   data_gbm_2 <- data_gbm_2[,colnames(data_gbm_2)!="ID"]
#   train <- data_gbm_2[train_set,]
#   test <- data_gbm_2[-train_set,]
#   boot_sample <-gbm(default_flag ~ .,
#                      data = train,
#                      n.trees = 1276,
#                      distribution = "bernoulli",
#                      interaction.depth = 5,
#                      shrinkage = 0.01,
#                      bag.fraction = 0.5,
#                      train.fraction = 0.8,
#                      cv.folds = 3)
#   gbm_test <-  predict(boot_sample, newdata = test, n.trees = 1276)
#   auc <-  roc(test$default_flag, gbm_test, plot = FALSE, col = "red")
#   auc$auc
# })
# boot_strap_gbm <- t(boot_strap_gbm)
# quantile(boot_strap_gbm)
# mean(boot_strap_gbm)
# the mean of bootstrap of gbm is 0.7845102

# do a bootstrap to calculate test error
boot_strap_gbm_2 <- sapply(1:100, function(x) {
  train_set <- sample(1:30000,24000)
  data_gbm_2 <- data_gbm
  data_gbm_2$AGE.group<-cut(data_gbm_2$AGE,c(20,40,60,80))
  data_gbm_2 <- data_gbm_2[,colnames(data_gbm_2)!="AGE"]
  data_gbm_2 <- data_gbm_2[,colnames(data_gbm_2)!="ID"]
  train <- data_gbm_2[train_set,]
  test <- data_gbm_2[-train_set,]
  boot_sample <-gbm(default_flag ~ .,
                    data = train,
                    n.trees = 1276,
                    distribution = "bernoulli",
                    interaction.depth = 5,
                    shrinkage = 0.01,
                    bag.fraction = 0.5,
                    train.fraction = 0.8,
                    cv.folds = 3)
  gbm_test <-  predict(boot_sample, newdata = test, n.trees = 1276)
  auc <-  roc(test$default_flag, gbm_test, plot = FALSE, col = "red")
  predict_class <- ifelse(gbm_test > coords(auc,"best")["threshold"],1,0)
  test_err <- with(test, cumsum( default_flag!=predict_class)[length(predict_class)]/(length(predict_class)))
  c(test_err=test_err,auc=auc$auc)
  
  })
boot_strap_gbm_2 <- t(boot_strap_gbm_2)
apply(boot_strap_gbm_2,2,quantile)
apply(boot_strap_gbm_2,2,mean)
re_sult_gbm_2 <- apply(boot_strap_gbm_2,2,quantile)
write.csv(re_sult_gbm_2,file = "result of gbm boostrap 2.csv")
# the mean of test error is 0.233

# test_err   auc
# 0%      0.191 0.764
# 25%     0.222 0.780
# 50%     0.232 0.785
# 75%     0.244 0.790
# 100%    0.271 0.798
# 
# 
# test_err      auc 
# 0.233    0.785


boot_strap_gbm <- sapply(1:100, function(x) {
  train_set <- sample(1:30000,24000)
  train <- data_gbm[train_set,]
  test <- data_gbm[-train_set,]
  boot_sample <-gbm(default_flag ~ .,
                    data = train,
                    n.trees = 847,
                    distribution = "bernoulli",
                    interaction.depth = 9,
                    shrinkage = 0.01,
                    bag.fraction = 0.5,
                    train.fraction = 0.8,
                    cv.folds = 3)
  gbm_test <-  predict(boot_sample, newdata = test, n.trees = 847)
  auc <-  roc(test$default_flag, gbm_test, plot = FALSE, col = "red")
  predict_class <- ifelse(gbm_test > coords(auc,"best")["threshold"],1,0)
  test_err <- with(test, cumsum( default_flag!=predict_class)[length(predict_class)]/(length(predict_class)))
  c(test_err=test_err,auc=auc$auc)
  
})
boot_strap_gbm <- t(boot_strap_gbm)
apply(boot_strap_gbm,2,quantile)
apply(boot_strap_gbm,2,mean)
re_sult_gbm <- apply(boot_strap_gbm,2,quantile)
write.csv(re_sult_gbm,file = "result of gbm boostrap.csv")

# test_err   auc
# 0%      0.202 0.767
# 25%     0.224 0.779
# 50%     0.236 0.783
# 75%     0.249 0.786
# 100%    0.272 0.799
# 
# 
# test_err      auc 
# 0.235    0.783 


#Random Forest 

# 30000 samples
# 23 predictor
# 2 classes: 'No', 'Yes' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 26999, 27000, 27000, 27000, 26999, 27000, ... 
# Resampling results:
#   
#   Accuracy  Kappa
# 0.82      0.364
# 
# Tuning parameter 'mtry' was held constant at a value of 4.8

# use caret to tune the parameters





# 
# # xgboost
# 
# data_xgb <- data
# colnames(data_xgb)[25] <- "default_flag"
# train_xgb <- data_xgb[,-1][t_rain,]
# test_xgb <- data_xgb[,-1][-t_rain,]
# 
# 
# data_train_xgb <- xgb.DMatrix(as.matrix(train_xgb[,-24]), label = train_xgb$default_flag)
# data_test_xgb <- xgb.DMatrix(as.matrix(test_xgb[,-24]), label = test_xgb$default_flag)
# 
# ntree_auc <- matrix(nrow = 2,ncol = 10)
# 
# for(max_depth in 1:10){
#   set.seed(1121)
#   xgb_model <- xgb.train(data = data_train_xgb,
#                          params = list(objective = "binary:logistic",
#                                        eta = 0.1,
#                                        max.depth = max_depth,
#                                        subsample = 0.5,
#                                        min_child_weight=50,
#                                        colsample_bytree = 1,
#                                        nthread = 3,
#                                        eval_metric = "auc"
#                          ),
#                          watchlist = list(test = data_test_xgb),
#                          nrounds = 500,
#                          early_stopping_rounds = 40,
#                          print_every_n = 100
#   )
#   ntree_auc[1,max_depth] <- xgb_model$best_ntreelimit
#   ntree_auc[2,max_depth] <- xgb_model$best_score
# }
# plot(1:10,ntree_auc[2,],type = "l",col="blue")
# which.max(ntree_auc[2,])
# # choose the max_depth of 7
# 
# set.seed(1121)
# xgb_model <- xgb.train(data = data_train_xgb,
#                              params = list(objective = "binary:logistic",
#                                            eta = 0.1,
#                                            max.depth = 7,
#                                            subsample = 0.5,
#                                            min_child_weight=50,
#                                            colsample_bytree = 1,
#                                            nthread = 3,
#                                            eval_metric = "auc"
#                              ),
#                              watchlist = list(test = data_test_xgb),
#                              nrounds = 500,
#                              early_stopping_rounds = 40,
#                              print_every_n = 100
# )
# 
# print(xgb_model$best_score)
# 
# xgb_test <- predict(xgb_model,newdata = as.matrix(test_xgb[,-24]),ntreelimit=xgb_model$best_ntreelimit)
# auc_xgb <- roc(test_xgb$default_flag, xgb_test, plot = TRUE, col = "blue")
# print(auc_xgb)
# # Area under the curve: 0.7853