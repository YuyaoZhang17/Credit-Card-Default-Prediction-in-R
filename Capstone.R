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

#import data
print("Import Data")
data_default <- read.csv("D:/NYU/Curriculum/CapstoneProject/UCI_Credit_Card.csv",header=TRUE)

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
data_pca <- read.csv("D:/NYU/Curriculum/CapstoneProject/UCI_Credit_Card.csv",header=TRUE)
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
#install.packages('randomForest')
#install.packages('rfUtilities')
library('rfUtilities')
library('randomForest')
# seperating the training and testing samples,we want to 
# use 80% of the data to train the model
n_train <- 0.8*nrow(data_default)
set.seed(2212)
t_rain <- sample(1:nrow(data_default),n_train)
#data_train <- data_default[t_rain,]
#data_train

oob_err <- matrix(nrow=5,ncol=24) # the out-of-bag error
test_err <- matrix(nrow=5,ncol=24) # test_error, which is the mean_squared error
# mtry is the variable number that each tree will split 
n_tree <- c(10,50,100,150,200)
# for tree number equals 10
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default,
                                subset=t_rain,mtry=mtry,ntree=n_tree[1])
  oob_err[1,mtry] <- random_forest$err.rate[n_tree[1]]
  pred <- predict(random_forest, data_default[-t_rain,])
  test_err[1,mtry] <- with(data_default[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 50  
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default,
                                subset=t_rain,mtry=mtry,ntree=n_tree[2])
  oob_err[2,mtry] <- random_forest$err.rate[n_tree[2]]
  pred <- predict(random_forest, data_default[-t_rain,])
  test_err[2,mtry] <- with(data_default[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 100
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default,
                                subset=t_rain,mtry=mtry,ntree=n_tree[3])
  oob_err[3,mtry] <- random_forest$err.rate[n_tree[3]]
  pred <- predict(random_forest, data_default[-t_rain,])
  test_err[3,mtry] <- with(data_default[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 150
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default,
                                subset=t_rain,mtry=mtry,ntree=n_tree[4])
  oob_err[4,mtry] <- random_forest$err.rate[n_tree[4]]
  pred <- predict(random_forest, data_default[-t_rain,])
  test_err[4,mtry] <- with(data_default[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 200
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default,
                                subset=t_rain,mtry=mtry,ntree=n_tree[5])
  oob_err[5,mtry] <- random_forest$err.rate[n_tree[5]]
  pred <- predict(random_forest, data_default[-t_rain,])
  test_err[5,mtry] <- with(data_default[-t_rain,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}


test_err
oob_err
apply(test_err,1,min)
apply(oob_err,1,min)


matplot(1:mtry, t(test_err), pch=23,col = c("red","orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree,pch=23,col = c("red","orange","green","blue","black") )

matplot(1:mtry, t(oob_err), pch = 23, col = c("red","orange","green","blue","black") ,type = "b", ylab="OOB Error")
legend("topright",legend=n_tree,pch=23,col = c("red","orange","green","blue","black") )

matplot(3:mtry, t(test_err[2:5,3:24]), pch=23,col = c("orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree[2:5],pch=23,col = c("orange","green","blue","black") )

matplot(3:mtry, t(oob_err[2:5,3:24]), pch=23,col = c("orange","green","blue","black") ,type = "b", ylab="Test Error")
legend("topright",legend=n_tree[2:5],pch=23,col = c("orange","green","blue","black") )

which.min(test_err[4,])
oob_err[4,9]
which.min(oob_err[4,])
# the test accuracy comes to converge to a level at the 150 trees
# so we just choose the model of 100 trees and the mtry number of 9
random_forest <- randomForest(formula=default_flag ~ .,data=data_default,
                              subset=t_rain,mtry=9,ntree=150)
random_forest

# try to adjust the age into age groups
data_default$AGE.group<-cut(data_default$AGE,c(20,40,60,80))
data_default$AGE.group
data_default_2 <- data_default[,-6]
head(data_default_2)


n_train_2 <- 0.8*nrow(data_default_2)
set.seed(2212)
t_rain_2 <- sample(1:nrow(data_default_2),n_train_2)

oob_err_2 <- matrix(nrow=5,ncol=24) # the out-of-bag error
test_err_2 <- matrix(nrow=5,ncol=24) # test_error, which is the mean_squared error
# mtry is the variable number that each tree will split 
n_tree <- c(10,50,100,150,200)
# for tree number equals 10
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[1])
  oob_err_2[1,mtry] <- random_forest$err.rate[n_tree[1]]
  pred <- predict(random_forest, data_default_2[-t_rain_2,])
  test_err_2[1,mtry] <- with(data_default_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 50  
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[2])
  oob_err_2[2,mtry] <- random_forest$err.rate[n_tree[2]]
  pred <- predict(random_forest, data_default_2[-t_rain_2,])
  test_err_2[2,mtry] <- with(data_default_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 100
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[3])
  oob_err_2[3,mtry] <- random_forest$err.rate[n_tree[3]]
  pred <- predict(random_forest, data_default_2[-t_rain_2,])
  test_err_2[3,mtry] <- with(data_default_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 150
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[4])
  oob_err_2[4,mtry] <- random_forest$err.rate[n_tree[4]]
  pred <- predict(random_forest, data_default_2[-t_rain_2,])
  test_err_2[4,mtry] <- with(data_default_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}

# for tree number equals 200
for (mtry in 1:24){
  random_forest <- randomForest(formula=default_flag ~ .,data=data_default_2,
                                subset=t_rain_2,mtry=mtry,ntree=n_tree[5])
  oob_err_2[5,mtry] <- random_forest$err.rate[n_tree[5]]
  pred <- predict(random_forest, data_default_2[-t_rain_2,])
  test_err_2[5,mtry] <- with(data_default_2[-t_rain_2,], cumsum( default_flag!=pred)[length(pred)]/(length(pred)))
}


test_err_2
oob_err_2


# gradient boosting machine model
