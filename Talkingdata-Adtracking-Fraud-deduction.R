
#Import the  data and install related libraries
  

train_path <- ("https://raw.githubusercontent.com/subha89/TalkingData-Fault-Deduction/master/")
test_path <- ("https://raw.githubusercontent.com/subha89/TalkingData-Fault-Deduction/master/")

if(!require(tidyverse)) install.packages("tidyverse", repos ="http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos ="http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos ="http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos ="http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos ="http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos ="http://cran.us.r-project.org")
if(!require(DMwR)) install.packages("DMwR", repos ="http://cran.us.r-project.org")


#Read the data file
  
train <- fread(paste0(train_path,"train_sample.csv"), sep=",", 
               na.strings = "", 
               stringsAsFactors=T,
               nrows = 100000,
               data.table = F)
str(train)
test <- fread(paste0(test_path,"test.csv"), 
              sep=",", 
              na.strings = "", 
              stringsAsFactors=T,
              nrows = 100000,
              data.table = F)
str(test)

#There is no difference between train and test data except we need to predict target (is_attributed) in test and attributed_time (Time taken to download Application) is not given in test data)


colSums(is.na(train))


#There is no missing value at all, data is very clean and clear

colSums(train=='')


#Attributes_time (Time taken to download) having blank entries, this is logically correct 

#Lets check the target variable how many are not downloaded in train data

table(train$is_attributed)


#Our assumption is correct since blank entries in Attributes_time is matching with Application not downlaoded in train data.As it's logically correct, we don't need do any further action on this and also notice that, this variable is not present in test data, so no point of keeping it in the train data too


train$attributed_time=NULL
train$click_time <- as.POSIXct(as.character(train$click_time), format = "%d/%m/%Y %H:%M")


#Get the data from click_time in train data and split it into additional columns

train$year=year(train$click_time)
train$month=month(train$click_time)
train$days=weekdays(train$click_time)
train$hour=hour(train$click_time)


#Data Cleaning
  
#After getting new feature, let's remove original "click_time" variable

train$click_time=NULL


#Check the unique number for each of the feature

apply(train,2, function(x) length(unique(x)))


#By looking into unique value, we can see data collected for one month in a year, so no point of keeping month and year variables

train$month=NULL
train$year=NULL

#Exploratory Data Analysis

#Convert variables into respective data type

train$is_attributed=as.factor(train$is_attributed)
train$days=as.factor(train$days)
train$os = as.factor(train$os)
train$device = as.factor(train$device)
train$channel = as.factor(train$channel)
train$hour = as.factor(train$hour)
str(train)

#App was downloaded v/s App id for marketing

p1 <- ggplot(train,aes(x=is_attributed,y=app,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Application ID v/s Is_attributed")+
  xlab("App ID") +
  labs(fill = "is_attributed")

p2 <- ggplot(train,aes(x=app,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  scale_x_continuous(breaks = c(0,50,100,200,300,400))+
  ggtitle("Application ID v/s Is_attributed")+
  xlab("App ID") +
  labs(fill = "is_attributed")  

p3=ggplot(train,aes(x=is_attributed,y=app,fill=is_attributed))+
  geom_violin()+
  ggtitle("Application ID v/s Is_attributed")+
  xlab("App ID") +
  labs(fill = "is_attributed")  


grid.arrange(p1,p2, p3, nrow=1,ncol=3)

#App was downloaded vs OS version id of user mobile phone

p4=ggplot(train,aes(x=os,y=is_attributed,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Os version v/s Is_attributed")+
  xlab("OS version") +
  labs(fill = "is_attributed")  

p6=ggplot(train,aes(x=is_attributed,y=os,fill=is_attributed))+
  geom_violin()+
  ggtitle("Os version v/s Is_attributed")+
  xlab("Os version") +
  labs(fill = "is_attributed")  
grid.arrange(p4, p6, nrow=2,ncol=2)


#App was downloaded v/s ip address of click

p7=ggplot(train,aes(x=is_attributed,y=ip,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("IP Address v/s Is_attributed")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")  


p8=ggplot(train,aes(x=ip,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  scale_x_continuous(breaks = c(0,50,100,200,300,400))+
  ggtitle("IP Address v/s Is_attributed")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")  



p9=ggplot(train,aes(x=is_attributed,y=ip,fill=is_attributed))+
  geom_violin()+
  ggtitle("IP Address v/s Is_attributed")+
  xlab("Ip Adresss of click") +
  labs(fill = "is_attributed")  

grid.arrange(p7,p8, p9, nrow=2,ncol=2)

#Does specific hour play any role in downloading

p16=ggplot(train,aes(x=hour,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Hour v/s Is_attributed ")+
  xlab("Hour") +
  labs(fill = "is_attributed")  

p17=ggplot(train,aes(x=is_attributed,y=hour,fill=is_attributed))+
  geom_boxplot()+
  ggtitle("Hour v/s Is_attributed")+
  xlab("Hour") +
  labs(fill = "is_attributed")  

p18=ggplot(train,aes(x=is_attributed,y=channel,fill=is_attributed))+
  geom_violin()+
  ggtitle("Hour v/s Is_attributed")+
  xlab("Hour") +
  labs(fill = "is_attributed")  

grid.arrange(p16,p17, p18, nrow=2,ncol=2)

#There is slight difference in both the distribution, we can say least important feature

#Does Particular day play any role in downloading application?

p19=ggplot(train,aes(x=days,fill=is_attributed))+
  geom_density()+facet_grid(is_attributed~.)+
  ggtitle("Day of a week v/s Is_attributed ")+
  xlab("Os version") +
  labs(fill = "is_attributed")  


p20=ggplot(train,aes(x=days,fill=is_attributed))+geom_density(col=NA,alpha=0.35)+
  ggtitle("days v/s click")+
  xlab("Day of a week v/s Is_attributed ") +
  ylab("Total Count") +
  labs(fill = "is_attributed")  

grid.arrange(p19,p20, ncol=2)

#Machine learning algorithms and cross validation

#Now we will apply decission tree and cross validation to the training data set and check the accuracy.

#Running the model using all the features in the data set*

set.seed(1234)
library(caret)
cv.10 <- createMultiFolds(train$is_attributed, k = 10, times = 10)


ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)

set.seed(1234)

Model_CDT <- train(x = train[,-6], y = train[,6], method = "rpart", 
                   tuneLength = 20,
                   trControl = ctrl)

PRE_VDTS=predict(Model_CDT$finalModel,data=train,type="class")
confusionMatrix(PRE_VDTS,train$is_attributed)


varImp(Model_CDT)


#Even though overall accurcay is very high but specificity is too low

#Running the model with selected features


str(train)
train$days=NULL
train$hour=NULL
#train$os=NULL
#train$device=NULL
str(train)

set.seed(1234)

Model_CDT1 <- train(x = train[,-6], y = train[,6], 
                    method = "rpart", 
                    tuneLength = 20,
                    trControl = ctrl)

PRE_VDTS1=predict(Model_CDT1$finalModel,data=train,type="class")
confusionMatrix(PRE_VDTS1,train$is_attributed)


#Second model gives the same accuracy, however there is drastic change in specificity.

#Dividing the data into test and training data

train$app=NULL
train$ip=NULL
set.seed(5000)
ind=createDataPartition(train$is_attributed,times=1,p=0.7,list=FALSE)
train_val=train[ind,]
test_val=train[-ind,]

#Check the proportion  and its the same

round(prop.table(table(train$is_attributed)*100),digits = 3)

round(prop.table(table(train_val$is_attributed)*100),digits = 3)

round(prop.table(table(test_val$is_attributed)*100),digits = 3)


#Notice, how well caret divided the data into 70% to 30% ratio and also it make sure that no change in the proportion of target variable

#Data Balancing using Smote

set.seed(1234)
smote_train = SMOTE(is_attributed ~ ., data  = train_val)                         
table(smote_train$is_attributed)

#we now use Smote_train data set and use decision tree algorithm and check on the accuracy.

set.seed(1234)
cv.10 <- createMultiFolds(smote_train$is_attributed, k = 10, times = 10)
# Control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,
                     index = cv.10)
##Train the data
Model_CDT <- train(x = smote_train[,-4], 
                   y = smote_train[,4], 
                   method = "rpart", 
                   tuneLength = 20,
                   trControl = ctrl)


PRE_VDTS=predict(Model_CDT$finalModel,newdata=test_val,type="class")
confusionMatrix(PRE_VDTS,test_val$is_attributed)

#We are able to complete decision tree with 0.94% accuracy, and specificity increased to 0.85% (Remember, drastic increase in specificty after data balance)

#Reference Github Link:
#[https://raw.githubusercontent.com/sowmi121/Talkingdata-Adtracking-Fraud-deduction]
