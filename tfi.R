Skip to content
This repository  
Pull requests
Issues
Gist
 @alexandrudaia
 Unwatch 2
  Star 0
  Fork 0
alexandrudaia/restaurants
Branch: master  restaurants/AlexBogdanAndBastard.R
@alexandrudaiaalexandrudaia on May 26 Update AlexBogdanAndBastard.R
1 contributor
RawBlameHistory     457 lines (398 sloc)  18.047 kB


 
 
library(caret)
library(data.table)
library(randomForest)
setwd("D:/rest")
library(ggplot2)
#########################read  the  data and  format  Pi=1,37 to numeric#######################################################
train=read.table("D:/rest/train.csv",header=T,sep=",")
test=read.table("D:/rest/test.csv",header=T,sep=",")
train=train[,-1]
test=test[,-1]
train[,c(5:41)]=sapply(train[,c(5:41)],as.numeric)
test[,c(5:41)]=sapply(test[,c(5:41)],as.numeric)
#########################compute Onicescu   information energies###############################################################
energies=c()
for( i in 1:nrow(train))
{
    t=table(as.numeric(train[i,5:41]))
    probabilities=t/37
    e=sum(probabilities^2)
    energies=c(energies,e)
}
train=cbind(energies,train)
#makes a  copy of  energies  from my train
trainEnergies=energies
energies=c()

for( i in 1:nrow(test))
{
    t=table(as.numeric(test[i,5:41]))
    probabilities=t/37
    e=sum(probabilities^2)
    energies=c(energies,e)
}
test=cbind(energies,test)
#makes  a copy of energies  from my test
testEnergies=energies
############################################SLICE  THE DATE  AS NUMERIC ######################################################
train$Open.Date=as.Date(train$Open.Date,"%m/%d/%Y")
test$Open.Date=as.Date(test$Open.Date, "%m/%d/%Y")

year = as.numeric(format(train$Open.Date, format = "%Y"))
month = as.numeric(format(train$Open.Date, format = "%m"))
day = as.numeric(format(train$Open.Date, format = "%d"))
train=cbind(year,month,day,train)
year = as.numeric(format(test$Open.Date, format = "%Y"))
month = as.numeric(format(test$Open.Date, format = "%m"))
day = as.numeric(format(test$Open.Date, format = "%d"))
test=cbind(year,month,day,test)
############################################REMOVE Open.Date###################################################################
train=train[,-c(5,6,8)]
test=test[,-c(5,6,8)]  

################## make energies  for  features  where mutual information exists#######################3
ndx=c("3","4","5" , "6" , "9", "10", "11" ,"13" ,"14" ,"15" ,"16" ,"17" ,"18", "19", "23" ,"24", "25" ,"28" ,"29" ,"31")
trainmi=train[,6:42][as.numeric(ndx)]
testmi=test[,6:42][as.numeric(ndx)]

energiesmi=c()
for( i in 1:nrow(trainmi))
{
    t=table(as.numeric(trainmi[i,1:20]))
    probabilities=t/20
    e=sum(probabilities^2)
    energiesmi=c(energiesmi,e)
}
train=cbind(train,energiesmi)
energiesmi=c()
for( i in 1:nrow(testmi))
{
    t=table(as.numeric(testmi[i,1:20]))
    probabilities=t/20
    e=sum(probabilities^2)
    energiesmi=c(energiesmi,e)
}
test=cbind(test,energiesmi)

trainenergimi=train$energiesmi
testenergiesmi=test$energiesmi


###########################################compute energires for features where is low mutual information

id=c(seq(1:37))
nonmutual=setdiff(id,ndx)

trainNonMutual=train[,6:42][as.numeric(nonmutual)]
testNonMutual=test[,6:42][as.numeric(nonmutual)]
energienonmi=c()
for( i in 1:nrow(trainNonMutual))
{
    t=table(as.numeric(trainNonMutual[i,1:17]))
    probabilities=t/17
    e=sum(probabilities^2)
    energienonmi=cbind(energienonmi,e)
}
train=cbind(train,as.numeric(energienonmi))
names(train)[45]="energienonmi"

energienonmi=c()
for( i in 1:nrow(testNonMutual))
{
    t=table(as.numeric(testNonMutual[i,1:17]))
    probabilities=t/17
    e=sum(probabilities^2)
    energienonmi=cbind(energienonmi,e)
}
test=cbind(test,as.numeric(energienonmi))
names(test)[44]="energienonmi"
tempTrain=train
tempTest=test
trainenerginonmi=tempTrain$energienonmi
testenergiesnonmi=tempTest$energienonmi
##########################################################################################################################
#caution trainmi  model should be created in advance
# names(trainmi)
# [1] "Days"       "year"       "month"      "day"        "energies"   "City.Group"
# [7] "P3"         "P4"         "P5"         "P6"         "P9"         "P10"       
#[13] "P11"        "P13"        "P14"        "P15"        "P16"        "P17"       
#[19] "P18"        "P19"        "P23"        "P24"        "P25"        "P28"       
#[25] "P29"        "P31"        "revenue"    "energiesmi" "ennonmi" 
#caution Bogdan model should be  trained   before in order  to add  Days
#names(train)
# [1] "year"         "month"        "day"          "energies"     "City.Group"  
# [6] "P1"           "P2"           "P3"           "P4"           "P5"          
#[11] "P6"           "P7"           "P8"           "P9"           "P10"         
#[16] "P11"          "P12"          "P13"          "P14"          "P15"         
#[21] "P16"          "P17"          "P18"          "P19"          "P20"         
#[26] "P21"          "P22"          "P23"          "P24"          "P25"         
#[31] "P26"          "P27"          "P28"          "P29"          "P30"         
#[36] "P31"          "P32"          "P33"          "P34"          "P35"         
#[41] "P36"          "P37"          "revenue"      "energiesmi"   "energienonmi"
#[46] "Days"        
#predict  on train set
only=cbind(train[,c(1,2,3,4,5,6)],train[,6:42][c(7,9,13,33)],train$revenue)
names(only)[11]="revenue"

model1=train(train$revenue~.,method="lasso",preProcess="pca",data=train,trControl = trainControl(method = "cv"))
model2=train(train$revenue~.,method="lm",preProcess="pca",data=train,trControl = trainControl(method = "cv"))
model3=train(train$revenue~.,method="gbm",preProcess="pca",data=train,trControl = trainControl(method = "cv"))
model4=train(train$revenue~.,method="rf",data=train,trControl = trainControl(method = "cv"))
model5=train(train$revenue~.,method="svmLinear",data=train,trControl = trainControl(method = "cv"))
model6=train(train$revenue~.,method="svmRadial",data=train,trControl = trainControl(method = "cv"))
model7=train(train$revenue~.,method="svmRadial",data=train,trControl = trainControl(method = "cv"),preProcess="pca")
model8 <- ksvm(revenue~.,data=train,
                  kernel="rbf",C=80)
model9 <- ksvm(revenue~.,data=train,
                  kernel="laplacedot",C=137)			
model10=    ksvm(revenue~.,data=trainmi,type="nu-svr",
               kernel="laplacedot",C=100)	
model11 <- ksvm(revenue~.,data=trainmi[,-c(1,2,3,4,5,6,28,29)],type="nu-svr",
                  kernel="laplacedot",C=30)	
model12=	train(trainmi$revenue~.,method="svmRadial",data=trainmi,trControl = trainControl(method = "cv"))	
model13=	train(trainmi$revenue~.,method="pls",data=trainmi,trControl = trainControl(method = "cv"))		
moel14=   ksvm(revenue~.,data=train,
                  kernel="laplacedot",C=50)	
moel15=   ksvm(revenue~.,data=train[,-c(which(colnames(train) %in% c("P7","P9","P13","P30","P33")))],
                  kernel="rbfdot",C=200)	
model16=	train(train$revenue~.,method="rf",data=train[,-c(which(colnames(train) %in% c("P7","P9","P13","P30","P33")))],trControl = trainControl(method = "cv"))
model17=	train(train$revenue~.,method="pls",data=train[,-c(which(colnames(train) %in% c("P7","P9","P13","P30","P33")))],trControl = trainControl(method = "cv"),
preProcess="pca")
#moel18=   ksvm(revenue~.,data=only,
                  kernel="laplacedot",C=75)					
#model19=	train(only$revenue~.,method="rf",data=only,trControl = trainControl(method = "cv"))			  
##predic on test  set 
ptrain1=predict(model1,train[,-43])
ptrain2=predict(model2,train[,-43])
ptrain3=predict(model3,train[,-43])
ptrain4=predict(model4,train[,-43])
ptrain5=predict(model5,train[,-43])
ptrain6=predict(model6,train[,-43])
ptrain7=predict(model7,train[,-43])
ptrain8=predict(model8,train[,-43])
ptrain9=predict(model9,train[,-43])
ptrain10=predict(model10,trainmi[,-27])
ptrain11=predict(model11,trainmi[,-c(1,2,3,4,5,6,27,28,29)])
ptrain12=predict(model12,trainmi[,-27])
ptrain13=predict(model13,trainmi[,-27])
ptrain14=predict(moel14,train[,-43])
ptrain15=predict(moel15,train[,-c(43,which(colnames(train) %in% c("P7","P9","P13","P30","P33")))])
ptrain16=predict(model16,train[,-c(43,which(colnames(train) %in% c("P7","P9","P13","P30","P33")))])
ptrain17=predict(model17,train[,-c(43,which(colnames(train) %in% c("P7","P9","P13","P30","P33")))])
#ptrain18=predict(moel18,only[,-11])
#ptrain19=predict(model19,only[,-11])



ptrain1=as.data.frame(ptrain1)
ptrain2=as.data.frame(ptrain2)
ptrain3=as.data.frame(ptrain3)
ptrain4=as.data.frame(ptrain4)
ptrain5=as.data.frame(ptrain5)
ptrain6=as.data.frame(ptrain6)
ptrain7=as.data.frame(ptrain7)
ptrain8=as.data.frame(ptrain8)
ptrain9=as.data.frame(ptrain9)
ptrain10=as.data.frame(ptrain10)
ptrain11=as.data.frame(ptrain11)
ptrain12=as.data.frame(ptrain12)
ptrain13=as.data.frame(ptrain13)
ptrain14=as.data.frame(ptrain14)
ptrain15=as.data.frame(ptrain15)
ptrain16=as.data.frame(ptrain16)
ptrain17=as.data.frame(ptrain17)
 


b=cbind(ptrain1,ptrain2,ptrain3,ptrain4,ptrain5,ptrain6,ptrain7,ptrain8,ptrain9,ptrain10,
ptrain11,ptrain12,ptrain13,ptrain15,ptrain16,ptrain17, train$revenue)
names(b)=c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12"
,"m13","m15","m16","m17","revenue")
best=train(b$revenue~.,method="rf",data=b,trControl = trainControl(method = "cv"))
varImpPlot(best$finalModel)
ptest1=predict(model1,test)
ptest2=predict(model2,test)
ptest3=predict(model3,test)
ptest4=predict(model4,test)
ptest5=predict(model5,test)
ptest6=predict(model6,test)
ptest7=predict(model7,test)
ptest8=predict(model8,test)
ptest9=predict(model9,test)
ptest10=predict(model10,testmi)
ptest11=predict(model11,testmi[,-c(1,2,3,4,5,6,27,28)])
ptest12=predict(model12,testmi)
ptest13=predict(model13,testmi)
ptest14=predict(moel14,test)

ptest15=predict(moel15,test[,-c(which(colnames(test) %in% c("P7","P9","P13","P30","P33")))])
ptest16=predict(model16,test[,-c(which(colnames(test) %in% c("P7","P9","P13","P30","P33")))])
ptest17=predict(model17,test[,-c(which(colnames(test) %in% c("P7","P9","P13","P30","P33")))])
##ptest18=predict(moel18,only)
# ptest19=predict(model19,only[,-11])



ptest1=as.data.frame(ptest1)
ptest2=as.data.frame(ptest2)
ptest3=as.data.frame(ptest3)
ptest4=as.data.frame(ptest4)
ptest5=as.data.frame(ptest5)
ptest6=as.data.frame(ptest6)
ptest7=as.data.frame(ptest7)
ptest8=as.data.frame(ptest8)
ptest9=as.data.frame(ptest9)
ptest10=as.data.frame(ptest10)
ptest11=as.data.frame(ptest11)
ptest12=as.data.frame(ptest12)
ptest13=as.data.frame(ptest13)
ptest14=as.data.frame(ptest14)
ptest15=as.data.frame(ptest15)
ptest16=as.data.frame(ptest16)
ptest17=as.data.frame(ptest17)



#######################################################now  get  Bogdans's models ###################################
library(randomForest)
library(caret)
library(lubridate)
library(elasticnet)

#Set working directory to the folder with data and load datasets

train=read.csv("train.csv")
test=read.csv("test.csv")

#Add Days variable
train$Date=mdy(as.character(train$Open.Date))
train$today=today()
train$time=new_interval(start = train$Date,end = train$today)
train$period=as.period(train$time, unit = "days")
for (i in 1:nrow(train)){train$Days[i]=unlist(strsplit(as.character(train$period[i]),"d"))[1]}
train$Days=as.numeric(train$Days)
train=train[,-c(44:47)]

#Correct levels in City Type with those present in test
#train$Type <- factor(train$Type , levels=c(levels(test$Type), 'fgh'))

test$Date=mdy(as.character(test$Open.Date))
test$today=today()
test$time=new_interval(start = test$Date,end = test$today)
test$period=as.period(test$time, unit = "days")
for (i in 1:nrow(test)){test$Days[i]=unlist(strsplit(as.character(test$period[i]),"d"))[1]}
test$Days=as.numeric(test$Days)
test=test[,-c(43:46)]
train$Days=log(train$Days)
train$P6=log(train$P6)
train$P22=log(train$P22)
train$P28=log(train$P28)
train$Days_sq=train$Days^2
train$P6_sq=train$P6^2

test$Days=log(test$Days)
test$P6=log(test$P6)
test$P22=log(test$P22)
test$P28=log(test$P28)
test$Days_sq=test$Days^2
test$P6_sq=test$P6^2
trControl=trainControl(method="repeatedcv", repeats=3)
lm_submit=train(log(revenue) ~ poly((Days), 2) + P28 + P22 + City.Group + I(Type == "IL") + 
                  poly(P6, 2), data = train,method="lm",trControl = trControl)
lasso_submit=train(log(revenue) ~ ., data = train[-c(1:3, 5)], method = "lasso", preProcess = "pca", trControl = trControl)
pls_submit=train(log(revenue) ~ ., data = train[-c(1:3, 5)], method = "pls", preProcess = "pca", trControl = trControl)
RF_submit=randomForest(log(revenue) ~ ., data = train[-c(1:3, 5)], mtry = 2, ntrees = 10000, importance = T)
	#predict for train			  
		 lmtrain=exp(predict(lm_submit,train))
				  lassotrain=exp(predict(lasso_submit,train))
				  plstrain=exp(predict(pls_submit,train))
				  rftrain=exp(predict(RF_submit,train))
				  	  
				  lmtrain=as.data.frame(lmtrain)
				  lassotrain=as.data.frame(lassotrain)
				  plstrain=as.data.frame(plstrain)
				  rftrain=as.data.frame(rftrain)		
     #predict  for test
	  lmtest=exp(predict(lm_submit,test))
				  lassotest=exp(predict(lasso_submit,test))
				  plstest=exp(predict(pls_submit,test))
				  rftest=exp(predict(RF_submit,test))
				  
				  lmtest=as.data.frame(lmtest)
				  lassotest=as.data.frame(lassotest)
				  plstest=as.data.frame(plstest)
				  rftest=as.data.frame(rftest)
	 
				 trainDays=train$Days
				 testDays=test$Days
				 train=tempTrain
				 test=tempTest
				 train=cbind(train,trainDays)
				 test=cbind(test,testDays)
				 names(train)[46]="Days"
				 names(test)[45]="Days"
				 tempTrain=train
				 tempTest=test
				  ###############################################################################################
				  ##############  DEALING  WIH   EXTREME VALUES

			  CUTH =8000000
 
outlier=ifelse(train$revenue>CUTH,1,-1)
newTrain=cbind(train$Days,trainEnergies,ptrain4,ptrain3,rftrain,outlier)###only models  with  trees
newTest=cbind(test$Days,testEnergies,ptest4,ptest3,rftest)
names(newTest)[1:5]=c("d","e","m1","m3","m4")
names(newTrain)[1:5]=c("d","e","m1","m3","m4")
newTrain=cbind(newTrain,trainenergimi)
newTest=cbind(newTest,testenergiesmi)
names(newTrain)[7]="energiesmi"
names(newTest)[6]="energiesmi"
 newTrain=cbind(newTrain,trainenerginonmi)
 newTest=cbind(newTest,testenergiesnonmi)	
names(newTrain)[8]="energiesnonmi"
names(newTest)[7]="energiesnonmi"
newTrain$outlier=as.factor(newTrain$outlier)
modelOutlier2=train(newTrain$outlier~.,method="svmLinear",data=newTrain) 
#modelOutlier2=ksvm(outlier~.,data=newTrain,type="C-bsvc",
                #  kernel="laplacedot",C=10)	
predictOutliers=predict(modelOutlier2,newTest)
indexOutliers=which(predictOutliers=="1")
pred=(ptest3+ptest4+plstest)/3
#################################################inflate with model for mi  features##########################
#model  with   features  with   biger mutual information  with revenue  but  not   within them  yiedls   higher  revenue
#when  fitting a moderate non-linear  learner
ndx=c("3","4","5" , "6" , "9", "10", "11" ,"13" ,"14" ,"15" ,"16" ,"17" ,"18", "19", "23" ,"24", "25" ,"28" ,"29" ,"31")

trainmi=cbind(train$Days,tempTrain[,1:5],tempTrain[,6:42][as.numeric(ndx)],tempTrain$revenue)
testmi=cbind(test$Days,tempTest[,1:5],tempTest[,6:42][as.numeric(ndx)])
names(trainmi)[27]="revenue"
names(trainmi)[1]="Days"
names(testmi)[1]="Days"
trainmi=cbind(trainmi,trainenergimi)
testmi=cbind(testmi,testenergiesmi)
names(trainmi)[28]="energiesmi"
names(testmi)[27]="energiesmi"



trainmi=cbind(trainmi,trainenerginonmi)
testmi=cbind(testmi,testenergiesnonmi)
names(trainmi)[29]="ennonmi"
names(testmi)[28]="ennonmi"
model=train(trainmi$revenue~.,method="svmLinear",data=trainmi,preProcess="pca")
p3=predict(model,testmi)
p3=as.data.frame(p3)
p=as.data.frame(pred)
for( i in indexOutliers)
{
    p[i,]=max(ptest4[i,],ptest3[i,],plstest[i,],p3[i,])#*1.5 in model 2
}
 

}

 # 1)run  the above  thing  with mention you   must  save  Bogdan  days  feature  and  add  it  to  train  and   test  from the   begining
      # resulting prediction  m1
 # 2)run  the  above thing with mention  that  in  the conditional max  there is  an 1.5 inflation
      # resulting prediction m2
 # 3)run the  first thing   and replace   the conditional   max  svmLinear model with:
 #  modelOutlier2=ksvm(outlier~.,data=newTrain,type="C-bsvc",
 #                 kernel="laplacedot",C=10)	
      #resulting prediction m#
 # 4)run the  first model    but in the  outlier  detection feed in  the  train  the result  from :
           # -model 15
		   # -model 9
		   #- model 10
		 
#outlier=ifelse(train$revenue>CUTH,1,-1)
#newTrain=cbind(train$Days,trainEnergies,ptrain15,ptrain9,ptrain10,outlier )
#newTest=cbind(test$Days,testEnergies,ptest15,ptest9,ptest19)
#names(newTest)[1:5]=c("d","e","m1","m3","m4")
#names(newTrain)[1:5]=c("d","e","m1","m3","m4")
#modelOutlier2=train(newTrain$outlier~.,method="svmLinear",data=newTrain[,-7])
#predictOutliers=predict(modelOutlier2,newTest[,-6])
#indexOutliers=which(predictOutliers=="1")
#p=as.data.frame(pred)
#for( i in indexOutliers)
#{
#    p[i,]=max(ptest15[i,],ptest10[i,],ptest9[i,],p3[i,])#*1.5 in model 2
#}

         #resulting prediction m4
##############################################################################################################################
    # After  that => 3 best models:
#p1=avg(m1,m2,m3,m3,m4)
#p2=avg(p1,p4)
#p3=weighted  average  of  m1,m2,m3,m4 with somking of  ,,z'' score where  zi=(mean(mi)-mean(train$revenue))/sd(train$revenue)
###############################################################################################################################
                                               #final prediction #

											   
		m1=read.table("p1.csv",header=T,sep=",")
	    m2=read.table("p2.csv",header=T,sep=",")
	    m3=read.table("p3.csv",header=T,sep=",")
	 
			  m=ksvm(revenue~.,data=train,type="C-bsvc",
     kernel="rbfdot",C=10)
	 mpredict=predict(m,test)
	 revenue=mpredict
	 revenue=as.data.frame(revenue) 
	 newTrain=cbind(m1[,2],m2[,2],m2[,2],revenue)
	 train=newTrain
	 test=train[,-4]
     				
					
					model=train(train$revenue~.,method="pls",preProcess="pca",data=train,trControl = trainControl(method = "cv"))
					pred=predict(model,test)
					#detect  outliers in     pred with 1)
 
 
 
 
 
Status API Training Shop Blog About Help
© 2015 GitHub, Inc. Terms Privacy Security Contact
