#install.packages("pROC")
#Load libraries
library(pROC)
rm(list=ls())

d=read.csv("C:/MSTM/Course/Study Material/Supply Chain Analytics/Risk and Classification/Supplier_Bankruptcy_Risk.csv", header=TRUE);

colnames(d)

d1=d[,-1]

n=dim(d1)[1];

ind=sample.int(n,size=floor(0.75*n))

train=d1[ind,];
test=d1[-ind,];

m1=glm(Bankruptcy~., data=train, family="binomial");
summary(m1)

p=predict(m1, newdata=test, type="response")

plot(p)

plot(p, test$Bankruptcy, col=test$Bankruptcy+1)

theta=0.5;

p1=ceiling(p-theta)

ContingencyTable=xtabs(~p1+test$Bankruptcy);

print(ContingencyTable)

Accuracy=(ContingencyTable[1,1]+ContingencyTable[2,2])/dim(test)[1];

print(Accuracy)

Sensitivity=ContingencyTable[2,2]/(ContingencyTable[1,2]+ContingencyTable[2,2])

Specificity=ContingencyTable[1,1]/(ContingencyTable[1,1]+ContingencyTable[2,1])

Sensitivity
Specificity

plot(Sensitivity,Specificity)

library(pROC)


roc=roc(test$Bankruptcy, p)

plot(roc)

auc=auc(roc)

print(auc)

length(roc$thresholds)


#Find Optimal Threshold Value
dist = c()
for(i in 1:length(roc$thresholds)){
  m <- sqrt((1-roc$sensitivities[i])^2+(1-roc$specificities[i])^2)
  dist <- c(dist,m)
}

ind <- which(dist == min(dist))
opt_theta <- roc$thresholds[ind]
opt_theta
