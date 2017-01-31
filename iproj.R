Fires<- read.table("C:/I/ds/forestfires.csv", header=TRUE, sep ="," )
View(Fires)


library(ISLR)
library(MASS)
attach(Fires)

lm.fit<-lm(ln_area~wind)
lm.fit
summary(lm.fit)

lm.fit.mult<-lm(ln_area~wind+temp+RH+rain)
summary(lm.fit.mult)

lm.fit.mult<-lm(ln_area~wind+temp+RH+rain+month+day+X+Y)
summary(lm.fit.mult)

Fires$ln_area<-log(area+1)
Fires$ln_area<- round(Fires$ln_area,1)

air<- read.table("C:/I/ds/train_users_2/train_users_2.csv", header=TRUE, sep ="," )
View(air)
------------------------------------------------------------------------------------------------
acute<- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt", sep ="," )
View(acute)
summary(acute)
attach(acute)

colnames(acute)<- c("Variance", "Skewness", "Curtosis", "Entropy", "Class")
m.Variance<- glm(Class ~ Variance,family = binomial)
summary(m.Variance)

m<- glm(Class~Variance+Skewness+Curtosis+Entropy,family=binomial)
summary(m)

pred.probs<- predict(m,type = "response")
length(pred.probs)
pred.default<-rep("No",1372)
pred.default[pred.probs > 0.5]<- "Yes"

table(pred.default)

confusion.matrix<- table(Class,pred.default)
addmargins(confusion.matrix)
print(addmargins(confusion.matrix))

-----------------------------------------------------------------------------------------------
bank<- read.table("C:/I/ds/bank/bank.csv", header=TRUE, sep =";" )
View(bank)
summary(bank)
attach(bank)

m<- glm(y ~ age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,family=binomial)
summary(m)

pred.probs<- predict(m,type = "response")
length(pred.probs)
pred.default<-rep("No",4521)
pred.default[pred.probs > 0.5]<- "Yes"

table(pred.default)

confusion.matrix<- table(y,pred.default)
addmargins(confusion.matrix)
print(addmargins(confusion.matrix))

bank1<- read.table("C:/I/ds/bank/bank-full.csv", header=TRUE, sep =";" )
View(bank1)
summary(bank1)
attach(bank1)

m<- glm(y ~ age+job+marital+education+default+balance+housing+loan+contact+day+month+duration+campaign+pdays+previous+poutcome,family=binomial)
summary(m)

pred.probs<- predict(m,type = "response")
length(pred.probs)
pred.default<-rep("No",45211)
pred.default[pred.probs > 0.5]<- "Yes"

table(pred.default)

confusion.matrix<- table(y,pred.default)
addmargins(confusion.matrix)
print(addmargins(confusion.matrix))

bankadd<- read.table("C:/I/ds/bank-additional/bank-additional/bank-additional.csv", header=TRUE, sep =";" )
View(bankadd)
summary(bankadd)
attach(bankadd)

m<- glm(y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed,family=binomial)
summary(m)

pred.probs<- predict(m,type = "response")
length(pred.probs)
pred.default<-rep("No",4119)
pred.default[pred.probs > 0.5]<- "Yes"

table(pred.default)

confusion.matrix<- table(y,pred.default)
addmargins(confusion.matrix)
print(addmargins(confusion.matrix))

bankadd1<- read.table("C:/I/ds/bank-additional/bank-additional/bank-additional-full.csv", header=TRUE, sep =";" )
View(bankadd1)
summary(bankadd1)
attach(bankadd1)

m<- glm(y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+duration+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx+euribor3m+nr.employed,family=binomial)
summary(m)

pred.probs<- predict(m,type = "response")
length(pred.probs)
pred.default<-rep("No",41188)
pred.default[pred.probs > 0.5]<- "Yes"

table(pred.default)

confusion.matrix<- table(y,pred.default)
addmargins(confusion.matrix)
print(addmargins(confusion.matrix))
