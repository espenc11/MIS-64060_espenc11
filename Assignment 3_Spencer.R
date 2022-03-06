str(UniversalBank)
library(caret)
library(class)
library(ISLR)

#Task A
Train_Index = createDataPartition(UniversalBank$Personal.Loan, p=0.6, list=FALSE)
Train.df=UniversalBank[Train_Index,]
Validation.df=UniversalBank[-Train_Index,]
mytable <- xtabs(~ CreditCard+Online+Personal.Loan, data=Train.df)
ftable(mytable)

#Task B
#probability of a customer to accept a loan offer who already has a credit card and is online is 0.094 (51/51+490)

#Task C
table(Online=Train.df$Online, Personal.Loan=Train.df$Personal.Loan)
table(CreditCard=Train.df$CreditCard, Personal.Loan=Train.df$Personal.Loan)

#Task D
library(e1071)
#i P(CC=1 | Loan=1) (83/83+207) = 0.29 is the probability that someone with a credit card will accept the loan offer
#ii P(Online=1 | Loan=1) (175/175+115) = 0.60 is the probability that someone with online banking will accept the loan offer
#iii P(Loan=1 | proportion of loan accepters) (207+83)/(207+83+1906+804) = 0.97 is the probability loans accepted given loan acceptors
#iv P(CC=1 | Loan=0) (804/804+1906) = .297 is the probability that someone with a credit card will decline the loan offer
#v P(Online=1 | Loan=0) (1618/1618+1092) = 0.597 is the probability that someone with an online account will decline the loan offer
#vi P(Loan=0) (2710/2710+290) = 0.90 is the probability of a loan being declined

#Task E
nb_model <- naiveBayes(Personal.Loan~CreditCard+Online, data=Train.df)
Predicted_test_labels <- predict(nb_model,Train.df)
#P(Loan=1 | CC=1, Online=1) = 0.29*0.60 = 0.89 probability that the loan is accepted given they have a credit card and online account

#Task F
#This is higher than the prediction of 0.094 above, since it assumes all variables are independent. The pivot table is more accurate than the Naive Bayes

#Task G
#see naive bayes run above
# The entries in the table that are needed to compute P(Loan=1 | CC=1, Online=1) is the 490 declines and 51 acceptances
To_Predict=data.frame(CreditCard=1, Online=1)
predict(nb_model,To_Predict, type='raw')
#The value is 0.093 is very close to the original prediction from the pivot table of 0.094.