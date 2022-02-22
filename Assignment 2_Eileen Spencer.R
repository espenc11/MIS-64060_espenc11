UniversalBank$ID<-NULL
UniversalBank$ZIP.Code<-NULL
library(caret)
libary(class)
summary(UniversalBank)

dummies <- dummyVars(Personal.Loan ~ ., data = UniversalBank)
UniversalBank_dummy=as.data.frame(predict(dummies, newdata = UniversalBank))
head(UniversalBank_dummy)

Norm_model <- preProcess(UniversalBank_dummy,
                         method = c("center", "scale"))
UniversalBank_norm=predict(Norm_model,UniversalBank_dummy)
summary(UniversalBank_norm)
UniversalBank_norm$Personal.Loan=UniversalBank$Personal.Loan

Train_Index = createDataPartition(UniversalBank$Personal.Loan,p=0.6, list=FALSE)
Train.df=UniversalBank_norm[Train_Index,]
Validation.df=UniversalBank_norm[-Train_Index,]

#Task 1:
To_Predict=data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Education=2, Mortgage=0, Securities.Account=0, CD.Account=0, Online=1, CreditCard=1)
print(To_Predict)

To_Predict_Norm=predict(Norm_model,To_Predict)
print(To_Predict_Norm)

library(caret)
library(class)
Prediction <-knn(train=Train.df[,1:11],
                 test=To_Predict_Norm[,1:11],
                 cl=Train.df$Personal.Loan,
                 k=1)
print(Prediction)
#Customer would be classified as accepting the loan
#Task 2
set.seed(123)
fitControl <- trainControl(method = "repeatedcv",
                           number = 3,
                           repeats = 2)
searchGrid=expand.grid(k=1:10)
Knn.model=train(Personal.Loan~.,
                data=Train.df,
                method='knn',
                tuneGrid=searchGrid,
                trControl = fitControl,)
Knn.model

#Task 3
predictions<-predict(Knn.model,Validation.df)
confusionMatrix(predictions,Validation.df$Personal.Loan)

#Task 4
To_Predict=data.frame(Age=40, Experience=10, Income=84, Family=2, CCAvg=2, Education=2, Mortgage=0, Securities.Account=0, CD.Account=0, Online=1, CreditCard=1)
To_Predict_Norm=predict(Norm_model,To_Predict)
predict(Knn.model,To_Predict_Norm)
#Customer would be classified as not accepting the loan

#Task 5
Train_Index = createDataPartition(UniversalBank,p=0.6, list=FALSE)
Validate_Index = createDataPartition(UniversalBank,p=0.3, list=FALSE)
Test_Index = createDataPartition(UniversalBank,p=0.2, list = FALSE)

Prediction <-knn(train=Train.df[1:11],
                 test=To_Predict_Norm[1:11],
                 cl=Train.df$Personal.Loan,
                 k=3)
print(Prediction)
confusionMatrix(predictions,Validation.df$Personal.Loan)
