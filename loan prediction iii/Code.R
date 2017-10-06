
########### IMPORTING #####################

#import files and join them
train <- read.csv("train.csv") ; test <- read.csv("test.csv")
test$Loan_Status <-NA
combi <- rbind(train, test)
summary(combi)







########### FEATURE ENGINEERING ##########

combi$income <- combi$ApplicantIncome + combi$CoapplicantIncome





########### TREATING VARIABLES ###########

#MARRIED : missing married means not married
levels(combi$Married)[1] <- "No"
summary(combi)


#Dependents : missing dependents means none
levels(combi$Dependents)[1] <- 0


#Self_Employed : missing self employed means not self employed
levels(combi$Self_Employed)[1] <- "No"


#LOANAMOUNT : predict using linear model
combi$LoanAmount[is.na(combi$LoanAmount)] <- predict(lm(LoanAmount ~ Married + Education + ApplicantIncome + 
     CoapplicantIncome + Loan_Amount_Term + Credit_History + Property_Area , combi), combi)
combi$LoanAmount[is.na(combi$LoanAmount)] <- predict(lm(LoanAmount ~ Married + Education + ApplicantIncome + 
    CoapplicantIncome + Loan_Amount_Term + Credit_History + Property_Area , combi), combi)



#Loan_Amount_Term : Using most frequented terms
combi$Loan_Amount_Term[is.na(combi$Loan_Amount_Term)] <- median(combi$Loan_Amount_Term, na.rm = TRUE)


#CREDIT HISTORY : 

      # bad history : loan approved #
      #max loan amount : 
      max(subset(combi , Loan_Status=="Y" & Credit_History == 0 & ApplicantIncome < 10000)[9])
      #min income : 
      min(subset(combi , Loan_Status=="Y" & Credit_History == 0)[14])
      
      #good history : loan not approved #
      #min loan amount : 
      min(subset(combi , Loan_Status=="N" & Credit_History == 1)[9])
      #max income : 
      max(subset(combi , Loan_Status=="N" & Credit_History == 1)[14])

      #concluding : Hist=0 & Loan=Y :: Gender=M / Edu = Grad / Selfemp = N / income > 4900(M) | 10K(F)
      subset(combi, Credit_History == 0 & Loan_Status == "Y")

      #Loan=Y CH=0 Amt>4900
      combi[is.na(combi$Credit_History) & combi$Education == "Not Graduate" & combi$Self_Employed == "Yes" 
            & combi$income < 4900 ,] 

            
#minimum total income for loan status to be Y for both good and bad credit hist
aggregate(combi$income ~ combi$Credit_History + combi$Loan_Status, combi, FUN = "min")[c(3,4),]

#imputing the credit history values based on total income (4900)
combi$Credit_History[combi$income > 4900 & is.na(combi$Credit_History)] <- 1
combi$Credit_History[combi$income < 4900 & is.na(combi$Credit_History)] <- 0



#Gender : using decision tree
# library(rpart)
# combi$Gender[combi$Gender == ""] <- predict(rpart(Gender ~ Married + Dependents + Education + Self_Employed
#               + ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + 
#               Property_Area , data = combi , method = "class") 
#             , combi, type="class")

#gender : using mode
levels(combi$Gender)[1] <- "Male"





summary(combi)




########### FEATURE ENGINEERING II ##########

combi$emi <- combi$LoanAmount / combi$Loan_Amount_Term








########### DIVIDING DATASETS BACK ###########

train <- combi[!is.na(combi$Loan_Status),]
test <- combi[is.na(combi$Loan_Status),]
test$Loan_Status <- NULL







########### LOGISTIC REGRESSION ###########

model <- glm(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + 
               ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + 
               Property_Area + emi
             , train , family=binomial("logit"))
summary(model)

train$predicted <- predict(model, train, "response")
table(Actual = train$Loan_Status , Predicted = train$predicted > 0.58)

test$Loan_Status <- predict(model, test, "response")
test$Loan_Status <- ifelse(test$Loan_Status > 0.58, "Y" , "N")

write.csv(data.frame(Loan_ID = test$Loan_ID, Loan_Status = test$Loan_Status) , "out.csv" , row.names = FALSE)








########### RANDOM FOREST ###########

library(randomForest)

model.rf <- randomForest(as.factor(Loan_Status) ~ Gender + Married + Dependents + Education + Self_Employed +
                           ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + 
                           Credit_History + Property_Area + emi
                         , data=train, importance=TRUE, ntree=2000)
varImpPlot(model.rf)

Predicted.rf <- predict(model.rf, test)
write.csv(data.frame(Loan_ID = test$Loan_ID, Loan_Status = Predicted.rf), "out.csv", row.names = FALSE)









########### DECISION TREE ###########

library(rpart)
model.dt <- rpart(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + 
                    ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + 
                    Property_Area + emi
                     , data=train, method="class")
library(rattle)
fancyRpartPlot(model.dt)

Predicted.dt <- predict(model.dt, test, type = "class")
write.csv(data.frame(Loan_ID = test$Loan_ID, Loan_Status = Predicted.dt), "out.csv", row.names = FALSE)






########### SVM ###########

library(e1071)    

model.sv <- sv(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + 
                   ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + 
                   Property_Area , data=train)
summary(model.sv)

Predicted.sv <- predict(model.sv, test)
write.csv(data.frame(Loan_ID = test$Loan_ID, Loan_Status = Predicted.sv), "out.csv", row.names = FALSE)





########### NAIVE BAYES ###########

library(e1071)    

model.nb <- naiveBayes(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + 
                   ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History + 
                   Property_Area , data=train)
summary(model.nb)

Predicted.nb <- predict(model.nb, test)
write.csv(data.frame(Loan_ID = test$Loan_ID, Loan_Status = Predicted.nb), "out.csv", row.names = FALSE)






########### GBM ###########

library(caret)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) 

model.gb <- train(Loan_Status ~ Gender + Married + Dependents + Education + Self_Employed + 
                        ApplicantIncome + CoapplicantIncome + LoanAmount + Loan_Amount_Term + Credit_History
                      + Property_Area + emi
             , data = train, method = "gbm", trControl = fitControl,verbose = FALSE)

Predicted.gb <- predict(model.gb, test, type = "prob")[,2]
Predicted.gb <- ifelse(Predicted.gb > 0.60, "Y" , "N")

write.csv(data.frame(Loan_ID = test$Loan_ID, Loan_Status = Predicted.gb), "out.csv", row.names = FALSE)



