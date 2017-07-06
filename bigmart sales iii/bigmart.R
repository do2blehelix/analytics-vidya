
########################### IMPORT & EXPLORE ###########################\

#import the datasets
train <- read.csv("train.csv")
test <- read.csv("test.csv")

#create a new variable in test file 
test$Item_Outlet_Sales <- NA

#combine train and test data
combi <- rbind(train, test)





########################### FEATURE ENGINEERING ###########################\

#create a new column for how many years in action and remove the original variable
combi$Year <- 2013 - combi$Outlet_Establishment_Year ; combi$Outlet_Establishment_Year <- NULL

#Item_Identifier
combi$item <- as.factor(substr(combi$Item_Identifier , 0 , 2))
combi$code <- as.integer(substr(combi$Item_Identifier , 4 , 6))

#Fat in Non Consumables
combi$Item_Fat_Content <- ifelse(combi$item == "NC", "No Fat" , as.character(combi$Item_Fat_Content))
combi$Item_Fat_Content <- as.factor(combi$Item_Fat_Content)

#Outlet Identifier
combi$outlet <- as.numeric(substr(combi$Outlet_Identifier, 4 , 6))



########################### DATA MANIPULATION ###########################\

#Item_Fat_Content
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))

#Outlet_Size
levels(combi$Outlet_Size)[1] <- "Other"


#Item_Weight
library(randomForest)
rf <- randomForest(Item_Weight ~ Item_Type + Item_MRP + Item_Visibility + Item_Fat_Content + item + code , 
                   combi[!is.na(combi$Item_Weight),] , importance = TRUE)
varImpPlot(rf)
combi$Item_Weight[is.na(combi$Item_Weight)] <- predict(rf , combi)


#Item_Visibility : replacing 0 values by mean
combi$Item_Visibility[combi$Item_Visibility == 0] <- mean(combi$Item_Visibility, na.rm=TRUE)





summary(combi)



########################### FEATURE ENGINEERING II ###########################\

#Perweightcost
combi$cpw <- combi$Item_Weight / combi$Item_MRP


summary(combi)



#divide data set
train <- combi[1:nrow(train),] ; test <- combi[-(1:nrow(train)),]








########################### REGRESSION ###########################\ 1201

#linear regression
model <- lm(log(Item_Outlet_Sales) ~ Item_Weight + Item_Fat_Content + 
              Item_Visibility + Item_Type + Item_MRP + Outlet_Size + Outlet_Location_Type + 
              Outlet_Type + Year + item + code + cpw + outlet
            , data = train) ; summary(model)
model2 <- step(model , direction = 'both') ; summary(model2)


#plotting the graphs
par(mfrow=c(2,2)) ; plot(model)

#check rmse
library(Metrics) ; rmse(train$Item_Outlet_Sales, exp(model$fitted.values))

#check vif
library(car) ; vif(model2)

#export dataset
test$Item_Outlet_Sales <- predict.lm(model2 , newdata = test)
test$Item_Outlet_Sales <- exp(test$Item_Outlet_Sales)
write.csv(test[c(1,7,11)], file = "OUT.csv" , row.names = FALSE)






########################### DECISION TREE ###########################\ 1174

library(rpart)
model.dt <- rpart(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + 
              Item_Visibility + Item_Type + Item_MRP + Outlet_Size + Outlet_Location_Type + 
              Outlet_Type + Year + item + code + cpw + outlet
                   , data=train )
library(rattle)
fancyRpartPlot(model.dt)


write.csv(data.frame(Item_Identifier = test$Item_Identifier, 
                     Outlet_Identifier = test$Outlet_Identifier,
                     Item_Outlet_Sales = predict(model.dt, test)), "OUT.csv", row.names = FALSE)








########################### RANDOM FOREST ###########################\ 1167

library(randomForest)
model.rf <- randomForest(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + 
                           Item_Visibility + Item_Type + Item_MRP + Outlet_Size + Outlet_Location_Type + 
                           Outlet_Type + Year + item + code + cpw + outlet
                         , data=train, importance=TRUE, ntree=2000)
varImpPlot(model.rf)

write.csv(data.frame(Item_Identifier = test$Item_Identifier, 
                     Outlet_Identifier = test$Outlet_Identifier,
                     Item_Outlet_Sales = predict(model.rf, test)), "OUT.csv", row.names = FALSE)






########################### GRADIENT BOOSTING ###########################\ 1146

library(caret)

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) 

fit <- train(Item_Outlet_Sales ~ Item_Weight + Item_Fat_Content + 
               Item_Visibility + Item_Type + Item_MRP + Outlet_Size + Outlet_Location_Type + 
               Outlet_Type + Year + item + code + cpw + outlet
             , data = train, method = "gbm", trControl = fitControl, verbose = FALSE)

write.csv(data.frame(Item_Identifier = test$Item_Identifier, 
                     Outlet_Identifier = test$Outlet_Identifier,
                     Item_Outlet_Sales = predict(fit, test)), "OUT.csv", row.names = FALSE)




