
########################### LINEAR REGRESSION ###########################\

#load data
 train <- read.csv("train.csv")
 test <- read.csv("test.csv")

#create a new variable in test file 
 test$Item_Outlet_Sales <- 1

#combine train and test data
 combi <- rbind(train, test)

#impute missing value in Item_Weight
 combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)

#impute 0 in item_visibility
 combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility), combi$Item_Visibility)

#rename level in Outlet_Size
 levels(combi$Outlet_Size)[1] <- "Other"

#rename levels of Item_Fat_Content
 library(plyr)
 combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,c("LF" = "Low Fat", "reg" = "Regular"))
 combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))

#create a new column 2013 - Year
 combi$Year <- 2013 - combi$Outlet_Establishment_Year

#drop variables not required in modeling
 library(dplyr)
 combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Outlet_Establishment_Year))

#divide data set
 new_train <- combi[1:nrow(train),]
 new_test <- combi[-(1:nrow(train)),]

#linear regression
 linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
 summary(linear_model)
 
#regression plots
 par(mfrow=c(2,2))
 plot(linear_model)
 
#model is suffering from heteroskedasticy (residuals vs fitted plot has pattern)
#A common practice to tackle heteroskedasticity is by taking the log of response variable.
 linear_model <- lm(log(Item_Outlet_Sales) ~ ., data = new_train)
 summary(linear_model)

#calculating RMSE
 library(Metrics)
 rmse(new_train$Item_Outlet_Sales, exp(linear_model$fitted.values))
 
 
 
 
 
 
 
 
 
 
 
 
########################### DECISION TREE ###########################\
 
 
#loading required libraries
  library(rpart)
  library(e1071)
  library(rpart.plot)
  library(caret)
 
#setting the tree control parameters
  fitControl <- trainControl(method = "cv", number = 5)
  cartGrid <- expand.grid(.cp=(1:50)*0.01)
 
#decision tree
  tree_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
  print(tree_model)
  
#The final value for cp = 0.01. You can also check the table populated in console for more information. 
#The model with cp = 0.01 has the least RMSE. Let’s now build a decision tree with 0.01 as complexity parameter.
  main_tree <- rpart(Item_Outlet_Sales ~ ., data = new_train, control = rpart.control(cp=0.01))
  prp(main_tree)

#checking RMSE
  pre_score <- predict(main_tree, type = "vector")
  rmse(new_train$Item_Outlet_Sales, pre_score)
  
  
  
  
  
  
  
  
  
########################### RANDOM FOREST ###########################\

#load randomForest library
  library(randomForest)
  
#set tuning parameters
 control <- trainControl(method = "cv", number = 5)
  
#random forest model
 rf_model <- train(Item_Outlet_Sales ~ ., data = new_train, method = "parRF", trControl = control, prox = TRUE, allowParallel = TRUE)
  
#check optimal parameters
 print(rf_model)
  
#random forest model
 forest_model <- randomForest(Item_Outlet_Sales ~ ., data = new_train, mtry = 15, ntree = 1000)
 print(forest_model)
 varImpPlot(forest_model)  
  
 main_predict <- predict(main_tree, newdata = new_test, type = "vector")
 sub_file <- data.frame(Item_Identifier = test$Item_Identifier, Outlet_Identifier = test$Outlet_Identifier,       Item_Outlet_Sales = main_predict)
 write.csv(sub_file, 'Decision_tree_sales.csv')
 
 
