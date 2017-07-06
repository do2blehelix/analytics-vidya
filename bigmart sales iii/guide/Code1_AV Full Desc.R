
########################### IMPORT ###########################\

#import datasets
train <- read.csv("train.csv")
test <- read.csv("test.csv")


#check summary stats
dim(train) ; dim(test)
str(train)
colSums(is.na(train))   #check for missing values
summary(train)          #full summary stats




########################### PLOT GRAPHS ###########################\

#bivariate visual analysis
library(ggplot2)
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + geom_point(size = 2.5, color="navy") + xlab("Item Visibility") + ylab("Item Outlet Sales") + ggtitle("Item Visibility vs Item Outlet Sales")
ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + geom_bar(stat = "identity", color = "purple") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + ggtitle("Outlets vs Total Sales") + theme_bw()
ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + geom_bar( stat = "identity") +theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + xlab("Item Type") + ylab("Item Outlet Sales")+ggtitle("Item Type vs Sales")
ggplot(train, aes(Item_Type, Item_MRP)) +geom_boxplot() +ggtitle("Box Plot") + theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + xlab("Item Type") + ylab("Item MRP") + ggtitle("Item Type vs Item MRP")





########################### DATA EXPLORATION ###########################\

#check out the summary of test data : very similar to train data
summary(test)

#create a new variable with (DV) in the test dataset and initialize it to 1
test$Item_Outlet_Sales <-  1

#better to combine both datasets so data can be prep-ed together
combi <- rbind(train, test)





########################### DATA PREPARATION ###########################\

#imputing item weight with median
combi$Item_Weight[is.na(combi$Item_Weight)] <- median(combi$Item_Weight, na.rm = TRUE)
table(is.na(combi$Item_Weight))


#considering 0 item visibility as missing and imputing as necessary 
combi$Item_Visibility <- ifelse(combi$Item_Visibility == 0,
                                median(combi$Item_Visibility), combi$Item_Visibility)

#renaming Outlet size blanks to others
levels(combi$Outlet_Size)[1] <- "Other"


#renaming item fat content
library(plyr)
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "reg" = "Regular"))
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content, c("low fat" = "Low Fat"))
table(combi$Item_Fat_Content)





########################### DATA MANIPULATION / FEATURE ENGG ###########################\

#count of outlet identifiers
library(dplyr)
a <- combi %>% group_by(Outlet_Identifier) %>% tally()
head(a)
names(a)[2] <- "Outlet_Count"
combi <- full_join(a, combi, by = "Outlet_Identifier")


#Count of Item Identifiers
b <- combi %>% group_by(Item_Identifier) %>% tally()
head(b)
names(b)[2] <- "Item_Count"
combi <- merge(b, combi, by = "Item_Identifier")


#Outlet Years
c <- combi%>%
  select(Outlet_Establishment_Year)%>% 
  mutate(Outlet_Year = 2013 - combi$Outlet_Establishment_Year)
#combi <- combi <- full_join(c, combi)
combi <- cbind (c[2], combi)



#Item Type New
q <- substr(combi$Item_Identifier,1,2)
unique(q)
q <- gsub("FD","Food",q)
q <- gsub("DR","Drinks",q)
q <- gsub("NC","Non-Consumable",q)
unique(q)
table(q)

combi$Item_Type_New <- q






########################### DATA ENCODING / DUMMY VARIABLES ###########################\

#Converting item fat content char values to integers (label encoding)
combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)

#sample of one hot encoding / flag variable / dummy variable
#-1 tells R, to encode all variables in the data frame, but suppress the intercept. 
#So, what will happen if you don’t write -1 ? model.matrix will skip the first level of the factor, 
#thereby resulting in just 2 out of 3 factor levels (loss of information)
sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
head(demo_sample)



#alternate to model.matrix 
library(dummies)
combi <- dummy.data.frame(combi, names = c('Outlet_Size','Outlet_Location_Type','Outlet_Type', 'Item_Type_New'),  sep='_')
str (combi)


#dropping converted/identifier variables
combi <- select(combi, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content,Outlet_Establishment_Year,Item_Type))
str(combi)





########################### LINEAR REGRESSION ###########################\

#dividing the combi dataset into train and test dataset to run the modeling process
new_train <- combi[1:nrow(train),]
new_test <- combi[-(1:nrow(train)),]


#running the first iteration of multiple regression
linear_model <- lm(Item_Outlet_Sales ~ ., data = new_train)
summary(linear_model)


#R2 is rather low after 1st iteration. Checking correlation
cor(new_train)
cor(new_train$Outlet_Count, new_train$`Outlet_Type_Grocery Store`)


## R2 remains low. Ditching model and starting from scratch >>>>>>>>>
rm(list=ls())
