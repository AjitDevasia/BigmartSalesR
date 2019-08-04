#Loading Libraries
library(tidyverse)
library(dplyr)
library(DataExplorer)
library(data.table)
library(ggplot2)
library(vcd)
library(rpart)
library(mice)
library(randomForest)
library(plyr)
library(mice)
library(DMwR)
#install.packages("Hmisc")
library(Hmisc)
library(caret)
#install.packages("caretEnsemble")
library(caretEnsemble)



#Reading training and Testing Data
bigmart_train<- read.csv("train.csv",stringsAsFactors = TRUE)
bigmart_test <- read.csv("test.csv")
# Both Train and test datasets have near identical columns except for the predictor outcome Item_Sales

#Size of Datasets
object.size(bigmart_train)/10^6
#0.63 MB of Dataset

#Profile of Datasets
str(bigmart_train)
str(bigmart_test)

#Identifying NAs in both train and test Datasets

bigmart_train %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))

bigmart_test %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.))))
#Item_Weight has 1463 NAs in train and 976 NAs in test data

###--- Cleansing each and every Discrete variable in both datasets

# Starting with Item_Fat_Content
  prop.table(table(bigmart_train$Item_Fat_Content))
  prop.table(table(bigmart_test$Item_Fat_Content))
  levels(bigmart_train$Item_Fat_Content)
  str(bigmart_train$Item_Fat_Content)
  revalue(bigmart_train$Item_Fat_Content,c("LF"="Low Fat", "low fat" = "Low Fat",
                                           "reg" = "Regular")) -> bigmart_train$Item_Fat_Content
  #Cleaning the Test table
  levels(bigmart_test$Item_Fat_Content)
  
  revalue(bigmart_test$Item_Fat_Content,c("LF"="Low Fat", "low fat" = "Low Fat",
                                           "reg" = "Regular")) -> bigmart_test$Item_Fat_Content

  # Both train and test table have Low Fat: Regular in 65:35 split
  ggplot(data=bigmart_train, mapping = aes(x=Item_Fat_Content, fill = Item_Fat_Content)) +
    geom_bar()
  
# Analyzing Item_Type for both Train and test data
  prop.table(table(bigmart_train$Item_Type))
  prop.table(table(bigmart_test$Item_Type))
  levels(bigmart_train$Item_Type)
  levels(bigmart_test$Item_Type)
  ggplot(data=bigmart_train, mapping = aes(x=Item_Type, fill = Item_Type)) +
    geom_bar()
  ggplot(data=bigmart_test, mapping = aes(x=Item_Type, fill = Item_Type)) +
    geom_bar() + facet_grid(~Item_Fat_Content)
  
  #Analyzing Item_Visibility, detecting outliers and replacing them with NAs. Finally replacing
  # the NAs with predictive numbers
  hist(bigmart_train$Item_Visibility)
  boxplot(bigmart_train$Item_Visibility)
  boxplot(bigmart_test$Item_Visibility, main =" Item visibility Test")
  # Outlier detected in the train and test datasets
  
  bigmart_train$Item_Visibility <- ifelse(bigmart_train$Item_Visibility %in% 
                                            boxplot.stats(bigmart_train$Item_Visibility)$out, NA,
                                            bigmart_train$Item_Visibility)
  
  bigmart_test$Item_Visibility <- ifelse(bigmart_test$Item_Visibility %in% 
                                            boxplot.stats(bigmart_test$Item_Visibility)$out, NA,
                                          bigmart_test$Item_Visibility)
  
  #Imputing Outliers
  bigmart_train$Item_Visibility[is.na(bigmart_train$Item_Visibility)] <- mean(bigmart_train$Item_Visibility, na.rm = T)
  
  bigmart_test$Item_Visibility[is.na(bigmart_test$Item_Visibility)] <- mean(bigmart_test$Item_Visibility, na.rm = T)
  
  #Analyzing Outlet Identifier variable
  str(bigmart_train$Outlet_Identifier)
  levels(bigmart_train$Outlet_Identifier)   
  prop.table(table(bigmart_train$Outlet_Identifier))
  q<-ggplot(data=bigmart_train, mapping = aes(x=Outlet_Identifier, fill = Outlet_Identifier)) + geom_bar() 
    q+ theme(axis.text.x= element_text(angle=45, hjust=1)) 
    p<- ggplot(data=bigmart_test, mapping = aes(x=Outlet_Identifier, fill = Outlet_Identifier)) + geom_bar() 
    p + theme(axis.text.x= element_text(angle=45, hjust=1))
    
  #Except for Out10 and out19 every other outlier has the same contribution
    
  # Analyzing the year of establishment
    prop.table(table(bigmart_train$Outlet_Establishment_Year))
    
    p<- ggplot(data=bigmart_test, mapping = aes(x=Outlet_Establishment_Year, fill = Outlet_Establishment_Year)) + geom_bar() 
    p + theme(axis.text.x= element_text(angle=45, hjust=1))
    str(bigmart_train$Outlet_Establishment_Year)
    bigmart_train$Outlet_Establishment_Year <- as.factor(bigmart_train$Outlet_Establishment_Year)
    bigmart_test$Outlet_Establishment_Year <- as.factor(bigmart_test$Outlet_Establishment_Year)
    #Maximum outlets were started in 1985 with minimum being 1998
    
  # Analyzing Outlet size
    str(bigmart_train$Outlet_Size)
    levels(bigmart_train$Outlet_Size)    
    bigmart_train$Outlet_Size <- ifelse(bigmart_train$Outlet_Size== "", NA, bigmart_train$Outlet_Size)
    levels(bigmart_test$Outlet_Size)
    bigmart_test$Outlet_Size <- ifelse(bigmart_test$Outlet_Size== "", NA, bigmart_test$Outlet_Size)
    
  #Imputing NAs in Categorical variable
    
    fit <-  rpart(Outlet_Size ~ Item_Visibility + Item_Fat_Content+Item_MRP,
                  data = bigmart_train[!is.na(bigmart_train$Outlet_Size),],
                  method = "anova")
    bigmart_train$Outlet_Size[is.na(bigmart_train$Outlet_Size)] <-
      predict(fit, bigmart_train[is.na(bigmart_train$Outlet_Size),])
    bigmart_train$Outlet_Size <- as.factor(bigmart_train$Outlet_Size)
    
    prop.table(table(bigmart_train$Outlet_Size))
    revalue(bigmart_train$Outlet_Size,c("2"="High", "3" = "Medium", "3.23818092589563" = "Medium",
                                            "4" = "Small")) -> bigmart_train$Outlet_Size
    
   prop.table(table(bigmart_test$Outlet_Size))
   bigmart_test$Outlet_Size <- as.factor(bigmart_test$Outlet_Size)
   revalue(bigmart_test$Outlet_Size,c("2"="High", "3" = "Medium",
                                       "4" = "Small")) -> bigmart_test$Outlet_Size
   #Medium Size outlets dominate the store size
   
   #Analyzing outlet location type
   prop.table(table(bigmart_train$Outlet_Location_Type))
   prop.table(table(bigmart_test$Outlet_Location_Type))
   
   #Tier 3 contributes maximum of the location types
   
   prop.table(table(bigmart_train$Outlet_Type))
   prop.table(table(bigmart_test$Outlet_Type))
   str(bigmart_train$Outlet_Type) 
  # Supermarket type1 contributes maximum to the store type
   
   # Analyzing Sales figures w.r.to each and every Categorical variable
   ggplot(data=bigmart_train,mapping =aes(x=Item_Fat_Content , y= Item_Outlet_Sales, fill = Item_Fat_Content)) +
     geom_bar(stat = "identity") 
     
    #Sales of low fat is much higher than that of Regular
   ggplot(data=bigmart_train,mapping =aes(x=Item_Visibility , y= Item_Outlet_Sales)) +
     geom_point(stat = "identity") 
   #High visibility doesnt actually convert to high item sales
   
   ggplot(data=bigmart_train,mapping =aes(x= Item_Type , y= Item_Outlet_Sales, fill = Item_Type)) +
     geom_bar(stat = "identity") +
     theme(axis.text.x= element_text(angle=45, hjust=1))
   #Fruits and Vegetables and Snack foods contribute maximum to teh Sales
   ggplot(data=bigmart_train,mapping =aes(x= Outlet_Identifier, y= Item_Outlet_Sales, fill = Outlet_Identifier)) +
     geom_bar(stat = "identity") +
     theme(axis.text.x= element_text(angle=45, hjust=1))
   
   #Outlet 27 is the maximum selling outlet
   ggplot(data=bigmart_train,mapping =aes(x= Outlet_Establishment_Year , y= Item_Outlet_Sales)) +
     geom_bar(stat = "identity") +
     theme(axis.text.x= element_text(angle=45, hjust=1))
   
   #1985 achieved the highest Sales in all years
   ggplot(data=bigmart_train,mapping =aes(x= Outlet_Size , y= Item_Outlet_Sales)) +
     geom_bar(stat = "identity") +
     theme(axis.text.x= element_text(angle=45, hjust=1))
   #Medium size outlet do the maximum business for Bigmart
   ggplot(data=bigmart_train,mapping =aes(x= Outlet_Location_Type , y= Item_Outlet_Sales, fill= Outlet_Location_Type)) +
     geom_bar(stat = "identity") +
     theme(axis.text.x= element_text(angle=45, hjust=1))
  
   #Tier 3 cities produce the maximum sales
   
   ggplot(data=bigmart_train,mapping =aes(x= Outlet_Type, y= Item_Outlet_Sales, fill= Outlet_Type)) +
     geom_bar(stat = "identity") +
     theme(axis.text.x= element_text(angle=45, hjust=1))
   #Supermarket Type 1 contributes maximum Sales
   ggplot(data=bigmart_train,mapping =aes(x= Item_MRP , y= Item_Outlet_Sales)) + geom_point() +
     geom_smooth(method = "lm")
   str(bigmart_train$Item_MRP)
  levels(bigmart_train$Item_Fat_Content)
  levels(bigmart_train$Item_Type)
  levels(bigmart_train$Outlet_Identifier)
  levels(bigmart_train$Outlet_Establishment_Year)
  levels(bigmart_train$Outlet_Size)
  levels(bigmart_train$Outlet_Location_Type)
  levels(bigmart_train$Outlet_Type)
  levels(bigmart_train$Item_Outlet_Sales)
  
  bigmart_train$test <- factor(bigmart_train$Item_Type,levels = unique(c(bigmart_train$Item_Type)))
  str(bigmart_train$test)  
  levels(bigmart_train$test)  
  bigmart_train$test <-as.character(bigmart_train$Item_Type)  
  str(bigmart_train$test)  
  trimws(bigmart_train$test)  
  bigmart_train$test <- as.factor(bigmart_train$test)
  bigmart_train$Item_Type <- bigmart_train$test
  str(bigmart_train$Item_Type)    
  cor(bigmart_train$Item_MRP,bigmart_train$Item_Outlet_Sales)
  bigmart_train <- select(bigmart_train,-c(test))
  # Building model
  summary(big_train_model)
  big_train_model <- lm(Item_Outlet_Sales ~ Item_Fat_Content + Item_Type + 
    Item_MRP + Outlet_Identifier , data = bigmart_train)
  bigmart_sub <- bigmart_train %>%
                      select(-Item_Identifier,-Outlet_Identifier)
  set.seed(366284)
  inTrain <- createDataPartition(y=bigmart_sub$Item_Outlet_Sales,p=0.7,list=FALSE)
  train <- bigmart_sub[inTrain,]
  test <- bigmart_sub[-inTrain,]
  # Building Models 
  control <- trainControl(method = "repeatedcv",number = 10,repeats = 3, savePredictions = TRUE,classProbs = TRUE)
  algorithmList <- c('glm','glmnet','lm','ranger','treebag','gbm')
  models <- caretList(Item_Outlet_Sales~., train,trControl=control,methodList = algorithmList)
  