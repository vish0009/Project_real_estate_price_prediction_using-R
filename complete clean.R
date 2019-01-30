library(ggplot2)
library(tm)
data<- read.csv("data/train.csv", stringsAsFactors = FALSE)

str(data)

#checking columns and visualizing

table(is.na(data))
plot(table(data$area_type)) # catagorical values
plot(table(data$availability)) # large number of ready to move
plot(table(data$location))
plot(table(data$size))# most columns seems to lie between 4-7 bhk
plot(table(data$society)) # data seems to have large no of missing values
plot(table(data$total_sqft))
plot(table(data$bath))# large no of house ahve 3 bath
plot(table(data$price)) # seems to have normal destribution wiht positive skew


# there are only 4 catogerical  values in area_type column we can asign 1,2,3,4 to each catogarical value
data$area_type=lapply(data$area_type , function(a)if (a=="Carpet  Area"){
  return(1)}
  else if (a=="Built-up  Area"){
    return(2)}
  else if (a=="Plot  Area"){
    return(3)}
  else if (a=="Super built-up  Area"){
    return(4)}) 
data$area_type=as.numeric(data$area_type)

boxplot(data[,1])

# we can change availability to ready to move or not ready
# applying steming , removing numbers 
data_corpus=VCorpus(VectorSource(data$availability ))
data_corpus=tm_map(data_corpus,stemDocument)
data_corpus=tm_map(data_corpus,content_transformer(tolower))
data_corpus=tm_map(data_corpus,removePunctuation)
data_corpus=tm_map(data_corpus,removeNumbers)
as.character( data_corpus[[1]])
dictCorpus<- lapply(data_corpus, scan_tokenizer)

myDf <- data.frame(text = sapply(dictCorpus, paste, collapse = " "), stringsAsFactors = FALSE)

data$availability=myDf$text
unique(data$availability)

# replace all immediat possess to readi to move
data$availability=sub("immediat possess","readi to move",data$availability)
data$availability=sub("readi to move","ready to move",data$availability)
unique(data$availability)

# assigning 0 if it unavailable and 1 if its ready to move
data$availability=lapply(data$availability , function(a)if (a=="ready to move"){
  return(1)}
  else{
    return(0)}) 

data$availability=as.numeric(data$availability)
boxplot(data[2])

str(data)

# cleaning data$size column
# by removing strings so only numeric values exist
data2_corpus=VCorpus(VectorSource(data$size))
data2_corpus=tm_map(data2_corpus,stemDocument)
data2_corpus=tm_map(data2_corpus,content_transformer(tolower))
data2_corpus=tm_map(data2_corpus,removePunctuation)
data2_corpus=tm_map(data2_corpus,removeWords,"bhk")
data2_corpus=tm_map(data2_corpus,removeWords,"bedroom")
data2_corpus=tm_map(data2_corpus,removeWords,"rk")

as.character( data2_corpus[[1]])
dictCorpus<- lapply(data2_corpus, scan_tokenizer)

myDf <- data.frame(text = sapply(dictCorpus, paste, collapse = " "), stringsAsFactors = FALSE)
data$size=as.numeric(myDf$text)
#verfiing na values in the rows
sum(is.na(data$balcony))

str(data)
# converting total sqrft coloum to numeric attributes
data$total_sqft=as.numeric(data$total_sqft)
sum(is.na(data$total_sqft))


# basic tect cleaning on location data to get least number of factors
unique(data$location)

data_corpus=VCorpus(VectorSource(data$location))
data_corpus=tm_map(data_corpus,stemDocument)
data_corpus=tm_map(data_corpus,content_transformer(tolower))
data_corpus=tm_map(data_corpus,removePunctuation)
data_corpus=tm_map(data_corpus,removeNumbers)
as.character( data_corpus[[1]])
dictCorpus<- lapply(data_corpus, scan_tokenizer)

myDf <- data.frame(text = sapply(dictCorpus, paste, collapse = " "), stringsAsFactors = FALSE)
data$location=myDf$text

str(data)
# using label encoder method on location data to give each location unique number
data$location=as.numeric(factor(data$location))
str(data)
table(data$location)



# society column
factor(data$society)
sum(unique(table(data$society)))
sum(table(data$society))
missingvalues=sum(unique(table(data$society)))/sum(table(data$society)) #.503
missingvalues
# droping societ column since 50% of the column has missing values
# creating train data with all but society column
temp<- c("area_type","availability","location","size","total_sqft","bath","balcony","price")

finaldata=na.exclude(data[temp])
table(is.na(finaldata)) # seeing weather we have any NA values

table()
#split test and train data
library(caret)
temp_part=createDataPartition(finaldata$price,p=0.8,list = FALSE)
data_train=finaldata[temp_part,]
str(data_train)
data_test=finaldata[-temp_part,]
str(data_test)
data_temp=data_test[1:7]

#applying algorithm
library(caret)
library(e1071)
library(C50)
library(gbm)
model=train(price~.,data=data_train,method="lm")
predection=predict(model,data_temp)

# evaluation
# total deviation from actual results
deviation=sum(data_test$price)/(sum(predection))
deviation
plot(predection,type = "l")
plot(data_test$price,type = "l")
tempdf=data.frame(data_test$price,predection)
tempdf

write.csv(tempdf,"C:/Users/vish/Desktop/viz.csv")
