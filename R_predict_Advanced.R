gc()
#setting the Working directory.
setwd("E:/GIT_Hub/WallMart")

#importing the data set.
store.DS <- read.csv("all/stores.csv")
train.DS <- read.csv('all/train.csv')
features.DS <- read.csv('all/features.csv')


#loading the 'dplyr' library into the working directory for data transformation
library(dplyr)
#converting Data Frames into Local data frame.
train.DF <- tbl_df(train.DS)
store.Df <- tbl_df(store.DS)
features.DF <- tbl_df(features.DS)
# removing the data frames from the working directory.
rm(train.DS,store.DS,features.DS)
colnames(store.Df)
colnames(train.DF)

#appying inner_join from the dplyr to the tables store.Df,train.DF
#on the column Store.
joined.DF <- inner_join(store.Df,train.DF,by = ('Store'))
colnames(joined.DF)
colnames(features.DF)

#formating the Date column of both the tables what R understands
#making it compitable for inner join.
joined.DF$Date <- as.Date(joined.DF$Date, format = "%Y-%m-%d")
features.DF$Date <- as.Date(features.DF$Date,format = "%d-%b-%Y")

#appying inner_join from the dplyr to the tables joined.DF and the features.DF
joined.DF <- inner_join(joined.DF,features.DF,
                          by = c('Store','Date','IsHoliday'))

#removing all the unused tables.
rm(features.DF,store.Df,train.DF)
###############################################################################

#counting number of NAs in the data set if there is any.
sum(is.na(joined.DF))

##Handling the missing Values.
library(caret)
library(lattice)
library(ggplot2)

# incresing the memory space to R
memory.limit(size = 1024000)

#creating dummy Data set.
dummy.joined.DF <- dummyVars(~ ., data = joined.DF)
train.dummy <- predict(dummy.joined.DF, joined.DF)

#checking wherther the number of NAs are same in the original table as well as 
#the the table created by dummyVars.
sum(is.na(joined.DF))
sum(is.na(train.dummy))






#Imputating all the NAs
pre.process <- preProcess(train.dummy, method = "bagImpute")


imputed.data <- predict(pre.process,newdata = train.dummy)

View(imputed.data)

colnames(joined.DF)
sapply(joined.DF, class)
preProcValues <- preProcess(joined.DF, method = "knnImpute")

train_processed <- predict(preProcValues, joined.DF)

sum(is.na(train_processed))
