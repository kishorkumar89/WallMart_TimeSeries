gc()
#setting the Working directory.
setwd("E:/WallMart_TimeSeries-master")

#importing Libraries
library(dplyr)    #loading the 'dplyr' library into the working directory for data transformation
library(caret)    # Handling the missing Values.
library(lattice)
library(ggplot2)  # Visualization
library(RANN)
library(stats)
library(plotly)


#importing the data set.
store.DS    <- read.csv("all/stores.csv")
train.DS    <- read.csv('all/train.csv')
features.DS <- read.csv('all/features.csv')

#converting Data Frames into Local data frame.
train.DF    <- tbl_df(train.DS)
store.Df    <- tbl_df(store.DS)
features.DF <- tbl_df(features.DS)

# removing the data frames from the working directory.
rm(train.DS,store.DS,features.DS)
colnames(store.Df)
colnames(train.DF)

#appying inner_join from the dplyr to the tables store.Df,train.DF
#on the column Store.
joined.DF <- inner_join(store.Df,train.DF)
colnames(joined.DF)
colnames(features.DF)

#formating the Date column of both the tables what R understands
#making it compitable for inner join.
joined.DF$Date   <- as.Date(joined.DF$Date   ,format = "%Y-%m-%d")
features.DF$Date <- as.Date(features.DF$Date ,format = "%d-%b-%Y")

#appying inner_join from the dplyr to the tables joined.DF and the features.DF
joined.DF <- inner_join(joined.DF,features.DF)
joined.DF.BKP <- joined.DF # Creating a backup File

#removing all the unused tables.
rm(features.DF,store.Df,train.DF)
###############################################################################

#counting number of NAs in the data set if there is any.
sapply(joined.DF, function(x) sum(is.na(x)))

# Skipping all rows where we see NAs
joined.DF <- select(joined.DF, -MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

# Aggeregating the data glabally.

global_Sales <- 
joined.DF%>%
  group_by(Date)%>%
  summarise(Sales = sum(Weekly_Sales))

global_Sales.uni <- global_Sales[,2]

global_Sales.ts <- ts(global_Sales.uni, frequency = 56.25)
global_Sales.ts <- ts(as.vector(global_Sales.ts), frequency = 56)
decompose <- stl(global_Sales.ts, s.window = 56)
decompose <- as.matrix(decompose)
decom <- as.data.frame(decompose$time.series)
decom$Date <- global_Sales$Date
decom$Data <- global_Sales$Sales

p1 <- plot_ly(data = decom, x=~Date, y=~Data, type = 'scatter', mode = 'line')
p2 <- plot_ly(data = decom, x=~Date, y=~seasonal, type = 'scatter', mode = 'line')
p3 <- plot_ly(data = decom, x=~Date, y=~trend, type = 'scatter', mode = 'line')
p4 <- plot_ly(data = decom, x=~Date, y=~remainder, type = 'scatter', mode = 'line')

subplot(p1,p2,p3,p4, nrows = 4)


train <- head(global_Sales, n=123)
test  <- tail(global_Sales, n=20)






# incresing the memory space to R
memory.limit(size = 1024000)


#creating dummy Data set.
dummy.joined.DF <- dummyVars(~ ., data = joined.DF, fullRank = TRUE)
train.dummy     <- predict(dummy.joined.DF, joined.DF)

#checking wherther the number of NAs are same in the original table as well as the the table created by dummyVars.
sum(is.na(joined.DF))
sum(is.na(train.dummy))

#Imputating all the NAs
# pre.process  <- preProcess(train.dummy, method = "knnImpute", k = 5)
# imputed.data <- predict(pre.process,newdata = train.dummy)
# colnames(joined.DF)
# sapply(joined.DF, class)
# preProcValues <- preProcess(joined.DF, method = "knnImpute")
# train_processed <- predict(preProcValues, joined.DF)
# sum(is.na(train_processed))



