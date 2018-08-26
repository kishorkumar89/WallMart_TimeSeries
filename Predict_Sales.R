setwd("E:/GIT_Hub/WallMart")

train_ds = read.csv("all/train.csv",header = TRUE, stringsAsFactors = TRUE)
store_ds = read.csv("all/stores.csv",header = TRUE, stringsAsFactors = TRUE)

#colnames(train_ds)
#colnames(store_ds)
#Applying inner Join on the stores and Train Data set
library(dplyr)
#converting into Local Data frame.
train_ds <- tbl_df(train_ds)
store_ds <- tbl_df(store_ds)

Join_Table <- inner_join(train_ds, store_ds, by = c("Store"))
join_Table_Back <- Join_Table

length(Join_Table)
#removing the unused Columns
rm(train_ds)
rm(store_ds)
gc()
summary(Join_Table)
head(Join_Table)
#subset_train_ds <- filter(train_ds,Dept == 1)
library(plotly)

#plot_ly(subset_train_ds,x = ~Store,y = ~Weekly_Sales,color = 'IsHoliday')

######################################################################################
#In this para we want to compare the BIC of the Individual Dependtent Variable
#i.e if by adding a column does it Increse or decrese , If it Decreses it perfect
#else we would leave that variable
######################################################################################
colnames(Join_Table)
BIC(lm(Join_Table$Weekly_Sales~1))                    #9653600
BIC(lm(Join_Table$Weekly_Sales~Join_Table$IsHoliday)) #9653544
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Date))      #9652822
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Dept))      #9644272
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Type))      #9638376
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Store))     #9650542
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Size))      #9627773

#scaling all the variables of a Dsata Frame...
for (i in 1:length(Join_Table)) {
  if (i == 1 | i == 2 | i == 4 | i == 7) {
  Join_Table[,i] <- scale(Join_Table[,i])
  }
  if (i == 3) {
    Join_Table$Date = as.Date(Join_Table$Date)
    Join_Table$Year <- format(as.POSIXct(strptime(Join_Table$Date,"%Y-%m-%d",tz = "")) ,format = "%Y")
    Join_Table$Month <- format(as.POSIXct(strptime(Join_Table$Date,"%Y-%m-%d",tz = "")) ,format = "%m")
    Join_Table$Day <- format(as.POSIXct(strptime(Join_Table$Date,"%Y-%m-%d",tz = "")) ,format = "%d")
  } 
  if (i == 5) {
    Join_Table$IsHoliday <- as.numeric(Join_Table$IsHoliday)
    Join_Table[,i] <- scale(Join_Table[,i])
  }
  if (i == 6) {
    Join_Table$Type <- as.numeric(Join_Table$Type)
    Join_Table[,i] <- scale(Join_Table[,i])
  }
}
rm(i)
# checking the BIC values afeter scaling...
BIC(lm(Join_Table$Weekly_Sales~1))                    #1196389
BIC(lm(Join_Table$Weekly_Sales~Join_Table$IsHoliday)) #1196333
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Date))      #1195611
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Dept))      #1187061
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Type))      #1181165
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Store))     #1193331
BIC(lm(Join_Table$Weekly_Sales~Join_Table$Size))      #1170563

#choosing the cobination of Dependent Variables.
BIC(lm(Join_Table$Weekly_Sales~Join_Table$IsHoliday+ leaps
         Join_Table$Date))                            #1195611
BIC(lm(Join_Table$Weekly_Sales~Join_Table$IsHoliday+
         Join_Table$Dept))                            #1187005
BIC(lm(Join_Table$Weekly_Sales~Join_Table$IsHoliday+
         Join_Table$Type))                            #1181108

Model <- lm(Join_Table$Weekly_Sales~.,data = Join_Table)
summary(Model)
Model$residuals

# which subset should we select to mimimize tge BIC 
library(leaps)
subset_Model  <- regsubsets(Weekly_Sales~.,data = Join_Table,nbest = 2,nvmax = 5,really.big = FALSE)

summary(subset_Model)
plot(subset_Model,scale = 'bic')



#step up and down linear models....
empty_Model <- lm(Weekly_Sales~1,data = Join_Table)
Full_Model <- lm(Weekly_Sales~.,data = Join_Table)

#Backward elimination Model
step_Model <- step(Full_Model,direction = 'backward')

#Forward selection Model
forward_Model <- step(empty_Model , scope = list(lower = empty_Model, upper = Full_Model),direction = 'forward')

#random Forest Implementation 
require(randomForest)

#removing the categorical Variables
summary(Join_Table)
str(Join_Table)
Join_Table$Date <- as.Date(Join_Table$Date)
Join_Table$Date <- NULL
Join_Table$IsHoliday <- NULL
gc()
memory.limit(size = 50000)
memory.limit()
randomForest_Model <- randomForest(Weekly_Sales~.,data = Join_Table)

#---------------------------------------------------------
#checking which columns are factors
str(Join_Table)
is.data.frame(Join_Table)

Join_Table$Date <- as.Date(Join_Table$Date)

Join_Table$Type <- `levels<-.factor`(Join_Table$Type,c(1,2,3))
levels(Join_Table$Type)

#scaling the int variables
Join_Table$Store <- scale(Join_Table$Store) 
Join_Table$Dept <- scale(Join_Table$Dept)
Join_Table$Weekly_Sales <- scale(Join_Table$Weekly_Sales)
Join_Table$Type <- scale(as.numeric(Join_Table$Type))
Join_Table$Type <- scale(Join_Table$Size)
Join_Table$IsHoliday <- scale(as.numeric(Join_Table$IsHoliday))



head(Join_Table)

#applying the Forward backward Elimination Method Again
empty_Model <- lm(Weekly_Sales~1,data = Join_Table)
Full_Model <- lm(Weekly_Sales~.,data = Join_Table)

#Backward
step_Model <- step(Full_Model,direction = 'backward')
#Forward
forward_Model <- step(empty_Model , scope = list(lower = empty_Model, upper = Full_Model),direction = 'forward')

#predicting the Future Sales.
train_ds <- train_ds[,c(-5)]
is.data.frame(train_ds)
train_ds$Date <- as.Date(train_ds$Date)

empty_Model <- lm(Weekly_Sales~1,data = train_ds)
Full_Model <- lm(Weekly_Sales~.,data = train_ds)
step_Model <- step(Full_Model,direction = 'backward')

summary(step_Model)
test <- read.csv('all/sampleSubmission.csv',header = TRUE)
test

predicted_Values <- predict.glm(Full_Model, newdata = train_ds[,c(-4)], interval = "prediction")


#Store      Dept       Date Weekly_Sales  IsHoliday       Type      Size Year Month Day
#Store + Dept + IsHoliday + Type + Size + Year + Month + Day
library(DMwR)
predicted_Values <- predict( Full_Model , newdata = Join_Table_Predict )
Row_Number <- c(1:length(predicted_Values))
plot_values <- data.frame(Row_Number, predicted_Values,Join_Table[4])


