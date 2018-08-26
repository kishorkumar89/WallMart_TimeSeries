setwd("E:/GIT_Hub/WallMart")

train_ds = read.csv("all/train.csv",header = TRUE)
store_ds = read.csv("all/stores.csv",header = TRUE)
features_ds = read.csv("all/features.csv" ,header = TRUE)

library(dplyr)

#converting the data frame into local data frame
train_DF <- tbl_df(train_ds)
store.DF <- tbl_df(store_ds)
features.DF <- tbl_df(features_ds)

#removing the Unused data frames
rm(train_ds)
rm(store_ds)
rm(features_ds)
#printing 10 rows of the train data set,
print(train_DF, n = 10)

#the date column seems to be a factor. So we need to convert it into Date
mutate(train_DF, Date = as.Date(Date))
mutate(store.DF, as.Date(Date))
mutate(features.DF, Date = as.Date(Date))
#Joining the tables together.
joined_train_store <- inner_join(train_DF,store.DF ,by = c('Store'))

mutate(joined_train_store, Date = as.Date(Date))

joined_train_store_features <- 
  inner_join(joined_train_store,features.DF ,by = c("Store", "Date", "IsHoliday"))

#converting logicals to Into 1 or 0
train_DF %>%
  mutate(IsHoliday = as.integer(IsHoliday),Date = as.Date(Date))

store.DF %>%
  mutate(Type = labels(store.DF$Type))

mutate(store.DF, Type = as.factor(Type))
labels(store.DF$Type)

