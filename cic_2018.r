# create training and testing subset

# import libraries
library(ggplot2)
library("caret")



# set working dir
setwd('C:/Users/sreej/Desktop/SDN/SDN_R_Code/CIC2018')

# Start writing to an output file
#sink('cor.txt')

# Clear the workspace
rm(list = ls())

# Load training data
train_raw1 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thuesday-20-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw1$Label, p = 0.2, list = FALSE)
train_raw1 <- train_raw1[inTrain,]

train_raw2 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Wednesday-14-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw2$Label, p = 0.2, list = FALSE)
train_raw2 <- train_raw2[inTrain,]

train_raw3 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Wednesday-21-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw3$Label, p = 0.2, list = FALSE)
train_raw3 <- train_raw3[inTrain,]

train_raw4 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Wednesday-28-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw4$Label, p = 0.2, list = FALSE)
train_raw4 <- train_raw4[inTrain,]

train_raw5 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thursday-22-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw5$Label, p = 0.2, list = FALSE)
train_raw5 <- train_raw5[inTrain,]

train_raw6 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thursday-01-03-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw6$Label, p = 0.2, list = FALSE)
train_raw6 <- train_raw6[inTrain,]

train_raw7 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thursday-15-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw7$Label, p = 0.2, list = FALSE)
train_raw7 <- train_raw7[inTrain,]

train_raw8 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Friday-02-03-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw8$Label, p = 0.2, list = FALSE)
train_raw8 <- train_raw8[inTrain,]

train_raw9 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Friday-16-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw9$Label, p = 0.2, list = FALSE)
train_raw9 <- train_raw9[inTrain,]

train_raw10 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Friday-23-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
inTrain <- createDataPartition(y = train_raw10$Label, p = 0.2, list = FALSE)
train_raw10 <- train_raw10[inTrain,]

train_raw = rbind(train_raw1[, 5:84], train_raw2, train_raw3, train_raw4, train_raw5, train_raw6, train_raw7, train_raw8, train_raw9, train_raw10)

d = dim(train_raw)

# Observe the data
names(train_raw)





# Observe the distribution of labels
sum_label <- aggregate(rep(1, d[1]), by = list(train_raw$Label), FUN = sum)
names(sum_label) <- c("label", "count")
barplot(beside = TRUE, (sum_label$count),
       names.arg = sum_label$label, ylim = c(0,max(sum_label$count)),
       xlab = "Label", ylab = "Count",
       col = "Blue", main = "The distribution of labels")

# print the distribution in numbers
print(sum_label)


# save the full dataset
#write.csv(train_raw, file = "CIC2018_Full.csv", row.names=FALSE)



train_raw = read.csv("dataset/CIC2018_Full.csv", header=T)

library("caret")

inTrain <- createDataPartition(y = train_raw$Label, p = 0.2, list = FALSE)
inTest <- createDataPartition(y = train_raw$Label, p = 0.2, list = FALSE)
testing_subset_train <- train_raw[inTrain,]
testing_subset_test <- train_raw[inTest,]

# save the subset dataset of training and testing
write.csv(testing_subset_train, file = "CIC2018_subset_train.csv", row.names=FALSE)
write.csv(testing_subset_test, file = "CIC2018_subset_test.csv", row.names=FALSE)

d = dim(testing_subset_train)



# Observe the distribution of new subset labels
rm(sum_label)
sum_label <- aggregate(rep(1, d[1]), by = list(testing_subset_train$Label), FUN = sum)
names(sum_label) <- c("label", "count")
barplot(beside = TRUE, (sum_label$count),
       names.arg = sum_label$label, ylim = c(0,max(sum_label$count)),
       xlab = "Label", ylab = "Count",
       col = "Blue", main = "The distribution of labels")

# # print the distribution in numbers
print(sum_label)
