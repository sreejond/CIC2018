# create training and testing subset

# install.packages("lattice", repos = "http://cran.r-project.org")
# install.packages("gtools", repos = "http://cran.r-project.org")
# install.packages("caTools", repos = "http://cran.r-project.org")
# install.packages("DMwR")

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
train_raw1 = train_raw1[!train_raw1$Label == "Label",]
train_raw1 = downSample(train_raw1, factor(train_raw1$Label))

train_raw2 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Wednesday-14-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw2 = train_raw2[!train_raw2$Label == "Label",]
train_raw2 = downSample(train_raw2, factor(train_raw2$Label))

train_raw3 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Wednesday-21-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw3 = train_raw3[!train_raw3$Label == "Label",]
train_raw3 = downSample(train_raw3, factor(train_raw3$Label))

train_raw4 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Wednesday-28-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw4 = train_raw4[!train_raw4$Label == "Label",]
train_raw4 = downSample(train_raw4, factor(train_raw4$Label))

train_raw5 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thursday-22-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw5 = train_raw5[!train_raw5$Label == "Label",]
train_raw5 = downSample(train_raw5, factor(train_raw5$Label))

train_raw6 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thursday-01-03-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw6 = train_raw6[!train_raw6$Label == "Label",]
train_raw6 = downSample(train_raw6, factor(train_raw6$Label))

train_raw7 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Thursday-15-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw7 = train_raw7[!train_raw7$Label == "Label",]
train_raw7 = downSample(train_raw7, factor(train_raw7$Label))

train_raw8 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Friday-02-03-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw8 = train_raw8[!train_raw8$Label == "Label",]
train_raw8 = downSample(train_raw8, factor(train_raw8$Label))

train_raw9 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Friday-16-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw9 = train_raw9[!train_raw9$Label == "Label",]
train_raw9 = downSample(train_raw9, factor(train_raw9$Label))

train_raw10 = read.csv("dataset/Processed Traffic Data for ML Algorithms/Friday-23-02-2018_TrafficForML_CICFlowMeter.csv", header=T)
train_raw10 = train_raw10[!train_raw10$Label == "Label",]
train_raw10 = downSample(train_raw10, factor(train_raw10$Label))

train_raw = rbind(train_raw1[, 5:84], train_raw2[, 1:80], train_raw3[, 1:80], train_raw4[, 1:80], train_raw5[, 1:80], train_raw6[, 1:80], train_raw7[, 1:80], train_raw8[, 1:80], train_raw9[, 1:80], train_raw10[, 1:80])

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
write.csv(train_raw, file = "dataset/CIC2018_Full.csv", row.names=FALSE)

# remove small attack type from dataset. 1. DDOS attack-LOIC-UDP 2. Brute Force -Web 3. Brute Force -XSS 4. SQL Injection
train_raw_less_features = train_raw[!(train_raw$Label == "DDOS attack-LOIC-UDP"),]
train_raw_less_features = train_raw_less_features[!(train_raw_less_features$Label == "Brute Force -Web"),]
train_raw_less_features = train_raw_less_features[!(train_raw_less_features$Label == "Brute Force -XSS"),]
train_raw_less_features = train_raw_less_features[!(train_raw_less_features$Label == "SQL Injection"),]

# save the less feature dataset
write.csv(train_raw_less_features, file = "dataset/CIC2018_Less_Features.csv", row.names=FALSE)


# down sample dataset
#train_raw_downsample = downSample(train_raw_less_features, factor(train_raw_less_features$Label))
train_raw_downsample = downSample(train_raw, factor(train_raw$Label))

# save the down sample dataset
write.csv(train_raw_downsample, file = "dataset/CIC2018_Downsample.csv", row.names=FALSE)



train_raw = read.csv("dataset/CIC2018_Full.csv", header=T)

library("caret")

inTrain <- createDataPartition(y = train_raw$Label, p = 0.4, list = FALSE)
testing_subset <- train_raw[inTrain,]
#inTest <- createDataPartition(y = train_raw$Label, p = 0.2, list = FALSE)
inTrain <- createDataPartition(y = testing_subset$Label, p = 0.5, list = FALSE)
testing_subset_train <- testing_subset[inTrain,]
testing_subset_test <- testing_subset[-inTrain,]

# save the subset dataset of training and testing
write.csv(testing_subset_train, file = "dataset/CIC2018_subset_train.csv", row.names=FALSE)
write.csv(testing_subset_test, file = "dataset/CIC2018_subset_test.csv", row.names=FALSE)

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
