# create training and testing subset

# import libraries
library(ggplot2)
library("caret")
library(randomForest)



# set working dir
#setwd('C:/Users/sreej/Desktop/SDN/SDN_R_Code/CIC2018')

# Start writing to an output file
#sink('cor.txt')

# Clear the workspace
rm(list = ls())

# Load training data 
testing_subset_train = read.csv("dataset/CIC2018_subset_train.csv", header=T, stringsAsFactors = FALSE)

# replace Inf with NA
is.na(testing_subset_train) <- sapply(testing_subset_train, is.infinite)

# removed NA value
testing_subset_train = na.omit(testing_subset_train)

# sort dataset with respect to date
testing_subset_train = testing_subset_train[order(as.Date(testing_subset_train$Timestamp, format="%d/%m/%Y %I:%M:%S")),]

# convert timestamp to julian time
testing_subset_train$Timestamp = as.numeric(as.POSIXct(testing_subset_train$Timestamp, format="%d/%m/%Y %I:%M:%S"))

# Observe the data
names(testing_subset_train)




# Label preprocess
# The result is classified into 4 groups 
# Subsetting the Label variable into 4 groups.
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-GoldenEye"] = "DOS"
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-Hulk"] = "DOS"
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-SlowHTTPTest"] = "DOS"
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-Slowloris"] = "DOS"
testing_subset_train$Label[testing_subset_train$Label == "DDOS attack-HOIC"] = "DDOS"
testing_subset_train$Label[testing_subset_train$Label == "DDOS attack-LOIC-UDP"] = "DDOS"
testing_subset_train$Label[testing_subset_train$Label == "DDoS attacks-LOIC-HTTP"] = "DDOS"
testing_subset_train$Label[testing_subset_train$Label == "Brute Force -Web"] = "Web"
testing_subset_train$Label[testing_subset_train$Label == "Brute Force -XSS"] = "Web"
testing_subset_train$Label[testing_subset_train$Label == "SQL Injection"] = "Web"
testing_subset_train$Label[testing_subset_train$Label == "FTP-BruteForce"] = "Bruteforce"
testing_subset_train$Label[testing_subset_train$Label == "SSH-Bruteforce"] = "Bruteforce"
testing_subset_train$Label = factor(testing_subset_train$Label)




# Observe the distribution of Labels
d = dim(testing_subset_train)
sum_Label <- aggregate(rep(1, d[1]), by = list(testing_subset_train$Label), FUN = sum)
names(sum_Label) <- c("Label", "count")
barplot(beside = TRUE, (sum_Label$count), 
        names.arg = sum_Label$Label, ylim = c(0,max(sum_Label$count)),
        xlab = "Label", ylab = "Count",
        col = "Blue", main = "The distribution of Labels")

# print the distribution in numbers
print(sum_Label)





# Load testing data 
testing_subset_test = read.csv("dataset/CIC2018_subset_test.csv", header=T, stringsAsFactors = FALSE)

# replace Inf with NA
is.na(testing_subset_test) <- sapply(testing_subset_test, is.infinite)

# removed NA value
testing_subset_test = na.omit(testing_subset_test)

# sort dataset with respect to date
testing_subset_test = testing_subset_test[order(as.Date(testing_subset_test$Timestamp, format="%d/%m/%Y %I:%M:%S")),]

# convert timestamp to julian time
testing_subset_test$Timestamp = as.numeric(as.POSIXct(testing_subset_test$Timestamp, format="%d/%m/%Y %I:%M:%S"))

# Observe the data
names(testing_subset_test)




# Label preprocess
# The result is classified into 4 groups 
# Subsetting the Label variable into 4 groups.
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-GoldenEye"] = "DOS"
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-Hulk"] = "DOS"
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-SlowHTTPTest"] = "DOS"
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-Slowloris"] = "DOS"
testing_subset_test$Label[testing_subset_test$Label == "DDOS attack-HOIC"] = "DDOS"
testing_subset_test$Label[testing_subset_test$Label == "DDOS attack-LOIC-UDP"] = "DDOS"
testing_subset_test$Label[testing_subset_test$Label == "DDoS attacks-LOIC-HTTP"] = "DDOS"
testing_subset_test$Label[testing_subset_test$Label == "Brute Force -Web"] = "Web"
testing_subset_test$Label[testing_subset_test$Label == "Brute Force -XSS"] = "Web"
testing_subset_test$Label[testing_subset_test$Label == "SQL Injection"] = "Web"
testing_subset_test$Label[testing_subset_test$Label == "FTP-BruteForce"] = "Bruteforce"
testing_subset_test$Label[testing_subset_test$Label == "SSH-Bruteforce"] = "Bruteforce"
testing_subset_test$Label = factor(testing_subset_test$Label)




# Observe the distribution of Labels
rm(sum_label)
d = dim(testing_subset_test)
sum_Label <- aggregate(rep(1, d[1]), by = list(testing_subset_test$Label), FUN = sum)
names(sum_Label) <- c("Label", "count")
barplot(beside = TRUE, (sum_Label$count), 
        names.arg = sum_Label$Label, ylim = c(0,max(sum_Label$count)),
        xlab = "Label", ylab = "Count",
        col = "Blue", main = "The distribution of Labels")

# print the distribution in numbers
print(sum_Label)





# Create final training data with the important features
testing_subset_train_imp_features <- testing_subset_train[, c("Timestamp", "Dst.Port", "Init.Fwd.Win.Byts", "Fwd.Seg.Size.Min", "Fwd.Header.Len", 
                                        "Init.Bwd.Win.Byts", "TotLen.Fwd.Pkts", "ACK.Flag.Cnt", "Subflow.Fwd.Byts", 
                                        "RST.Flag.Cnt", "ECE.Flag.Cnt", "Bwd.Pkt.Len.Std", 
                                        "Bwd.Header.Len", "Fwd.Pkt.Len.Max", "Fwd.IAT.Min", "Fwd.IAT.Tot", 
                                        "Fwd.Pkt.Len.Mean", "Tot.Fwd.Pkts", "Subflow.Fwd.Pkts", "Fwd.Seg.Size.Avg",
                                        "Fwd.IAT.Max", "Flow.Pkts.s", "Flow.Duration", "Fwd.IAT.Mean", 
                                        "URG.Flag.Cnt", "Pkt.Len.Std", "Label" )]

testing_subset_test_imp_features <- testing_subset_test[, c("Timestamp", "Dst.Port", "Init.Fwd.Win.Byts", "Fwd.Seg.Size.Min", "Fwd.Header.Len", 
                                        "Init.Bwd.Win.Byts", "TotLen.Fwd.Pkts", "ACK.Flag.Cnt", "Subflow.Fwd.Byts", 
                                        "RST.Flag.Cnt", "ECE.Flag.Cnt", "Bwd.Pkt.Len.Std", 
                                        "Bwd.Header.Len", "Fwd.Pkt.Len.Max", "Fwd.IAT.Min", "Fwd.IAT.Tot", 
                                        "Fwd.Pkt.Len.Mean", "Tot.Fwd.Pkts", "Subflow.Fwd.Pkts", "Fwd.Seg.Size.Avg",
                                        "Fwd.IAT.Max", "Flow.Pkts.s", "Flow.Duration", "Fwd.IAT.Mean", 
                                        "URG.Flag.Cnt", "Pkt.Len.Std", "Label" )]


# testing code for low power machine <---------------
# inTrain <- createDataPartition(y = testing_subset_train_imp_features$Label, p = 0.5, list = FALSE)
# inTest <- createDataPartition(y = testing_subset_test_imp_features$Label, p = 0.5, list = FALSE)
# testing_subset_train_imp_features <- testing_subset_train_imp_features[inTrain,]
# testing_subset_test_imp_features <- testing_subset_test_imp_features[inTest,]
#------------>


# Apply random forest
rfModelFit <- train(Label ~ ., method = "rf", data = testing_subset_train_imp_features)
saveRDS(object = rfModelFit, file = "train_by_rf_testing_subset_train_25_features.rds")
#rfModelFit = readRDS("train_by_rf_testing_subset_train_25_features.rds")
rfModelFit


getTree(rfModelFit$finalModel, k = 2)




pred <- predict(rfModelFit, testing_subset_test_imp_features); 
testing_subset_test_imp_features$predRight <- pred == testing_subset_test_imp_features$Label
A = table(pred, testing_subset_test_imp_features$Label)
A
round(prop.table(A,1)*100, 2)

# accuracy on testing set
mean(pred == testing_subset_test_imp_features$Label)
