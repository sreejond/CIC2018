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
testing_subset_train = read.csv("dataset/CIC2018_Downsample.csv", header=T, stringsAsFactors = FALSE)

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
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-GoldenEye"] = "DoS_attacks_GoldenEye"
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-Hulk"] = "DoS_attacks_Hulk"
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-SlowHTTPTest"] = "DoS_attacks_SlowHTTPTest"
testing_subset_train$Label[testing_subset_train$Label == "DoS attacks-Slowloris"] = "DoS_attacks_Slowloris"
testing_subset_train$Label[testing_subset_train$Label == "DDOS attack-HOIC"] = "DDOS_attack_HOIC"
testing_subset_train$Label[testing_subset_train$Label == "DDOS attack-LOIC-UDP"] = "DDOS_attack_LOIC_UDP"
testing_subset_train$Label[testing_subset_train$Label == "DDoS attacks-LOIC-HTTP"] = "DDoS_attacks_LOIC_HTTP"
testing_subset_train$Label[testing_subset_train$Label == "Brute Force -Web"] = "Brute_Force_Web"
testing_subset_train$Label[testing_subset_train$Label == "Brute Force -XSS"] = "Brute_Force_XSS"
testing_subset_train$Label[testing_subset_train$Label == "SQL Injection"] = "SQL_Injection"
testing_subset_train$Label[testing_subset_train$Label == "FTP-BruteForce"] = "FTP_BruteForce"
testing_subset_train$Label[testing_subset_train$Label == "SSH-Bruteforce"] = "SSH_Bruteforce"
#testing_subset_train$Label[testing_subset_train$Label == "Infilteration"] = "Bruteforce"
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
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-GoldenEye"] = "DoS_attacks_GoldenEye"
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-Hulk"] = "DoS_attacks_Hulk"
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-SlowHTTPTest"] = "DoS_attacks_SlowHTTPTest"
testing_subset_test$Label[testing_subset_test$Label == "DoS attacks-Slowloris"] = "DoS_attacks_Slowloris"
testing_subset_test$Label[testing_subset_test$Label == "DDOS attack-HOIC"] = "DDOS_attack_HOIC"
testing_subset_test$Label[testing_subset_test$Label == "DDOS attack-LOIC-UDP"] = "DDOS_attack_LOIC_UDP"
testing_subset_test$Label[testing_subset_test$Label == "DDoS attacks-LOIC-HTTP"] = "DDoS_attacks_LOIC_HTTP"
testing_subset_test$Label[testing_subset_test$Label == "Brute Force -Web"] = "Brute_Force_Web"
testing_subset_test$Label[testing_subset_test$Label == "Brute Force -XSS"] = "Brute_Force_XSS"
testing_subset_test$Label[testing_subset_test$Label == "SQL Injection"] = "SQL_Injection"
testing_subset_test$Label[testing_subset_test$Label == "FTP-BruteForce"] = "FTP_BruteForce"
testing_subset_test$Label[testing_subset_test$Label == "SSH-Bruteforce"] = "SSH_Bruteforce"
# testing_subset_test$Label[testing_subset_test$Label == "Infilteration"] = "Bruteforce"
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


# testing_subset_train_imp_features <- testing_subset_train[, c("Timestamp", "Dst.Port", "Protocol", "Flow.Duration", "Tot.Fwd.Pkts", 
#                                         "Tot.Bwd.Pkts", "TotLen.Fwd.Pkts", "TotLen.Bwd.Pkts", "Fwd.Pkt.Len.Max", 
#                                         "Fwd.Pkt.Len.Min", "Fwd.Pkt.Len.Mean", "Fwd.Pkt.Len.Std", 
#                                         "Bwd.Pkt.Len.Max", "Bwd.Pkt.Len.Min", "Bwd.Pkt.Len.Mean", "Bwd.Pkt.Len.Std", 
#                                         "Flow.Byts.s", "Flow.Pkts.s", "Flow.IAT.Mean", "Flow.IAT.Std",
#                                         "Flow.IAT.Max", "Flow.IAT.Min", "Fwd.IAT.Tot", "Fwd.IAT.Mean", 
#                                         "Fwd.IAT.Std", "Fwd.IAT.Max", "Fwd.IAT.Min", "Label" )]
# 
# testing_subset_test_imp_features <- testing_subset_test[, c("Timestamp", "Dst.Port", "Protocol", "Flow.Duration", "Tot.Fwd.Pkts", 
#                                         "Tot.Bwd.Pkts", "TotLen.Fwd.Pkts", "TotLen.Bwd.Pkts", "Fwd.Pkt.Len.Max", 
#                                         "Fwd.Pkt.Len.Min", "Fwd.Pkt.Len.Mean", "Fwd.Pkt.Len.Std", 
#                                         "Bwd.Pkt.Len.Max", "Bwd.Pkt.Len.Min", "Bwd.Pkt.Len.Mean", "Bwd.Pkt.Len.Std", 
#                                         "Flow.Byts.s", "Flow.Pkts.s", "Flow.IAT.Mean", "Flow.IAT.Std",
#                                         "Flow.IAT.Max", "Flow.IAT.Min", "Fwd.IAT.Tot", "Fwd.IAT.Mean", 
#                                         "Fwd.IAT.Std", "Fwd.IAT.Max", "Fwd.IAT.Min", "Label" )]


# testing code for low power machine <---------------
# inTrain <- createDataPartition(y = testing_subset_train_imp_features$Label, p = 0.5, list = FALSE)
# inTest <- createDataPartition(y = testing_subset_test_imp_features$Label, p = 0.5, list = FALSE)
# testing_subset_train_imp_features <- testing_subset_train_imp_features[inTrain,]
# testing_subset_test_imp_features <- testing_subset_test_imp_features[inTest,]
#------------>


#ctrl <- trainControl(sampling = "smote")

# Apply random forest
# rfModelFit <- train(Label ~ ., method = "rf", data = testing_subset_train_imp_features)
# #rfModelFit <- train(Label ~ ., method = "rf", data = testing_subset_train_imp_features, trControl = ctrl)
# saveRDS(object = rfModelFit, file = "cic2018_rfModel_smote_downsample.rds")
# #rfModelFit = readRDS("train_by_rf_testing_subset_train_25_features.rds")
# rfModelFit

numFolds <- trainControl(method = "cv", number = 10, classProbs=TRUE)
tunegrid <- expand.grid(.mtry=c(1:10))

rfModelFit = train(Label ~ ., data = testing_subset_train_imp_features, method = "rf", metric = "ROC", trControl = numFolds, tuneGrid = tunegrid)
saveRDS(object = rfModelFit, file = "cic2018_rfModel_downsample_cv.rds")

rfModelFit= readRDS("cic2018_rfModel_downsample_cv.rds")
getTree(rfModelFit$finalModel, k = 2)




pred <- predict(rfModelFit, testing_subset_test_imp_features); 

#levels(pred) = levels(testing_subset_test_imp_features$Label)

testing_subset_test_imp_features$predRight <- pred == testing_subset_test_imp_features$Label
A = table(testing_subset_test_imp_features$Label, pred)
A
round(prop.table(A,1)*100, 2)




## model measurements

n = sum(A) # number of instances
nc = nrow(A) # number of classes
diag = diag(A) # number of correctly classified instances per class 
rowsums = apply(A, 1, sum) # number of instances per class
colsums = apply(A, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes


accuracy = sum(diag) / n 
## Per-class Precision, Recall, and F-1
precision = diag / colsums 
recall = diag / rowsums 
f1 = 2 * ((precision * recall) / (precision + recall))
data.frame(precision, recall, f1) 

## Macro-averaged Metrics
macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)
data.frame(macroPrecision, macroRecall, macroF1)

## auc
library(pROC)
auc = multiclass.roc(as.numeric(testing_subset_test_imp_features$Label), as.numeric(pred))