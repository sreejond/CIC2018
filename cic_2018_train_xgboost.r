# create training and testing subset

# import libraries
library(ggplot2)
library(caret)
library(xgboost)



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

attach(testing_subset_train)



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
testing_subset_train$Label = as.numeric(factor(testing_subset_train$Label, levels = c("Benign", "Bot", "Brute_Force_Web", "Brute_Force_XSS", "DDOS_attack_HOIC", "DDOS_attack_LOIC_UDP", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SQL_Injection", "SSH_Bruteforce"))) - 1




# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# testing_subset_train_scaled <- as.data.frame(lapply(testing_subset_train[1:79], normalize))
# testing_subset_train_scaled$Label <- testing_subset_train$Label
# 
# library(nnet)
# # Encode as a one hot vector multilabel data
# testing_subset_train_encoded <- cbind(testing_subset_train_scaled[, 1:79], class.ind(as.factor(testing_subset_train_scaled$Label)))
# # Set labels name
# names(testing_subset_train_encoded) <- c(names(testing_subset_train_scaled)[1:79], "Benign", "Bot", "Brute_Force_Web", "Brute_Force_XSS", "DDOS_attack_HOIC", "DDOS_attack_LOIC_UDP", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SQL_Injection", "SSH_Bruteforce")
# 





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
testing_subset_test$Label = as.numeric(factor(testing_subset_test$Label, levels = c("Benign", "Bot", "Brute_Force_Web", "Brute_Force_XSS", "DDOS_attack_HOIC", "DDOS_attack_LOIC_UDP", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SQL_Injection", "SSH_Bruteforce"))) - 1




# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# testing_subset_test_scaled <- as.data.frame(lapply(testing_subset_test[1:79], normalize))
# testing_subset_test_scaled$Label <- testing_subset_test$Label
# 
# # Encode as a one hot vector multilabel data
# testing_subset_test_encoded <- cbind(testing_subset_test_scaled[, 1:79], class.ind(as.factor(testing_subset_test_scaled$Label)))
# # Set labels name
# names(testing_subset_test_encoded) <- c(names(testing_subset_test_scaled)[1:79], "Benign", "Bot", "Brute_Force_Web", "Brute_Force_XSS", "DDOS_attack_HOIC", "DDOS_attack_LOIC_UDP", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SQL_Injection", "SSH_Bruteforce")
# 





# Create final training data with the important features
testing_subset_train_imp_features <- testing_subset_train[, c("Timestamp", "Dst.Port", "Init.Fwd.Win.Byts", "Fwd.Seg.Size.Min", "Fwd.Header.Len", 
                                                                      "Init.Bwd.Win.Byts", "TotLen.Fwd.Pkts", "ACK.Flag.Cnt", "Subflow.Fwd.Byts", 
                                                                      "RST.Flag.Cnt", "ECE.Flag.Cnt", "Bwd.Pkt.Len.Std", 
                                                                      "Bwd.Header.Len", "Fwd.Pkt.Len.Max", "Fwd.IAT.Min", "Fwd.IAT.Tot", 
                                                                      "Fwd.Pkt.Len.Mean", "Tot.Fwd.Pkts", "Subflow.Fwd.Pkts", "Fwd.Seg.Size.Avg",
                                                                      "Fwd.IAT.Max", "Flow.Pkts.s", "Flow.Duration", "Fwd.IAT.Mean", 
                                                                      "URG.Flag.Cnt", "Pkt.Len.Std",  "Label")]
                                                                      # "Benign", "Bot", "Brute_Force_Web", "Brute_Force_XSS", "DDOS_attack_HOIC", "DDOS_attack_LOIC_UDP", 
                                                                      # "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", 
                                                                      # "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SQL_Injection", "SSH_Bruteforce")]

testing_subset_test_imp_features <- testing_subset_test[, c("Timestamp", "Dst.Port", "Init.Fwd.Win.Byts", "Fwd.Seg.Size.Min", "Fwd.Header.Len", 
                                                                    "Init.Bwd.Win.Byts", "TotLen.Fwd.Pkts", "ACK.Flag.Cnt", "Subflow.Fwd.Byts", 
                                                                    "RST.Flag.Cnt", "ECE.Flag.Cnt", "Bwd.Pkt.Len.Std", 
                                                                    "Bwd.Header.Len", "Fwd.Pkt.Len.Max", "Fwd.IAT.Min", "Fwd.IAT.Tot", 
                                                                    "Fwd.Pkt.Len.Mean", "Tot.Fwd.Pkts", "Subflow.Fwd.Pkts", "Fwd.Seg.Size.Avg",
                                                                    "Fwd.IAT.Max", "Flow.Pkts.s", "Flow.Duration", "Fwd.IAT.Mean", 
                                                                    "URG.Flag.Cnt", "Pkt.Len.Std", "Label")]
                                                                    # "Benign", "Bot", "Brute_Force_Web", "Brute_Force_XSS", "DDOS_attack_HOIC", 
                                                                    # "DDOS_attack_LOIC_UDP", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", 
                                                                    # "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", 
                                                                    # "FTP_BruteForce", "Infilteration", "SQL_Injection", "SSH_Bruteforce")]


# testing_subset_train_imp_features <- testing_subset_train_encoded[, c("Timestamp", "Dst.Port", "Protocol", "Flow.Duration", "Tot.Fwd.Pkts",
#                                         "Tot.Bwd.Pkts", "TotLen.Fwd.Pkts", "TotLen.Bwd.Pkts", "Fwd.Pkt.Len.Max",
#                                         "Fwd.Pkt.Len.Min", "Fwd.Pkt.Len.Mean", "Fwd.Pkt.Len.Std",
#                                         "Bwd.Pkt.Len.Max", "Bwd.Pkt.Len.Min", "Bwd.Pkt.Len.Mean", "Bwd.Pkt.Len.Std",
#                                         "Flow.Byts.s", "Flow.Pkts.s", "Flow.IAT.Mean", "Flow.IAT.Std",
#                                         "Flow.IAT.Max", "Flow.IAT.Min", "Fwd.IAT.Tot", "Fwd.IAT.Mean",
#                                         "Fwd.IAT.Std", "Fwd.IAT.Max", "Fwd.IAT.Min",
#                                         "Benign", "Bot", "DDOS attack-HOIC", "DDoS attacks-LOIC-HTTP",
#                                         "DoS attacks-GoldenEye", "DoS attacks-Hulk", "DoS attacks-SlowHTTPTest", "DoS attacks-Slowloris",
#                                         "FTP-BruteForce", "Infilteration", "SSH-Bruteforce")]
#  
# testing_subset_test_imp_features <- testing_subset_test_encoded[, c("Timestamp", "Dst.Port", "Protocol", "Flow.Duration", "Tot.Fwd.Pkts",
#                                         "Tot.Bwd.Pkts", "TotLen.Fwd.Pkts", "TotLen.Bwd.Pkts", "Fwd.Pkt.Len.Max",
#                                         "Fwd.Pkt.Len.Min", "Fwd.Pkt.Len.Mean", "Fwd.Pkt.Len.Std",
#                                         "Bwd.Pkt.Len.Max", "Bwd.Pkt.Len.Min", "Bwd.Pkt.Len.Mean", "Bwd.Pkt.Len.Std",
#                                         "Flow.Byts.s", "Flow.Pkts.s", "Flow.IAT.Mean", "Flow.IAT.Std",
#                                         "Flow.IAT.Max", "Flow.IAT.Min", "Fwd.IAT.Tot", "Fwd.IAT.Mean",
#                                         "Fwd.IAT.Std", "Fwd.IAT.Max", "Fwd.IAT.Min",
#                                         "Benign", "Bot", "DDOS attack-HOIC", "DDoS attacks-LOIC-HTTP", 
#                                         "DoS attacks-GoldenEye", "DoS attacks-Hulk", "DoS attacks-SlowHTTPTest", "DoS attacks-Slowloris", 
#                                         "FTP-BruteForce", "Infilteration", "SSH-Bruteforce")]


# testing code for low power machine <---------------
# inTrain <- createDataPartition(y = testing_subset_train_imp_features$Label, p = 0.5, list = FALSE)
# inTest <- createDataPartition(y = testing_subset_test_imp_features$Label, p = 0.5, list = FALSE)
# testing_subset_train_imp_features <- testing_subset_train_imp_features[inTrain,]
# testing_subset_test_imp_features <- testing_subset_test_imp_features[inTest,]
#------------>




# n <- names(testing_subset_train_imp_features)
# f <- as.formula(paste("Benign + Bot + DDOS_attack_HOIC + DDoS_attacks_LOIC_HTTP + DoS_attacks_GoldenEye + DoS_attacks_Hulk + DoS_attacks_SlowHTTPTest + DoS_attacks_Slowloris + FTP_BruteForce + Infilteration + SSH_Bruteforce + DDOS_attack_LOIC_UDP + SQL_Injection + Brute_Force_XSS + Brute_Force_Web ~",
#                       paste(n[!n %in% c("Benign", "Bot", "DDOS_attack_HOIC", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SSH_Bruteforce", "DDOS_attack_LOIC_UDP", "SQL_Injection", "Brute_Force_XSS", "Brute_Force_Web")], collapse = " + ")))
# f

# nnModel <- neuralnet(formula = f, data = testing_subset_train_imp_features, hidden = c(10, 5, 3, 2), linear.output = FALSE, threshold = 0.01, stepmax = 1e+07)
# saveRDS(object = nnModel, file = "cic2018_nnModel_downsample.rds")
# #nnModel = readRDS("nnModel_on_100_test_set_10-5-2.rds")
# nnModel$result.matrix
# plot(nnModel)

new_train = model.matrix(~ ., data = testing_subset_train_imp_features[,1:26]);
xgb_train <- xgb.DMatrix(data = new_train, label = testing_subset_train_imp_features$Label)

# Set parameters(default)
params <- list(booster = "gbtree", objective = "multi:softprob", num_class = 15, eval_metric = "mlogloss")

# Calculate # of folds for cross-validation
xgbcv <- xgb.cv(params = params, data = xgb_train, nrounds = 100, nfold = 5, showsd = TRUE, stratified = TRUE, print.every.n = 10, early_stop_round = 20, maximize = FALSE, prediction = TRUE)

# xgbModel <- xgboost(data = model.matrix(~ ., data = testing_subset_train_imp_features[,1:26]), 
#                label = testing_subset_train_imp_features$Label, 
#                eta = 0.1,
#                max_depth = 15, 
#                nround=25, 
#                subsample = 0.5,
#                colsample_bytree = 0.5,
#                seed = 1,
#                eval_metric = "merror",
#                objective = "multi:softprob",
#                num_class = 15,
#                nthread = 3
# )


saveRDS(object = xgbModel, file = "cic2018_xgbModel_downsample.rds")



# Compute predictions
pr.nn <- compute(nnModel, testing_subset_test_imp_features[, 1:26])
pr.nn

results <- data.frame(pr.nn$net.result)
names(results) = c("Benign", "Bot", "DDOS_attack_HOIC", "DDoS_attacks_LOIC_HTTP", "DoS_attacks_GoldenEye", "DoS_attacks_Hulk", "DoS_attacks_SlowHTTPTest", "DoS_attacks_Slowloris", "FTP_BruteForce", "Infilteration", "SSH_Bruteforce")

roundedresults <- sapply(results, round, digits = 0)
roundedresultsdf = data.frame(roundedresults)

predictedResults = cbind(1:nrow(roundedresultsdf), max.col(roundedresultsdf))
predictedResults = predictedResults[,2]
actualResults = cbind(1:nrow(testing_subset_test_imp_features[, 27:37]), max.col(testing_subset_test_imp_features[, 27:37]))
actualResults = actualResults[,2]

# confustion matrix
confusion_matrix = table(predictedResults, actualResults)
confusion_matrix
round(prop.table(confusion_matrix, 1) * 100, 2)


# accuracy on testing set
mean(predictedResults == actualResults)
