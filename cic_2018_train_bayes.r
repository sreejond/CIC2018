# create training and testing subset

# import libraries
library(ggplot2)
library("caret")
library("klaR")



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

# Apply naive bayes

# set up 10-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 10
  )
  
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
  adjust = seq(0, 5, by = 1)
)

bayesModel <- train(
  x = testing_subset_train_imp_features[,1:26],
  y = testing_subset_train_imp_features$Label,
  method = "nb",
  trControl = train_control,
  tuneGrid = search_grid,
  preProc = c("BoxCox", "center", "scale", "pca")
)

saveRDS(object = bayesModel, file = "cic2018_bayesModel_downsample_cv.rds")


# top 5 modesl
nb.m2$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# plot search grid results
plot(nb.m2)

# results for best model
confusionMatrix(nb.m2)


pred <- predict(nb.m2, newdata = testing_subset_test_imp_features)
confusionMatrix(pred, testing_subset_test_imp_features$Label)
