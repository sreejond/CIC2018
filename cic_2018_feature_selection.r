# create training and testing subset

# import libraries
library(ggplot2)



# set working dir
#setwd('C:/Users/sreej/Desktop/SDN/SDN_R_Code/CIC2018')

# Start writing to an output file
#sink('cor.txt')

# Clear the workspace
rm(list = ls())

# Load training data 
testing_subset_train = read.csv("dataset/CIC2018_subset_train.csv", header=T, stringsAsFactors = FALSE)
testing_subset_train = na.omit(testing_subset_train)

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




# Feature Selection by using Boruta function
sample_train=testing_subset_train[sample(nrow(testing_subset_train), replace=F, size=0.05*nrow(testing_subset_train)), ]
library(Boruta)
boruta.train <- Boruta(testing_subset_train$Label ~ ., data = testing_subset_train, doTrace = 2, maxRuns=100)
print(boruta.train)
plot(boruta.train)
boruta.train$finalDecision


#take a call on tentative features
boruta.bank <- TentativeRoughFix(boruta.train)
print(boruta.bank)

# plot with all the feature names written properly
plot(boruta.bank, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.bank$ImpHistory),function(i)
  boruta.bank$ImpHistory[is.finite(boruta.bank$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.bank$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.bank$ImpHistory), cex.axis = 0.7)


getSelectedAttributes(boruta.bank, withTentative = F)
bank_df <- attStats(boruta.bank)
print(bank_df)

saveRDS(object = boruta.train, file = "Boruta_feature_selection_on_testing_subset_train_2018.rds")