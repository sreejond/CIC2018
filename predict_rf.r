library(ggplot2)
library("caret")
library(randomForest)


rfModelFit = readRDS("train_by_rf_testing_subset_train_25_features_downsample.rds")
rfModelFit


getTree(rfModelFit$finalModel, k = 2)




pred <- predict(rfModelFit, testing_subset_test_imp_features); 
pred

testing_subset_test_imp_features$predRight <- pred == testing_subset_test_imp_features$Label
testing_subset_test_imp_features$predRight

# accuracy on testing set
mean(pred == testing_subset_test_imp_features$Label)

A = table(pred, testing_subset_test_imp_features$Label)
A
round(prop.table(A,1)*100, 2)





# subset(test_test, Label == "Benign")