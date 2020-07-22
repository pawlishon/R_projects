shot_ms<-subset(shot2,shot2$current.shot.outcome!="BLOCKED")
shot_ms$current.shot.outcome<-factor(shot_ms$current.shot.outcome, levels = c("MISSED","SCORED"))
str(shot_ms$current.shot.outcome)
prop.table(table(shot_ms$current.shot.outcome))
shot_ms<-shot_ms[-8]
shotms_n <- as.data.frame(lapply(shot_ms[c(5,7,9,11,12,15,16,17)], normalize))
shotms_train <- shotms_n[1:5460, ]
shotms_test <- shotms_n[5461:6825, ]

shotms_train_labels <- shot_ms[1:5460, 10]
shotms_test_labels <- shot_ms[5461:6825, 10]
########################################
library("class")
shotms_test_pred <- knn(train = shotms_train, test = shotms_test,
                       cl = shotms_train_labels, k = 45)
library("plotROC")
roc.estimate <- calculate_roc(shotms_test_pred, shotms_test_labels)
single.rocplot <- ggroc(roc.estimate)

###################################


library(C50)
shotmsd_train <- shot_ms[1:5460, ]
shotmsd_test <- shot_ms[5461:6825, ]
shotms_model <- C5.0(shotmsd_train[,-10],shotms_train_labels)

shotmsd_pred <- predict(shotms_model, shotmsd_test)

roc.estimate1 <- calculate_roc(shotmsd_pred, shotms_test_labels)
single.rocplot1 <- ggroc(roc.estimate1)
library("ggplot2")

plot(single.rocplot,main="Krzywa ROC dla knn")
plot_journal_roc(single.rocplot1)

install.packages("caret")
install.packages("e1071")
library("caret")
library("e1071")
confusionMatrix(data = shotms_test_pred,shotms_test_labels)
confusionMatrix(data = shotmsd_pred,shotms_test_labels)
