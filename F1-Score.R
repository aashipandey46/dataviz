library("tibble")
library("klaR")
library("caret")
library("dplyr")
library("naivebayes")
library("e1071")



# est val split
train = as.tibble(smsspam_train)

train = train %>% mutate(label = as.factor(label))


which(apply(train, 2, sum) == 0)

train1 <- train[ - as.numeric(which(apply(train, 2, var) == 0))]

train1
train1 <- aggregate(.~ label, train, sum)

trn_idx = sample(nrow(train), size = 0.8 * nrow(train1))
est = train[trn_idx, ]
val = train1[-trn_idx, ]

view(train1)



# Fitting Naive Bayes
nb_mod = naiveBayes(label ~ ., data = est)
y-pred <- predict(nb_mod, newdata = train)

prop.table(table(train$label))
prop.table(table(nb_mo))

col_vec = colnames(est)
nb_mod
naive_bayes()
naive_bayes(label)
em_vec = c()


apply(train[,c(1)], 1, sum)










###########################################################


# Fitting logistic model
log_mod = glm(label ~., data = est, family = "binomial")

# Calculating accuracy
calc_acc = function(actual, predicted){
  mean(actual == predicted)
}

pred = factor(ifelse(predict(log_mod, val, type = "response") > 0.5, "1", "0"))
calc_acc(smsspam_test$label, smsspam_test$pred_label)


# Predicting on the test data set
smsspam_test$pred_label = factor(ifelse(predict(log_mod, smsspam_test, type = "response") > 0.5, "1", "0"))
col_name_vec = colnames(train)
col_name_vec
smsspam_test$pred_label <- as.factor(smsspam_test$pred_label)

write.table(smsspam_test$pred_label, "lg_prediction.txt")
