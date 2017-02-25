set.seed(1)
randomforest_model <- train(diagnosis ~ ., data=training_data, method="rf")

randomforest_model
randomforest_model$finalModel
prediction1 <- predict(randomforest_model, test_data)
confusionMatrix(prediction1, test_data$diagnosis, positive="M")
#fine tuning "mtry" parameter
control <- trainControl(method="repeatedcv", repeats=3)

grid <- expand.grid(mtry = c(1, 2, 3, 5, 7, 10, 15, 20, 30))

set.seed(1)
rf_tune_model <- train(diagnosis ~ ., data=training_data, method="rf",
                       tuneGrid = grid, trControl = control)

rf_tune_model
rf_tune_model$finalModel
#tuning model for mtry
plot(rf_tune_model)

#Predicting model after tuning
prediction2 <- predict(rf_tune_model, test_data)
confusionMatrix(prediction2, test_data$diagnosis, positive="M")

#variables used
varImp(rf_tune_model)

#comparing roc plots for cart and random forest
#cart
cart_roc <- roc(test_data$diagnosis,
                predict(cart_tune_model, test_data, type = "prob")[,"M"],
                levels = rev(levels(test_data$diagnosis)))
auc(cart_roc)

#rf 
rf_roc <- roc(test_data$diagnosis,
              predict(rf_tune_model, test_data, type = "prob")[,"M"],
              levels = rev(levels(test_data$diagnosis)))
auc(rf_roc)

rocobj1 <- plot.roc(cart_roc, main="ROC Curves", col="#008600")
rocobj2 <- lines.roc(rf_roc,  col="#1c61b6")
legend("bottomright", legend=c("CART", "RF"), col=c("#008600", "#1c61b6"), lwd=2)
