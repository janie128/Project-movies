library(caret)
library(dplyr)
  
set.seed(123)

# Create subset for machine learning training
mlFinal <- select(final, adjGross, reviewCount, reviewScore, star1Count, star2Count, star3Count,
                  star4Count, star5Count, wordCountLow, wordCountMid, wordCountHigh, wordCountAvg,
                  goodAvgTFIDF, badAvgTFIDF)
mlClassFinal <- mlFinal[(mlFinal$reviewCount > 50),]
# Sort adjGross into buckets by order of magnitude
mlClassFinal <- mlClassFinal %>% mutate(buckets = cut(adjGross,
                                                      breaks=c(0,1e5,1e6,1e7,1e8,1e9)))
mlClassFinal <- select(mlClassFinal, -(adjGross))
mlClassFinal$buckets <- droplevels(mlClassFinal$buckets)
  
# Create training and testing set
inTrain <- createDataPartition(mlClassFinal$buckets, p=0.7, list=FALSE)
training <- mlClassFinal[inTrain,]
testing <- mlClassFinal[-inTrain,]
  
# Set resampling method
trainCtrl <- trainControl(method = "repeatedCV", number=10, repeats=3)
  
# Test three models: 1) Random Forest 2) Naive Bayes 3) Boosted Logistic Regression
set.seed(123)
modelFitRF <- train(buckets ~ ., data=training, method = "rf", prox=TRUE, trainControl=trainCtrl)
set.seed(123)
modelFitNB <- train(buckets ~ ., data=training, method = "nb", trainControl=trainCtrl)
set.seed(123)
modelFitLB <- train(buckets ~ ., data=training, method = "LogitBoost", trainControl=trainCtrl)
# Comparison of the three models
results <- resamples(list(modelFitRF, modelFitNB, modelFitLB))
summary(results)
  
# Run a grid search on Random Forest
trainCtrl <- trainControl(method = "repeatedCV", number=10, repeats=3)
set.seed(123)
modelFitTuneRF <- train(buckets ~ ., data=training, method = "rf", prox=TRUE, trainControl=trainCtrl,
                        tuneGrid = expand.grid(mtry = c(2,3,4,5,6,7,8)))
  
png(filename="model_tuning.png", width=500, height=400)
gg <- ggplot(modelFitTuneRF, aes(colour= Resampled))
print(gg + labs(x="mtry", title="Random Forest mtry Optimization") + ylim(c(0.605,0.615)) +
        theme(axis.title.x = element_text(size=16), axis.text.x = element_text(size=14),
              axis.title.y = element_text(size=16), axis.text.y = element_text(size=14),
              plot.title = element_text(size=20, face="bold")))
dev.off()
  
png(filename="model_features.png", width=600, height=480)
print(ggplot(varImp(modelFitTuneRF)))
dev.off()
  
predictions <- predict(modelFitTuneRF, newdata = testing)
confusionMatrix(predictions, testing$buckets)
  
  
  
# -----Plot confusion matrix
#test <- as.data.frame(confusionMatrix(predictions, testing$buckets)$table)
#test$Freq[1:5] <- round(test$Freq[1:5]/sum(test$Freq[1:5]),3)
#test$Freq[6:10] <- round(test$Freq[6:10]/sum(test$Freq[6:10]),3)
#test$Freq[11:15] <- round(test$Freq[11:15]/sum(test$Freq[11:15]),3)
#test$Freq[16:20] <- round(test$Freq[16:20]/sum(test$Freq[16:20]),3)
#test$Freq[21:25] <- round(test$Freq[21:25]/sum(test$Freq[21:25]),3)
#ggplot(test) + geom_tile(aes(x=Reference, y=Prediction, fill=Freq)) +
#  scale_x_discrete(name="Reference") + scale_y_discrete(name="Prediction")
# -------------------------------
  
  
#ggplot(testing, aes(x=log10(predictions), y=log10(adjGross))) + geom_point(size=2)
