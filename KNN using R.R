library(caret)
library(class)

fruit = read.table("C:/Users/Amit/Desktop/Python Programming/course3_downloads/fruit_data_with_colors.txt", sep="\t", header=TRUE)
fruit$fruit_label = NULL

labels = fruit$fruit_name
fruit$fruit_name = NULL

dmy = dummyVars("~.",data=fruit)
fruit = data.frame(predict(dmy, fruit))
fruit = data.frame(bind_cols(fruit_name=labels, fruit))


split = createDataPartition(fruit$fruit_name, p=0.7, list=FALSE)
train = fruit[split,]
test = fruit[-split,]


for (i in 1:10)
{
  knnMod = knn(train[,-1], test[,-1], train[,1], k=i)
  accuracy = round(sum(knnMod==test[,1]) / nrow(test) * 100,3)
  print(paste0("K = ", i, "  Accuracy = ", accuracy))
}


knnMod_final = knn(train[,-1], test[,-1], train[,1], k=4)
confusionMatrix(knnMod_final, test[,1])




# Modeling usingthe Caret package
# -------------------------------

fruit = read.table("C:/Users/Amit/Desktop/Python Programming/course3_downloads/fruit_data_with_colors.txt", sep="\t", header=TRUE)
fruit$fruit_label = NULL

labels = fruit$fruit_name
fruit$fruit_name = NULL

dmy = dummyVars("~.",data=fruit)
fruit = data.frame(predict(dmy, fruit))
fruit = data.frame(bind_cols(fruit_name=labels, fruit))


split = createDataPartition(fruit$fruit_name, p=0.7, list=FALSE)
train = fruit[split,]
test = fruit[-split,]

fitControl = trainControl(method="repeatedcv", repeats=3)

knnMod2 = train(fruit_name ~ ., data=train, 
                method="knn",
                trControl=fitControl,
                preProcess=c("center","scale"),
                tuneLength=10)

plot(knnMod2)

pred = predict(knnMod2, newdata=test)
confusionMatrix(pred, test[,1])

# Conclusion : We see that using the Caret package and doing cross validation has drastically increased the accuracy from 80% to 93%
# Also, for KNN the data should be scaled and centered. The accuracy is much lower if we don't scale the data
