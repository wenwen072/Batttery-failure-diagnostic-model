# ML1 Random Forest (RF): using randomForest package to train on the previous dataset with PCA
library(randomForest)
library(caret)

#
# 1. Random forest on dataset with one pricipal component: default parameters
modelrf1 <- randomForest(label ~., data = train1, importance = TRUE)
print(modelrf1)
err1 <- modelrf1$err.rate
head(err1)
oob_err1 <- err1[nrow(err1), "OOB"]
print(oob_err1)
plot(modelrf1) # to get "error vs no.tree" graph
legend(x = "right", legend = colnames(err1), fill = 1:ncol(err1))

predrf1 <- predict(modelrf1, test1, type = "class") # make a prediction
mean(predrf1 == test1$label) # calculate accuracy
table(test1$label,predrf1) # create confusion matrix

# find the right mtry and plot graph: using tunerf
# Choose ntree = 100 sufficient enough to perform well
bestmtry1 <- tuneRF(x = train1[,2:8761], y = train1[,1], stepFactor=1.5, improve=1e-5, ntree=100)
print(bestmtry1) # choose mtry=62 

# Random forest on dataset with one pricipal component: ntree=100, mtry=62
modelrf2 <- randomForest(label ~., data = train1,ntree = 100, mtry = 62, importance = TRUE)
print(modelrf2)
err2 <- modelrf2$err.rate
head(err2)
oob_err2 <- err2[nrow(err2), "OOB"]
print(oob_err2)
plot(modelrf2)
legend(x = "right", legend = colnames(err2), fill = 1:ncol(err2))

predrf2 <- predict(modelrf2, test1, type = "class") 
mean(predrf2 == test1$label)
table(test1$label,predrf2) 

#
# 2. Random forest on dataset with 75 pricipal components: default parameters
modelrf75 <- randomForest(label ~., data = train75, importance = TRUE)
print(modelrf75)
err75 <- modelrf75$err.rate
head(err75)
oob_err75 <- err75[nrow(err75), "OOB"]
print(oob_err75)
plot(modelrf75) # to get "error vs no.tree" graph
legend(x = "right", legend = colnames(err75), fill = 1:ncol(err75))

predrf75 <- predict(modelrf75, test75, type = "class")
mean(predrf75 == test75$label)
table(test75$label,predrf75)

# find the right mtry and plot graph: using tunerf
# Choose ntree = 100 sufficient enough to perform well
bestmtry75 <- tuneRF(x = train75[,2:8761], y = train75[,1], stepFactor=1.5, improve=1e-5, ntree=100)
print(bestmtry75)

# Random forest on dataset with 75 pricipal components: ntree=100, mtry=42
modelrf275 <- randomForest(label ~., data = train75,ntree = 100, mtry = 42, importance = TRUE)
print(modelrf275)
err275 <- modelrf275$err.rate
head(err275)
oob_err275 <- err275[nrow(err275), "OOB"]
print(oob_err275)
plot(modelrf275)
legend(x = "right", legend = colnames(err275), fill = 1:ncol(err275))

predrf275 <- predict(modelrf275, test75, type = "class")
mean(predrf275 == test75$label)
table(test75$label,predrf275)

#
# 3. Random forest on dataset with 150 pricipal components: default parameters
modelrf150 <- randomForest(label ~., data = train150, importance = TRUE)
print(modelrf150)
err150 <- modelrf150$err.rate
head(err150)
oob_err150 <- err150[nrow(err150), "OOB"]
print(oob_err150)
plot(modelrf150) # to get "error vs no.tree" graph
legend(x = "right", legend = colnames(err150), fill = 1:ncol(err150))

predrf150 <- predict(modelrf150, test150, type = "class")
mean(predrf150 == test150$label)
table(predrf150, test150$label)

# find the right mtry and plot graph: using tunerf
# Choose ntree = 100 sufficient enough to perform well
bestmtry150 <- tuneRF(x = train150[,2:8761], y = train150[,1], stepFactor=1.5, improve=1e-5, ntree=100)
print(bestmtry150)

# Random forest on dataset with 150 pricipal components: ntree=100, mtry=62
modelrf2150 <- randomForest(label ~., data = train150,ntree = 100, mtry = 62, importance = TRUE)
print(modelrf2150)
err2150 <- modelrf2150$err.rate
head(err2150)
oob_err2150 <- err2150[nrow(err2150), "OOB"]
print(oob_err2150)
plot(modelrf2150) # to get "error vs no.tree" graph
legend(x = "right", legend = colnames(err2150), fill = 1:ncol(err2))

predrf2150 <- predict(modelrf2150, test150, type = "class")
mean(predrf2150 == test150$label)
table(test150$label,predrf2150)