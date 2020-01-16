# ML3 Support Vector Machine (SVM): using xgboost, caret package to train on the previous dataset with PCA
library(caTools)
library(e1071)

# Perform dimensionality reduction: PCA
pca <- prcomp(soc_wide[,3:8762],center = FALSE,scale=FALSE)
plot(pca)

pca_1 = data.frame(pca$x[,1])
pca_75 = data.frame(pca$x[,1:75])
pca_150 = data.frame(pca$x[,1:150])

pca_1$label = soc_wide$label
pca_75$label = soc_wide$label
pca_150$label = soc_wide$label

set.seed(123)
split_1 = sample.split(pca_1$label, SplitRatio = 0.8)
train_1 = subset(pca_1, split == TRUE)
test_1 = subset(pca_1, split == FALSE)

split_75 = sample.split(pca_75$label, SplitRatio = 0.8)
train_75 = subset(pca_75, split == TRUE)
test_75 = subset(pca_75, split == FALSE)

split_150 = sample.split(pca_150$label, SplitRatio = 0.8)
train_150 = subset(pca_150, split == TRUE)
test_150 = subset(pca_150, split == FALSE)

#
# 1. Support Vector Machine on dataset with one pricipal component
# 1.1 Using linear kernel
svm_classifier_linear1 = svm(formula = label ~ .,
                              data = train_1,
                              type = 'C-classification',
                              kernel = 'linear')

y_pred_linear1 = predict(svm_classifier_linear1, newdata = test_1[-2])
cm_linear1 = table(test_1[,2], y_pred_linear1)
cm_linear1

# 1.2 Using polynomial kernel
svm_classifier_polynomial1 = svm(formula = label ~ .,
                                  data = train_1,
                                  type = 'C-classification',
                                  kernel = 'polynomial')

y_pred_polynomial1 = predict(svm_classifier_polynomial1, newdata = test_1[-2])
cm_polynomial1 = table(test_1[,2], y_pred_polynomial1)
cm_polynomial1

# 1.3 Using gaussian radial kernel
svm_classifier_radial75 = svm(formula = label ~ .,
                              data = train_1,
                              type = 'C-classification',
                              kernel = 'radial')

y_pred_radial1 = predict(svm_classifier_radial1, newdata = test_1[-2])
cm_radial1 = table(test_1[,2], y_pred_radial1)
cm_radial1

# 1.4 Using sigmoid
svm_classifier_sigmoid1 = svm(formula = label ~ .,
                               data = train_1,
                               type = 'C-classification',
                               kernel = 'sigmoid')

y_pred_sigmoid1 = predict(svm_classifier_sigmoid1, newdata = test_1[-2])
cm_sigmoid1 = table(test_1[,2], y_pred_linear1)
cm_sigmoid1

#
# 2. Support Vector Machine on dataset with 75 pricipal components
# 2.1 Using linear kernel
svm_classifier_linear75 = svm(formula = label ~ .,
                            data = train_75,
                            type = 'C-classification',
                            kernel = 'linear')

y_pred_linear75 = predict(svm_classifier_linear75, newdata = test_75[-76])
cm_linear75 = table(test_75[,76], y_pred_linear75)
cm_linear75

# 2.2 Using polynomial kernel
svm_classifier_polynomial75 = svm(formula = label ~ .,
                                data = train_75,
                                type = 'C-classification',
                                kernel = 'polynomial')

y_pred_polynomial75 = predict(svm_classifier_polynomial75, newdata = test_75[-76])
cm_polynomial75 = table(test_75[,76], y_pred_polynomial75)
cm_polynomial75

# 2.3 Using gaussian radial kernel
svm_classifier_radial75 = svm(formula = label ~ .,
                            data = train_75,
                            type = 'C-classification',
                            kernel = 'radial')

y_pred_radial75 = predict(svm_classifier_radial75, newdata = test_75[-76])
cm_radial75 = table(test_75[,76], y_pred_radial75)
cm_radial75

# 2.4 Using sigmoid
svm_classifier_sigmoid75 = svm(formula = label ~ .,
                             data = train_75,
                             type = 'C-classification',
                             kernel = 'sigmoid')

y_pred_sigmoid75 = predict(svm_classifier_sigmoid75, newdata = test_75[-76])
cm_sigmoid75 = table(test_75[,76], y_pred_linear75)
cm_sigmoid75

#
# 3. Support Vector Machine on dataset with 150 pricipal components
# 3.1 Using linear kernel
svm_classifier_linear150 = svm(formula = label ~ .,
                              data = train_150,
                              type = 'C-classification',
                              kernel = 'linear')

y_pred_linear150 = predict(svm_classifier_linear150, newdata = test_150[-151])
cm_linear150 = table(test_150[,151], y_pred_linear150)
cm_linear150

# 3.2 Using polynomial kernel
svm_classifier_polynomial150 = svm(formula = label ~ .,
                                  data = train_150,
                                  type = 'C-classification',
                                  kernel = 'polynomial')

y_pred_polynomial150 = predict(svm_classifier_polynomial150, newdata = test_150[-151])
cm_polynomial150 = table(test_150[,151], y_pred_polynomial150)
cm_polynomial150

# 3.3 Using gaussian radial kernel
svm_classifier_radial150 = svm(formula = label ~ .,
                              data = train_150,
                              type = 'C-classification',
                              kernel = 'radial')

y_pred_radial150 = predict(svm_classifier_radial150, newdata = test_150[-151])
cm_radial150 = table(test_150[,151], y_pred_radial150)
cm_radial150

# 3.4 Using sigmoid
svm_classifier_sigmoid150 = svm(formula = label ~ .,
                               data = train_150,
                               type = 'C-classification',
                               kernel = 'sigmoid')

y_pred_sigmoid150 = predict(svm_classifier_sigmoid150, newdata = test_150[-151])
cm_sigmoid150 = table(test_150[,151], y_pred_linear150)
cm_sigmoid150


