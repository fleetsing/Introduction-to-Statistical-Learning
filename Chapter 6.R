
# Install and import the relevant libraries.

library(ISLR)
library(leaps)
library(glmnet)
library(pls)

# BEST SUBSET SELECTION

# Import the Hitters data into a new data frame.
df <- Hitters

names(df)
dim(df)
head(df)

# Check the data for NA values.
sum(is.na(df))
df <- na.omit(df)
sum(is.na(df))

# Use the regsubsets() function from the leaps library to perform best subset selection on the data.
regfit.full <- regsubsets(Salary ~ . , data = df)
summary(regfit.full)

# Set the nvmax option to 19 to extend the selection up to all the variables
regfit.full <- regsubsets(Salary ~ . , data = df, nvmax = 19)

reg.summary <- summary(regfit.full)
reg.summary

# Plot the RSS, adjusted R2, Cp, and BIC for all of the models at once to help decide which model to select.
par(mfrow = c(2, 2))

plot(reg.summary$rss, xlab = "Number of variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of variables", ylab = "Adjusted RSq", type = "l")

# Use the which.max() and points() functions to identify and identify the model with the highest adjusted R squared statistic.
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Plot the Cp and BIC statistics, and indicate the models with the smallest statistic using which.min().
plot(reg.summary$cp, xlab = "Number of variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points (10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of variables", ylab = "BIC", type = "l")
which.min((reg.summary$bic))
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# Use plot() command to display the selected variables for the best model, ranked according to the BIC, Cp, adjusted R2, or AIC.
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")

# FORWARD AND BACKWARD STEPWISE SELECTION

# Use the regsubsets() function to perform forward stepwise and backward stepwise selection.
regfit.fwd <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ . , data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)

# Compare the results of the three selection methods with seven predictors.
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

# CHOOSING AMONG MODELS USING THE VALIDATION SET AND APPROACH AND CROSS-VALIDATION

# Split the data into train and test subsets.
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(df), rep = TRUE)
test <- (!train)

# Apply regsubsets() to the training set in order to perform best subset selection.
regfit.best <- regsubsets(Salary ~ . , data = df[train,], nvmax = 19)
summary(regfit.best)

# Compute the validation set error for the best model of each model size.
test.mat <- model.matrix(Salary ~ . , data = df[test,])

val.errors <- rep(NA, 19)

for (i in 1:19) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)]%*%coefi
  val.errors[i] <- mean((df$Salary[test] - pred)^2)
}

val.errors
which.min(val.errors)
coef(regfit.best, 10)

# Use the previous steps to create a prediction function for regsubsets.
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call [[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# Perform best subset selection on the full data set, and select the best ten-variable model.
regfit.best <- regsubsets(Salary ~ . , data = df, nvmax = 19)
coef(regfit.best, 10)

# Try to choose among the models of different sizes using cross-validation.
k <- 10

set.seed(1)
folds <- sample(1:k, nrow(df), replace = TRUE)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

for (j in 1:k) {
  best.fit <- regsubsets(Salary ~ . , data = df[folds != j,], nvmax = 19)
  for (i in 1:19) {
    pred = predict(best.fit, df[folds == j,], id = i)
    cv.errors[j, i] <- mean((df$Salary[folds == j] - pred)^2)
  }
}

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

par(mfrow = c(1,1))
plot(mean.cv.errors, type = "b")

# Perform best subset selection on the full data set in order to obtain the cross-validation selected model.
reg.best <- regsubsets(Salary ~ . , data = df, nvmax = 19)
coef(reg.best, which.min(mean.cv.errors))

# RIDGE REGRESSION

# Create the required matrix x and vector y.
x <- model.matrix(Salary ~ . , df)[,-1]
y <- df$Salary

# Use glmnet() to fit a ridge regression model (alpha = 0).
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
summary(ridge.mod)

# Split the samples into a training set and a test set.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

# Fit a ridge regression model on the training set, and evaluate its MSE on the test set, using λ = 4.
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,])
mean((ridge.pred - y.test)^2)

# Compare performing ridge regression with λ = 4 to just performing least squares regression (λ = 0).
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T)
mean((ridge.pred - y.test)^2)

lm(y ~ x, subset = train)
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,]

# Choose λ by performing ten-fold cross-validation with the cv.glmnet() function.
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0, nfolds = 10)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

# Get the test MSE.
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2)

# Refit the ridge regression model on the full data set, using the value of λ chosen by cross-validation.
out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s = bestlam)[1:20,]

# LASSO

# Use glmnet() to fit a lasso model (alpha = 1).
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# Perform cross-validation and compute the associated test error.
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2)

# Refit the lasso model on the full data set, using the value of λ chosen by cross-validation.
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef !=0 ]

# PRINCIPAL COMPONENTS REGRESSION

# Apply PCR to the data, in order to predict Salary.
set.seed(2)
pcr.fit <- pcr(Salary ~ . , data = df, scale = TRUE, validation = "CV")
summary(pcr.fit)

# Plot the cross-validation scores using the validationplot() function.
validationplot(pcr.fit, val.type = "MSEP")

# Perform PCR on the training data and evaluate its test set performance.
set.seed(1)
pcr.fit <- pcr(Salary ~ . , data = df, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

# Compute the test MSE.
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7)
mean((pcr.pred - y.test)^2)

# Fit PCR on the full data set, using M = 7, the number of components identified by cross-validation.
pcr.fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

# PARTIAL LEAST SQUARES

# Implement partial least squares (PLS) using the plsr() function.
set.seed(1)
pls.fit <- plsr(Salary ~ . , data = df, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)

# Evaluate the test set MSE that corresponds to the lowest cross-validation error.
pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2)

# Perform PLS using the full data set, using M = 2, the number of components identified by cross-validation.
pls.fit <- plsr(Salary ~ . , data = df, scale = TRUE, ncomp = 2)
summary(pls.fit)
