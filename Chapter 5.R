
# Install and import the relevant libraries.
library(ISLR)
library(boot)

# THE VALIDATION SET APPROACH

# Set the seed and create a random sample for subsetting the data into training and test sets.
set.seed(1)
train <- sample(392,196)

# Fit a linear regression model using the training set.
lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)

# Use predict() to estimate the response for the observations, and mean() to calculate the MSE for the validation set.
mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)

# Use poly() to estimate the test error for the polynomial nad cubic regressions.
lm.fit2 <- lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((Auto$mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 <- lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((Auto$mpg - predict(lm.fit3, Auto))[-train]^2)

# LEAVE-ONE-OUT CROSS-VALIDATION

# Use cv.glm() to estimate the test MSE.
glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Create a loop that checks the cross-validation error estimates for polynomials of order 1 to 5.
cv.error <- rep(0, 5)

for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
print(cv.error)

# K-FOLD CROSS-VALIDATION

# Use cv.glm() on the data with K = 10.
set.seed(17)
cv.error.10 <- rep(0, 5)

for (i in 1:5) {
  glm.fit <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K = 10)$delta[1]
}
print(cv.error.10)

# BOOTSTRAP

# Create a function which takes as input the (X,Y) data as well as a vector indicating which observations should be used to estimate α.
alpha.fn <- function(data, index) {
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X,Y)) / (var(X) + var(Y) - 2 * cov(X,Y)))
}

# Use the function to estimate α using all 100 observations from the Portfolio data set.
alpha.fn(Portfolio, 1:100)

# Use the function to estimate α using 100 random observations with replacement from the Portfolio data set
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

# Use the boot() function to automate the randomization for R = 1000 boostrap estimates of α.
boot(Portfolio, alpha.fn, R = 1000)

# Create a function which takes in the data and indices for the observations, and returns the intercept and slope estimates for the linear regression model.
boot.fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

# Apply this function to the full Auto data set in order to compute the estimates of β0 and β1.
boot.fn(Auto, 1:nrow(Auto))

# Use the function to compute the estimates of the intercept and slope of a random sampling with replacement from the data.
boot.fn(Auto, sample(392,392, replace = T))

# Use the boot() function to compute the standard errors of 1,000 bootstrap estimates for the intercept and slope terms.
boot(Auto, boot.fn, R = 1000)

# Use summary() to get the standard errors for the coefficients for a linear regression model.
summary(lm(mpg ~ horsepower, data = Auto))$coef

# Compute the bootstrap standard error estimates and the standard linear regression estimates for a quadratic model.
boot.fn <- function(data, index) {
  coef(lm(mpg ~ horsepower + I(horsepower^2), data = Auto, subset = index))
}

set.seed(1)
boot(Auto, boot.fn, R = 1000)

summary(lm(mpg ~ horsepower + I(horsepower^2), data = Auto))$coef
