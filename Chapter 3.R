
# Install and import the relevant libraries.
library(MASS)
library(ISLR)
library(car)

# SIMPLE LINEAR REGRESSION

# Form a simple linear regression model (lm.fit) with medv as the response and lstat as the predictor.
lm.fit <- lm(medv ~ lstat, data = Boston)

# Get more information on the created model.
lm.fit
summary(lm.fit)
names(lm.fit)

# Coefficients.
coef(lm.fit)

# Confidence intervals.
confint(lm.fit)

# Produce confidence intervals and prediction intervals for the prediction of medv for a given lstat.
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "confidence")
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), interval = "predict")

# Plot medv and lstat along with the least squares regression line.
plot(Boston$lstat, Boston$medv); abline(lm.fit)

# Split the display screen into separate panels so that the diagnostic plots can be viewed simultaneously.
par(mfrow = c(2,2))
plot(lm.fit)
par(mfrow = c(1,1))

# Compute the residuals from a linear regression fit using the residuals() function.
residuals(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Leverage statistics can be computed for any number of predictors using the hatvalues() function.
plot(hatvalues(lm.fit))

# The which.max() function identifies the index of the largest element of a vector.
which.max(hatvalues(lm.fit))


# MULTIPLE LINEAR REGRESSION

# The syntax lm(y∼x1+x2+x3) is used to fit a model with three predictors, x1, x2, and x3.
lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)

# We can use the following short-hand to use all the variables in the data set as predictors.
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

# We can access the individual components of a summary object by name.
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

# The vif() function, part of the car package, can be used to compute variance inflation factors.
vif(lm.fit)

# The following syntax results in a regression using all predictors except age and indus.
lm.fit1 <- lm(medv ~ . -age -indus, data = Boston)
summary(lm.fit1)

# The syntax lstat:black tells R to include an interaction term between lstat and black.
# The syntax lstat*age simultaneously includes lstat, age, and the interaction term lstat×age as predictors;
# it is a shorthand for lstat+age+lstat:age.
summary(lm(medv ~ lstat * age, data = Boston))


# NON-LINEAR TRANSFORMATIONS OF THE PREDICTORS

# Given a predictor X, we can create a predictor X^2 using I(X^2).
lm.fit2 <- lm(medv ~ lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

# anova() function can be used to quantify the extent to which the quadratic fit is superior to the linear.
lm.fit <- lm(medv ~ lstat, data = Boston) 
anova(lm.fit, lm.fit2)

par(mfrow=c(2,2))
plot(lm.fit2)

# poly() function can be used to create a polynomial within lm().
lm.fit5 <- lm(medv ~ poly(lstat, 5), data = Boston)
summary(lm.fit5)
plot(lm.fit5)

lm.fit6 <- lm(medv ~ poly(lstat, 6), data = Boston)
summary(lm.fit6)
plot(lm.fit6)

# We are in no way restricted to using polynomial transforma- tions of the predictors.
summary(lm(medv ~ log(rm), data = Boston))


# QUALITATIVE PREDICTORS

# We will now examine the Carseats data, and attempt to predict Sales based on a number of predictors.
fix(Carseats)
names(Carseats)

lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)

# The contrasts() function returns the coding that R uses for the dummy variables.
contrasts(Carseats$ShelveLoc)

