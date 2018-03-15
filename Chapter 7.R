
# Install and import the relevant libraries.

library(ISLR)
library(splines)
library(gam)
library(akima)

# POLYNOMIAL REGRESSION AND STEP FUNCTIONS

# Import the used data set.
df <- Wage

# Fit a fourth-degree orthogonal polynomial model.
fit <- lm(wage ~ poly(age, 4), data = df)
summary(fit)
coef(summary(fit))

# Fit a fourth-degree polynomial model.
fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = df)
coef(summary(fit2))

# Create a grid of values for age at which we want predictions.
agelims <- range(df$age)
age.grid <- seq(from = agelims[1], to = agelims[2])

# Call the generic predict() function, specifying that we want standard errors as well.
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
head(se.bands)

# Plot the data and add the fit from the degree-4 polynomial.
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(df$age, df$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Degree 4-polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Fit models ranging from linear to a degree-5 polynomial and seek to determine the simplest sufficient model.
fit.1 <- lm(wage ~ age, data = df)
fit.2 <- lm(wage ~ poly(age, 2), data = df)
fit.3 <- lm(wage ~ poly(age, 3), data = df)
fit.4 <- lm(wage ~ poly(age, 4), data = df)
fit.5 <- lm(wage ~ poly(age, 5), data = df)

# Use the anova() function to test the null hypothesis that a model M1 is sufficient to explain the data.
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# Apply the glm() function using family="binomial" in order to fit a polynomial logistic regression model.
fit <- glm(I(wage > 250) ~ poly(age, 4), data = df, family = binomial)
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)

# Calculate the confidence intervals.
pfit = exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# Plot the model.
plot(df$age, I(df$wage > 250), xlim = agelims, type = "n", ylim = c(0, .2))
points(jitter(df$age), I((df$wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

# Use the cut() function in order to fit a step function.
table(cut(df$age, 4))
fit <- lm(wage ~ cut(age, 4), data = df)
coef(summary(fit))

# SPLINES

# Fit wage to age using a regression spline, and plot the data.
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = df)
pred <- predict(fit, newdata = list(age = age.grid), se = T)

plot(df$age, df$wage, col = "grey")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(age.grid, pred$fit - 2 * pred$se.fit, lty = "dashed")

# Use the df option to produce a spline with knots at uniform quantiles of the data.
dim(bs(df$age, knots = c(25, 40, 60)))
dim(bs(df$age, df = 6))
attr(bs(df$age, df = 6), "knots")

# Fit a natural spline with four degrees of freedom.
fit2 <- lm(wage ~ ns(age, df = 4), data = df)
pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
lines(age.grid, pred2$fit, col = "red", lwd = 2)

# Fit a smoothing spline.
plot(df$age, df$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing spline")

fit <- smooth.spline(df$age, df$wage, df = 16)
fit2 <- smooth.spline(df$age, df$wage, cv = TRUE)
fit2$df

lines(fit, col = "red", lwd = 2)
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Perform local linear regression using spans of 0.2 and 0.5.
plot(df$age, df$wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = df)
fit2 <- loess(wage ~ age, span = .5, data = df)

lines(age.grid, predict(fit, data.frame(age = age.grid)), col = "red", lwd = 2)
lines(age.grid, predict(fit2, data.frame(age = age.grid)), col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"), col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# GENERAL ADDITIVE MODELS

# Fit a GAM to predict wage using natural spline functions of year and age, treating education as a qualitative predictor.
gam1 <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = df)

# Use the gam() function in order to fit a GAM.
gam.m3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = df)

# Plot the function.
par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")

plot.gam(gam1, se = TRUE, col = "red")

# Perform a series of ANOVA tests to determine the best of the three models given.
gam.m1 <- gam(wage ~ s(age, 5) + education, data = df)
gam.m2 <- gam(wage ~ year + s(age, 5) + education, data = df)
anova(gam.m1, gam.m2, gam.m3, test = "F")

# Make predictions on the training set.
preds <- predict(gam.m2, newdata = df)

# Use local regression fits as building blocks in a GAM, using the lo() function.
gam.lo <- gam(wage ~ s(year, df = 4) + lo(age, span = .7) + education, data = df)
plot.gam(gam.lo, se = TRUE, col = "green")

gam.lo.i <- gam(wage ~ lo(year, age, span = 0.5) + education, data = df)
plot(gam.lo.i)

# Fit a logistic regression GAM.
gam.lr <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = df)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")

table(df$education, I(df$wage > 250))

gam.lr.s <- gam(I(wage > 250) ~ year + s(age, df = 5) + education, family = binomial, data = df, subset = (education != "1. < HS Grad"))
plot(gam.lr.s, se = T, col = "green")
