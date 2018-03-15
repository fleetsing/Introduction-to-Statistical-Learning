
# Install and import the relevant libraries.
library(ISLR)
library(MASS)
library(class)

# THE STOCK MARKET DATA

# Create a data frame.
df <- Smarket
print(head(df))
print(dim(df))
print(str(df))
print(summary(df))

# Check the correlation matrix.
print(cor(df[,-9]))

# Plot the Volume term.
plot(df$Volume)

# LOGISTIC REGRESSION

# Fit a logistic regression model that tries to predict the Direction using Lag1 through Lag5 and Volume.
log.model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial(link = "logit"), data = df)
summary(log.model)

# Check the coefficients of the model.
coef(log.model)
summary(log.model)$coef
summary(log.model)$coef[, 4]

# Predict the probabilities of the stock market going up for the training data.
model.probabilities <- predict(log.model, type = "response")
head(model.probabilities)
contrasts(df$Direction)

# Make the predictions given the probabilities.
model.predictions <- NULL

for (i in 1:length(model.probabilities)) {
  if (model.probabilities[i] > 0.5) {
    model.predictions[i] <- "Up"
    
  } else {
    model.predictions[i] <- "Down"
  
  }
}

# Create the confusion matrix for the predictions.
table(model.predictions, df$Direction)

# Calculate the precision of the model (probability that a predicted Up is correct).
507 / (507 + 457)

# Calculate the recall of the model (probablity that a predicted Down is correct).
507 / (507 + 141)

# Calculate what percentage of the predictions are correct.
(145 + 507) / length(model.predictions)
mean(model.predictions == df$Direction)

# Create a model using the data from years 2001 through 2004, and try to predict the Direction for 2005.
train <- df$Year < 2005
df.2005 <- df[!train,]
dim(df.2005)
Direction.2005 <- df$Direction[!train]

# Fit a logistic regression model using only the subset of the obser- vations that correspond to dates before 2005.
log.model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family = binomial(link = "logit"), data = df, subset = train)
model.probabilities <- predict(log.model, df.2005, type = "response")

model.predictions <- NULL

for (i in 1:length(model.probabilities)) {
  if (model.probabilities[i] > 0.5) {
    model.predictions[i] <- "Up"
    
  } else {
    model.predictions[i] <- "Down"
    
  }
}

table(model.predictions, Direction.2005)
mean(model.predictions == Direction.2005)
mean(model.predictions != Direction.2005)

# LINEAR DISCRIMINANT ANALYSIS

# Fit an LDA model using the lda() function from the MASS library.
model.lda <- lda(Direction ~ Lag1 + Lag2, data = df, subset = train)
model.lda

lda.predict <- predict(model.lda, df.2005)
names(lda.predict)

lda.class <- lda.predict$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
mean(lda.class != Direction.2005)

# QUADRATIC DISCRIMINANT ANALYSIS

# Fit a QDA model using the qda() function from the MASS library.
model.qda <- qda(Direction ~ Lag1 + Lag2, data = df, subset = train)
model.qda

qda.predict <- predict(model.qda, df.2005)
names(qda.predict)

qda.class <- qda.predict$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
mean(qda.class != Direction.2005)

# K NEAREST NEIGHBOURS

# Fit a KNN model using the knn() function from the class library.
train.X <- cbind(df$Lag1, df$Lag2)[train,]
test.X <- cbind(df$Lag1, df$Lag2)[!train,]
train.direction <- df$Direction[train]

set.seed(1)
knn.predict <- knn(train.X, test.X, train.direction, k = 1)

table(knn.predict, Direction.2005)
mean(knn.predict == Direction.2005)
mean(knn.predict != Direction.2005)


