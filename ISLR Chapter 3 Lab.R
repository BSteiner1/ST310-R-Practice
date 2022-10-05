library(MASS)
library(ISLR2)

# Practice
df <- data.frame(team=c('A', 'B', 'C', NA, 'E'),
                 points=c(99, 90, 86, 88, 95),
                 assists=c(NA, 28, NA, NA, 34),
                 rebounds=c(30, 28, 24, 24, NA))

# Sum gives us a count
sapply(df, function(x) sum(is.na(x)))

# Explore the data
head(Boston)
names(Boston)
dim(Boston)
# Metadata
?Boston
# View(Boston)
attach(Boston)

# Perform Linear Regression
lm.fit <- lm(medv ~ lstat, data = Boston)

# Summary statistics on the regression
lm.fit
summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# Individual predictions
predict(lm.fit, data.frame(lstat = (c(5,10,15))), interval = "confidence")

# Plot the regression
plot(lstat, medv, col = "red", pch = 20)
# Add regression line
abline(lm.fit, lwd = 2)
# Add horizontal line for mean y-value
abline(mean(medv), 0, col = "blue", lwd = 1.5)

# Examine some new plots
# In particular, "Residuals vs Fitted" is of importance
par(mfrow = c(2,2))
plot(lm.fit)

# Resize plot window and new residuals plot
par(mfrow = c(2,1))
plot(predict(lm.fit), residuals(lm.fit), col = "red", pch = 20)
abline(0, 0, lwd = 2)
# Studentised residual plot
plot(predict(lm.fit), rstudent(lm.fit), col = "red", pch = 20)
abline(0, 0, lwd = 2)

par(mfrow = c(1,1))
# Identify leverage by index
which.max(hatvalues(lm.fit))
plot(hatvalues(lm.fit), pch = 20, col = rm)

# Syntax is: regress y on x1,x2,x3 etc.
lm.fit0 <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit0)

# Fit all other variables
lm.fit <- lm(medv ~ . , data = Boston)
summary(lm.fit)

# How much has our R-Squared value changed by?
increase <- summary(lm.fit)$r.sq - summary(lm.fit0)$r.sq
increase <- signif(increase, 3)
increase
print(paste("Increase in R-Squared value: ",increase))

# Examine VIF
library(car)
vif(lm.fit)
# VIF's are mostly low (<5) so we have low multicollinearity

# Run a regression excluding "age" since it has a large p-value
lm.fit1 <- lm(medv ~ . - age, data = Boston)
summary(lm.fit1)

# Include interaction terms
summary(lm(medv ~ lstat * age, data = Boston))

# Non-Parametric model
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
plot(predict(lm.fit2), residuals(lm.fit2), col = "red", pch = 20)
abline(0, 0, lwd = 2)
# Perform ANOVA on the two models
anova(lm.fit, lm.fit2)
# View other plots
par(mfrow = c(2,2))
plot(lm.fit2)

# Create a fifth-order polynomial fit
lm.fit5 <- lm(medv ~ poly(lstat, 5))
summary(lm.fit5)

# Visual comparison of the different models
par(mfrow = c(2,2))
plot(predict(lm.fit), residuals(lm.fit), col = "red", pch = 20)
abline(0, 0, lwd = 2)
plot(predict(lm.fit2), residuals(lm.fit2), col = "red", pch = 20)
abline(0, 0, lwd = 2)
plot(predict(lm.fit5), residuals(lm.fit5), col = "red", pch = 20)
abline(0, 0, lwd = 2)
par(mfrow = c(1,1))

# Logarithmic transformation
summary(lm(medv ~ log(rm), data = Boston))

