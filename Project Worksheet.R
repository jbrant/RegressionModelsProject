library(ggplot2)
library(gridExtra)
library(ggfortify)

#############################################################
## Load the dataset and discuss the structure and attributes
#############################################################

### Note: this includes the executive summary ###

## Load the dataset
data(mtcars)

## Rename dataset columns in order to improve readability
names(mtcars) <- c(
  "MPG"
  ,"Cylinders"
  ,"Displacement"
  ,"Horsepower"
  ,"RearAxleRatio"
  ,"Weight"
  ,"QuarterMileTime"
  ,"VS"
  ,"Transmission"
  ,"NumForwardGears"
  ,"NumCarburetors"
)

## Convert transmission value to string literals
mtcars$Transmission[mtcars$Transmission == 0] <- "Automatic"
mtcars$Transmission[mtcars$Transmission == 1] <- "Manual"

## Convert cylinders, transmission, V/S, forward gears, and carburetors to factors
mtcars$Cylinders <- as.factor(mtcars$Cylinders)
mtcars$Transmission <- as.factor(mtcars$Transmission)
mtcars$VS <- as.factor(mtcars$VS)
mtcars$NumForwardGears <- as.factor(mtcars$NumForwardGears)
mtcars$NumCarburetors <- as.factor(mtcars$NumCarburetors)

############################
## Exploratory Data Analysis
############################

## Print out the dataset variables
names(mtcars)

## Take a look at the data set structure
str(mtcars)

## Print overview of data
summary(mtcars)

######################################
## Transmission effect on Fuel Economy
######################################

### Note: this answers the question of manual being better than automatic for MPG ###

### Note the hypothesis:
### Null Hypothesis: no MPG difference between automatic and manual
### Alt Hypothesis: difference between automatic and manual

## Display box plot to get an idea of automatic vs manual range
ggplot(mtcars, aes(Transmission, MPG)) +
  geom_boxplot(alpha=I(2/3), aes(color=Transmission)) +
  labs(
    title = "MPG per Transmission") +
  theme(plot.title = element_text(face = "bold", size = 16, vjust = 1),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none")

## Run Welch two-sample t-test on MPG for the two transmission types at 95% CI
t.test(MPG ~ Transmission, mtcars, conf.level = 0.95, var.equal = FALSE, paired = FALSE)

### Note: Reject null

###############################################################
## Quantifying Differential Transmission effect on Fuel Economy
###############################################################

### Note: this answers the question of quantifying the difference between the two ###

### Note:
### Null hypothesis: transmission type solely responsible for fuel efficiency
### Alt hypothesis: confounders exist that contribute as much or more than transmission type

## Perform linear regression with all variables
full.model <- lm(MPG ~ ., mtcars)

## Run stepwise regression (i.e. selection in both directions) 
## to select best explanatory variables
stepwise.selected.best.model <- step(full.model, direction = "both")
summary(stepwise.selected.best.model)

### Note the following:
### 1. Presence of additional variables and P-value indicates 
###    a highly significant result in favor of the alternative hypothesis.
### 2. Some variables are more significant explanatory variables than others
###    (6 cylinder and horsepower 95% CI, weight 99% CI)
### 3. Adjusted R^2 indicates this model explains more than 84% of variability.

## Perform stepwise regression on model including transmission 
## as the only explanatory variable
transmission.model <- lm(MPG ~ Transmission, mtcars)
stepwise.selected.transmission <- summary(step(transmission.model, direction = "both"))

## Run analysis of variance on the two candidate models
anova(transmission.model, stepwise.selected.best.model)

### Note: the residual sum of squares is much lower for the model including
### the engine displacement, horsepower, weight, and transmission (as opposed
### to the one with just the transmission), indicating a better fit.  Moreover,
### there's a highly significant p-value (p < 0.01 at 99% CI), so the null 
### hypothesis that fuel economy is wholly explained by transmission can be 
### confidently rejected.

## Generate the residual and diagnostic plots
autoplot(stepwise.selected.best.model)

### Note: the residuals vs. fitted plot indicates some level of homoscedasticity in the lack
### of correlation between residuals and fitted values.

### Note: the Q-Q plot depicts a tight fit of the standardized residuals to the normal line,
### indicating a roughly normal distribution of residuals.

### Note: the scale-location plot depicts a loess curve that slopes downward, and then
### relatively steeply upward. This would seem to indicate a less than ideal uniformity
### between variances.

### Note: the residuals vs. leverage indicates that the distribution of leverage vs. residuals
### is fairly well-balanced; however, the bulk of the data resides in the downard sloping
### portion of the line, indicating overall high leverage and low residual.

#############
## Conclusion
#############

## 1. Motor Trend can be reasonably assured that overall, cars with manual transmissions will
##    have better fuel economy than cars with automatic transmissions if all other factors are
##    held constant.
## 2. Transmission is not the only variable that has an effect on fuel economy, nor is its
##    effect the most pronounced.  In addition to transmission, greater mass, 
##    more horsepower, and larger engine sizes are all inversely related to fuel economy.