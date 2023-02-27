# Data-analytics-on-adult-data

```{r setup, include=FALSE}
# load required package
library(dplyr)
library(tidyselect)
library(tidyverse)
library(ISLR)
library(tidyverse)
library(glm2)
library(glm.predict)
library(MASS)
```

```{r}
#importing the data
library(readr)
adult <- read_csv("C:/Users/Victor/Desktop/Adult")
View(adult)
```

1.Build a logistic regression model to predict the income of a person based on their age, education (as a number, with variable education.num), and the hours worked per week. Obtain the summary of the model.

```{r, error=FALSE, warning=FALSE}
# load adult data set
data(adult)
#transforming the income to binary
adult$income_binary <- ifelse(adult$income == ">50K", 1, 0)


# build logistic regression model
logit_model <- glm(adult$income_binary ~ adult$age + adult$`education-num` + adult$`hours-per-week`, data = adult, family = "binomial")


# obtain summary of the model
summary(logit_model)
```
The output shows the results of a logistic regression model fit using the glm function in R. The model is trying to predict the binary variable adult$income_binary (which takes on the value of 0 or 1) based on the continuous variables adult$age, adult$education-num, and adult$hours-per-week.

The Deviance Residuals section shows the minimum, first quartile, median, third quartile, and maximum values of the deviance residuals, which are a measure of the goodness-of-fit of the model. In this case, all the values are very small (on the order of 10^-6), which indicates that the model is fitting the data well.

The Coefficients section shows the estimated coefficients for each variable, along with their standard errors, z-values, and p-values. The intercept has a large negative value, which indicates that the probability of having an income greater than 50K is very low when all the predictor variables are zero. The coefficients for the predictor variables are very small, which suggests that they are not very strong predictors of the outcome variable. The large p-values indicate that none of the predictor variables are statistically significant in predicting the outcome.

The Null deviance and Residual deviance sections show the deviance values for the null model (which has no predictors) and the fitted model, respectively. The difference between these values indicates the improvement in deviance due to the addition of the predictor variables. The AIC value provides a measure of the relative goodness-of-fit of the model, with lower values indicating better fit. In this case, the AIC is very low (8), which indicates a good fit.

Finally, the Number of Fisher Scoring iterations section shows the number of iterations required to fit the model using the Fisher scoring algorithm, which is used to estimate the maximum likelihood parameters in logistic regression.

2.Are there any variables that should be removed from the model from the previous exercise? If so, remove the variables and return the model.
```{r, warning=FALSE}
# Check the significance of each variable
summary(logit_model)

# Based on the p-values, remove the least significant variable, which is hours.per.week in this case
logit_model2 <- glm(adult$income_binary ~ adult$age + adult$`education-num` , data = adult, family = "binomial")

# obtain summary of the model
summary(logit_model2)

```
This output is the result of fitting a binary logistic regression model to predict the probability of a binary income variable based on the age and education level of the individuals in the adult dataset.

The "Coefficients" section displays the estimated coefficients for the intercept and the two predictor variables, age and education level. The estimates suggest that age and education level are not significant predictors of income, as both have estimated coefficients close to zero, and very large standard errors.

The "Deviance Residuals" section displays the residuals of the model. The residuals are all very small, suggesting that the model fits the data very well.

The "Null deviance" and "Residual deviance" sections display the deviance of the null model (which only includes an intercept term) and the residual deviance of the logistic regression model, respectively. The small residual deviance suggests that the model fits the data well.

The AIC value of 6 indicates that this model is a good fit for the data, as it has a low AIC value.

Overall, the output suggests that the model does not find age or education level to be significant predictors of income, and the model has a very good fit to the data.

3.Write the descriptive form of the final logistic regression model from the previous exercise.
The final logistic regression model is:
```{r, warning=FALSE}
logit_model2 <- glm(adult$income_binary ~ adult$`education-num` , data = adult, family = "binomial")
summary(logit_model2)
```

This output is the result of fitting a binary logistic regression model to predict the probability of a binary income variable based on the education level of the individuals in the adult dataset.

The "Coefficients" section displays the estimated coefficients for the intercept and the education level predictor variable. The estimate for the education level variable is positive, suggesting that higher education level is associated with higher income. However, the very small coefficient estimate and large standard error indicate that this relationship is not statistically significant.

The "Deviance Residuals" section displays the residuals of the model. The residuals are all very small, suggesting that the model fits the data very well.

The "Null deviance" and "Residual deviance" sections display the deviance of the null model (which only includes an intercept term) and the residual deviance of the logistic regression model, respectively. The small residual deviance suggests that the model fits the data well.

The AIC value of 4 indicates that this model is a good fit for the data, as it has a low AIC value.

Overall, the output suggests that the model does not find education level to be a significant predictor of income, but the model has a very good fit to the data.

The dependent variable is income, and the independent variables are age and education.num.

4.Interpret the coefficient of the age variable.
```{r, warning=FALSE}
logit_model3 <- glm(adult$income_binary ~ adult$age , data = adult, family = "binomial")
summary(logit_model3)
```

The coefficient of the age variable is 1.675e-16. This coefficient indicates the estimated change in the response variable (dependent variable) for a one-unit increase in the predictor variable (independent variable), which in this case is age.

Since the coefficient is very small, i.e., 1.675e-16, it implies that there is no significant relationship between age and the response variable. This can be further supported by the high p-value (Pr(>|z|)) of 1.000, indicating that age is not a statistically significant predictor of the response variable at the chosen level of significance.

It's worth noting that the intercept's coefficient of -2.657e+01 is not relevant in this interpretation as it represents the estimated value of the response variable when the predictor variable (age) is equal to 0, which is not applicable in this case as age cannot be zero.


5.Find the impact on the probability of having high income for every 10 years a person is older.
To find the impact on the probability of having a high income for every 10 years a person is older, we need to calculate the exponentiated coefficient of the age variable raised to the power of 10.
```{r}
exp(1.675e-16 * 10)

```
The result is 1. This means that for every 10 years increase in age, the odds of having a high income increases upto 1, or the probability of having a high income is nearly 1.

6.	Interpret the coefficient of the education.num variable
The coefficients of education.num represent the estimated effect of the variable "education.num" on the outcome variable in a statistical model.

In this case, the intercept coefficient (-2.657e+01) indicates the expected outcome variable value when education.num is zero. However, in this case, it is not relevant because education.num cannot take a zero value, as it is a measure of years of education.

The coefficient for "education.num" (1.314e-13) suggests that there is a positive relationship between education.num and the outcome variable, but the estimated effect is so small that it is practically negligible. Specifically, the coefficient suggests that for every additional year of education, the outcome variable increases by 1.314e-13 units, which is essentially zero.

The standard error (8.809e+02) indicates the degree of uncertainty in the estimated coefficient, which is relatively high in this case. The p-value (1.000) suggests that the coefficient for education.num is not statistically significant, which means that the observed relationship between education.num and the outcome variable could be due to chance and not a true relationship in the population.

Overall, based on the coefficient and p-value, it appears that education.num does not have a meaningful effect on the outcome variable in this model.



7.Find the impact on the probability of having high income for every four more years of education a person has.
```{r, warning=FALSE}
# Build a logistic regression model
model1 <- glm(adult$income_binary ~ adult$`education-num` , data = adult, family = "binomial")

# Get the coefficient estimate and its standard error
summary(model1)$coef
```
The logistic regression coefficient for education-num is 1.314454e-13, which indicates that for every one-unit increase in education-num (i.e., for every additional year of education), the estimated increase in the log-odds of having high income is 1.314454e-13. To interpret this effect in terms of probability, we need to exponentiate this coefficient to obtain the odds ratio
```{r}
# Exponentiate the coefficient estimate to get the odds ratio
odds_ratio <- exp(1.314454e-13)
odds_ratio

```
This odds ratio of 1 means that, for every additional year of education, the odds of having high income increase by a factor of 1, or essentially by 0%.

To calculate the impact of every four more years of education on the probability of having high income, we can multiply the odds ratio by itself four times, since the coefficient is for a one-unit increase in education-num
```{r}
exp(1.314454e-13 * 4)

```
This odds ratio of 1 means that, for every additional four years of education, the odds of having high income increase by a factor of 1, or essentially by 0%. Therefore, based on this logistic regression model, having more education does not appear to have a meaningful impact on the probability of having high income.



8: Interpret the coefficient of the hours.per.week variable
To interpret the coefficient of the hours.per.week variable, we will build a logistic regression model using the glm() function and the income variable as the response variable and the hours.per.week variable as the predictor. We will then use the summary() function to obtain the coefficient estimate and its standard error.
```{r, warning=FALSE}
# Build a logistic regression model
model2 <- glm(adult$income_binary ~ adult$`hours-per-week`, data = adult, family = "binomial")

# Get the coefficient estimate and its standard error
summary(model2)$coef

```
Based on the results provided, the coefficient for the hours.per.week variable is 3.306701e-16. This coefficient represents the change in the response variable for a one-unit increase in the hours.per.week variable, while holding all other variables constant.

However, it's important to note that the coefficient has a very small value and a very large standard error of 183.1305. Additionally, the p-value for the coefficient is 1.0000000, which indicates that it is not statistically significant at any reasonable level of significance.

Therefore, it's difficult to interpret the coefficient meaningfully. It's possible that the hours.per.week variable is not important in predicting the response variable and that other variables may have a stronger effect. Further investigation is needed to determine the importance of the hours.per.week variable in the model.


9.To find the impact on the probability of having high income for every five more hours per week a person works, you can use logistic regression. First, create a binary variable indicating if a person has a high income:
```{r, warning=FALSE}
adult <- adult %>%
  mutate(high_income = ifelse(income == ">50K", 1, 0))

model3 <- glm(high_income ~ adult$`hours-per-week`, data = adult, family = "binomial")
summary(model3)

```
Based on the provided results, the coefficient for the hours-per-week variable is 3.307e-16. This coefficient represents the change in the log odds of having a high income for a one-unit increase in hours-per-week, holding all other variables constant.

To find the impact on the probability of having a high income for every five more hours per week a person works, we need to calculate the odds ratio for a five-unit increase in the hours-per-week variable. The odds ratio can be calculated as:

exp(5 * coefficient)

Substituting the coefficient value, we get:

exp(5 * 3.307e-16) = 1.000001654

This means that a five-unit increase in the hours-per-week variable is associated with a very small increase in the odds of having a high income, approximately by a factor of 1.000001654.

To convert the odds ratio to a probability, we need to use the following formula:

probability = odds / (1 + odds)

Assuming a baseline odds of 0.5 (which is the midpoint between 0 and 1), we can calculate the probability of having a high income with and without the five-unit increase in hours-per-week as follows:

probability with increase = 0.5 * 1.000001654 / (1 + 0.5 * 1.000001654) = 0.500000827

probability without increase = 0.5 / (1 + 0.5) = 0.3333333

The difference in probability is:

probability with increase - probability without increase = 0.500000827 - 0.3333333 = 0.1666675

Therefore, a five-unit increase in hours-per-week is associated with an increase in the probability of having a high income by approximately 0.1666675 or 16.67%. However, as noted earlier, the coefficient for the hours-per-week variable is not statistically significant, so this result should be interpreted with caution.

10.To obtain the predicted values using the model from the previous exercise, you can use the predict() function:
```{r}
predicted_values <- predict(model3)
summary(predicted_values)

```
```{r}
plot(predicted_values, adult$`education-num`, xlab = "Predicted values", ylab = "Actual values")
```
The predicted values are nearly same to the actual values

11.To build a poisson regression model to predict the years of education a person has based on age and hours per week, we can use the following code:
```{r}
model4 <- glm(adult$`education-num` ~ adult$age + adult$`hours-per-week`, data = adult, family = "poisson")
summary(model4)

```

12.To determine if there are any other variables that should be removed from the model, you can use the stepAIC() function from the MASS package
```{r, warning=FALSE, error=FALSE}

model5 <- stepAIC(model4)
summary(model5)
```

13.To write the descriptive form of the final poisson regression model, you can use the exp() function to exponentiate the coefficients
```{r}
exp(coef(model5))

```
Years of education = 1.000425age+ 1.002966hours-per-week+8.792550

14.To obtain the predicted values using the final poisson regression model and compare them to the actual values, you can use the predict() function:
```{r}
#To obtain the predicted values, you can use the predict() function
predicted_values2 <- predict(model5)

#To compare the predicted values with the actual values, you can use a scatter plot
plot(predicted_values2, adult$`education-num`, xlab = "Predicted values", ylab = "Actual values")

```
The comparison of predicted and actual values is as shown above.


```{r}
cor(predicted_values2, adult$`education-num`)

```
The result of 0.1476652 indicates that there is a weak positive correlation between predicted_values2 and adult$education-num. However, it is important to note that correlation does not imply causation, and other factors may be influencing the relationship between these variables. Additionally, the strength and direction of the relationship may vary depending on the context and the data being analyzed.

