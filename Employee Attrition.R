
library(tidyverse)

library(caret)

library(GGally)

library(broom)

library(readxl) # read excel files

library(pROC) # Sampling-over and under, ROC and AUC curve

library(margins) # for average marginal effects
### Reading Data + Creating Regression ----------------------------------------------------------------

data=read_excel("Employee_Data_Project.xlsx")
summary(data)

# check missing values
data |>
  summarize(
    across(everything(), function(x) sum(is.na(x)))
  )

na <- sum(data == "NA")
print(na)
data[data=="NA"] <- NA
data <- na.omit(data)

data$Attrition <- ifelse(data$Attrition == "Yes", 1, 0)

data$TotalWorkingYears <- as.numeric(data$TotalWorkingYears)
data$NumCompaniesWorked <- as.numeric(data$NumCompaniesWorked)
data$EnvironmentSatisfaction <- as.numeric(data$EnvironmentSatisfaction)
data$JobSatisfaction <- as.numeric(data$JobSatisfaction)

str(data)

# Creating Training Variables
test_idx <- createDataPartition(
  data$Attrition,
  p = 0.3
)


data_test <- data[test_idx[[1]], ]

data_train <- data[-test_idx[[1]], ]

validation_idx <- createDataPartition(
  data_train$Attrition,
  p = 0.3
)

data_validate <- data[validation_idx[[1]], ]

data_train <- data_train[-validation_idx[[1]], ]

logistic_model <- glm(
  Attrition ~ . - EmployeeID,
  data = data_train,
  family = binomial("logit")
)

# Summarize the model
summary(logistic_model)

# + BusinessTravel (Frequent or Rarely), + Marital Status (Single), + NumCompaniesWorked, - TotalWorkingYears, - YearsWithCurrManager, 
# - EnvironmentSatisfaction, and - JobSatisfaction are all significant predictors (p-value < 0.05) of attrition, and age!

# Income, Gender, DistanceFromHome, and Education do not show significant relationships with attrition (p-value > 0.05)
# Current Null Deviance: 1874.9, Residual Deviance: 1609.4, AIC: 1645.4

#### Marginal Distribution----------------------------------------------------------------
# Check the marginal distribution
data_marginal_dist <- 
  data %>%
  mutate(
    #    age_cat = case_when(
    #      Age >= 40 ~ "Old",
    #      TRUE ~ "Young"
    #    ),
    attrition_cat = case_when(
      Attrition == 1 ~ "Left",
      TRUE ~ "Stayed"
    )
  ) %>%
  # Select the attrition category and create a frequency table
  select(attrition_cat) %>% ### Add Age Cat if I'm including age <<<<
  table() %>%
  # Calculate the proportions for attrition (Yes/No)
  proportions() 


# Print the marginal di
print(data_marginal_dist)

data_marginal_dist_age <- 
  data %>%
  mutate(
    age_cat = case_when(
      Age >= 40 ~ "Old",
      TRUE ~ "Young"
    ),
    attrition_cat = case_when(
      Attrition == 1 ~ "Left",
      TRUE ~ "Stayed"
    )
  ) %>%
  # Select the attrition category and create a frequency table
  select(age_cat, attrition_cat) %>% 
  table() %>%
  # Calculate the proportions for attrition (Yes/No)
  proportions() 

print(data_marginal_dist_age)

#### Plotting Age vs Attrition 4 ----------------------------------------------------------------
data_train |>
  mutate(
    Attrition = factor(Attrition)
  ) |>
  select(Age, Attrition) |>
  ggpairs()

data_train |>
  mutate(
    Attrition = factor(Attrition)
  ) |>
  select(Age, Attrition) |>
  ggpairs(aes(color = Attrition, alpha = 0.3))

#<<

# Fit a logistic regression model with Age as a predictor
logistic_model_2 <- glm(Attrition ~ Age, data = data_train, family = binomial("logit"))

# Extract the coefficients from the fitted logistic regression model
coefs <- coef(logistic_model_2)

# Create the plot for Age vs. Attrition
data_train %>%
  ggplot(aes(x = Age, y = Attrition)) + 
  geom_point() +  # Actual data points
  geom_line(aes(y = 1 / (1 + exp(-1 * (coefs[1] + coefs[2] * Age))))) +  # Fitted line from logistic model
  ylab("Probability of Attrition") + 
  labs(
    title = "Probability of Attrition by Age",
    subtitle = "Actual Data Points (Points) vs. Fitted Curve (Line)"
  ) 
# probability of attrition decreases as age increases. 
# Younger employees have a higher likelihood of attrition, while older employees are less likely to leave



#### Model comparison -----------------------------------------------

#<<<<<<<<<<<<<<<<<<<<<<<<<<  AIC <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Model 1: One-variable model with `Age`
model_1 <- glm(Attrition ~ Age, data = data_train, family = binomial("logit"))
summary(model_1)
## AIC: 1844.6 # I tried using Data Validate but it wouldn't work!!

# Model 2: Two-variable model with `Age` and `Gender`
model_2 <- glm(Attrition ~ Age + Gender, data = data_train, family = binomial("logit"))
summary(model_2)
## AIC: 1845.4

# Model 3: Three-variable model with `Age`, `Gender`, and `JobSatisfaction`
model_3 <- glm(Attrition ~ Age + Gender + JobSatisfaction, data = data_train, family = binomial("logit"))
summary(model_3)
## AIC: 1819.8

# Model 4: Interaction model with `Age`, `Gender`, `JobSatisfaction`, `Income`, and interaction `Gender:Income`
model_4 <- glm(Attrition ~ Age * JobSatisfaction  + Gender * Income + JobSatisfaction * Income, data = data_validate, family = binomial("logit"))
## AIC: 1821.9
#<<<<<<<<<<<<<<<<<<<<<<<<  Precision & Recall <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##Model 1
confusion1 <- confusionMatrix( 
  data = case_when(
    model_1$fitted.values >= 0.5 ~ 1,
    TRUE ~ 0
  ) |>
    factor(),
  reference = data_train$Attrition |> 
    factor()
)
confusion1$table # see our confusion matrix
confusion1$byClass # get precision and recall
##Model 2
confusion2 <- confusionMatrix( 
  data = case_when(
    model_2$fitted.values >= 0.5 ~ 1,
    TRUE ~ 0
  ) |>
    factor(),
  reference = data_train$Attrition |> 
    factor()
)
confusion2$table 
confusion2$byClass 
##Model 3
confusion3 <- confusionMatrix( 
  data = case_when(
    model_3$fitted.values >= 0.5 ~ 1,
    TRUE ~ 0
  ) |>
    factor(),
  reference = data_train$Attrition |> 
    factor()
)
confusion3$table 
confusion3$byClass 
## Model 4
confusion4 <- confusionMatrix( 
  data = case_when(
    model_4$fitted.values >= 0.5 ~ 1,
    TRUE ~ 0
  ) |>
    factor(),
  reference = factor(data_validate$Attrition, levels = c(0, 1)) ### Had to search this up
)
confusion4$table 
confusion4$byClass 
#<<<<<<<<<<<<<<<<<<<<<<<<  AUC <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Create a tibble with actual values and predicted probabilities 
f_roc_model_1 <- tibble(
  actual = data_validate$Attrition,
  predicted = predict(model_1, data_validate, type = "response")
) |>
  roc("actual", "predicted")

plot(f_roc_model_1)

f_roc_model_1$auc 

#model 2
f_roc_model_2 <- tibble(
  actual = data_validate$Attrition,
  predicted = predict(model_2, data_validate, type = "response")
) |>
  roc("actual", "predicted") 

plot(f_roc_model_2)

#Model 3
f_roc_model_3 <- tibble(
  actual = data_validate$Attrition,
  predicted = predict(model_3, data_validate, type = "response")
) |>
  roc("actual", "predicted") 

plot(f_roc_model_3)

f_roc_model_3$auc

#Model 4
f_roc_model_4 <- tibble(
  actual = data_validate$Attrition,
  predicted = predict(model_4, data_validate, type = "response")
) |>
  roc("actual", "predicted") 

plot(f_roc_model_4)

f_roc_model_4$auc 

### Line graph -----------------------------------------------------------------
# Model 1: One-variable model with `Age`
preds_test_1 <- predict(model_1, newdata = data_test, type = "response")

# Create ROC curve for Model 1 using the test set
roc_test_1 <- roc(
  data = tibble(
    actual = data_test |> select(Attrition) |> unlist(),
    predicted = preds_test_1
  ),
  "actual",
  "predicted"
)
# Plot the ROC curve for Model 1
roc_test_1$auc
#0.608


# Model 2: Two-variable model with `Age` and `Gender`
preds_test_2 <- predict(model_2, newdata = data_test, type = "response")
roc_test_2 <- roc(
  data = tibble(
    actual = data_test |> select(Attrition) |> unlist(),
    predicted = preds_test_2
  ),
  "actual",
  "predicted"
)

roc_test_2$auc
#0.602


# Model 3: Three-variable model with `Age`, `Gender`, and `JobSatisfaction`
preds_test_3 <- predict(model_3, newdata = data_test, type = "response")
roc_test_3 <- roc(
  data = tibble(
    actual = data_test |> select(Attrition) |> unlist(),
    predicted = preds_test_3
  ),
  "actual",
  "predicted"
)
roc_test_3$auc
#0.632

# Model 4: Interaction model with `Age`, `Gender`, `JobSatisfaction`, `Income`, and interaction `Gender:Income`
preds_test_4 <- predict(model_4, newdata = data_test, type = "response")
roc_test_4 <- roc(
  data = tibble(
    actual = data_test |> select(Attrition) |> unlist(),
    predicted = preds_test_4
  ),
  "actual",
  "predicted"
)
roc_test_4$auc
#0.646


plot(roc_test_1, col = "blue")
lines(roc_test_2, col = "red")
lines(roc_test_3, col = "green")
lines(roc_test_4, col = "purple")

legend("bottomright", 
       legend = c("Model 1: Age", 
                  "Model 2: Age + Gender", 
                  "Model 3: Age + Gender + JobSatisfaction", 
                  "Model 4: Interaction Model"), 
       col = c("blue", "red", "green", "purple"), lwd = 2)


### Conclusion
# The AUC values for all four models are very similar, ranging between 0.647 and 0.6497.
# Model 3 has the highest AUC value of 0.6497, which indicates a slight improvement in discriminatory power compared to the other models.
