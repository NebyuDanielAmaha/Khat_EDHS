library(tableone)
library(survey)
library(broom)
library(lavaan)

# Define variables to include in Table 1
vars <- c("maternal_age3","residence" ,"religion2", "education3", "wealth3", 
          "working", "smoking_status", "drinks_alcohol",
          "husband_edu", "husband_help", 
          "husband_alcohol", "husband_chat_ever")

# Define stratifying variable: maternal khat chewing
strata <- "s1107a"

# Create Table 1
tab1 <- CreateTableOne(vars = vars, strata = strata, data = data, factorVars = vars)

# Print with standardized formatting
print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE)

#output
# Extract table as data.frame
tab1_df <- as.data.frame(print(tab1, showAllLevels = TRUE, quote = FALSE, noSpaces = TRUE))

# Write to CSV
# write.csv(tab1_df, "table1_results.csv", row.names = FALSE)


#Table 2

# Multivariable logistic regression
model_full <- glm(s1107a ~ maternal_age3 + residence +religion2 + education3 + wealth3 +
                    working + smoking_status + drinks_alcohol +husband_edu + husband_help +
                    husband_alcohol + husband_chat_ever,
                  data = data, family = binomial)

# Model summary
summary(model_full)

# Odds ratios with 95% CI
exp(cbind(OR = coef(model_full), confint(model_full)))

#Export
results_full <- tidy(model_full, conf.int = TRUE, exponentiate = TRUE) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# Save to CSV
# write.csv(results_full, "table2_results.csv", row.names = FALSE)

#Table 3

# Dummy code the variables using model.matrix (drops reference level automatically)
data_model <- data %>%
  # Create dummy variables for each multi-level factor
  mutate(
    # maternal_age3: ref = "15-24"
    age25_34 = ifelse(maternal_age3 == "25-34", 1, 0),
    age35_49 = ifelse(maternal_age3 == "35-49", 1, 0),
    
    # education3: ref = "No education"
    edu_primary = ifelse(education3 == "Primary", 1, 0),
    edu_secondary_plus = ifelse(education3 == "Secondary+", 1, 0),
    
    # wealth3: ref = "Poor"
    wealth_middle = ifelse(wealth3 == "Middle", 1, 0),
    wealth_rich = ifelse(wealth3 == "Rich", 1, 0),
    
    # husband_edu: ref = "No education"
    husbedu_primary = ifelse(husband_edu == "Primary", 1, 0),
    husbedu_secondary_plus = ifelse(husband_edu == "Secondary+", 1, 0)
  ) %>%
  # Keep binary factors as-is (they are already 0/1 or factors with 2 levels)
  # But ensure others are numeric or binary factors
  mutate(
    residence = as.numeric(residence == "Urban"),  # Urban = 1
    religion2 = as.numeric(religion2 == "Muslim"),  # Muslim = 1
    working = as.numeric(working == "Yes"),
    smoking_status = as.numeric(smoking_status == "Yes"),
    drinks_alcohol = as.numeric(drinks_alcohol == "Yes"),
    husband_help = as.numeric(husband_help == "Yes"),
    husband_alcohol = as.numeric(husband_alcohol == "Yes"),
    husband_chat_ever = as.numeric(husband_chat_ever == "Yes")
  )

sem_model <- "
  s1107a ~ age25_34 + age35_49 + residence +
           religion2 + 
           edu_primary + edu_secondary_plus +
           wealth_middle + wealth_rich +
           working +
           smoking_status +
           drinks_alcohol +
           husbedu_primary + husbedu_secondary_plus +
           husband_help +
           husband_alcohol +
           husband_chat_ever
"

fit_lavaan <- sem(
  model = sem_model,
  data = data_model,
  ordered = "s1107a",        # s1107a is binary
  estimator = "WLSMV"        # For categorical outcome
)

# Check results
summary(fit_lavaan, standardized = TRUE)

estimates <- parameterEstimates(fit_lavaan, standardized = TRUE) %>%
  filter(op == "~", lhs == "s1107a") %>%
  select(Predictor = rhs, Coef = est, SE = se, p.value = pvalue, `2.5%` = ci.lower, `97.5%` = ci.upper)

print(estimates)
write.csv(estimates, "table3_results_sep3.csv", row.names = FALSE)

#Table 3
# sem_model <- "
#   s1107a ~ maternal_age3 + religion2 + education3 + wealth3 + working +
#   smoking_status + drinks_alcohol + husband_edu + husband_help + husband_alcohol + husband_chat_ever
#   "
# #fit the model
# 
# # Fit the Bayesian SEM model with logistic link
# fit_lavaan <- sem(
#   model = sem_model,
#   data = data,
#   ordered = "s1107a",   # specify binary/categorical variable
#   estimator = "WLSMV"   # robust estimator suitable for categorical outcomes
# )
# 
# # View summary with posterior estimates
# summary(fit_lavaan, standardized = TRUE)
# 
# # Get parameter estimates
# estimates <- parameterEstimates(fit_lavaan, standardized = TRUE)
# 
# # # Filter regression paths to s1107a
# # regression_estimates <- estimates %>%
# #   filter(op == "~", lhs == "s1107a")
# # 
# # # Add odds ratios and credible intervals
# # regression_estimates <- regression_estimates %>%
# #   mutate(
# #     OR = exp(est),
# #     OR_lower = exp(ci.lower),
# #     OR_upper = exp(ci.upper)
# #   ) %>%
# #   select(rhs, est, se, pvalue, OR, OR_lower, OR_upper)
# 
# # Filter only the regression paths (predictors → s1107a)
# regression_estimates <- estimates %>%
#   filter(lhs == "s1107a", op == "~") %>%
#   select(
#     Predictor = rhs,
#     Coef = est,
#     SE = se,
#     z = z,
#     p.value = pvalue,
#     `2.5%` = ci.lower,
#     `97.5%` = ci.upper,
#     Std.All = std.all  # Standardized probit coefficient
#   ) %>%
#   mutate(
#     across(c(Coef, SE, z, `2.5%`, `97.5%`, Std.All), ~ round(., 3)),
#     p.value = round(p.value, 4)
#   )
# 
# # Display the OR results
# print(regression_estimates)
# 
# # write.csv(regression_estimates, "table3_results.csv", row.names = FALSE)

