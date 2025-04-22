library(tidyverse)
library(broom)
library(car)
library(pROC)
library(PRROC)
library(boot)
library(MASS)

# Recode CVD outcome
clean_data2 <- clean_data2 %>%
  mutate(CVD = ifelse(CVD == "Yes", 1, 0))

# Define predictor variables
predictor_vars <- setdiff(names(clean_data2), c("id", "CVD"))

# Univariable Log Regression
univariable_results <- data.frame(Variable = character(), p_value = numeric(), stringsAsFactors = FALSE)

for (var in predictor_vars) {
  formula <- as.formula(paste("CVD ~", var))
  model <- glm(formula, data = clean_data2, family = binomial)

  if (is.factor(clean_data2[[var]]) || is.character(clean_data2[[var]])) {
    # Use Likelihood Ratio Test for categorical variables
    null_model <- glm(CVD ~ 1, data = clean_data2, family = binomial)
    p_val <- anova(null_model, model, test = "LRT")$`Pr(>Chi)`[2]
  } else {
    # Use Wald test for continuous variables
    p_val <- summary(model)$coefficients[2, "Pr(>|z|)"]
  }

  univariable_results <- rbind(univariable_results, data.frame(Variable = var, p_value = p_val, stringsAsFactors = FALSE))
}

print(univariable_results)

# Select variables (p < 0.2) for multivariable analysis
selected_vars <- univariable_results %>%
  filter(p_value < 0.2) %>%
  pull(Variable)

print(selected_vars)

# Fit multivariable log model
multivariable_formula <- as.formula(paste("CVD ~", paste(selected_vars, collapse = " + ")))
multivariable_model <- glm(multivariable_formula, data = clean_data2, family = binomial)

# AIC & BIC-selection
# Backward Selection - AIC
final_model_aic <- stepAIC(multivariable_model, direction = "backward", trace = TRUE)

# Perform Selection - BIC
final_model_bic <- stepAIC(multivariable_model, direction = "backward", k = log(nrow(clean_data2)), trace = TRUE)

lr_test <- anova(model_A, model_B, test = "Chisq")
print(lr_test)

# Multicollinearity check 
vif_values <- vif(final_model_aic)
print("VIF values for the multivariable model:")
print(vif_values)

# Model Performance - ROC Curve and AUC
clean_data2_subset <- clean_data2 %>% 
  dplyr::select(CVD, all_of(selected_vars)) %>% 
  drop_na()

clean_data2_subset$predicted_prob <- predict(final_model_aic, newdata = clean_data2_subset, type = "response")

# Compute ROC and AUC
roc_obj <- roc(clean_data2_subset$CVD, clean_data2_subset$predicted_prob)
auc_value <- auc(roc_obj)
ci_auc <- ci.auc(roc_obj)

print(paste("AUC:", round(auc_value, 4), "(95% CI:", round(ci_auc[1], 4), "-", round(ci_auc[3], 4), ")"))

# Optimal cutoff Selection
optimal_coords <- coords(
  roc_obj, x = "best", best.method = "youden",
  ret = c("threshold", "specificity", "sensitivity")
)

optimal_cutoff <- optimal_coords[1]
sensitivity <- optimal_coords[3]
specificity <- optimal_coords[2]

cat(sprintf(
  "Optimal Cutoff: %.2f, Sensitivity: %.2f, Specificity: %.2f\n",
  optimal_cutoff, sensitivity, specificity
))


# Compute AUPRC

# AUPRC function 
auprc_function <- function(data, indices) {
  sampled_data <- data[indices, ]  # Resample data
  pr_obj <- pr.curve(scores.class0 = sampled_data$predicted_prob,
                     weights.class0 = sampled_data$CVD, curve = FALSE)
  return(pr_obj$auc.integral)
}

# Bootstrap AUPRC to get 95% CI
set.seed(1122)  
boot_results <- boot(data = clean_data2_subset, statistic = auprc_function, R = 1000) 

ci_95 <- boot.ci(boot_results, type = "perc")$percent[4:5]

# Print AUPRC with 95% CI
auprc_value <- round(boot_results$t0, 4)
ci_lower <- round(ci_95[1], 4)
ci_upper <- round(ci_95[2], 4)

print(paste("AUPRC of the logistic regression model:", 
            auprc_value, 
            "(95% CI:", ci_lower, "-", ci_upper, ")"))





