##################################################
############# 02 Regression Analyses #############
##################################################

### 1) Preparations 

# Set working directory

input <- "/Users/Author/Desktop/Documents/NLE&SE/Analyses"
setwd(input)

# Load data 

load("NLESE_lgmm.RData")
load("NLESE_topmod.RData")

# Load relevant packages

library(tidyverse)
library(lcmm)
library(stats)
library(nnet)
library(broom)
library(pscl)

# Filter data set for time point 1 (as other time points are not relevant for the regression analysis)

lgmm_data <- lgmm_data %>% filter(time == "t1")

# Relevel factors in data set

lgmm_data$gender <- relevel(lgmm_data$gender, ref = "female")

lgmm_data$born <- relevel(lgmm_data$born, ref = "yes")

lgmm_data$eventtype <- relevel(lgmm_data$eventtype, ref = "Ende einer Beziehung")

lgmm_data$education <- relevel(lgmm_data$education, ref = "lower education")

# Set group with least amount of change as reference category

summary(topmod)

lgmm_data$class <- as.factor(lgmm_data$class)

lgmm_data$class <- relevel(lgmm_data$class, ref = "1")

### 2) Stepwise multiple Regression with best fitting latent class growht model

# Stepwise regression for each predictor-category for group 1 

lm_grp1_null <- lm(prob1 ~ 1, data = lgmm_data)


lm_grp1_type_only <- lm(prob1 ~ eventtype, data = lgmm_data)

lm_grp1_type_other <- lm(prob1 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                         extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)

lm_grp1_perc_only <- lm(prob1 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww, data = lgmm_data)

lm_grp1_perc_other <- lm(prob1 ~ eventtype + extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)

lm_grp1_pers_only <- lm(prob1 ~ extra + agree + consc + neuro + open, data = lgmm_data)

lm_grp1_pers_other <- lm(prob1 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           gender + age + education + income + born, data = lgmm_data)

lm_grp1_demo_only <- lm(prob1 ~ gender + age + education + income + born, data = lgmm_data)

lm_grp1_demo_other <- lm(prob1 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open, data = lgmm_data)


lm_grp1_full <- lm(prob1 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                          extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)




# Evaluate relative importance of the predictors in group 1 using R-Squared

summary(lm_grp1_null)

summary(lm_grp1_type_only)
summary(lm_grp1_type_other)

summary(lm_grp1_perc_only)
summary(lm_grp1_perc_other)

summary(lm_grp1_pers_only)
summary(lm_grp1_pers_other)

summary(lm_grp1_demo_only)
summary(lm_grp1_demo_other)

summary(lm_grp1_full)


# Stepwise regression four group 2

lm_grp2_null <- lm(prob2 ~ 1, data = lgmm_data)


lm_grp2_type_only <- lm(prob2 ~ eventtype, data = lgmm_data)

lm_grp2_type_other <- lm(prob2 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


lm_grp2_perc_only <- lm(prob2 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww, data = lgmm_data)

lm_grp2_perc_other <- lm(prob2 ~ eventtype + extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


lm_grp2_pers_only <- lm(prob2 ~ extra + agree + consc + neuro + open, data = lgmm_data)

lm_grp2_neuro_only <- lm(prob2 ~ neuro, data = lgmm_data)

lm_grp2_pers_other <- lm(prob2 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           gender + age + education + income + born, data = lgmm_data)

lm_grp2_neuro_other <- lm(prob2 ~ extra + agree + consc + open + eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           gender + age + education + income + born, data = lgmm_data)


lm_grp2_demo_only <- lm(prob2 ~ gender + age + education + income + born, data = lgmm_data)

lm_grp2_demo_other <- lm(prob2 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open, data = lgmm_data)


lm_grp2_full <- lm(prob2 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                          extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)

# Evaluate relative importance of the predictors in group 2 using R-Squared

summary(lm_grp2_null)

summary(lm_grp2_type_only)
summary(lm_grp2_type_other)

summary(lm_grp2_perc_only)
summary(lm_grp2_perc_other)

summary(lm_grp2_pers_only)
summary(lm_grp2_neuro_only)
summary(lm_grp2_pers_other)
summary(lm_grp2_neuro_other)

summary(lm_grp2_demo_only)
summary(lm_grp2_demo_other)

summary(lm_grp2_full)


# Stepwise regression four group 3

lm_grp3_null <- lm(prob3 ~ 1, data = lgmm_data)


lm_grp3_type_only <- lm(prob3 ~ eventtype, data = lgmm_data)

lm_grp3_type_other <- lm(prob3 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


lm_grp3_perc_only <- lm(prob3 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww, data = lgmm_data)

lm_grp3_perc_other <- lm(prob3 ~ eventtype + extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


lm_grp3_pers_only <- lm(prob3 ~ extra + agree + consc + neuro + open, data = lgmm_data)

lm_grp3_pers_other <- lm(prob3 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           gender + age + education + income + born, data = lgmm_data)


lm_grp3_demo_only <- lm(prob3 ~ gender + age + education + income + born, data = lgmm_data)

lm_grp3_demo_other <- lm(prob3 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open, data = lgmm_data)


lm_grp3_full <- lm(prob3 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                          extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)




# Evaluate relative importance of the predictors in group 3 using R-Squared

summary(lm_grp3_null)

summary(lm_grp3_type_only)
summary(lm_grp3_type_other)

summary(lm_grp3_perc_only)
summary(lm_grp3_perc_other)

summary(lm_grp3_pers_only)
summary(lm_grp3_pers_other)

summary(lm_grp3_demo_only)
summary(lm_grp3_demo_other)

summary(lm_grp3_full)

# Stepwise regression four group 4

lm_grp4_null <- lm(prob4 ~ 1, data = lgmm_data)

lm_grp4_type_only <- lm(prob4 ~ eventtype, data = lgmm_data)

lm_grp4_type_other <- lm(prob4 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


lm_grp4_perc_only <- lm(prob4 ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww, data = lgmm_data)

lm_grp4_perc_other <- lm(prob4 ~ eventtype + extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


lm_grp4_pers_only <- lm(prob4 ~ extra + agree + consc + neuro + open, data = lgmm_data)

lm_grp4_pers_other <- lm(prob4 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           gender + age + education + income + born, data = lgmm_data)


lm_grp4_demo_only <- lm(prob4 ~ gender + age + education + income + born, data = lgmm_data)

lm_grp4_demo_other <- lm(prob4 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open, data = lgmm_data)

lm_grp4_full <- lm(prob4 ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                          extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


# Evaluate relative importance of the predictors in group 4 using R-Squared

summary(lm_grp4_null)

summary(lm_grp4_type_only)
summary(lm_grp4_type_other)

summary(lm_grp4_perc_only)
summary(lm_grp4_perc_other)

summary(lm_grp4_pers_only)
summary(lm_grp4_pers_other)

summary(lm_grp4_demo_only)
summary(lm_grp4_demo_other)

summary(lm_grp4_full)

### 3) Stepwise multinomial regression (incl. sensitivity analysis)

# Exclude participants with low probabilities for all groups (< 0.50)

lgmm_data_filtered <- lgmm_data %>% filter(!(prob1 < 0.5 & prob2 < 0.5 & prob3 < 0.5 & prob4 < 0.5))

# Stepwise multinomial regression for each predictor-category using filtered data

mnls_null <- multinom(class ~ 1, data = lgmm_data_filtered)


mnls_type_only <- multinom(class ~ eventtype, data = lgmm_data_filtered)

mnls_type_other <- multinom(class ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data_filtered)


mnls_perc_only <- multinom(class ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww, data = lgmm_data_filtered)

mnls_perc_other <- multinom(class ~ eventtype + extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data_filtered)


mnls_pers_only <- multinom(class ~ extra + agree + consc + neuro + open, data = lgmm_data_filtered)

mnls_pers_other <- multinom(class ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           gender + age + education + income + born, data = lgmm_data_filtered)


mnls_demo_only <- multinom(class ~ gender + age + education + income + born, data = lgmm_data_filtered)

mnls_demo_other <- multinom(class ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                           extra + agree + consc + neuro + open, data = lgmm_data_filtered)


mnls_full <- multinom(class ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                          extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data_filtered)

# Calculate pseudo R-squared

mcfadden_mnls_type_only <- pR2(mnls_type_only)["McFadden"]
mcfadden_mnls_type_other <- pR2(mnls_type_other)["McFadden"]
mcfadden_mnls_perc_only <- pR2(mnls_perc_only)["McFadden"]
mcfadden_mnls_perc_other <- pR2(mnls_perc_other)["McFadden"]
mcfadden_mnls_pers_only <- pR2(mnls_pers_only)["McFadden"]
mcfadden_mnls_pers_other <- pR2(mnls_pers_other)["McFadden"]
mcfadden_mnls_demo_only <- pR2(mnls_demo_only)["McFadden"]
mcfadden_mnls_demo_other <- pR2(mnls_demo_other)["McFadden"]
mcfadden_mnls_full_model <- pR2(mnls_full)["McFadden"]

# Calculate Odds Ratios and Confidence Intervals

sum_mnls_type_only <- tidy(mnls_type_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnls_type_other <- tidy(mnls_type_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnls_perc_only <- tidy(mnls_perc_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnls_perc_other <- tidy(mnls_perc_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnls_pers_only <- tidy(mnls_pers_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnls_pers_other <- tidy(mnls_pers_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnls_demo_only <- tidy(mnls_demo_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnls_demo_other <- tidy(mnls_demo_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnls_full <- tidy(mnls_full) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

# Stepwise multinomial regression for each predictor-category using unfiltered data

mnl_null <- multinom(class ~ 1, data = lgmm_data)


mnl_type_only <- multinom(class ~ eventtype, data = lgmm_data)

mnl_type_other <- multinom(class ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                              extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


mnl_perc_only <- multinom(class ~ chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww, data = lgmm_data)

mnl_perc_other <- multinom(class ~ eventtype + extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)


mnl_pers_only <- multinom(class ~ extra + agree + consc + neuro + open, data = lgmm_data)

mnl_pers_other <- multinom(class ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                              gender + age + education + income + born, data = lgmm_data)


mnl_demo_only <- multinom(class ~ gender + age + education + income + born, data = lgmm_data)

mnl_demo_other <- multinom(class ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                              extra + agree + consc + neuro + open, data = lgmm_data)


mnl_full <- multinom(class ~ eventtype + chall + emosig + extcon + extrao + impact + predict + socstat + val + changeww +
                             extra + agree + consc + neuro + open + gender + age + education + income + born, data = lgmm_data)

# Calculate pseudo R-squared

mcfadden_mnl_type_only <- pR2(mnl_type_only)["McFadden"]
mcfadden_mnl_type_other <- pR2(mnl_type_other)["McFadden"]
mcfadden_mnl_perc_only <- pR2(mnl_perc_only)["McFadden"]
mcfadden_mnl_perc_other <- pR2(mnl_perc_other)["McFadden"]
mcfadden_mnl_pers_only <- pR2(mnl_pers_only)["McFadden"]
mcfadden_mnl_pers_other <- pR2(mnl_pers_other)["McFadden"]
mcfadden_mnl_demo_only <- pR2(mnl_demo_only)["McFadden"]
mcfadden_mnl_demo_other <- pR2(mnl_demo_other)["McFadden"]
mcfadden_mnl_full_model <- pR2(mnl_full)["McFadden"]

# Calculate Odds Ratios and Confidence Intervals

sum_mnl_type_only <- tidy(mnl_type_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnl_type_other <- tidy(mnl_type_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnl_perc_only <- tidy(mnl_perc_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnl_perc_other <- tidy(mnl_perc_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnl_pers_only <- tidy(mnl_pers_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnl_pers_other <- tidy(mnl_pers_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnl_demo_only <- tidy(mnl_demo_only) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )
sum_mnl_demo_other <- tidy(mnl_demo_other) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

sum_mnl_full <- tidy(mnl_full) %>%
  mutate(
    Odds_Ratio = exp(estimate), # Odds Ratios
    CI_Lower = exp(estimate - 1.96 * std.error), # Lower CI
    CI_Upper = exp(estimate + 1.96 * std.error)  # Upper CI
  )

vec <- c(1,2,3)

### 4) Export results

save.image(file = "Results.RData")









