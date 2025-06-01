#######################################
###### 01 Data preparation extern #####
#######################################

#### 1) Preparations -----------------------------------------------------------

# Load packages

library(tidyverse)

# Set working directory 

input <- "/Users/Author/Desktop/Documents/NLE&SE/Data"
output <- "/Users/Author/Desktop/Documents/NLE&SE/Analyses"
setwd(input)

# Load data

load("NLESE_clean_wide.RData")

#### 2) Recode reversed keyed-items --------------------------------------------

# T1
data$t1_ecq.13r <- 6- data$t1_ecq.13
data$t1_ecq.14r <- 6- data$t1_ecq.14
data$t1_ecq.21r <- 6- data$t1_ecq.21
data$t1_ecq.23r <- 6- data$t1_ecq.23
data$t1_ecq.24r <- 6- data$t1_ecq.24
data$t1_ecq.30r <- 6- data$t1_ecq.30
data$t1_ecq.32r <- 6- data$t1_ecq.32
data$t1_ecq.33r <- 6- data$t1_ecq.33

data$t1_bfi.1r <- 6 - data$t1_bfi.1
data$t1_bfi.5r <- 6 - data$t1_bfi.5
data$t1_bfi.7r <- 6 - data$t1_bfi.7
data$t1_bfi.8r <- 6 - data$t1_bfi.8
data$t1_bfi.12r <- 6 - data$t1_bfi.12
data$t1_bfi.14r <- 6 - data$t1_bfi.14

data$t1_rses.2r <- 7 - data$t1_rses.2
data$t1_rses.5r <- 7 - data$t1_rses.5
data$t1_rses.6r <- 7 - data$t1_rses.6
data$t1_rses.8r <- 7 - data$t1_rses.8
data$t1_rses.9r <- 7 - data$t1_rses.9

# T2

data$t2_rses.2r <- 7 - data$t2_rses.2
data$t2_rses.5r <- 7 - data$t2_rses.5
data$t2_rses.6r <- 7 - data$t2_rses.6
data$t2_rses.8r <- 7 - data$t2_rses.8
data$t2_rses.9r <- 7 - data$t2_rses.9

# T3

data$t3_rses.2r <- 7 - data$t3_rses.2
data$t3_rses.5r <- 7 - data$t3_rses.5
data$t3_rses.6r <- 7 - data$t3_rses.6
data$t3_rses.8r <- 7 - data$t3_rses.8
data$t3_rses.9r <- 7 - data$t3_rses.9

# T4

data$t4_rses.2r <- 7 - data$t4_rses.2
data$t4_rses.5r <- 7 - data$t4_rses.5
data$t4_rses.6r <- 7 - data$t4_rses.6
data$t4_rses.8r <- 7 - data$t4_rses.8
data$t4_rses.9r <- 7 - data$t4_rses.9

# T5

data$t5_rses.2r <- 7 - data$t5_rses.2
data$t5_rses.5r <- 7 - data$t5_rses.5
data$t5_rses.6r <- 7 - data$t5_rses.6
data$t5_rses.8r <- 7 - data$t5_rses.8
data$t5_rses.9r <- 7 - data$t5_rses.9


#### 3) Compute scale scores ---------------------------------------------------

# Big Five traits
data$t1_extra <- rowMeans(data.frame(data$t1_bfi.1r, data$t1_bfi.2, data$t1_bfi.3))
data$t1_agree <- rowMeans(data.frame(data$t1_bfi.4, data$t1_bfi.5r, data$t1_bfi.6))
data$t1_consc <- rowMeans(data.frame(data$t1_bfi.7r, data$t1_bfi.8r, data$t1_bfi.9))
data$t1_neuro <- rowMeans(data.frame(data$t1_bfi.10, data$t1_bfi.11, data$t1_bfi.12r))
data$t1_open <- rowMeans(data.frame(data$t1_bfi.13, data$t1_bfi.14r, data$t1_bfi.15))

# Perceived event characteristics
data$t1_ecq.chall <- rowMeans(data.frame(data$t1_ecq.1, data$t1_ecq.2, data$t1_ecq.3, data$t1_ecq.4))
data$t1_ecq.emosig <- rowMeans(data.frame(data$t1_ecq.5, data$t1_ecq.6, data$t1_ecq.7, data$t1_ecq.8))
data$t1_ecq.extcon <- rowMeans(data.frame(data$t1_ecq.9, data$t1_ecq.10, data$t1_ecq.11, data$t1_ecq.12))
data$t1_ecq.extrao <- rowMeans(data.frame(data$t1_ecq.13r, data$t1_ecq.14r, data$t1_ecq.15, data$t1_ecq.16))
data$t1_ecq.impact <- rowMeans(data.frame(data$t1_ecq.17, data$t1_ecq.18, data$t1_ecq.19, data$t1_ecq.20))
data$t1_ecq.predict <- rowMeans(data.frame(data$t1_ecq.21r, data$t1_ecq.22, data$t1_ecq.23r, data$t1_ecq.24r))
data$t1_ecq.socstat <- rowMeans(data.frame(data$t1_ecq.25, data$t1_ecq.26, data$t1_ecq.27, data$t1_ecq.28))
data$t1_ecq.val <- rowMeans(data.frame(data$t1_ecq.29, data$t1_ecq.30r, data$t1_ecq.31, 
                                       data$t1_ecq.32r, data$t1_ecq.33r, data$t1_ecq.34))
data$t1_ecq.changeww <- rowMeans(data.frame(data$t1_ecq.35, data$t1_ecq.36, data$t1_ecq.37, data$t1_ecq.38))

# Self-Esteem

data$t1_se <- rowMeans(data.frame(data$t1_rses.1, data$t1_rses.2r, data$t1_rses.3, data$t1_rses.4, data$t1_rses.5r,
                                  data$t1_rses.6r, data$t1_rses.7, data$t1_rses.8r, data$t1_rses.9r, data$t1_rses.10))

data$t2_se <- rowMeans(data.frame(data$t2_rses.1, data$t2_rses.2r, data$t2_rses.3, data$t2_rses.4, data$t2_rses.5r,
                                  data$t2_rses.6r, data$t2_rses.7, data$t2_rses.8r, data$t2_rses.9r, data$t2_rses.10))

data$t3_se <- rowMeans(data.frame(data$t3_rses.1, data$t3_rses.2r, data$t3_rses.3, data$t3_rses.4, data$t3_rses.5r,
                                  data$t3_rses.6r, data$t3_rses.7, data$t3_rses.8r, data$t3_rses.9r, data$t3_rses.10))

data$t4_se <- rowMeans(data.frame(data$t4_rses.1, data$t4_rses.2r, data$t4_rses.3, data$t4_rses.4, data$t4_rses.5r,
                                  data$t4_rses.6r, data$t4_rses.7, data$t4_rses.8r, data$t4_rses.9r, data$t4_rses.10))

data$t5_se <- rowMeans(data.frame(data$t5_rses.1, data$t5_rses.2r, data$t5_rses.3, data$t5_rses.4, data$t5_rses.5r,
                                  data$t5_rses.6r, data$t5_rses.7, data$t5_rses.8r, data$t5_rses.9r, data$t5_rses.10))

#### 4) Create and recode factors ----------------------------------------------

# Gender

data$t1_gender.f <- factor(data$t1_gender, levels = c(1,2,3), labels = c("male", "female", "other"))

# Education

data$t1_education.f <- as.numeric(dplyr::recode(data$t1_education, "1" = "1", "2" = "1", "3" = "1", "4" = "1", "5" = "2", "6" = "2", "7" = "3"))
data$t1_education.f <- factor(data$t1_education.f, levels = c(1,2,3), labels = c("lower education", "higher education", "other education"))

# Check number of subjects per level
sum(data$t1_education.f == "lower education", na.rm = TRUE)
sum(data$t1_education.f == "higher education", na.rm = TRUE)
sum(data$t1_education.f == "other education", na.rm = TRUE)

# Set values for factor-level "other education" to NA's, due to low number of subjects
data$t1_education.f <- as.character(data$t1_education.f)
data$t1_education.f[data$t1_education.f == "other education"] <- NA
data$t1_education.f <- as.factor(data$t1_education.f)

# Migration status 

data$t1_born.f <- factor(data$t1_born, levels = c(1,2), labels = c("yes", "no"))

# Event type

data$t1_event.f <- as.factor(data$t1_event)

#### 5) Compute time variables -------------------------------------------------

# Linear time variable (Weeks)
data$t1_time.lin <- data$t1_weeks.ago
data$t2_time.lin <- as.numeric(data$t1_weeks.ago + difftime(data$t2_date, data$t1_date, unit = "week"))
data$t3_time.lin <- as.numeric(data$t1_weeks.ago + difftime(data$t3_date, data$t1_date, unit = "week"))
data$t4_time.lin <- as.numeric(data$t1_weeks.ago + difftime(data$t4_date, data$t1_date, unit = "week"))
data$t5_time.lin <- as.numeric(data$t1_weeks.ago + difftime(data$t5_date, data$t1_date, unit = "week"))

# Linear time variable (Months)

data$t1_time.lin.m <- data$t1_time.lin / 4.345
data$t2_time.lin.m <- data$t2_time.lin / 4.345
data$t3_time.lin.m <- data$t3_time.lin / 4.345
data$t4_time.lin.m <- data$t4_time.lin / 4.345
data$t5_time.lin.m <- data$t5_time.lin / 4.345

# Quadratic time variables (Weeks)
data$t1_time.quad <- data$t1_time.lin^2
data$t2_time.quad <- data$t2_time.lin^2
data$t3_time.quad <- data$t3_time.lin^2
data$t4_time.quad <- data$t4_time.lin^2
data$t5_time.quad <- data$t5_time.lin^2

# Quadratic time variables (Months)

data$t1_time.quad.m <- data$t1_time.quad / 4.345
data$t2_time.quad.m <- data$t2_time.quad / 4.345
data$t3_time.quad.m <- data$t3_time.quad / 4.345
data$t4_time.quad.m <- data$t4_time.quad / 4.345
data$t5_time.quad.m <- data$t5_time.quad / 4.345

#### 6) Standardize variables --------------------------------------------------

# self-esteem (t1-t5)
data <- data %>% mutate(
  t1_se_mean = mean(t1_se, na.rm = TRUE),
  t1_se_sd = sd(t1_se, na.rm = TRUE)
)

data <- data %>% mutate(
  t1_se_z = (t1_se - t1_se_mean) / t1_se_sd,
  t2_se_z = (t2_se - t1_se_mean) / t1_se_sd,
  t3_se_z = (t3_se - t1_se_mean) / t1_se_sd,
  t4_se_z = (t4_se - t1_se_mean) / t1_se_sd,
  t5_se_z = (t5_se - t1_se_mean) / t1_se_sd
)

# big five (t1 only)
data <- data %>% mutate(
  t1_extra_mean = mean(t1_extra, na.rm = TRUE),
  t1_agree_mean = mean(t1_agree, na.rm = TRUE),
  t1_consc_mean = mean(t1_consc, na.rm = TRUE),
  t1_neuro_mean = mean(t1_neuro, na.rm = TRUE),
  t1_open_mean = mean(t1_open, na.rm = TRUE),
  
  t1_extra_sd = sd(t1_extra, na.rm = TRUE),
  t1_agree_sd = sd(t1_agree, na.rm = TRUE),
  t1_consc_sd = sd(t1_consc, na.rm = TRUE),
  t1_neuro_sd = sd(t1_neuro, na.rm = TRUE),
  t1_open_sd = sd(t1_open, na.rm = TRUE)
)

data <- data %>% mutate(
  t1_extra_z = (t1_extra - t1_extra_mean) / t1_extra_sd,
  t1_agree_z = (t1_agree - t1_agree_mean) / t1_agree_sd,
  t1_consc_z = (t1_consc - t1_consc_mean) / t1_consc_sd,
  t1_neuro_z = (t1_neuro - t1_neuro_mean) / t1_neuro_sd,
  t1_open_z = (t1_open - t1_open_mean) / t1_open_sd
)

# perceived event characteristics (t1 only)
data <- data %>% mutate(
  t1_ecq.chall_mean = mean(t1_ecq.chall, na.rm = TRUE),
  t1_ecq.emosig_mean = mean(t1_ecq.emosig, na.rm = TRUE),
  t1_ecq.extcon_mean = mean(t1_ecq.extcon, na.rm = TRUE),
  t1_ecq.extrao_mean = mean(t1_ecq.extrao, na.rm = TRUE),
  t1_ecq.impact_mean = mean(t1_ecq.impact, na.rm = TRUE),
  t1_ecq.predict_mean = mean(t1_ecq.predict, na.rm = TRUE),
  t1_ecq.socstat_mean = mean(t1_ecq.socstat, na.rm = TRUE),
  t1_ecq.val_mean = mean(t1_ecq.val, na.rm = TRUE),
  t1_ecq.changeww_mean = mean(t1_ecq.changeww, na.rm = TRUE),
  
  t1_ecq.chall_sd = sd(t1_ecq.chall, na.rm = TRUE),
  t1_ecq.emosig_sd = sd(t1_ecq.emosig, na.rm = TRUE),
  t1_ecq.extcon_sd = sd(t1_ecq.extcon, na.rm = TRUE),
  t1_ecq.extrao_sd = sd(t1_ecq.extrao, na.rm = TRUE),
  t1_ecq.impact_sd = sd(t1_ecq.impact, na.rm = TRUE),
  t1_ecq.predict_sd = sd(t1_ecq.predict, na.rm = TRUE),
  t1_ecq.socstat_sd = sd(t1_ecq.socstat, na.rm = TRUE),
  t1_ecq.val_sd = sd(t1_ecq.val, na.rm = TRUE),
  t1_ecq.changeww_sd = sd(t1_ecq.changeww, na.rm = TRUE)
)

data <- data %>% mutate(
  t1_ecq.chall_z = (t1_ecq.chall - t1_ecq.chall_mean) / t1_ecq.chall_sd,
  t1_ecq.emosig_z = (t1_ecq.emosig - t1_ecq.emosig_mean) / t1_ecq.emosig_sd,
  t1_ecq.extcon_z = (t1_ecq.extcon - t1_ecq.extcon_mean) / t1_ecq.extcon_sd,
  t1_ecq.extrao_z = (t1_ecq.extrao - t1_ecq.extrao_mean) / t1_ecq.extrao_sd,
  t1_ecq.impact_z = (t1_ecq.impact - t1_ecq.impact_mean) / t1_ecq.impact_sd,
  t1_ecq.predict_z = (t1_ecq.predict - t1_ecq.predict_mean) / t1_ecq.predict_sd,
  t1_ecq.socstat_z = (t1_ecq.socstat - t1_ecq.socstat_mean) / t1_ecq.socstat_sd,
  t1_ecq.val_z = (t1_ecq.val - t1_ecq.val_mean) / t1_ecq.val_sd,
  t1_ecq.changeww_z = (t1_ecq.changeww - t1_ecq.changeww_mean) / t1_ecq.changeww_sd
)

#### 7) Exclusion criteria -----------------------------------------------------

# Participants with no/incorrect instructed response items were already excluded in intern data preparation

# Exclude participants who report an event that occurred more than 6 weeks ago

data <- filter(data, t1_weeks.ago <= 5 & !is.na(t1_weeks.ago))

### 8) Create long format data set

# Rename existing wide data set for clarity reasons

data_wide_full <- data
rm(data)

# Transform to long format

data_long_full <- data_wide_full %>%
  mutate_all(as.character) %>%
  pivot_longer(
    cols = -c(id),          
    names_to = c("time", "variable"),  
    names_pattern = "(t\\d+)_(.+)",    
    values_to = "value"                
  )

data_long_full <- data_long_full %>%
  pivot_wider(
    names_from = variable,  
    values_from = value 
  )

data_long_full <- data_long_full %>%
  mutate(across(!c(id, time, date, education, income, event, event.f, gender.f, education.f, born.f), as.numeric))

data_long_full <- data_long_full %>%
  mutate(across(c(event.f, gender.f, education.f, born.f), as.factor))

### 9) Create wide data set with only the variables relevant for planned analysis

data_wide <- dplyr::select(data_wide_full, id, t1_gender.f, t1_age, t1_education.f, t1_income, t1_born.f, t1_event.f,
                           t1_se_z, t2_se_z, t3_se_z, t4_se_z, t5_se_z,
                           t1_extra_z, t1_agree_z, t1_consc_z, t1_neuro_z, t1_open_z,
                           t1_ecq.chall_z, t1_ecq.emosig_z, t1_ecq.extcon_z, t1_ecq.extrao_z, 
                           t1_ecq.impact_z, t1_ecq.predict_z, t1_ecq.socstat_z, t1_ecq.val_z, t1_ecq.changeww_z,
                           t1_time.lin, t2_time.lin, t3_time.lin, t4_time.lin, t5_time.lin,
                           t1_time.quad, t2_time.quad, t3_time.quad, t4_time.quad, t5_time.quad,
                           t1_time.lin.m, t2_time.lin.m, t3_time.lin.m, t4_time.lin.m, t5_time.lin.m,
                           t1_time.quad.m, t2_time.quad.m, t3_time.quad.m, t4_time.quad.m, t5_time.quad.m)

### 9) Convert wide data set with only relevant variables in to long format

data_long <- data_wide %>%
  mutate_all(as.character) %>%
  pivot_longer(
    cols = -c(id),          
    names_to = c("time", "variable"),  
    names_pattern = "(t\\d+)_(.+)",    
    values_to = "value"                
  )

data_long <- data_long %>%
  pivot_wider(
    names_from = variable,  
    values_from = value 
  )

data_long <- data_long %>%
  mutate(across(!c(id, time, event.f, gender.f, education.f, born.f), as.numeric))

data_long <- data_long %>%
  mutate(across(c(event.f, gender.f, education.f, born.f), as.factor))

data_long <- dplyr::rename(data_long,
                           gender = gender.f,
                           education = education.f,
                           born = born.f,
                           eventtype = event.f,
                           self_esteem = se_z,
                           extra = extra_z,
                           agree = agree_z,
                           consc = consc_z,
                           neuro = neuro_z,
                           open = open_z,
                           chall = ecq.chall_z,
                           emosig = ecq.emosig_z,
                           extcon = ecq.extcon_z,
                           extrao = ecq.extrao_z,
                           impact = ecq.impact_z,
                           predict = ecq.predict_z,
                           socstat = ecq.socstat_z,
                           val = ecq.val_z,
                           changeww = ecq.changeww_z)
                           
### 9) Export datasets -------------------------------------------------------

save(data_wide_full, file = "NLESE_wide_full.RData")
save(data_long_full, file = "NLESE_long_full.RData")
save(data_wide, file = "NLESE_wide.RData")
save(data_long, file = "NLESE_long.RData")






