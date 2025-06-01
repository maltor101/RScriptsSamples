#####################################################################################################################################################
# Author: Marco Altorfer
# Data Preparation
######################################################################################################################################################

# 1) General ----
rm(list = ls())
gc()
library(tidyverse)
library(here)

# Read Raw Data
# SHPP = read.spss("SHPLONG_P_USER.sav",to.data.frame=T, use.value.labels=F, use.missings=F)
# save.image("~/Library/CloudStorage/GoogleDrive-marco.altorfer@gmail.com/My Drive/UZH/Psychology/Ma Thesis/Data/Data/DataSHPLong.RData")

#2) Select and Rename Variables ----

load("Data/241025DataSHPLong.RData")

n_distinct(SHPP$IDPERS)

# Select and transform the data
SHPP <- SHPP %>%
  # Select relevant columns
  select(
    IDPERS, YEAR, CIVSTA, SEX, AGE, OWNKID,
    WSTAT, OCCUPA, PE03, PI01, PL01, PW228, PL06, PL11, PL16, PA05, PA06, PC02, PC44, PQL04,PW92,
  ) %>%
  # Rename columns
  rename(
    #Satisfaction_Finances = PI01,
    Work_Income_Satisfaction = PW92,
    Work_Satisfaction = PW228,
    Leisure_Satisfaction = PA05,
    #Satisfaction_Leisure_Activities = PA06,
    Health_Satisfaction = PC02,
    General_Life_Satisfaction = PC44,
    Personal_Relationships_Satisfaction = PQL04,
    Illness_or_Accident_of_Self = PL01,
    Illness_or_Accident_of_Close_Relationship = PL06,
    Death_of_Close_Relationship = PL11,
    Termination_of_Close_Relationship = PL16,
    Graduation = PE03,
    Childbirth = OWNKID
  )

save.image("Data/250416DataSHPLongSelect.RData")

# 3) Filter and Modify Variables ----
load("Data/250416DataSHPLongSelect.RData")

# Filter
SHPP <- SHPP %>% filter(AGE >= 18)

# Sort
SHPP <- SHPP %>%
  arrange(IDPERS, YEAR)

# Replace negative values with NA
SHPP[SHPP < 0] <- NA

# Replace no= 2 to 0
SHPP <- SHPP %>%
  mutate(across(c(Graduation, Illness_or_Accident_of_Self, Illness_or_Accident_of_Close_Relationship, 
                  Death_of_Close_Relationship, Termination_of_Close_Relationship), 
                ~ if_else(. == 2, 0, .)))

# Registered Partnership as Marriage und dissolved partnership als divorced
original_attributes <- attributes(SHPP$CIVSTA)
SHPP <- SHPP %>%
  mutate(CIVSTA = recode(CIVSTA, `6` = 2, `7` = 4))
attributes(SHPP$CIVSTA) <- original_attributes
rm(original_attributes)

# Count
#write.csv(data.frame(Variable = names(colSums(is.na(SHPP))), NA_Count = colSums(is.na(SHPP))), "count_original.csv", row.names = FALSE)
#write.csv(as.data.frame(t(sapply(lapply(SHPP, function(x) list(NA_count = sum(is.na(x)), Zero_count = sum(x == 0, na.rm = TRUE), One_count = sum(x == 1, na.rm = TRUE))), unlist))), "250416_count_original.csv")


## 3.1) Create Life Events

### 3.1.1 Marriage ----
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Marriage Event Detection: Ensures transition to CIVSTA == 2 only from a different status
    Marriage = if_else(!is.na(CIVSTA) & !is.na(lag(CIVSTA)) & CIVSTA == 2 & lag(CIVSTA) != 2, 1L, 0L),
    Marriage_Count = cumsum(replace_na(Marriage, 0L))
  ) %>%
  ungroup()

# Print Max Count
print(max(SHPP$Marriage_Count, na.rm = TRUE))

# Generate First, Second, and Third Marriage Events
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Marriage_First  = if_else(Marriage_Count == 1 & lag(Marriage_Count, default = 0L) == 0, 1L, 0L),
    Marriage_Second = if_else(Marriage_Count == 2 & lag(Marriage_Count, default = 0L) == 1, 1L, 0L),
    Marriage_Third  = if_else(Marriage_Count == 3 & lag(Marriage_Count, default = 0L) == 2, 1L, 0L)
  ) %>%
  ungroup()

# Restore Original NA Structure
SHPP <- SHPP %>%
  mutate(across(starts_with("Marriage_"), ~ if_else(is.na(CIVSTA), NA_integer_, .)))


### 3.1.2) Divorce ####
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Divorce Event Detection: Ensures transition to CIVSTA == 3 only from a different status
    Divorce = if_else(!is.na(CIVSTA) & !is.na(lag(CIVSTA)) & CIVSTA == 3 & lag(CIVSTA) != 3, 1L, 0L),
    Divorce_Count = cumsum(replace_na(Divorce, 0L))
  ) %>%
  ungroup()

print(max(SHPP$Divorce_Count, na.rm = TRUE))

SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Divorce_First  = if_else(Divorce_Count == 1 & lag(Divorce_Count, default = 0L) == 0, 1L, 0L),
    Divorce_Second = if_else(Divorce_Count == 2 & lag(Divorce_Count, default = 0L) == 1, 1L, 0L),
    Divorce_Third  = if_else(Divorce_Count == 3 & lag(Divorce_Count, default = 0L) == 2, 1L, 0L)
  ) %>%
  ungroup()

SHPP <- SHPP %>%
  mutate(across(starts_with("Divorce_"), ~ if_else(is.na(CIVSTA), NA_integer_, .)))

### 3.1.3) Widowhood ####
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Widowhood Event Detection: Ensures transition to CIVSTA == 4 only from a different status
    Widowhood = if_else(!is.na(CIVSTA) & !is.na(lag(CIVSTA)) & CIVSTA == 4 & lag(CIVSTA) != 4, 1L, 0L),
    Widowhood_Count = cumsum(replace_na(Widowhood, 0L))
  ) %>%
  ungroup()

print(max(SHPP$Widowhood_Count, na.rm = TRUE))

SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Widowhood_First  = if_else(Widowhood_Count == 1 & lag(Widowhood_Count, default = 0L) == 0, 1L, 0L),
    Widowhood_Second = if_else(Widowhood_Count == 2 & lag(Widowhood_Count, default = 0L) == 1, 1L, 0L)
  ) %>%
  ungroup()

SHPP <- SHPP %>%
  mutate(across(starts_with("Widowhood_"), ~ if_else(is.na(CIVSTA), NA_integer_, .)))


### 3.1.4) Job Loss ####

# Define working status based on OCCUPA categories
SHPP <- SHPP %>%
  mutate(working = if_else(OCCUPA %in% c(1, 2, 3, 5, 6), 1L, 0L))

SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Transitions from working to unemployed
    Unemployment = if_else(!is.na(OCCUPA) & !is.na(lag(working)) & OCCUPA == 10 & lag(working) == 1, 1L, 0L),
    Unemployment_Count = cumsum(replace_na(Unemployment, 0L))
  ) %>%
  ungroup()

print(max(SHPP$Unemployment_Count, na.rm = TRUE))

SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Unemployment_First  = if_else(Unemployment_Count == 1 & lag(Unemployment_Count, default = 0L) == 0, 1L, 0L),
    Unemployment_Second = if_else(Unemployment_Count == 2 & lag(Unemployment_Count, default = 0L) == 1, 1L, 0L),
    Unemployment_Third  = if_else(Unemployment_Count == 3 & lag(Unemployment_Count, default = 0L) == 2, 1L, 0L),
    Unemployment_Fourth = if_else(Unemployment_Count == 4 & lag(Unemployment_Count, default = 0L) == 3, 1L, 0L)
  ) %>%
  ungroup()

SHPP <- SHPP %>%
  mutate(across(starts_with("Unemployment_"), ~ if_else(is.na(OCCUPA), NA_integer_, .)))


### 3.1.5) Employment ####
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    #transition unemployed to employed
    Employment = if_else(!is.na(OCCUPA) & !is.na(lag(working)) & lag(OCCUPA) == 10 & working == 1, 1L, 0L),
    Employment_Count = cumsum(replace_na(Employment, 0L))
  ) %>%
  ungroup()

print(max(SHPP$Employment_Count, na.rm = TRUE))

SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Employment_First  = if_else(Employment_Count == 1 & lag(Employment_Count, default = 0L) == 0, 1L, 0L),
    Employment_Second = if_else(Employment_Count == 2 & lag(Employment_Count, default = 0L) == 1, 1L, 0L),
    Employment_Third  = if_else(Employment_Count == 3 & lag(Employment_Count, default = 0L) == 2, 1L, 0L),
    Employment_Fourth = if_else(Employment_Count == 4 & lag(Employment_Count, default = 0L) == 3, 1L, 0L)
  ) %>%
  ungroup()

SHPP <- SHPP %>%
  mutate(across(starts_with("Employment_"), ~ if_else(is.na(OCCUPA), NA_integer_, .)))


### 3.1.6) Retirement ####
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Retirement = if_else(!is.na(OCCUPA) & !is.na(lag(working)) & OCCUPA == 8 & lag(working) == 1, 1L, 0L),
    Retirement_Count = cumsum(replace_na(Retirement, 0L))
  ) %>%
  ungroup()

print(max(SHPP$Retirement_Count, na.rm = TRUE))

SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Retirement_First  = if_else(Retirement_Count == 1 & lag(Retirement_Count, default = 0L) == 0, 1L, 0L),
    Retirement_Second = if_else(Retirement_Count == 2 & lag(Retirement_Count, default = 0L) == 1, 1L, 0L),
    Retirement_Third  = if_else(Retirement_Count == 3 & lag(Retirement_Count, default = 0L) == 2, 1L, 0L)
  ) %>%
  ungroup()

SHPP <- SHPP %>%
  mutate(across(starts_with("Retirement_"), ~ if_else(is.na(OCCUPA), NA_integer_, .)))

### 3.1.7) Childbirth ####

# Step 1: Create an imputed variable (preserving NA)
SHPP <- SHPP %>%
  mutate(Childbirth_imputed = if_else(is.na(Childbirth), NA_integer_, Childbirth))

# Step 2: Compute an event indicator: if the current value is greater than the previous, mark as a new event.
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Childbirth_Event = if_else(row_number() == 1, NA_integer_,
                              if_else(is.na(Childbirth_imputed) | is.na(lag(Childbirth_imputed)), NA_integer_,
                                      if_else(Childbirth_imputed > lag(Childbirth_imputed), 1L, 0L))),
    Childbirth_Count = if_else(is.na(Childbirth), NA_integer_, cumsum(replace_na(Childbirth_Event, 0L)))
  ) %>%
  ungroup()

# Step 3: Print the maximum count over all observations
print(max(SHPP$Childbirth_Count, na.rm = TRUE))

# Step 4: Create indicator variables for each event up to maximum
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    Childbirth_First  = if_else(is.na(Childbirth_Count), NA_integer_, if_else(Childbirth_Count == 1 & lag(Childbirth_Count, default = NA_integer_) == 0, 1L, 0L)),
    Childbirth_Second = if_else(is.na(Childbirth_Count), NA_integer_, if_else(Childbirth_Count == 2 & lag(Childbirth_Count, default = NA_integer_) == 1, 1L, 0L)),
    Childbirth_Third  = if_else(is.na(Childbirth_Count), NA_integer_, if_else(Childbirth_Count == 3 & lag(Childbirth_Count, default = NA_integer_) == 2, 1L, 0L)),
    Childbirth_Fourth = if_else(is.na(Childbirth_Count), NA_integer_, if_else(Childbirth_Count == 4 & lag(Childbirth_Count, default = NA_integer_) == 3, 1L, 0L)),
    Childbirth_Fifth  = if_else(is.na(Childbirth_Count), NA_integer_, if_else(Childbirth_Count == 5 & lag(Childbirth_Count, default = NA_integer_) == 4, 1L, 0L))
  ) %>%
  ungroup()

# Step 5: Restore original missingness in all derived variables
SHPP <- SHPP %>%
  mutate(across(starts_with("Childbirth_"), ~ if_else(is.na(Childbirth), NA_integer_, .)))

### 3.1.8) Death Close Relationship ----
# Step 1: Calculate Death_of_Close_Relationship_Count by counting every occurrence of 1
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Flag every row where Death_of_Close_Relationship equals 1
    Death_of_Close_Relationship_Event = if_else(Death_of_Close_Relationship == 1, 1L, 0L),
    Death_of_Close_Relationship_Count = cumsum(replace_na(Death_of_Close_Relationship_Event, 0L))
  ) %>%
  ungroup()

print(max(SHPP$Death_of_Close_Relationship_Count, na.rm = TRUE))

# Step 2: Create individual event indicators, marking only the row corresponding to each event order
SHPP <- SHPP %>%
  mutate(
    Death_of_Close_Relationship_First     = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 1, 1L, 0L),
    Death_of_Close_Relationship_Second    = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 2, 1L, 0L),
    Death_of_Close_Relationship_Third     = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 3, 1L, 0L),
    Death_of_Close_Relationship_Fourth    = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 4, 1L, 0L),
    Death_of_Close_Relationship_Fifth     = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 5, 1L, 0L),
    Death_of_Close_Relationship_Sixth     = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 6, 1L, 0L),
    Death_of_Close_Relationship_Seventh   = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 7, 1L, 0L),
    Death_of_Close_Relationship_Eighth    = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 8, 1L, 0L),
    Death_of_Close_Relationship_Ninth     = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 9, 1L, 0L),
    Death_of_Close_Relationship_Tenth     = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 10, 1L, 0L),
    Death_of_Close_Relationship_Eleventh  = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 11, 1L, 0L),
    Death_of_Close_Relationship_Twelfth   = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 12, 1L, 0L),
    Death_of_Close_Relationship_Thirteenth= if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 13, 1L, 0L),
    Death_of_Close_Relationship_Fourteenth= if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 14, 1L, 0L),
    Death_of_Close_Relationship_Fifteenth = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 15, 1L, 0L),
    Death_of_Close_Relationship_Sixteenth = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 16, 1L, 0L),
    Death_of_Close_Relationship_Seventeenth = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 17, 1L, 0L),
    Death_of_Close_Relationship_Eighteenth = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 18, 1L, 0L),
    Death_of_Close_Relationship_Nineteenth = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 19, 1L, 0L),
    Death_of_Close_Relationship_Twentieth  = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 20, 1L, 0L),
    Death_of_Close_Relationship_Twentyfirst = if_else(Death_of_Close_Relationship_Event == 1 & Death_of_Close_Relationship_Count == 21, 1L, 0L)
  )


# Step 3: Restore original NA structure for individual event indicators
SHPP <- SHPP %>%
  mutate(across(starts_with("Death_of_Close_Relationship_"), ~ if_else(is.na(Death_of_Close_Relationship), NA_integer_, .)))



### 3.1.9) Graduation ----

### 9 Graduation
# Step 1: Calculate Graduation_Count by counting every occurrence of 1
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Flag every row where Graduation equals 1
    Graduation_Event = if_else(Graduation == 1, 1L, 0L),
    Graduation_Count = cumsum(replace_na(Graduation_Event, 0L))
  ) %>%
  ungroup()

# Print Max Count
print(max(SHPP$Graduation_Count, na.rm = TRUE))

# Step 2: Create individual event indicators, marking only the row corresponding to each event order
SHPP <- SHPP %>%
  mutate(
    Graduation_First   = if_else(Graduation_Event == 1 & Graduation_Count == 1, 1L, 0L),
    Graduation_Second  = if_else(Graduation_Event == 1 & Graduation_Count == 2, 1L, 0L),
    Graduation_Third   = if_else(Graduation_Event == 1 & Graduation_Count == 3, 1L, 0L),
    Graduation_Fourth  = if_else(Graduation_Event == 1 & Graduation_Count == 4, 1L, 0L),
    Graduation_Fifth   = if_else(Graduation_Event == 1 & Graduation_Count == 5, 1L, 0L),
    Graduation_Sixth   = if_else(Graduation_Event == 1 & Graduation_Count == 6, 1L, 0L),
    Graduation_Seventh = if_else(Graduation_Event == 1 & Graduation_Count == 7, 1L, 0L),
    Graduation_Eighth  = if_else(Graduation_Event == 1 & Graduation_Count == 8, 1L, 0L),
    Graduation_Ninth   = if_else(Graduation_Event == 1 & Graduation_Count == 9, 1L, 0L),
    Graduation_Tenth   = if_else(Graduation_Event == 1 & Graduation_Count == 10, 1L, 0L)
  )

# Step 3: Restore original NA structure for individual event indicators
SHPP <- SHPP %>%
  mutate(across(starts_with("Graduation_"), ~ if_else(is.na(Graduation), NA_integer_, .)))


### 3.1.10) Illness, Accident Self ----
# Step 1: Calculate Illness_or_Accident_of_Self_Count by counting every occurrence of 1
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Flag every row where Illness_or_Accident_of_Self equals 1
    Illness_or_Accident_of_Self_Event = if_else(Illness_or_Accident_of_Self == 1, 1L, 0L),
    Illness_or_Accident_of_Self_Count = cumsum(replace_na(Illness_or_Accident_of_Self_Event, 0L))
  ) %>%
  ungroup()

# Print Max Count
print(max(SHPP$Illness_or_Accident_of_Self_Count, na.rm = TRUE))

# Step 2: Create individual event indicators, marking only the row corresponding to each event order
SHPP <- SHPP %>%
  mutate(
    Illness_or_Accident_of_Self_First      = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 1, 1L, 0L),
    Illness_or_Accident_of_Self_Second     = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 2, 1L, 0L),
    Illness_or_Accident_of_Self_Third      = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 3, 1L, 0L),
    Illness_or_Accident_of_Self_Fourth     = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 4, 1L, 0L),
    Illness_or_Accident_of_Self_Fifth      = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 5, 1L, 0L),
    Illness_or_Accident_of_Self_Sixth      = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 6, 1L, 0L),
    Illness_or_Accident_of_Self_Seventh    = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 7, 1L, 0L),
    Illness_or_Accident_of_Self_Eighth     = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 8, 1L, 0L),
    Illness_or_Accident_of_Self_Ninth      = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 9, 1L, 0L),
    Illness_or_Accident_of_Self_Tenth      = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 10, 1L, 0L),
    Illness_or_Accident_of_Self_Eleventh   = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 11, 1L, 0L),
    Illness_or_Accident_of_Self_Twelfth    = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 12, 1L, 0L),
    Illness_or_Accident_of_Self_Thirteenth = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 13, 1L, 0L),
    Illness_or_Accident_of_Self_Fourteenth = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 14, 1L, 0L),
    Illness_or_Accident_of_Self_Fifteenth  = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 15, 1L, 0L),
    Illness_or_Accident_of_Self_Sixteenth  = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 16, 1L, 0L),
    Illness_or_Accident_of_Self_Seventeenth= if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 17, 1L, 0L),
    Illness_or_Accident_of_Self_Eighteenth = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 18, 1L, 0L),
    Illness_or_Accident_of_Self_Nineteenth = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 19, 1L, 0L),
    Illness_or_Accident_of_Self_Twentieth  = if_else(Illness_or_Accident_of_Self_Event == 1 & Illness_or_Accident_of_Self_Count == 20, 1L, 0L)
  )


# Step 3: Restore original NA structure for individual event indicators
SHPP <- SHPP %>%
  mutate(across(starts_with("Illness_or_Accident_of_Self_"), ~ if_else(is.na(Illness_or_Accident_of_Self), NA_integer_, .)))

### 3.1.11) Illness, Accident Close Relationship ----
# Step 1: Calculate Illness_or_Accident_of_Close_Relationship_Count by counting every occurrence of 1
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Flag every row where Illness_or_Accident_of_Close_Relationship equals 1
    Illness_or_Accident_of_Close_Relationship_Event = if_else(Illness_or_Accident_of_Close_Relationship == 1, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Count = cumsum(replace_na(Illness_or_Accident_of_Close_Relationship_Event, 0L))
  ) %>%
  ungroup()

# Print Max Count
print(max(SHPP$Illness_or_Accident_of_Close_Relationship_Count, na.rm = TRUE))

# Step 2: Create individual event indicators, marking only the row corresponding to each event order
SHPP <- SHPP %>%
  mutate(
    Illness_or_Accident_of_Close_Relationship_First      = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 1, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Second     = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 2, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Third      = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 3, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Fourth     = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 4, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Fifth      = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 5, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Sixth      = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 6, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Seventh    = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 7, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Eighth     = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 8, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Ninth      = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 9, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Tenth      = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 10, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Eleventh   = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 11, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Twelfth    = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 12, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Thirteenth = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 13, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Fourteenth = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 14, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Fifteenth  = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 15, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Sixteenth  = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 16, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Seventeenth= if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 17, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Eighteenth = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 18, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Nineteenth = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 19, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Twentieth  = if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 20, 1L, 0L),
    Illness_or_Accident_of_Close_Relationship_Twentyfirst= if_else(Illness_or_Accident_of_Close_Relationship_Event == 1 & Illness_or_Accident_of_Close_Relationship_Count == 21, 1L, 0L)
  )


# Step 3: Restore original NA structure for individual event indicators
SHPP <- SHPP %>%
  mutate(across(starts_with("Illness_or_Accident_of_Close_Relationship_"), ~ if_else(is.na(Illness_or_Accident_of_Close_Relationship), NA_integer_, .)))

### 3.1.12) Termination Close relationship ----
# Step 1: Calculate Termination_of_Close_Relationship_Count by counting every occurrence of 1
SHPP <- SHPP %>%
  group_by(IDPERS) %>%
  arrange(YEAR, .by_group = TRUE) %>%
  mutate(
    # Flag every row where Termination_of_Close_Relationship equals 1
    Termination_of_Close_Relationship_Event = if_else(Termination_of_Close_Relationship == 1, 1L, 0L),
    Termination_of_Close_Relationship_Count = cumsum(replace_na(Termination_of_Close_Relationship_Event, 0L))
  ) %>%
  ungroup()

# Print Max Count
print(max(SHPP$Termination_of_Close_Relationship_Count, na.rm = TRUE))

# Step 2: Create individual event indicators, marking only the row corresponding to each event order
SHPP <- SHPP %>%
  mutate(
    Termination_of_Close_Relationship_First      = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 1, 1L, 0L),
    Termination_of_Close_Relationship_Second     = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 2, 1L, 0L),
    Termination_of_Close_Relationship_Third      = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 3, 1L, 0L),
    Termination_of_Close_Relationship_Fourth     = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 4, 1L, 0L),
    Termination_of_Close_Relationship_Fifth      = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 5, 1L, 0L),
    Termination_of_Close_Relationship_Sixth      = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 6, 1L, 0L),
    Termination_of_Close_Relationship_Seventh    = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 7, 1L, 0L),
    Termination_of_Close_Relationship_Eighth     = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 8, 1L, 0L),
    Termination_of_Close_Relationship_Ninth      = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 9, 1L, 0L),
    Termination_of_Close_Relationship_Tenth      = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 10, 1L, 0L),
    Termination_of_Close_Relationship_Eleventh   = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 11, 1L, 0L),
    Termination_of_Close_Relationship_Twelfth    = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 12, 1L, 0L),
    Termination_of_Close_Relationship_Thirteenth = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 13, 1L, 0L),
    Termination_of_Close_Relationship_Fourteenth = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 14, 1L, 0L),
    Termination_of_Close_Relationship_Fifteenth  = if_else(Termination_of_Close_Relationship_Event == 1 & Termination_of_Close_Relationship_Count == 15, 1L, 0L)
  )


# Step 3: Restore original NA structure for individual event indicators
SHPP <- SHPP %>%
  mutate(across(starts_with("Termination_of_Close_Relationship_"), ~ if_else(is.na(Termination_of_Close_Relationship), NA_integer_, .)))

#check
SHPP %>%
  group_by(IDPERS) %>%
  summarize(Count_First = sum(Death_of_Close_Relationship_First, na.rm = TRUE)) %>%
  filter(Count_First > 1)

save.image("Data/250416DataSHPLongLifeEvents.RData")
#write.csv(SHPP, "250416DataSHPLongLifeEvents.csv")

## 3.2) include only events with more than 500 counts (except for Childbirth_First)
#count table
matching_variables <- grep("_First$|_Second$|_Third$|_Fourth$|_Fifth$|_Sixth$|_Seventh$|_Eighth$|_Ninth$|_Tenth$|_Eleventh$|_Twelfth$|_Thirteenth$|_Fourteenth$|_Fifteenth$|_Sixteenth$|_Seventeenth$|_Eighteenth$|_Nineteenth$", names(SHPP), value = TRUE)
count_table <- sapply(SHPP[, matching_variables], function(x) table(factor(x, levels = c(0, 1, NA), exclude = NULL)))
transposed_count_table <- t(count_table)
t(count_table)

SHPP <- SHPP %>%
  # Select relevant columns
  select(
    IDPERS, YEAR, CIVSTA, SEX, AGE, Childbirth, OCCUPA, working,
    #Satisfaction_Finances,
    Work_Income_Satisfaction,
    Work_Satisfaction,
    Leisure_Satisfaction,
    #Satisfaction_Leisure_Activities = PA06,
    Health_Satisfaction,
    General_Life_Satisfaction,
    Personal_Relationships_Satisfaction,
    Marriage_First,
    Divorce_First,
    Widowhood_First,
    Unemployment_First,
    Employment_First,
    Retirement_First,
    Childbirth_First, Childbirth_Second,
    Graduation_First, Graduation_Second, Graduation_Third,
    Illness_or_Accident_of_Self_First, Illness_or_Accident_of_Self_Second, Illness_or_Accident_of_Self_Third, Illness_or_Accident_of_Self_Fourth, Illness_or_Accident_of_Self_Fifth,  Illness_or_Accident_of_Self_Sixth,  Illness_or_Accident_of_Self_Seventh,  Illness_or_Accident_of_Self_Eighth,
    Death_of_Close_Relationship_First, Death_of_Close_Relationship_Second, Death_of_Close_Relationship_Third, Death_of_Close_Relationship_Fourth, Death_of_Close_Relationship_Fifth, Death_of_Close_Relationship_Sixth, Death_of_Close_Relationship_Seventh, Death_of_Close_Relationship_Eighth,
    Termination_of_Close_Relationship_First, Termination_of_Close_Relationship_Second, Termination_of_Close_Relationship_Third, Termination_of_Close_Relationship_Fourth,
    Illness_or_Accident_of_Close_Relationship_First, Illness_or_Accident_of_Close_Relationship_Second, Illness_or_Accident_of_Close_Relationship_Third, Illness_or_Accident_of_Close_Relationship_Fourth, Illness_or_Accident_of_Close_Relationship_Fifth, 
    Illness_or_Accident_of_Close_Relationship_Sixth, Illness_or_Accident_of_Close_Relationship_Seventh, Illness_or_Accident_of_Close_Relationship_Eighth, Illness_or_Accident_of_Close_Relationship_Ninth,
    Illness_or_Accident_of_Close_Relationship_Tenth, Illness_or_Accident_of_Close_Relationship_Eleventh
  )


save.image("Data/250416DataSHPLongLifeEventsSelect.RData")
#write.csv(SHPP, "Data/250416DataSHPLongLifeEventsSELECT.csv")


# 4) Create variables for regression #####

load("Data/250416DataSHPLongLifeEventsSelect.RData")

# Age squared
SHPP <- SHPP %>%
  mutate(AGE_squared = AGE^2)

# elevation bias
SHPP <- SHPP %>%
  group_by(IDPERS) %>%                     # Group by IDPERS
  arrange(YEAR, .by_group = TRUE) %>%      # Arrange data by YEAR within each IDPERS
  mutate(elevation = if_else(row_number() <= 3, 1, 0)) %>% # Set 1 for first 3 rows, 0 otherwise
  ungroup()  

### lag/lead event variables

# List of variables to apply the operation
variables_to_lag <- c(
  "Marriage_First", "Divorce_First", "Widowhood_First", "Unemployment_First", "Employment_First", "Retirement_First",
  "Childbirth_First", "Childbirth_Second",
  "Graduation_First", "Graduation_Second", "Graduation_Third",
  "Illness_or_Accident_of_Self_First", "Illness_or_Accident_of_Self_Second", "Illness_or_Accident_of_Self_Third", 
  "Illness_or_Accident_of_Self_Fourth", "Illness_or_Accident_of_Self_Fifth", "Illness_or_Accident_of_Self_Sixth", 
  "Illness_or_Accident_of_Self_Seventh", "Illness_or_Accident_of_Self_Eighth",
  "Death_of_Close_Relationship_First", "Death_of_Close_Relationship_Second", "Death_of_Close_Relationship_Third", 
  "Death_of_Close_Relationship_Fourth", "Death_of_Close_Relationship_Fifth", "Death_of_Close_Relationship_Sixth", 
  "Death_of_Close_Relationship_Seventh", "Death_of_Close_Relationship_Eighth",
  "Termination_of_Close_Relationship_First", "Termination_of_Close_Relationship_Second", 
  "Termination_of_Close_Relationship_Third", "Termination_of_Close_Relationship_Fourth",
  "Illness_or_Accident_of_Close_Relationship_First", "Illness_or_Accident_of_Close_Relationship_Second", 
  "Illness_or_Accident_of_Close_Relationship_Third", "Illness_or_Accident_of_Close_Relationship_Fourth", 
  "Illness_or_Accident_of_Close_Relationship_Fifth", "Illness_or_Accident_of_Close_Relationship_Sixth", 
  "Illness_or_Accident_of_Close_Relationship_Seventh", "Illness_or_Accident_of_Close_Relationship_Eighth", 
  "Illness_or_Accident_of_Close_Relationship_Ninth", "Illness_or_Accident_of_Close_Relationship_Tenth", 
  "Illness_or_Accident_of_Close_Relationship_Eleventh"
)



# lag 1 (lag 1 is the same as the year since events created as lag 1)
SHPP <- SHPP %>%
  mutate(across(all_of(variables_to_lag), 
                ~ .,  # Keep the same values
                .names = "{.col}_lag1"))


# lag2 (replace NAs with 0)
SHPP <- SHPP %>%
  arrange(IDPERS, YEAR) %>%
  group_by(IDPERS) %>%
  mutate(across(all_of(variables_to_lag), 
                ~ replace_na(lag(.x, 1), 0),  # Replace NA with 0
                .names = "{.col}_lag2")) %>%
  ungroup()

#lag3more
SHPP <- SHPP %>%
  arrange(IDPERS, YEAR) %>%
  group_by(IDPERS) %>%
  mutate(across(all_of(variables_to_lag), 
                ~ if_else(lag(.x, 2, default = 0) == 1, 1, 0),  
                .names = "{.col}_lag3more")) %>%
  mutate(across(ends_with("_lag3more"),
                ~ if_else(cumsum(replace_na(.x, 0)) > 0, 1, replace_na(.x, 0)),  
                .names = "{.col}")) %>%
  ungroup()

# lead1 (replace NAs with 0)
SHPP <- SHPP %>%
  arrange(IDPERS, YEAR) %>%
  group_by(IDPERS) %>%
  mutate(across(all_of(variables_to_lag), 
                ~ replace_na(lead(.x, 1), 0),  # Replace NA with 0
                .names = "{.col}_lead1")) %>%
  ungroup()

# lead2 (replace NAs with 0)
SHPP <- SHPP %>%
  arrange(IDPERS, YEAR) %>%
  group_by(IDPERS) %>%
  mutate(across(all_of(variables_to_lag), 
                ~ replace_na(lead(.x, 2), 0),  # Replace NA with 0
                .names = "{.col}_lead2")) %>%
  ungroup()


# #count table
# matching_variables2 <- grep("_lead1$|_lead2$|lag1$|lag2$|lag3more$", names(SHPP), value = TRUE)
# count_table2 <- sapply(SHPP[, matching_variables2], function(x) table(factor(x, levels = c(0, 1, NA), exclude = NULL)))
# transposed_count_table2 <- t(count_table2)
# write.csv(transposed_count_table2, "Tables/250416count_table_lag.csv")
# rm(matching_variables2, count_table2, transposed_count_table2, variables_to_lag)

#check
debug <- SHPP  %>%
  arrange(IDPERS, YEAR) %>%
  group_by(IDPERS) %>%
  select(
    IDPERS, YEAR,
    Marriage_First_lead2, Marriage_First_lead1, Marriage_First_lag1, Marriage_First_lag2, Marriage_First_lag3more,
    Illness_or_Accident_of_Self_First_lead2, Illness_or_Accident_of_Self_First_lead1, Illness_or_Accident_of_Self_First_lag1, Illness_or_Accident_of_Self_First_lag2, Illness_or_Accident_of_Self_First_lag3more
  )


#write.csv(debug, "250416_debug_lag.csv")
rm(debug)

save.image("Data/250416DataSHPLongCleaned.RData")
# write.csv(SHPP, "Tables/250416DataSHPLongCleaned.csv")
