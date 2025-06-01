#####################################################################################################################################################
# Author: Marco Altorfer
# Descriptives
######################################################################################################################################################

#1 General -----

rm(list = ls())
gc()

library(foreign)
library(tidyverse)
library(ggplot2)
library(here)
library(purrr)
library(tibble)
library(flextable)
library(officer)
library(tidyr)
library(ggcorrplot)

#2 Descriptive Statistics of Sample -----
load(here("Data/250416DataSHPLongSelect.RData"))

# Sort
SHPP <- SHPP %>%
  arrange(IDPERS, YEAR)

# Replace negative values with NA
SHPP[SHPP < 0] <- NA

# Number of observations whole data set
nrow(SHPP)

#unique persons whole data set
SHPP %>%
  summarise(unique_IDPERS = n_distinct(IDPERS))

## Inclusion Criterium
SHPP <- SHPP %>% filter(AGE >= 18)

#unique persons
SHPP %>%
  summarise(unique_IDPERS = n_distinct(IDPERS))

# Number of observations
nrow(SHPP)

#Demographics

round(mean(SHPP$AGE, , na.rm = TRUE), 4)
round(sd(SHPP$AGE, na.rm = TRUE), 4)

#Men=1, Women=0
SHPP$SEX <- ifelse(SHPP$SEX==2, 0, 1)
round(mean(SHPP$SEX, , na.rm = TRUE), 4)
round(sd(SHPP$SEX, na.rm = TRUE), 4)

#3 Descriptive Statistics of Variables ------
#load data
load(here("Data/250416DataSHPLongLifeEvents.RData"))

## 3.1 count table of repeated event ---- 
matching_variables <- grep("_First$|_Second$|_Third$|_Fourth$|_Fifth$|_Sixth$|_Seventh$|_Eighth$|_Ninth$|_Tenth$|_Eleventh$|_Twelfth$|_Thirteenth$|_Fourteenth$|_Fifteenth$|_Sixteenth$|_Seventeenth$|_Eighteenth$|_Nineteenth$", names(SHPP), value = TRUE)
count_table <- sapply(SHPP[, matching_variables], function(x) table(factor(x, levels = c(0, 1, NA), exclude = NULL)))
transposed_count_table <- t(count_table)
write.csv(transposed_count_table, "Tables/250409_count_table_life_events.csv")

## 3.2 Event Type Counts -----

# Predefined list of event types
event_types <- c(
  "Marriage",
  "Childbirth",
  "Divorce",
  "Widowhood",
  "Death_of_Close_Relationship",
  "Termination_of_Close_Relationship",
  "Illness_or_Accident_of_Self",
  "Illness_or_Accident_of_Close_Relationship",
  "Employment",
  "Graduation",
  "Retirement",
  "Unemployment"
)

# Initialize named vector for event counts
event_type_counts <- sapply(event_types, function(event) {
  # Select columns for this event (e.g., Marriage_First, Marriage_Second, ...)
  matching_vars <- grep(paste0("^", event, "_(First|Second|Third|Fourth|Fifth|Sixth|Seventh|Eighth|Ninth|Tenth|Eleventh|Twelfth|Thirteenth|Fourteenth|Fifteenth|Sixteenth|Seventeenth|Eighteenth|Nineteenth)$"),
                        names(SHPP), value = TRUE)
  
  # Count number of events (1s), summing across individuals and event instances
  sum(rowSums(SHPP[, matching_vars, drop = FALSE] == 1, na.rm = TRUE))
})

# Print result
print(event_type_counts)

# Total number of events
sum(event_type_counts)

## 3.3 Descriptive Statistics of Life Satisfaction -----

t(summary(SHPP[, c( 
  "General_Life_Satisfaction",
  "Health_Satisfaction",
  "Personal_Relationships_Satisfaction",
  "Leisure_Satisfaction",
  "Work_Satisfaction",
  "Work_Income_Satisfaction")]))

###3.3.1 Descriptive Statistics Table -----

vars <- c( 
  "General_Life_Satisfaction",
  "Health_Satisfaction",
  "Personal_Relationships_Satisfaction",
  "Leisure_Satisfaction",
  "Work_Satisfaction",
  "Work_Income_Satisfaction"
)

# Clean variable names
clean_variable_names <- function(x) {
  x %>%
    gsub("General_", "", .) %>%
    gsub("_Satisfaction", " Satisfaction", .) %>%
    gsub("_", " ", .)
}

# Compute descriptive statistics
result_df <- map_dfr(vars, function(var) {
  x <- SHPP[[var]]
  tibble(
    `Life Satisfaction Variable` = clean_variable_names(var),
    Mean     = round(mean(x, na.rm = TRUE), 2),
    Median   = round(median(x, na.rm = TRUE), 2),
    SD       = round(sd(x, na.rm = TRUE), 2),
    Q1       = round(quantile(x, 0.25, na.rm = TRUE), 2),
    Q3       = round(quantile(x, 0.75, na.rm = TRUE), 2),
    NAs      = format(sum(is.na(x)), big.mark = ",")
  )
})

# Build flextable
ft <- flextable(result_df) %>%
  theme_booktabs() %>%
  set_table_properties(layout = "autofit") %>%
  align(j = 2:ncol(result_df), align = "center", part = "all") %>%
  align(j = 1, align = "left", part = "all") %>%
  autofit()

# Title, subtitle, note
title_fpar <- fpar(ftext("Table 1", fp_text(bold = TRUE, font.size = 10)))
subtitle_fpar <- fpar(ftext("Descriptive Statistics for Life Satisfaction Variables", fp_text(italic = TRUE, font.size = 10)))
note_fpar <- fpar(
  ftext("Note. ", fp_text(italic = TRUE, font.size = 10)),
  ftext("SD = standard deviation; Q1 = 25th percentile; Q3 = 75th percentile; NAs = number of missing values.", fp_text(font.size = 10))
)

# Add to Word document
doc <- read_docx() %>%
  body_add_fpar(title_fpar) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(subtitle_fpar) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft) %>%
  body_add_par("", style = "Normal") %>%
  body_add_fpar(note_fpar) %>%
  body_add_par("", style = "Normal") %>%
  body_add_break()

print(doc, target = here("Tables", "250423_descriptives_custom.docx"))


### 3.3.2 Figure mean life satisfaction over time -----

# Clean variable names function
clean_names <- function(x) {
  x %>%
    gsub("General_", "", .) %>%
    gsub("_Satisfaction", " Satisfaction", .) %>%
    gsub("_", " ", .)
}

# Summarize mean satisfaction per year
means_by_year <- SHPP %>%
  group_by(YEAR) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(-YEAR, names_to = "Satisfaction_Variable", values_to = "Mean") %>%
  mutate(Satisfaction_Variable = clean_names(Satisfaction_Variable))

# Plot
satisfaction_plot <- ggplot(means_by_year, aes(x = YEAR, y = Mean, color = Satisfaction_Variable)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Year",
    y = "Mean Satisfaction",
    color = "Domain"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 13),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
satisfaction_plot

# Save
ggsave(
  filename = here("Figures", "250423mean_satisfaction_over_time.png"),
  plot = satisfaction_plot,
  width = 10,
  height = 6,
  dpi = 300
)

## 3.4 Correlation Matrix -----

# Define variables
event_vars <- c(
  "Marriage", "Childbirth", "Divorce", "Widowhood",
  "Death_of_Close_Relationship", "Termination_of_Close_Relationship",
  "Illness_or_Accident_of_Self", "Illness_or_Accident_of_Close_Relationship",
  "Employment", "Graduation", "Retirement", "Unemployment"
)

ls_vars <- c(
  "Health_Satisfaction",
  "Personal_Relationships_Satisfaction",
  "Leisure_Satisfaction",
  "Work_Satisfaction",
  "Work_Income_Satisfaction",
  "General_Life_Satisfaction"
)


# Subset and combine
cor_data <- SHPP %>% select(all_of(c(event_vars, ls_vars)))

# Compute correlation matrix
cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

#prettier labels
pretty_names <- setNames(gsub("_", " ", colnames(cor_matrix)), colnames(cor_matrix))

# Plot
corr_plot <- ggcorrplot(cor_matrix,
           type = "lower",
           lab = TRUE,
           lab_size = 2.8,
           colors = c("blue", "white", "red"),
           ggtheme = theme_minimal(base_size = 12)) +
  scale_x_discrete(labels = pretty_names) +
  scale_y_discrete(labels = pretty_names)

# Save
ggsave(
  filename = here::here("Figures", "250425_CorrelationMatrix.png"),
  plot = corr_plot,
  width = 10,
  height = 10,
  dpi = 300
)




