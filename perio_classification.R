# Load required libraries
library(dplyr)
library(data.table)

# Define adjacency mapping (FDI tooth notation)
adjacency_map <- list(
  "11" = c("12","21"), "12" = c("11", "13"), "13" = c("12", "14"), "14" = c("13", "15"), 
  "15" = c("14", "16"), "16" = c("15", "17"), "17" = c("16"), "21" = c("22","11"), 
  "22" = c("21", "23"), "23" = c("22", "24"), "24" = c("23", "25"), "25" = c("24", "26"), 
  "26" = c("25", "27"), "27" = c("26"), "31" = c("32","41"), "32" = c("31", "33"), 
  "33" = c("32", "34"), "34" = c("33", "35"), "35" = c("34", "36"), "36" = c("35", "37"), 
  "37" = c("36"), "41" = c("42","31"), "42" = c("41", "43"), "43" = c("42", "44"), 
  "44" = c("43", "45"), "45" = c("44", "46"), "46" = c("45", "47"), "47" = c("46")
)

# Function to count non-adjacent pairs of teeth
pair <- function(x) {
  sum(sapply(x, function(p) !(p[2] %in% adjacency_map[[as.character(p[1])]])))
}

# Extract CAL and PPD columns
cal_cols <- grep("_CAL$", colnames(selected_data), value = TRUE)
ppd_cols <- grep("_PD$", colnames(selected_data), value = TRUE)

# Remove second molars from PPD analysis
non_distal_pd_cols <- setdiff(ppd_cols, c("17DF_PD", "17DL_PD", "27DF_PD", "27DL_PD", 
                                          "37DF_PD", "37DL_PD", "47DF_PD", "47DL_PD"))

# Reshape CAL and PPD data to long format
cal_data <- selected_data %>% select(id, all_of(cal_cols))
ppd_data <- selected_data %>% select(id, all_of(non_distal_pd_cols))

setDT(cal_data)
setDT(ppd_data)

cal_data_long <- melt(cal_data, measure = patterns("^.+CAL"), variable.name = "site", value.name = "CAL") %>%
  mutate(Tooth = as.numeric(substr(site, 1, 2)), Site = substr(site, 3, (nchar(site) - 4)))

ppd_data_long <- melt(ppd_data, measure = patterns("^.+PD"), variable.name = "site", value.name = "PPD") %>%
  mutate(Tooth = as.numeric(substr(site, 1, 2)), Site = substr(site, 3, (nchar(site) - 3)))

# Identify maximum CAL per patient
max_cal_per_patient <- cal_data_long %>%
  filter(!is.na(CAL)) %>%
  group_by(id) %>%
  summarize(max_cal = max(CAL, na.rm = TRUE), .groups = 'drop')

# Identify patients with PD ≥6mm at ≥2 non-adjacent teeth (excluding second molars)
pd_6mm_non_adjacent <- ppd_data_long %>%
  filter(PD >= 6) %>%
  group_by(id, Tooth) %>%
  summarize(has_pd_6mm = n() > 0, .groups = 'drop') %>%
  group_by(id) %>%
  summarize(teeth_with_pd6 = list(Tooth), .groups = 'drop') %>%
  rowwise() %>%
  mutate(non_adjacent_teeth = pair(combn(teeth_with_pd6, 2, simplify = FALSE))) %>%
  filter(non_adjacent_teeth >= 2) %>%
  inner_join(periodontitis_cases, by = "id")

# Stage I: Highest interdental CAL is 1-2 mm
stage_I_complex <- max_cal_per_patient %>%
  filter(max_cal >= 1 & max_cal <= 2) %>%
  inner_join(periodontitis_cases, by = "id")

# Stage II: Highest interdental CAL is 3-4 mm and no PPD ≥ 6mm at ≥2 non-adjacent teeth
stage_II_complex <- max_cal_per_patient %>%
  filter(max_cal >= 3 & max_cal <= 4) %>%
  anti_join(pd_6mm_non_adjacent, by = "id") %>%
  inner_join(periodontitis_cases, by = "id")

# Stage III: Two conditions
stage_III_condition1 <- max_cal_per_patient %>%
  filter(max_cal >= 3 & max_cal <= 4) %>%
  inner_join(pd_6mm_non_adjacent, by = "id")

stage_III_condition2 <- max_cal_per_patient %>%
  filter(max_cal >= 5) %>%
  inner_join(periodontitis_cases, by = "id") %>%
  inner_join(teeth_condition_patients_stage_III, by = "id") # Must have ≥10 opposing pairs of natural teeth

stage_III_complex <- bind_rows(stage_III_condition1, stage_III_condition2)

# Stage IV: CAL ≥5mm but <10 opposing pairs of natural teeth
stage_IV_complex <- teeth_condition_patients_stage_IV %>%
  inner_join(max_cal_per_patient %>% filter(max_cal >= 5), by = "id")

# Complete Edentulism: Patients with zero teeth
fully_edentulous <- selected_data %>%
  filter(Total_Teeth == 0) %>%
  mutate(periodontitis_category = "Complete Edentulism")

# Assign ACES Classification with complexity factors
selected_data <- selected_data %>%
  mutate(
    ACES_with_complexity = case_when(
      id %in% stage_I_complex$id ~ "Stage I Periodontitis (Complex)",
      id %in% stage_II_complex$id ~ "Stage II Periodontitis (Complex)",
      id %in% stage_III_complex$id ~ "Stage III Periodontitis (Complex)",
      id %in% stage_IV_complex$id ~ "Stage IV Periodontitis (Complex)",
      id %in% fully_edentulous$id ~ "Complete Edentulism",
      TRUE ~ "Non-Classified"
    )
  )

# Merge classification into final dataset
final_classified_data <- final_classified_data %>%
  left_join(selected_data %>% select(id, ACES_with_complexity), by = "id")
