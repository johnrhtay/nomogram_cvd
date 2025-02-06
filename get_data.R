library(tidyverse)
library(nhanesA)    

# Define NHANES survey cycles and datasets of interest
cycles <- c("F", "G", "H")
datasets <- c("DEMO", "OHXDEN", "OHXPER", "SMQ", "PAQ", "SLQ", "BMX", "BPX", "BPQ", "DIQ", "DR1TOT", "DR2TOT", "INQ", "MCQ")

# Merge NHANES data by cycle
get_merged <- function(df, cycle) {
  purrr::reduce(df$data_translated, dplyr::left_join, by = 'SEQN') %>% 
    mutate(cycle = cycle) %>%
    as_tibble()
}

# Download and process NHANES data

nh_data <- tribble(
  ~cycl, ~q_typ,
  cycle, q_type
) %>% 
  unnest(cols = cycl) %>% 
  unnest(cols = q_typ) %>% 
  # needed to translate variables
  mutate(file= paste(q_type,cycl,sep = "_")) %>% 
  mutate(data_grp= case_when(q_typ=="DEMO"~ "DEMO",
                             grepl("FF",q_typ)~ "DIET",
                             grepl("Q",q_typ)~ "Q",
                             grepl("X",q_typ)~ "EXAM",
                             grepl("DR",q_typ)~ "DIET"
  )) %>%
  
  # To get variable names & their descriptions
  mutate(vars= map2(data_grp,file,nhanesTableVars)) %>%
  # Downloading data
  mutate(data= map(file, nhanes)) %>% 
  mutate(data= map(data, as_tibble))


# Filtering out any empty datasets
nh_data_filtered <- nh_data %>%
  mutate(dims = map_dbl(data, ~dim(.x)[1])) %>%
  filter(dims != 0) %>%
  mutate(var_names = map(data, colnames))

# Extract variable names and translating variables
nh_data_var <- nh_data_filtered %>%
  mutate(data_translated = pmap(list(file, var_names, data), nhanesTranslate))

# Merge data by cycle
cycle_data <- nh_data_var %>%
  select(cycle, data_translated) %>%
  group_by(cycle) %>%
  nest() %>%
  mutate(merged = map2(data, cycle, get_merged))

# Combine the cycles into a single dataset
rbind_data <- bind_rows(cycle_data$merged)

# Apply inclusion criteria
rbind_data <- rbind_data %>% 
  filter(!is.na(RIDAGEYR) & RIDAGEYR >= 30, !is.na(OHDEXCLU) & OHDEXCLU != "Yes")

# Define dental variables
tooth_vars <- paste0("OHX", sprintf("%02d", 1:32), "TC")
existing_teeth_vars <- tooth_vars[tooth_vars %in% colnames(rbind_data)]
rbind_data <- rbind_data %>% filter(rowSums(is.na(select(., all_of(existing_teeth_vars)))) == 0)

cat("Final number of participants after all exclusions:", nrow(rbind_data), "\n")

# Save final dataset
saveRDS(rbind_data, "nhanes_filtered_data_F_G_H_final_nomogram.rds")
