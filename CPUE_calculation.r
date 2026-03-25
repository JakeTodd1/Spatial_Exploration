library(tidyverse)
library(sf)

#load data
drift_data <- read_csv("data/3-Drift Information_25.csv")
fish_data <- read_csv("data/4-Caught Fishes_25.csv")

# calculate number of fish caught per trip 

fish_count <- fish_data %>%
  filter(!is.na(`Drift ID`)) %>%   # Remove rows with NA Drift ID
  group_by(`Drift ID`) %>%   # Count fish per drift
  summarise(
    total_fish_caught = n(),
    .groups = "drop"
  )

#calculate drift midpoints for spatial analysis 
drift_spatial <- drift_data %>%
  select(
    `Drift ID`, `Trip ID`, `Grid Cell ID`, `Site (MPA/ REF)`,
    `Total Angler Hrs`, `ST_LatDD`, `ST_LonDD`, `End_LatDD`, `End_LonDD`
  ) %>%
  distinct(`Drift ID`, .keep_all = TRUE) %>%
  mutate(
    drift_lat = (`ST_LatDD` + `End_LatDD`) / 2,
    drift_lon = (`ST_LonDD` + `End_LonDD`) / 2
  )

#calculate CPUE 
cpue_data <- drift_spatial %>%
  # Join with fish count
  left_join(
    fish_count,
    by = "Drift ID"
  ) %>%
  # Replace NA fish counts with 0 (no fish caught)
  mutate(
    total_fish_caught = replace_na(total_fish_caught, 0),
    # Calculate CPUE: fish per angler hour
    cpue = total_fish_caught / `Total Angler Hrs`,
    # Handle division by zero and infinite values
    cpue = ifelse(is.infinite(cpue) | is.na(cpue), 0, cpue),
    .after = `total_fish_caught`
  )

#CPUE summary stats
cpue_summary <- cpue_data %>%
  summarise(
    mean_cpue = mean(cpue, na.rm = TRUE),
    median_cpue = median(cpue, na.rm = TRUE),
    sd_cpue = sd(cpue, na.rm = TRUE),
    min_cpue = min(cpue, na.rm = TRUE),
    max_cpue = max(cpue, na.rm = TRUE)
  )
print(cpue_summary)

#summary stats by type
cpue_by_site <- cpue_data %>%
  group_by(`Site (MPA/ REF)`) %>%
  summarise(
    n_drifts = n(),
    mean_cpue = mean(cpue, na.rm = TRUE),
    median_cpue = median(cpue, na.rm = TRUE),
    sd_cpue = sd(cpue, na.rm = TRUE),
    .groups = "drop"
  )
print(cpue_by_site)

#plot

#export data 

# Final dataset with all relevant columns for spatial analysis

final_output <- cpue_data %>%
  select(
    `Drift ID`,
    `Trip ID`,
    `Grid Cell ID`,
    `Site (MPA/ REF)`,
    `Total Angler Hrs`,
    `total_fish_caught`,
    `cpue`,
    `ST_LatDD`,
    `ST_LonDD`,
    `End_LatDD`,
    `End_LonDD`,
    `drift_lat`,
    `drift_lon`
  )

# Export to CSV
write_csv(final_output, "cpue_by_drift_final.csv")

