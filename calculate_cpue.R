# ============================================================================
# CPUE Calculation and Data Preparation for Spatial Modeling (sdmTMB)
# ============================================================================
# Inputs:
#   data/3-Drift Information_25.csv  -- one row per drift with effort & coords
#   data/4-Caught Fishes_25.csv      -- one row per fish, linked by Drift ID
#
# Output:
#   output/cpue_by_drift_clean.csv   -- one row per drift with CPUE & midpoint
# ============================================================================

library(tidyverse)

# ============================================================================
# STEP 1: Load Data
# ============================================================================

drift_data <- read_csv("data/3-Drift Information_25.csv", show_col_types = FALSE)
fish_data  <- read_csv("data/4-Caught Fishes_25.csv",     show_col_types = FALSE)

cat("Drift data: ", nrow(drift_data), "rows,", ncol(drift_data), "columns\n")
cat("Fish data:  ", nrow(fish_data),  "rows,", ncol(fish_data),  "columns\n")

# ============================================================================
# STEP 2: Count Fish per Drift
# ============================================================================

fish_count <- fish_data %>%
  filter(!is.na(`Drift ID`)) %>%
  group_by(`Drift ID`) %>%
  summarise(total_fish_caught = n(), .groups = "drop")

cat("\nFish count summary:\n")
print(summary(fish_count$total_fish_caught))

# ============================================================================
# STEP 3: Merge Fish Counts with Drift Data and Calculate CPUE
# ============================================================================
# Effort column: "Total Angler Hrs" (product of anglers and drift time)
# CPUE = total fish caught / total angler hours

cpue_data <- drift_data %>%
  select(
    `Drift ID`,
    `Trip ID`,
    `ID Cell per Trip`,
    `Grid Cell ID`,
    `Site (MPA/ REF)`,
    `Drifting or Holding Station`,
    `Drift Time (hrs)`,
    `Total Angler Hrs`,
    `Total . Anglers Fishing`,
    ST_LatDD,
    ST_LonDD,
    End_LatDD,
    End_LonDD,
    `Drift Length (m)`,
    `Exclude:Gear-Specific CPUE`
  ) %>%
  # Join fish counts (drifts with no fish caught get NA -> replaced with 0)
  left_join(fish_count, by = "Drift ID") %>%
  mutate(total_fish_caught = replace_na(total_fish_caught, 0)) %>%
  # Calculate CPUE; set to NA where effort is zero or missing
  mutate(
    cpue = if_else(
      !is.na(`Total Angler Hrs`) & `Total Angler Hrs` > 0,
      total_fish_caught / `Total Angler Hrs`,
      NA_real_
    )
  )

# ============================================================================
# STEP 4: Calculate Drift Midpoint Coordinates
# ============================================================================

cpue_data <- cpue_data %>%
  mutate(
    mid_lat = if_else(
      !is.na(ST_LatDD) & !is.na(End_LatDD),
      (ST_LatDD + End_LatDD) / 2,
      coalesce(ST_LatDD, End_LatDD)   # use whichever is available
    ),
    mid_lon = if_else(
      !is.na(ST_LonDD) & !is.na(End_LonDD),
      (ST_LonDD + End_LonDD) / 2,
      coalesce(ST_LonDD, End_LonDD)
    )
  )

# ============================================================================
# STEP 5: Data Quality Checks
# ============================================================================

cat("\n=== Data Quality Report ===\n")
cat("Total drifts:                      ", nrow(cpue_data), "\n")
cat("Drifts with zero effort:           ",
    sum(cpue_data$`Total Angler Hrs` == 0, na.rm = TRUE), "\n")
cat("Drifts with missing effort:        ",
    sum(is.na(cpue_data$`Total Angler Hrs`)), "\n")
cat("Drifts with no fish caught:        ",
    sum(cpue_data$total_fish_caught == 0), "\n")
cat("Drifts with CPUE calculated:       ",
    sum(!is.na(cpue_data$cpue)), "\n")
cat("Drifts with missing mid_lat/lon:   ",
    sum(is.na(cpue_data$mid_lat) | is.na(cpue_data$mid_lon)), "\n")
cat("Drifts flagged for exclusion:      ",
    sum(cpue_data$`Exclude:Gear-Specific CPUE` == TRUE, na.rm = TRUE), "\n")

cat("\nCPUE summary (all drifts):\n")
print(summary(cpue_data$cpue))

# ============================================================================
# STEP 6: Build Final Clean Dataset
# ============================================================================
# Rename columns to snake_case for easier use in sdmTMB downstream work,
# and keep only the fields needed for spatial modelling.

final_data <- cpue_data %>%
  rename(
    drift_id          = `Drift ID`,
    trip_id           = `Trip ID`,
    id_cell_per_trip  = `ID Cell per Trip`,
    grid_cell_id      = `Grid Cell ID`,
    site              = `Site (MPA/ REF)`,
    drift_type        = `Drifting or Holding Station`,
    drift_time_hrs    = `Drift Time (hrs)`,
    total_angler_hrs  = `Total Angler Hrs`,
    total_anglers     = `Total . Anglers Fishing`,
    start_lat         = ST_LatDD,
    start_lon         = ST_LonDD,
    end_lat           = End_LatDD,
    end_lon           = End_LonDD,
    drift_length_m    = `Drift Length (m)`,
    exclude_cpue      = `Exclude:Gear-Specific CPUE`
  ) %>%
  select(
    drift_id,
    trip_id,
    id_cell_per_trip,
    grid_cell_id,
    site,
    drift_type,
    drift_time_hrs,
    total_angler_hrs,
    total_anglers,
    total_fish_caught,
    cpue,
    start_lat,
    start_lon,
    end_lat,
    end_lon,
    mid_lat,
    mid_lon,
    drift_length_m,
    exclude_cpue
  )

# ============================================================================
# STEP 7: Save Output
# ============================================================================

dir.create("output", showWarnings = FALSE)
write_csv(final_data, "output/cpue_by_drift_clean.csv")

cat("\n✓ CPUE calculation complete!\n")
cat("✓ Output saved to: output/cpue_by_drift_clean.csv\n")
cat("✓ Dataset contains", nrow(final_data), "drifts\n")
cat("\nFirst few rows of output:\n")
print(head(final_data))
