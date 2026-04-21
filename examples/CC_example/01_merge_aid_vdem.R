# ==============================================================================
# 01_merge_aid_vdem.R
# Purpose: Download World Bank ODA data, load V-Dem data, merge on ISO3 + year,
#          and save a clean panel dataset for analysis.
# ==============================================================================

# --- 0. Install packages if needed --------------------------------------------
# Run this block once if you haven't installed these packages before.
# install.packages(c("wbstats", "dplyr", "readr"))

library(wbstats)   # World Bank API wrapper
library(dplyr)     # Data wrangling
library(readr)     # Fast CSV reading/writing


# ==============================================================================
# STEP 1: Download World Bank ODA data
# ==============================================================================

# wb_data() fetches data from the World Bank API.
# - indicator: "DT.ODA.ODAT.GN.ZS" = Net ODA received as % of GNI
# - country = "all" pulls every country/territory in the database
# - start_date / end_date: year range

oda_raw <- wb_data(
  indicator  = "DT.ODA.ODAT.GN.ZS",
  country    = "all",
  start_date = 2000,
  end_date   = 2022
)

# Keep only the columns we need and give them clean names.
# wbstats returns: iso2c, iso3c, country, date, <indicator name>, ...
oda <- oda_raw %>%
  select(
    iso3c,            # ISO 3-letter country code  -- our merge key
    country_wb = country,
    year       = date,
    oda_pct_gni = DT.ODA.ODAT.GN.ZS
  )

cat("ODA data loaded:", nrow(oda), "rows,", n_distinct(oda$iso3c), "unique ISO3 codes\n")


# ==============================================================================
# STEP 2: Load V-Dem data and extract variables of interest
# ==============================================================================

# Path to the V-Dem CSV (v14, country-year dataset)
vdem_path <- "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session/CC_example/V-Dem-CY-Full+Others-v14.csv"

# read_csv() is faster than read.csv() for large files; suppress column-type
# messages with show_col_types = FALSE to keep the console tidy.
vdem_raw <- read_csv(vdem_path, show_col_types = FALSE)

# Select the six variables requested, then filter to 2000-2022
vdem <- vdem_raw %>%
  select(
    country_name,
    country_text_id,   # ISO3-style code in V-Dem -- our merge key
    year,
    v2x_polyarchy,     # Electoral democracy index (0-1)
    v2x_corr,          # Political corruption index (0-1)
    e_gdppc,           # GDP per capita (2011 USD PPP, Maddison)
    e_pop              # Population (thousands, Maddison)
  ) %>%
  filter(year >= 2000, year <= 2022)

cat("V-Dem data loaded:", nrow(vdem), "rows,", n_distinct(vdem$country_text_id),
    "unique country codes\n")


# ==============================================================================
# STEP 3: Merge the two datasets
# ==============================================================================

# --- 3a. Align the join keys --------------------------------------------------
# wbstats uses "iso3c"; V-Dem uses "country_text_id".
# Both are ISO 3166-1 alpha-3 codes *in most cases*, but there are known gaps:
#   - Kosovo:          World Bank = "XKX"; V-Dem = "XKX" (usually matches)
#   - Serbia:          World Bank split from Serbia & Montenegro post-2006
#   - South Sudan:     World Bank = "SSD"; V-Dem = "SSD"
#   - Taiwan:          not in World Bank; V-Dem = "TWN"
#   - West Bank/Gaza:  World Bank = "PSE"; not in V-Dem (or under "PSX")
# We will detect mismatches below and report them rather than silently dropping.

# Rename V-Dem's code column so it matches the ODA column name for the join.
vdem <- vdem %>%
  rename(iso3c = country_text_id)

# --- 3b. Full join diagnostic -------------------------------------------------
# A left_join on ODA keeps all ODA rows; unmatched V-Dem rows are dropped.
# We first do an anti_join in each direction to surface mismatches.

# Countries in ODA that have NO match in V-Dem
oda_no_match <- anti_join(oda, vdem, by = c("iso3c", "year")) %>%
  distinct(iso3c, country_wb) %>%
  arrange(iso3c)

# Countries in V-Dem that have NO match in ODA
vdem_no_match <- anti_join(vdem, oda, by = c("iso3c", "year")) %>%
  distinct(iso3c, country_name) %>%
  arrange(iso3c)

cat("\n--- Merge diagnostics ---\n")
cat("ODA country-codes with NO V-Dem match (", nrow(oda_no_match), "):\n")
print(oda_no_match)

cat("\nV-Dem country-codes with NO ODA match (", nrow(vdem_no_match), "):\n")
print(vdem_no_match)

# --- 3c. Perform the merge ----------------------------------------------------
# left_join: keep all ODA observations; attach V-Dem data where available.
# Country-years in ODA but not V-Dem will have NA on V-Dem columns.
merged <- left_join(oda, vdem, by = c("iso3c", "year"))

cat("\nMerge complete:", nrow(merged), "country-year rows\n")


# ==============================================================================
# STEP 4: Save the merged dataset
# ==============================================================================

# Save to the same folder as the source data so everything stays together.
output_path <- "C:/Users/Patrick Shea/Dropbox/AI Upskilling/Walkthru/april26session/CC_example/aid_vdem_merged.csv"

write_csv(merged, output_path)
cat("Saved merged dataset to:\n ", output_path, "\n")


# ==============================================================================
# STEP 5: Summary report
# ==============================================================================

cat("\n========== DATASET SUMMARY ==========\n")

cat("Total country-year observations:", nrow(merged), "\n")
cat("Year range:", min(merged$year), "to", max(merged$year), "\n")
cat("Unique countries (by ISO3):", n_distinct(merged$iso3c), "\n")

cat("\nMissing values by variable:\n")

# Loop over all substantive columns and count NAs
vars_to_check <- c("oda_pct_gni", "v2x_polyarchy", "v2x_corr", "e_gdppc", "e_pop")

for (v in vars_to_check) {
  n_miss  <- sum(is.na(merged[[v]]))
  pct_miss <- round(100 * n_miss / nrow(merged), 1)
  cat(sprintf("  %-20s  %5d missing  (%5.1f%%)\n", v, n_miss, pct_miss))
}

cat("======================================\n")
