# ---- Load libs & funs ----
library(tidyverse)
library(readxl)
library(geographr)
library(httr)
library(sf)
source("https://raw.githubusercontent.com/britishredcrosssociety/resilience-index/main/R/utils.R")

# ---- Load helpers ----
ltla21_codes <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  filter(str_detect(ltla21_code, "^E")) |>
  pull(ltla21_code)

ltla21_names <-
  boundaries_ltla21 |>
  st_drop_geometry() |>
  select(starts_with("ltla21_"))

# ---- Load data ----
tf <-
  download_file(
    "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fhealthandwellbeing%2fdatasets%2fhealthindexscoresengland%2fcurrent/healthindexscoresatnationalregionalandlocalauthoritylevelsenglandtimeseries.xlsx",
    ".xlsx"
  )

hi_domains_indicators <-
  read_excel(
    tf,
    sheet = "Table_7_2019_Index",
    skip = 4
  )

hi_overall <-
  read_excel(
    tf,
    sheet = "Table_2_Index_scores",
    skip = 2
  )

# ---- Clean ----
# Keep vars of interest
hi_domains_indicators_select <-
  hi_domains_indicators |>
  rename(ltla21_code = `Area Code`) |>
  select(-`Area Name`)

hi_overall_select <-
  hi_overall |>
  select(
    ltla21_code = `Area Code`,
    `Overall Score` = `2019`
  )

# Join overall and domain scores
hi_all <-
  hi_overall_select |>
  left_join(
    hi_domains_indicators_select,
    by = "ltla21_code"
  )

# Filter to only LTLA codes
hi_ltla <-
  hi_all |>
  filter(
    ltla21_code %in% ltla21_codes
  )

# Check that non-matched codes are not LTLA codes
unmatched_codes <-
  hi_all |>
  filter(
    !(ltla21_code %in% ltla21_codes)
  ) |>
  filter(str_detect(ltla21_code, "^E0")) |>
  pull(ltla21_code) |>
  length()

if (unmatched_codes != 0) {
  stop("Not all LTLA codes have been matched")
} else {
  print("Good job, you legend.")
}

# Check which codes are missing from 2021 lad code vector
# E06000053: Isles of Scilly
# E09000001: City of London
# From notes: "Except where combined with other areas as above, data are not
# provided for the Isles of Scilly or City of London due to small numbers.
setdiff(
  ltla21_codes, hi_ltla$ltla21_code
)

# Convert scores to ranks and deciles
hi_index_ranks <-
  hi_ltla |>
  normalise_indicators() |>
  mutate(
    across(
      where(is.numeric),
      inverse_rank
    )
  )

# Pivot
ltla_vul_england <-
  hi_index_ranks |>
  left_join(ltla21_names, by = c("ltla21_code" = "ltla21_code")) |>
  select(-ltla21_code) |>
  relocate(ltla21_name) |>
  mutate(ltla21_name = str_replace_all(ltla21_name, "'", "")) |>
  rename(area_name = ltla21_name) |>
  select(1:10) |>
  pivot_longer(cols = -area_name, names_to = "variable")

usethis::use_data(ltla_vul_england, overwrite = TRUE)