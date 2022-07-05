# ---- Notes ----
# Percentage of high deprivation areas in each ICS
# Use IMD R package to pull IMD data into R
# LSOA's are coterminous with ICS's (check geographr table)

# Perctange of left behind areas in each ICS
# Is a new community of needs index available at LSOA
# Can ward scores be fitted into ICS? geographr table indicates they are not
# coterminous

# Percentage of priority wards in each ICS
# To research

library(tidyverse)
library(geographr)
library(sf)
library(rmapshaper)

# Note: STP's have evolved into ICS's (i.e., they are identical):
# https://www.kingsfund.org.uk/blog/2018/05/what-has-stp-ics-done

ics_vul_england <-
  boundaries_stp21 |>
  mutate(stp21_name = str_replace_all(stp21_name, "'", "")) |>
  select(area_name = stp21_name, area_code = stp21_code) |>
  st_drop_geometry() |>
  mutate(`% high deprivation areas` = sample(42)) |>
  mutate(`% left behind areas` = sample(42)) |>
  mutate(`% priority wards` = sample(42)) |>
  select(-area_code) |>
  pivot_longer(-area_name, names_to = "variable")

usethis::use_data(ics_vul_england, overwrite = TRUE)