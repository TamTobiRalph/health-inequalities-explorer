library(tidyverse)
library(geographr)
library(sf)
library(rmapshaper)

# Note: STP's have evolved into ICS's (i.e., they are identical):
# https://www.kingsfund.org.uk/blog/2018/05/what-has-stp-ics-done

ics_cap_england <-
  boundaries_stp21 |>
  mutate(stp21_name = str_replace_all(stp21_name, "'", "")) |>
  select(area_name = stp21_name, area_code = stp21_code) |>
  st_drop_geometry() |>
  mutate(`A&E wait times` = sample(42)) |>
  mutate(`Day beds occupancy rate` = sample(42)) |>
  mutate(`Night beds occupancy rate` = sample(42)) |>
  mutate(`Cancer waiting lists` = sample(42)) |>
  mutate(`% Diagnostic waiting times` = sample(42)) |>
  mutate(`IAPT performance` = sample(42)) |>
  mutate(`Referral to treatement` = sample(42)) |>
  select(-area_code) |>
  pivot_longer(-area_name, names_to = "variable")

usethis::use_data(ics_cap_england, overwrite = TRUE)