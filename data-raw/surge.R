# Loading necessary packages
library(tidyverse)
library(httr)
library(readxl)
library(corrr)
library(janitor)
library(dplyr)

april_data <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Daily-Discharge-SitRep-Monthly-Data-Web-File-April2022-2.xlsx"
may_data <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/06/Daily-Discharge-SitRep-Monthly-Data-Web-File-May2022.xlsx"
# Load April raw data
 GET(
      april_data,
    write_disk(april <- tempfile(fileext = ".xlsx"))
  )


april_raw <-
  read_excel(
    april,
    sheet = "Table 2",
    skip = 59
  )

# Make colnames snake_case
april <-
  april_raw |>
  clean_names()

# Drop columns ( Selecting Number of patients who no longer meet the criteria to reside)
april_selected <-
  april|>
  select( region, org_code, org_name, 
          x4, x8, x12, x16, x20, x24, x28, x32, x36, x40, 
          x44, x48, x52, x56, x60, x64, x68, x72, x76, x80, 
          x84, x88, x92, x96, x100, x104, x108, x112, x116, x120,
  )


# Rename columns to date
april_rename <- rename(april_selected, c("2022-4-1"= "x4", "2022-4-2"= "x8","2022-4-3"= "x12",
                      "2022-4-4"= "x16", "2022-4-5"= "x20","2022-4-6"= "x24","2022-4-7"= "x28",
                      "2022-4-8"= "x32", "2022-4-9"= "x36","2022-4-10"= "x40","2022-4-11"= "x44",                
                      "2022-4-12"= "x48", "2022-4-13"= "x52","2022-4-14"= "x56","2022-4-15"= "x60",
                      "2022-4-16"= "x64", "2022-4-17"= "x68","2022-4-18"= "x72","2022-4-19"= "x76",
                      "2022-4-20"= "x80", "2022-4-21"= "x84","2022-4-22"= "x88","2022-4-23"= "x92",                
                      "2022-4-24"= "x96", "2022-4-25"= "x100","2022-4-26"= "x104","2022-4-27"= "x108",
                      "2022-4-28"= "x112","2022-4-29"= "x116","2022-4-30"= "x120"
))

april_rename$april_total <- rowSums(april_rename[, c("2022-4-1", "2022-4-2",   
         "2022-4-3", "2022-4-4", "2022-4-5", "2022-4-6", "2022-4-7",   
         "2022-4-8", "2022-4-9", "2022-4-10", "2022-4-11", "2022-4-12",  
         "2022-4-13", "2022-4-14", "2022-4-15", "2022-4-16", "2022-4-17",  
         "2022-4-18", "2022-4-19", "2022-4-20", "2022-4-21", "2022-4-22",  
         "2022-4-23", "2022-4-24", "2022-4-25", "2022-4-26", "2022-4-27",  
         "2022-4-28", "2022-4-29", "2022-4-30")])


april_select <- 
        april_rename|>
        select( region, org_code, org_name, april_total
        
        )
# Load May raw data

 GET(
      may_data,
    write_disk(may <- tempfile(fileext = ".xlsx"))
  )


may_raw <-
  read_excel(
    may,
    sheet = "Table 2",
    skip = 58
  )

# Make colnames snake_case
may <-
  may_raw |>
  clean_names()

# Drop columns
may_selected <-
  may|>
  select( region, org_code, org_name, 
          x4, x8, x12, x16, x20, x24, x28, x32, x36, x40, 
          x44, x48, x52, x56, x60, x64, x68, x72, x76, x80, 
          x84, x88, x92, x96, x100, x104, x108, x112, x116, x120,x124
  )

# Rename columns to date
may_rename <- rename(may_selected, c("2022-5-1"= "x4", "2022-5-2"= "x8","2022-5-3"= "x12",
                      "2022-5-4"= "x16", "2022-5-5"= "x20","2022-5-6"= "x24","2022-5-7"= "x28",
                      "2022-5-8"= "x32", "2022-5-9"= "x36","2022-5-10"= "x40","2022-5-11"= "x44",                
                      "2022-5-12"= "x48", "2022-5-13"= "x52","2022-5-14"= "x56","2022-5-15"= "x60",
                      "2022-5-16"= "x64", "2022-5-17"= "x68","2022-5-18"= "x72","2022-5-19"= "x76",
                      "2022-5-20"= "x80", "2022-5-21"= "x84","2022-5-22"= "x88","2022-5-23"= "x92",                
                      "2022-5-24"= "x96", "2022-5-25"= "x100","2022-5-26"= "x104","2022-5-27"= "x108",
                      "2022-5-28"= "x112","2022-5-29"= "x116","2022-5-30"= "x120", "2022-5-31"= "x124"
))

may_rename$may_total <- rowSums(may_rename[, c("2022-5-1", "2022-5-2",   
         "2022-5-3", "2022-5-4", "2022-5-5", "2022-5-6", "2022-5-7",   
         "2022-5-8", "2022-5-9", "2022-5-10", "2022-5-11", "2022-5-12",  
         "2022-5-13", "2022-5-14", "2022-5-15", "2022-5-16", "2022-5-17",  
         "2022-5-18", "2022-5-19", "2022-5-20", "2022-5-21", "2022-5-22",  
         "2022-5-23", "2022-5-24", "2022-5-25", "2022-5-26", "2022-5-27",  
         "2022-5-28", "2022-5-29", "2022-5-30", "2022-5-31")])

may_select <- 
        may_rename|>
        select( region, org_code, org_name, may_total
        
        )
may_selected <-
        may_select|>
        select(org_code, may_total)

discharge <-
  april_select |>
  left_join(
    may_selected,
    by = "org_code"
  )
#bed Occupancy


bed_day <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Day-Only-Web_File-Q4-2021-22-Final-OIUJK.xlsx"
bed_night <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q4-2021-22-Final-OIUJK.xlsx"
 
 GET(
    bed_day,
    write_disk(bed_day <- tempfile(fileext = ".xlsx"))
  )

raw_day <-
  read_excel(
    bed_day,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_day_sliced <-
  raw_day |>
  slice(-(1:2))

# Select cols
beds_day_vars <-
  beds_day_sliced |>
  select(
    `org_code` = `Org Code`,
    `total_beds_available_day`= `Total...6`,
    `general_acute_day_beds_occupied` = `General & Acute...13`,
    `%_general_acute_day_beds_occupied` = `General & Acute...19`,
  )

 GET(
    bed_night,
    write_disk(bed_night <- tempfile(fileext = ".xlsx"))
  )

raw_night <-
  read_excel(
    bed_night,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_night_sliced <-
  raw_night |>
  slice(-(1:2))

# Select cols
beds_night_vars <-
  beds_night_sliced |>
  select(
    `org_code` = `Org Code`,
    `total_beds_available_night`= `Total...6`,
    `general_acute_night_beds_occupied` = `General & Acute...13`,
    `%_general_acute_night_beds_occupied` = `General & Acute...19`,
  )

#Merging day and night bed together
bed_occupancy <-
  beds_day_vars |>
  left_join(
    beds_night_vars,
    by = "org_code"
  )

#Merging Discharge data with bed occupancy
correlation <-
  discharge |>
  left_join(
    bed_occupancy,
    by = "org_code"
  )



#correlation <- correlation |>
#      mutate("%_general_acute_day_beds_occupied" = 
#       replace("%_general_acute_day_beds_occupied", %_"general_acute_day_beds_occupied" == "-", 0))
summary(correlation)



cor <- 
  correlation |> 
  select(april_total, may_total, total_beds_available_day, general_acute_day_beds_occupied,
  total_beds_available_night, general_acute_night_beds_occupied
  )

 plot(cor)
 correlate(cor)


 # Replace '-' character with NA
corr <-
  correlation |>
  mutate(
    across(
      .cols = !c(`org_code`),
      ~ str_replace_all(.x, "-", NA_character_)
    )
  )

# Drop Na
  corr_ <-
  corr |>
  drop_na()

#Select the neccessary variables
  corr_no_na <- 
  corr_ |> 
  select(april_total, may_total, total_beds_available_day, general_acute_day_beds_occupied,
  total_beds_available_night, general_acute_night_beds_occupied
  )

#Getting to see the correlation
 plot(corr_no_na)
 correlate(corr_no_na)
