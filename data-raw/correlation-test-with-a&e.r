# Loading necessary packages
library(tidyverse)
library(httr)
library(readxl)
library(corrr)
library(janitor)
library(dplyr)
library(skimr)

#load url links
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

# Select and rename relevant columns
april_selected <-
  april_raw|>
  select(Region, `Org Code`, `Org Name`,
          "2022-4-1"= "...4", "2022-4-2"= "...8","2022-4-3"= "...12",
                      "2022-4-4"= "...16", "2022-4-5"= "...20","2022-4-6"= "...24","2022-4-7"= "...28",
                      "2022-4-8"= "...32", "2022-4-9"= "...36","2022-4-10"= "...40","2022-4-11"= "...44",                
                      "2022-4-12"= "...48", "2022-4-13"= "...52","2022-4-14"= "...56","2022-4-15"= "...60",
                      "2022-4-16"= "...64", "2022-4-17"= "...68","2022-4-18"= "...72","2022-4-19"= "...76",
                      "2022-4-20"= "...80", "2022-4-21"= "...84","2022-4-22"= "...88","2022-4-23"= "...92",                
                      "2022-4-24"= "...96", "2022-4-25"= "...100","2022-4-26"= "...104","2022-4-27"= "...108",
                      "2022-4-28"= "...112","2022-4-29"= "...116","2022-4-30"= "...120"
        )


#Pivot longer all the date column, get the mean and sum for each trust
april_pivot <- april_selected |> 
            pivot_longer(cols = !c(Region, `Org Code`, `Org Name`)) |>
            group_by(`Org Code`) |>
            mutate(april_mean = mean(value)) |>
            mutate(april_sum = sum(value)) |>
            distinct(Region, `Org Code`, `Org Name`, april_mean, april_sum) |>
            ungroup()

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

# Drop columns ( Selecting Number of patients who no longer meet the criteria to reside)
may_selected <-
  may_raw|>
  select(Region, `Org Code`, `Org Name`,
          "2022-5-1"= "...4", "2022-5-2"= "...8","2022-5-3"= "...12",
                      "2022-5-4"= "...16", "2022-5-5"= "...20","2022-5-6"= "...24","2022-5-7"= "...28",
                      "2022-5-8"= "...32", "2022-5-9"= "...36","2022-5-10"= "...40","2022-5-11"= "...44",                
                      "2022-5-12"= "...48", "2022-5-13"= "...52","2022-5-14"= "...56","2022-5-15"= "...60",
                      "2022-5-16"= "...64", "2022-5-17"= "...68","2022-5-18"= "...72","2022-5-19"= "...76",
                      "2022-5-20"= "...80", "2022-5-21"= "...84","2022-5-22"= "...88","2022-5-23"= "...92",                
                      "2022-5-24"= "...96", "2022-5-25"= "...100","2022-5-26"= "...104","2022-5-27"= "...108",
                      "2022-5-28"= "...112","2022-5-29"= "...116","2022-5-30"= "...120", "2022-5-31"= "...124"
        )


##Pivot longer all the date column, get the mean and sum for each trust
may_pivot <- may_selected |> 
            pivot_longer(cols = !c(Region, `Org Code`, `Org Name`)) |>
            group_by(`Org Code`) |>
            mutate(may_mean = mean(value)) |>
            mutate(may_sum = sum(value)) |>
            distinct(Region, `Org Code`, `Org Name`, may_mean, may_sum)|>
            ungroup() 

# Joining April and May discharge
discharge <-
  april_pivot |>
  left_join(
    may_pivot |> distinct(`Org Code`,may_mean, may_sum),
    by = "Org Code"
  )

###A&E
#Load A&E raw data

april_ae <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/April-2022-AE-by-provider-Z1l86.xls"
may_ae <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/06/May-2022-AE-by-provider-a5cdd.xls"
june_ae <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/07/June-2022-AE-by-provider-4H6D4.xls"
quater_ae <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/07/Quarter-1-2022-23-AE-by-provider-7W9zK.xls"

GET(
    april_ae,
    write_disk(april_ae <- tempfile(fileext = ".xls"))
  )

raw_ae <-
  read_excel(
    april_ae,
    sheet = "Provider Level Data",
    skip = 15
  )


ae_sliced <-
  raw_ae |>
  slice(-(1:2))

ae_select <- 
    ae_sliced|>
    select(`Org Code` = Code, `Total attendances`)


GET(
    may_ae,
    write_disk(may_ae <- tempfile(fileext = ".xls"))
  )


may_ae <-
  read_excel(
    may_ae,
    sheet = "Provider Level Data",
    skip = 15
  )

may_ae_sliced <-
  may_ae |>
  slice(-(1:2))



may_select <- 
    may_ae_sliced|>
    select(`Org Code` = Code, `May Total attendances` = `Total attendances` 
    
)

GET(
    june_ae,
    write_disk(june_ae <- tempfile(fileext = ".xls"))
  )

june_ae <-
  read_excel(
    june_ae,
    sheet = "Provider Level Data",
    skip = 15
  )

june_ae_sliced <-
  june_ae |>
  slice(-(1:2))

june_select <- 
    june_ae_sliced|>
    select(`Org Code` = Code, `June Total attendances`= `Total attendances` )

GET(
    quater_ae,
    write_disk(quater_ae <- tempfile(fileext = ".xls"))
  )

quater_ae <-
  read_excel(
    quater_ae,
    sheet = "Provider Level Data",
    skip = 15
  )

quater_ae_sliced <-
  quater_ae |>
  slice(-(1:2))

quater_select <- 
    quater_ae_sliced|>
    select(`Org Code` = Code, `Quater Total attendances`= `Total attendances` )


discharge_ae <-
  discharge |>
  left_join(
    ae_select |> distinct(`Org Code`, `Total attendances`),
    by = "Org Code" ) |>
    left_join(may_select |> distinct(`Org Code`, `May Total attendances`),
    by = "Org Code" ) |>
    left_join(june_select |> distinct(`Org Code`, `June Total attendances`),
    by = "Org Code" ) |>
    left_join(quater_select |> distinct(`Org Code`, `Quater Total attendances`),
    by = "Org Code" )



glimpse(discharge_ae)


correlate(discharge_ae |>
            select(where(is.double)))

### This has Weak correlation with discharge data. 
### This code could be written better