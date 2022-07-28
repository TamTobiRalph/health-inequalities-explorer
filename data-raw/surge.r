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
            distinct(Region, `Org Code`, `Org Name`, april_mean, april_sum) 

#Pivot longer all the date column, get the mean and sum for each trust
april_mean <- april_pivot |>
                  select(Region, `Org Code`, `Org Name`, april_mean)
                  

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


#
may_pivot <- may_selected |> 
            pivot_longer(cols = !c(Region, `Org Code`, `Org Name`)) |>
            group_by(`Org Code`) |>
            mutate(may_mean = mean(value)) |>
            mutate(may_sum = sum(value)) |>
            distinct(Region, `Org Code`, `Org Name`, may_mean, may_sum) 

may_mean <- may_pivot |>
                  select(Region, `Org Code`, `Org Name`, may_mean)


# April and May discharge
discharge <-
  april_pivot |>
  left_join(
    may_pivot |> distinct(may_mean, may_sum),
    by = "Org Code"
  )

# April and May discharge that includes just mean
discharge_mean <-
  april_mean |>
  left_join(
    may_mean |> distinct(may_mean),
    by = "Org Code"
  )

###Bed Occupancy
#Load Bed occupancy url
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
    `Org Code`,
    `total_beds_available_day`= `Total...6`,
    `general_acute_day_beds_occupied` = `General & Acute...13`
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
    `Org Code`,
    `total_beds_available_night`= `Total...6`,
    `general_acute_night_beds_occupied` = `General & Acute...13`
  )

#Merging day and night bed together
bed_occupancy <-
  beds_day_vars |>
  left_join(
    beds_night_vars,
    by = "Org Code"
  )

#Merging Discharge data with bed occupancy
correlation <-
  discharge |>
  left_join(
    bed_occupancy,
    by = "Org Code"
  )

#Merging Discharge data with bed occupancy
correlation_mean <-
  discharge_mean |>
  left_join(
    bed_occupancy,
    by = "Org Code"
  )


glimpse(correlation)
glimpse(correlation_mean)
summary(correlation)
summary(correlation_mean)

### ERROR

cbind(
   lapply(
     lapply(correlation, is.na)
     , sum)
   )


transform(correlation, may_mean = as.numeric(may_mean), may_sum = as.numeric(may_sum), 
               april_mean = as.numeric(april_mean), april_sum = as.numeric(april_sum), 
               total_beds_available_day = as.numeric(total_beds_available_day), 
               general_acute_day_beds_occupied = as.numeric(general_acute_day_beds_occupied),
               total_beds_available_night = as.numeric(total_beds_available_night), 
               general_acute_night_beds_occupied = as.numeric(general_acute_night_beds_occupied)
               )
cor <- correlation |> 
        select(may_mean,may_sum,april_mean,april_sum,total_beds_available_day,
        general_acute_day_beds_occupied,total_beds_available_night,general_acute_night_beds_occupied
        
        )

correlate(cor)
correlate(correlation)
library(skimr)
correlate(correlation$may_mean, correlation$general_acute_day_beds_occupied)
correlate(correlation$april_sum, correlation$general_acute_day_beds_occupied)

typeof(correlation$total_beds_available_day)
        



#Select the neccessary variables
cor <- 
  correlation |> 
  select(april_sum, may_sum, total_beds_available_day, general_acute_day_beds_occupied,
  total_beds_available_night, general_acute_night_beds_occupied
  )

cor |> skimr::skim()

#Getting to see the correlation
 plot(correlation |>
          select(april_mean, april_sum, may_mean, may_sum,
          total_beds_available_day,general_acute_day_beds_occupied,
          total_beds_available_night,general_acute_night_beds_occupied)
      )
 plot(correlation_mean |>
          select(april_mean, may_mean,
          total_beds_available_day,general_acute_day_beds_occupied,
          total_beds_available_night,general_acute_night_beds_occupied)
      )
 correlate(correlation_mean$april_mean,correlation_mean$may_mean,
          correlation_mean$total_beds_available_night,)

 install.packages("skimr")
correlation |> skimr::skim()
 # Replace '-' character with NA
#corr <-
  #correlation |>
  #mutate(
   # across(
    #  .cols = !c(Org Code)
     # ~ str_replace_all(.x, "-", NA_character_)
    #)
  #)

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

  # Change cols to double
cor_double <- as.integer(corr_no_na)
    

#Getting to see the correlation
 plot(corr_double)
 correlate(corr_no_na)
