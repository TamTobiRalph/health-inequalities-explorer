### The lack of avalability of discharge data (which only has the month of April & May) makes it an uphill
### task to create a model to predict surge durring the winter. The aim of this work below is to see other 
### variables that are closesly correlated to discharge and have sufficient of historical data to create a model
### to predict surge.


# Loading necessary packages
library(tidyverse)
library(httr)
library(readxl)
library(corrr)
library(janitor)
library(dplyr)
library(skimr)

#load url links for discharge data for April and May (June is not avalable at this time 29/7/2022)
april_data <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Daily-Discharge-SitRep-Monthly-Data-Web-File-April2022-2.xlsx"
may_data <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/06/Daily-Discharge-SitRep-Monthly-Data-Web-File-May2022.xlsx"

# Load April raw data
 GET(
      april_data,
    write_disk(april <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
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

# Loading the data into our enviroment and selecting the neccessary rows
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
    may_pivot |> distinct(`Org Code`, may_mean, may_sum),
    by = "Org Code"
  )

###Bed Occupancy
#Load Bed occupancy url for dfferent quaters
bed_day <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Day-Only-Web_File-Q4-2021-22-Final-OIUJK.xlsx"
bed_night <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q4-2021-22-Final-OIUJK.xlsx"
bed_day_3 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Day-Only-Web_File-Q3-2021-22-Final-QAZSD.xlsx"
bed_night_3 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q3-2021-22-Final.xlsx"
bed_day_2 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Day-Only-Web_File-Q2-2021-22-Final.xlsx"
bed_night_2 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q2-2021-22-Final.xlsx"
bed_day_1 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Day-Only-Web_File-Q1-2021-22-Final.xlsx"
bed_night_1 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q1-2021-22-Final.xlsx"

# Load bed day raw data for the 4th quater
 GET(
    bed_day,
    write_disk(bed_day <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
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

# Select relevant columns
beds_day_vars <-
  beds_day_sliced |>
  select(
    `Org Code`,
    `total_beds_available_day`= `Total...6`,
    `general_acute_day_beds_occupied` = `General & Acute...13`
  )

# Load bed night raw data for the 4th quater
 GET(
    bed_night,
    write_disk(bed_night <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
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

# Load bed day raw data for the 3rd quater
 GET(
    bed_day_3,
    write_disk(bed_day_3 <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
raw_day_3 <-
  read_excel(
    bed_day_3,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_day_sliced_3 <-
  raw_day_3 |>
  slice(-(1:2))

# Select cols
beds_day_vars_3 <-
  beds_day_sliced_3 |>
  select(
    `Org Code`,
    `3_total_beds_available_day`= `Total...6`,
    `3_general_acute_day_beds_occupied` = `General & Acute...13`
  )

# Load bed night raw data for the 3rd quater
GET(
    bed_night_3,
    write_disk(bed_night_3 <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
raw_night_3 <-
  read_excel(
    bed_night_3,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_night_sliced_3 <-
  raw_night_3 |>
  slice(-(1:2))

# Select cols
beds_night_vars_3 <-
  beds_night_sliced_3 |>
  select(
    `Org Code`,
    `3_total_beds_available_night`= `Total...6`,
    `3_general_acute_night_beds_occupied` = `General & Acute...13`
  )
  
# Load bed day raw data for the 2nd quater
GET(
    bed_day_2,
    write_disk(bed_day_2 <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
raw_day_2 <-
  read_excel(
    bed_day_2,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_day_sliced_2 <-
  raw_day_2 |>
  slice(-(1:2))

# Select cols
beds_day_vars_2 <-
  beds_day_sliced_2 |>
  select(
    `Org Code`,
    `2_total_beds_available_day`= `Total...6`,
    `2_general_acute_day_beds_occupied` = `General & Acute...13`
  )

# Load bed night raw data for the 2nd quater
GET(
    bed_night_2,
    write_disk(bed_night_2 <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
raw_night_2 <-
  read_excel(
    bed_night_2,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_night_sliced_2 <-
  raw_night_2 |>
  slice(-(1:2))

# Select cols
beds_night_vars_2 <-
  beds_night_sliced_2 |>
  select(
    `Org Code`,
    `2_total_beds_available_night`= `Total...6`,
    `2_general_acute_night_beds_occupied` = `General & Acute...13`
  )

# Load bed day raw data for the 1st quater
GET(
    bed_day_1,
    write_disk(bed_day_1 <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
raw_day_1 <-
  read_excel(
    bed_day_1,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_day_sliced_1 <-
  raw_day_1 |>
  slice(-(1:2))

# Select cols
beds_day_vars_1 <-
  beds_day_sliced_1 |>
  select(
    `Org Code`,
    `1_total_beds_available_day`= `Total...6`,
    `1_general_acute_day_beds_occupied` = `General & Acute...13`
  )

# Load bed night raw data for the 1st quater
GET(
    bed_night_1,
    write_disk(bed_night_1 <- tempfile(fileext = ".xlsx"))
  )

# Loading the data into our enviroment and selecting the neccessary rows
raw_night_1 <-
  read_excel(
    bed_night_1,
    sheet = "NHS Trust by Sector",
    skip = 14
  )

# remove first two entries (one is totals, other is blank)
beds_night_sliced_1 <-
  raw_night_1 |>
  slice(-(1:2))

# Select cols
beds_night_vars_1 <-
  beds_night_sliced_1 |>
  select(
    `Org Code`,
    `1_total_beds_available_night`= `Total...6`,
    `1_general_acute_night_beds_occupied` = `General & Acute...13`
  )


#Merging day and night bed together for all 4 quaters
bed_occupancy <-
  beds_day_vars |>
  left_join(
    beds_night_vars,
    by = "Org Code")|>
  left_join(
    beds_day_vars_3,
    by = "Org Code") |>
  left_join(
    beds_night_vars_3,
    by = "Org Code") |>
  left_join(
    beds_day_vars_2,
    by = "Org Code") |>
  left_join(
    beds_night_vars_2,
    by = "Org Code") |>
  left_join(
    beds_day_vars_1,
    by = "Org Code") |>
  left_join(
    beds_night_vars_1,
    by = "Org Code")
  

#Merging Discharge data with bed occupancy
correlation <-
  discharge |>
  left_join(
    bed_occupancy,
    by = "Org Code"
  )

glimpse(correlation)
summary(correlation)

# Let see which variable is closely correlated with discharge. 
correlate <- correlate(correlation|>
          select(where(is.double))
)

view(correlate)

### Total bed available at night has a strong correlation with discharge data. 
### Plus it has sufficent historical data to build a model.
