# Loading necessary packages
library(tidyverse)
library(httr)
library(readxl)
library(corrr)
library(janitor)
library(dplyr)
library(skimr)
library(ggplot2)
library(forecast)
library(tsibble)
library(feasts)
library(purrr)
library(broom)
library(lubridate)
library(magrittr)
library(fable)
library(fpp3)

###Bed Occupancy
# Load Bed occupancy url for dfferent quaters from the first quarter 2011 to the 4th quater of 2021
# However because of the skewed data in 2020 and part of 2021, as a result of covid, we only be using data from 2019

url_xlsx <- c(
  #q4_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q4-2021-22-Final-OIUJK.xlsx",
  #q3_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q3-2021-22-Final.xlsx",
  #q2_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q2-2021-22-Final.xlsx",
  #q1_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q1-2021-22-Final.xlsx",
  #q4_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q4-2020-21-Final-1.xlsx",
  #q3_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q3-2020-21-Final-1.xlsx",
  #q2_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q2-2020-21-Final-1.xlsx",
  #q1_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q1-2020-21-Final-1.xlsx",
  #q4_19_20 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/1920-Q4-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  q3_19_20 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/1920-Q3-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  q2_19_20 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/1920-Q2-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  q1_19_20 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/1920-Q1-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
  q4_18_19 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Beds-Open-Overnight-Web_File-Final_Q4_2018-19.xlsx",
  q3_18_19 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/11/Beds-Open-Overnight-Web_File-Final-Q3-201819.xlsx",       
  q2_18_19 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Beds-Open-Overnight-Q2-201819-REVISED-Web_File-Final.xlsx",       
  q1_18_19 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Beds-Open-Overnight-Q1-201819-REVISED-Web_File-Final.xlsx",
  q4_17_18 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Beds-Open-Overnight-Q4-201718-REVISED-Web_File-Final.xlsx",
  q3_17_18 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/05/Beds-Open-Overnight-Q3-201718-REVISED-Web_File-Final.xlsx",
  q2_17_18 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/01/Beds-Open-Overnight-Web_File-Q2-2017-18-Final.xlsx",
  q1_17_18 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/01/Beds-Open-Overnight-Web_File-Q1-2017-18-Final.xlsx",
  q4_16_17 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/11/Beds-Open-Overnight-Web_File-Q4-2016-17-Final-98123.xlsx",
  q3_16_17 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/11/Beds-Open-Overnight-Web_File-Q3-2016-17-Final-98123.xlsx",
  q2_16_17 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/11/Beds-Open-Overnight-Web_File-Q2-2016-17-Final-98123.xlsx",
  q1_16_17 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/11/Beds-Open-Overnight-Web_File-Q1-2016-17-Final-98123.xlsx",
  q4_15_16 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q4-2015-16-Revised-Aug16-Final-84398.xlsx",
  q3_15_16 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Final-Q3-2015-16-39849.xlsx",
  q2_15_16 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Final-Q2-2015-16-38498.xlsx",
  q1_15_16 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Final-Q1-2015-16-52341.xlsx",
  q4_14_15 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q4-2014-15-Revised-Nov15-Final-41885.xlsx",
  q3_14_15 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q3-2014-15-Revised-Nov15-Final-33314.xlsx",
  q2_14_15 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q2-2014-15-Revised-Nov15-Final-41447.xlsx",
  q1_14_15 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q1-2014-15-Revised-Nov15-Final-21447.xlsx",
  q4_13_14 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q4-2013-14-Final-Working-Revised-33335.xlsx"
)

#Loading bed occupancy url with different format "xls"
url_xls <- c(
  q3_13_14 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q3-2013-14-Revised-Final1.xls",
  q2_13_14 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q2-2013-14-Revised-Final1.xls",
  q1_13_14 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q1-2013-14-Nov-Revised-Revised-Final.xls",
  q4_12_13 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q4-2012-13-Revised-Final-79659.xls",
  q3_12_13 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q3-2012-13-Revised-Final-76557.xls",
  q2_12_13 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q2-2012-13-May-2013-Refresh-Final-8947-v2.xls",
  q1_12_13 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q1-2012-13-May-2013-Refresh-Final-47384.xls",
  q4_11_12 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q4-2011-12-November-2012-Refresh-v3-Final.xls",
  q3_11_12 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q3-2011-12-November-2012-Refresh-v2-Final.xls",
  q2_11_12 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q2-2011-12-November-2012-Refresh-v2-Final.xls",
  q1_11_12 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/Beds-Open-Overnight-Web_File-Q1-2011-12-November-2012-Refresh-v3-Final.xls"
)

# Creating an empty list that takes we can store all our bed occupancy data

#file_ <- list()

#i=1

# Creating a for loop that picks the exact data we want, clean it and stores it in our empty list for file format of xlxs
#for(qu in url_xlsx) { 
#  GET(
#    qu,
#    write_disk(bed_night <- tempfile(fileext = ".xlsx"))
#  ) 
#  file_[[i]] <-
#    read_excel(
#      bed_night,
#      sheet = "NHS Trust by Sector",
#      skip = 14) |>
#    slice(-(1:2)) |>
#    select(
#      `Org Code`,
#      `bed` = `General & Acute...13`,
#      
#    )
#  #mutate(quater=str_extract(qu, 'Q[1-4]'))
#  i=i+1
#}

# Creating a for loop that picks the exact data we want, clean it and stores it in our empty list for file format of xlx
#for(qu in url_xls) { 
#  GET(
#    qu,
#    write_disk(bed_night <- tempfile(fileext = ".xls"))
#  ) 
#  file_[[i]] <-
#    read_excel(
#      bed_night,
#      sheet = "NHS Trust by Sector",
#      skip = 14) |>
#    slice(-(1:2)) |>
#    select(
#      `Org Code`,
#      `bed` = `General & Acute...13`,
#      
#    )
#  #mutate(quater=str_extract(qu, 'Q[1-4]'))
#  i=i+1
#}

# Creating an empty list that takes we can store all our bed occupancy data
file_ <- list()

i=1

# Creating a for loop that picks the exact data we want, clean it, calculates the percentage and,
# stores it in our empty list for file format of xlxs
for(qu in url_xlsx) { 
  GET(
    qu,
    write_disk(bed_night <- tempfile(fileext = ".xlsx"))
  ) 
  file_[[i]] <-
    read_excel(
      bed_night,
      sheet = "NHS Trust by Sector",
      skip = 14) |>
   slice(-(1:2)) |>
    mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
    select(
      `Org Code`,
      `bed` = `bed_perc`,
      
    )
#mutate(quater=str_extract(qu, 'Q[1-4]'))
  i=i+1
}

# Creating a for loop that picks the exact data we want, clean it, calculates the percentage and,
# stores it in our empty list for file format of xls
for(qu in url_xls) { 
  GET(
    qu,
    write_disk(bed_night <- tempfile(fileext = ".xls"))
  ) 
  file_[[i]] <-
    read_excel(
      bed_night,
      sheet = "NHS Trust by Sector",
      skip = 14) |>
    slice(-(1:2)) |>
    mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
    select(
      `Org Code`,
      `bed` = `bed_perc`,
      
    )
  #mutate(quater=str_extract(qu, 'Q[1-4]'))
  i=i+1
}

# Joining all the data in our list, binding it with Org Code 
bed_join <- file_ |> reduce(left_join, by = "Org Code")

# Renaming the Quater Column
# This should be done in a better way, as it is hetic when you decide to cut some years off
#bed <- join_bed |> select(`Org Code`,
#          `2022 Q1`= "bed.x", `2021 Q4` = "bed.y", `2021 Q3` = "bed.x.x", `2021 Q2` = "bed.y.y", `2021 Q1` = "bed.x.x.x",
#          `2020 Q4` = "bed.y.y.y", `2020 Q3` = "bed.x.x.x.x", `2020 Q2` = "bed.y.y.y.y",`2020 Q1` = "bed.x.x.x.x.x",
#          `2019 Q4` = "bed.y.y.y.y.y", `2019 Q3` = "bed.x.x.x.x.x.x", `2019 Q2` = "bed.y.y.y.y.y.y", `2019 Q1` = "bed.x.x.x.x.x.x.x",
#          `2018 Q4` = "bed.y.y.y.y.y.y.y", `2018 Q3` = "bed.x.x.x.x.x.x.x.x", `2018 Q2` = "bed.y.y.y.y.y.y.y.y", `2018 Q1` = "bed.x.x.x.x.x.x.x.x.x",
#          `2017 Q4` = "bed.y.y.y.y.y.y.y.y.y", `2017 Q3` = "bed.x.x.x.x.x.x.x.x.x.x", `2017 Q2` = "bed.y.y.y.y.y.y.y.y.y.y", `2017 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x",
#          `2016 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y", `2016 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x", `2016 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y", `2016 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          `2015 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y",`2015 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x",`2015 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y",`2015 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          `2014 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2014 Q3` ="bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2014 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2014 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          `2013 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2013 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",`2013 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2013 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          `2012 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2012 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2012 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2012 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#         `2011 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2011 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2011 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"   
#) 


# Without the covid year
bed <- bed_join |> select(`Org Code`,
                          `2019 Q3`= "bed.x", `2019 Q2` = "bed.y", `2019 Q1` = "bed.x.x", `2018 Q4` = "bed.y.y", `2018 Q3` = "bed.x.x.x",
                          `2018 Q2` = "bed.y.y.y", `2018 Q1` = "bed.x.x.x.x", `2017 Q4` = "bed.y.y.y.y",`2017 Q3` = "bed.x.x.x.x.x",
                          `2017 Q2` = "bed.y.y.y.y.y", `2017 Q1` = "bed.x.x.x.x.x.x", `2016 Q4` = "bed.y.y.y.y.y.y", `2016 Q3` = "bed.x.x.x.x.x.x.x",
                          `2016 Q2` = "bed.y.y.y.y.y.y.y", `2016 Q1` = "bed.x.x.x.x.x.x.x.x", `2015 Q4` = "bed.y.y.y.y.y.y.y.y", `2015 Q3` = "bed.x.x.x.x.x.x.x.x.x",
                          `2015 Q2` = "bed.y.y.y.y.y.y.y.y.y", `2015 Q1` = "bed.x.x.x.x.x.x.x.x.x.x", `2014 Q4` = "bed.y.y.y.y.y.y.y.y.y.y", `2014 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x",
                          `2014 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y", `2014 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x", `2013 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y", `2013 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x",
                          `2013 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y",`2013 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x",`2012 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y",`2012 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
                          `2012 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2012 Q1` ="bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2011 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2011 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
                          `2011 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2011 Q1` = "bed")


# Data prep for python

# Renaming the Quater Column
#bed <- join_bed |> select(`Org Code`,
#          "2022-03-31" = "bed.x", "2021-12-31" = "bed.y", "2021-09-30" = "bed.x.x", "2021-06-30" = "bed.y.y", "2021-03-31" = "bed.x.x.x",
#          "2020-12-31" = "bed.y.y.y", "2020-09-30" = "bed.x.x.x.x", "2020-06-30" = "bed.y.y.y.y","2020-03-31" = "bed.x.x.x.x.x",
#          "2019-12-31" = "bed.y.y.y.y.y", "2019-09-30" = "bed.x.x.x.x.x.x", "2019-06-30" = "bed.y.y.y.y.y.y", "2019-03-31" = "bed.x.x.x.x.x.x.x",
#          "2018-12-31" = "bed.y.y.y.y.y.y.y", "2018-09-30" = "bed.x.x.x.x.x.x.x.x", "2018-06-30" = "bed.y.y.y.y.y.y.y.y", "2018-03-31" = "bed.x.x.x.x.x.x.x.x.x",
#          "2017-12-31" = "bed.y.y.y.y.y.y.y.y.y", "2017-09-30" = "bed.x.x.x.x.x.x.x.x.x.x", "2017-06-30" = "bed.y.y.y.y.y.y.y.y.y.y", "2017-03-31" = "bed.x.x.x.x.x.x.x.x.x.x.x",
#          "2016-12-31" = "bed.y.y.y.y.y.y.y.y.y.y.y", "2016-09-30" = "bed.x.x.x.x.x.x.x.x.x.x.x.x", "2016-06-30" = "bed.y.y.y.y.y.y.y.y.y.y.y.y", "2016-03-31" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          "2015-12-31" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y", "2015-09-30" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x","2015-06-30" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y","2015-03-31" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          "2014-12-31" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2014-09-30" ="bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", "2014-06-30" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2014-03-31" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          "2013-12-31" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2013-09-30" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x","2013-06-30" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2013-03-31" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          "2012-12-31" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2012-09-30" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", "2012-06-30" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2012-03-31" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
#          "2011-12-31" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", "2011-09-30" = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", "2011-06-30" = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"   
#) 

# We will duplicate this data for later when we want to run our model on the individual Trusts

multi_trust <- bed

# Getting the number of trust so when we want to fit a single model across we get the percentage and not just a 1000
# This doesn't work well as i have not replaced the missing values. So the percentage is around 62% which gives us false insight 

number_of_trusts <- length(unique(bed$`Org Code`))


# Converting all 0 values to NAs so as to be able to eliminate it
# This should be reviewed as we are loosing so many useful data
bed[bed==0] <- NA

##########################################################################################################################
# FITTING ONE SINGLE MODEL

# The next line of codes from (208 - 215) drops all NA values, Pivot longer, rename the Quater column and convert it to date type
# We summed all Trust for each quarter
bed_summary <- bed |> 
  drop_na()|>
  pivot_longer(cols = !c(`Org Code`))|>
  rename(quarter = name)|>
  mutate(quarter = yearquarter(quarter))|>
  group_by(quarter) |> 
  summarise(value = sum(value))

# Converting our data to a tssible object in order to run Time series analysis and models
bed_data_single_model <- tsibble(
  quarter = bed_summary$quarter,
  value = bed_summary$value,
  index = quarter
)


# Let us have a look at how our data looks over the years
autoplot(bed_data_single_model, value)

# Our data is ready, now let us fit our model
# Next we create a function that runs different ARIMA models and picks the best, 
# then forcast into the future. 

forecast_best_model <- function(.data){
  arima_models <- .data |>
  model(arima210 = ARIMA(value ~ pdq(2,1,0)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise=FALSE))
  
  best_model <- 
    arima_models |>
    glance() |>
    filter(AIC == min(AIC)) |>
    pull(.model)
  
  forecasted_data <-
    arima_models |>
    select(all_of(best_model))|>
    forecast(h=15)
    
  return(forecasted_data)
}

# Read about this later "all_of" as it stops the error
# Let's see if our function works
single_forecasted_data <- forecast_best_model(bed_data_single_model)

#Now let us have a look at our forcasted data in a plot
single_forecasted_data|> autoplot(bed_data_single_model)

# The function below forecast's and converts the output to a tsibble. 

#forecast_best_model_tsibble <- function(.data){
#  arima_models <- .data |>
#  model(arima210 = ARIMA(value ~ pdq(2,1,0)),
#        stepwise = ARIMA(value),
#        search = ARIMA(value, stepwise=FALSE))
#  
#  best_model <- 
#    arima_models |>
#    glance() |>
#    filter(AIC == min(AIC)) |>
#    pull(.model)
#  
#  forecasted_data <-
#    arima_models |>
#    select(all_of(best_model))|>
#    forecast(h=15)
#  
#  forecasted_data_tsibble <- tsibble(
#    quarter = forecasted_data$quarter,
#    value = forecasted_data$value,
#    index = quarter
#  )
    
#  return(forecasted_data_tsibble)
#}

# Let's see if our function works

#single_forecasted_tsibble <- forecast_best_model_tsibble(bed_data_single_model)
#single_forecasted_tsibble


#############################################################################################################
# Running Our Model on Multiple Trust
# Replace all NAN as NA so we can drop them
multi_trust[multi_trust==NaN] <- NA

bed_time <- multi_trust |> 
  drop_na()|>
  pivot_longer(cols = !c(`Org Code`)) |>
  rename(Quarter = name)|>
  mutate(Quarter = yearquarter(Quarter))


# Converting our data to a tssible object in order to run Time series analysis and models
multiple_trust <- tsibble(
  quarter = bed_time$Quarter,
  org_code = bed_time$`Org Code`,
  value = bed_time$value,
  key = org_code,
  index = quarter
)

# Nesting all the trust so we can run our models on all trust

multiple_trust_nested <- multiple_trust |>
  group_by(org_code)|>
  nest()

# Testing to see if a for loop will run on multiple trust
# i am only going to take a sample of 2 trusts
# I have put the break function for it to stop at 3
t <- 1
test_list_for_multiple_trusts <- list()

for (tr in multiple_trust_nested$data) {
 test_list_for_multiple_trusts[[t]] <- 
  forecast_best_model(tr)    
  t= t+1
  if (t == 3) {
    break
  }
} 

# Creating an emptylist to hold all our plots
# First let us plot one 
plot_test <- test_list_for_multiple_trusts[[1]] |> autoplot(multiple_trust_nested$data[[1]])
plot_test

# The code below doesn't work. Try it later
#plott <- 1
#plot_list <- list()

#for (h in test_list_for_multiple_trusts, trust in multiple_trust_nested$data) {
#        plot_list[[plott]] <- h |> autoplot(trust)
#        plott = plott + 1
#    if (plott ==2) {
#      break
#    }
#}

# Now to map, let see if the function can work on  multiple trust using map from purrr package

mulit_model_mapped <- map(.x = multiple_trust_nested$data, ~ forecast_best_model(.x))

# It works.
# I need to see how I can determine the length of my output just like my for loop

# Let us forecast and store it in a nest in our multi-trust_nested tsibble

# Not Working

#map_tsibble <- multiple_trust_nested |>
#  group_by(org_code) |>
#  nest()|>
#  mutate(model = map(.x = data, ~ forecast_best_model_tsibble(.x)))

#multiple_trust_nested

# Using tidy() to save our output as a dataframe/tssible
#multiple_trust_nested |>
#  group_by(org_code) |>
#  nest()|>
#  mutate(model = map(.x = data, ~ forecast_best_model|>
#  tidy()))

#multiple_trust_nested |>
#  group_by(org_code) |>
#  nest()|>
#  mutate(model = map(.x = data, ~ forecast_best_model)|>
#  tidy())


#####################################################################################

#     Things I should work on

# 1. [Line 24]  Find a better way to create a for loop
#               that will clean and store the bed data with different file format  

# 2. [Line 195] Find a better way to extra the quarter names and rename it automaticly
#               it will save time when you decided to run your model on partcular quarters 
#               different from what we used above

# 3. [Line 237] I need to handle the missing data as it i loose valuable data that will better 
#               train my model