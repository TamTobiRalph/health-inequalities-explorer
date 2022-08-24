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
#Load Bed occupancy url for dfferent quaters

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

file_ <- list()

i=1

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
    select(
    `Org Code`,
    `bed` = `General & Acute...13`,
    
    )
   #mutate(quater=str_extract(qu, 'Q[1-4]'))
  i=i+1
}

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
    select(
    `Org Code`,
    `bed` = `General & Acute...13`,
    
    )
#mutate(quater=str_extract(qu, 'Q[1-4]'))
  i=i+1
}


#file_ <- list()

#i=1

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
#   slice(-(1:2)) |>
#    mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
#    select(
#      `Org Code`,
#      `bed` = `bed_perc`,
#      
#    )
  #mutate(quater=str_extract(qu, 'Q[1-4]'))
#  i=i+1
#}

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
#    mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
#    select(
#      `Org Code`,
#      `bed` = `bed_perc`,
#      
#    )
#  #mutate(quater=str_extract(qu, 'Q[1-4]'))
#  i=i+1
#}

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

# We will duplicate this data for later when we want to run our model on the individual Trust

multi_trust <- bed


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
bed_sm <- tsibble(
  quarter = bed_summary$quarter,
  value = bed_summary$value,
  index = quarter
)

# Let us have a look at how our data looks over the years
autoplot(bed_sm, value)

# Let us check to see if we need differencing for our data to see if it is stationary
# using unit root test 
# We will be using Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test  

bed_sm |> 
  features(value, unitroot_kpss)

# Small p values such as (e.g., less than 0.05) indicates we need differencing
# Our p value is 0.01

# Another way is using KPSS tests to see how many differencing we need

bed_sm |> 
  features(value, unitroot_ndiffs)

# This shows we need to do a first order differencing and check if it is okay

bed_sm <- bed_sm |>
  mutate(diff_value = difference(value))

bed_sm |> 
  features(diff_value, unitroot_kpss)

bed_sm |>
  features(diff_value, unitroot_ndiffs)

# I also think i should do a seasonal differencing and see if that will be ok

bed_sm <- bed_sm |>
  mutate(log_value = difference(log(value), 12))

bed_sm |> 
  features(log_value, unitroot_kpss)


bed_sm |>
  features(log_value, unitroot_ndiffs)

# Our data is ready, now let us fit our model

fit <- bed_sm |>
  model(arima210 = ARIMA(value ~ pdq(2,1,0)),
        arima013 = ARIMA(value ~ pdq(0,1,3)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise=FALSE))

# Let us find out which model has the lowest AIC value and use that to forcast.
glance(fit) |> arrange(AICc) |> select(.model:BIC)

# Now let us forecast
forecast(fit, h=15) |>
  filter(.model=="arima210") |>
  autoplot(bed_sm) +
  labs(title = "One Single Model Using ARIMA ",
       y="Percentage")

# Seasonal Arima
seasonal_ <- bed_sm|>
  model(
    arima012011 = ARIMA(value ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(value ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(value, stepwise = FALSE, approx = FALSE)
  )

glance(seasonal_) |> arrange(AICc) |> select(.model:BIC)

forecast(seasonal_, h=35) |>
  filter(.model=="arima012011") |>
  autoplot(bed_sm) +
  labs(title = "One Single Model Using SARIMA ",
       y="Percentage OK ")


# Differencing

dif_fit <- bed_sm |>
  model(arima210 = ARIMA(diff_value ~ pdq(2,1,0)),
        arima013 = ARIMA(diff_value ~ pdq(0,1,3)),
        stepwise = ARIMA(diff_value),
        search = ARIMA(diff_value, stepwise=FALSE))

# Let us find out which model has the lowest AIC value and use that to forcast.
glance(dif_fit) |> arrange(AICc) |> select(.model:BIC)

# Now let us forecast
forecast(dif_fit, h=15) |>
  filter(.model=="search") |>
  autoplot(bed_sm) +
  labs(title = "Differencing Plot Using ARIMA",
       y="Percentage")

# Now lets see what the seasonal Arima does

seasonal_a <- bed_sm|>
  model(
    arima012011 = ARIMA(log_value ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(log_value ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(log_value, stepwise = FALSE, approx = FALSE)
  )

glance(seasonal_a) |> arrange(AICc) |> select(.model:BIC)

forecast(seasonal_a, h=15) |>
  filter(.model=="arima012011") |>
  autoplot(bed_sm) +
  labs(title = "Differencing Plot Using SARIMA",
       y="Percentage")


forecast(seasonal_a, h=35) |>
  filter(.model=="auto") |>
  autoplot(bed_sm) +
  labs(title = "Differencing Plot Using SARIMA",
       y="Percentage ")

#############################################################################################################
# Running Our Model on Multiple Trust

multi_trust[multi_trust==NaN] <- NA
bed_time <- multi_trust |> 
  drop_na()|>
  pivot_longer(cols = !c(`Org Code`)) |>
  rename(Quarter = name)|>
  mutate(Quarter = yearquarter(Quarter))


# Converting our data to a tssible object in order to run Time series analysis and models
multi <- tsibble(
  quarter = bed_time$Quarter,
  org_code = bed_time$`Org Code`,
  value = bed_time$value,
  key = org_code,
  index = quarter
)

malt <- multi |>
  group_by(org_code)|>
  nest()

a=1

kist <- list()

for (lo in malt$data) {
  
  kist[[a]] <- lo |>
  model(arima210 = ARIMA(value ~ pdq(2,1,0)),
        arima013 = ARIMA(value ~ pdq(0,1,3)),
        stepwise = ARIMA(value),
        search = ARIMA(value, stepwise=FALSE))
  a= a+1

}

jist <- list()
m=1
for (wu in kist) {
  jist[[m]] <- glance(wu) |> arrange(AICc) |> select(.model:BIC)

  m= m+1
}

#forecast(kist[[1]], h=15) |>
#  filter(.model=="arima013") |>
#  autoplot(bed_sm) +
#  labs(title = "TRUSTS",
#       y="Percentage")

