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


###Bed Occupancy
#Load Bed occupancy url for dfferent quaters

url_x <- c(
q4_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q4-2021-22-Final-OIUJK.xlsx",
q3_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q3-2021-22-Final.xlsx",
q2_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q2-2021-22-Final.xlsx",
q1_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q1-2021-22-Final.xlsx",
q4_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q4-2020-21-Final-1.xlsx",
q3_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q3-2020-21-Final-1.xlsx",
q2_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q2-2020-21-Final-1.xlsx",
q1_20_21 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/11/Beds-Open-Overnight-Web_File-Q1-2020-21-Final-1.xlsx",
q4_19_20 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/1920-Q4-Beds-Open-Overnight-Web_File-Final-DE5WC.xlsx",
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
url_s <- c(
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

for(qu in url_x) { 
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

for(qu in url_s) { 
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

kay <- file_ |> reduce(left_join, by = "Org Code")

bed <- kay |> select(`Org Code`,
          `2022 Q1`= "bed.x", `2021 Q4` = "bed.y", `2021 Q3` = "bed.x.x", `2021 Q2` = "bed.y.y", `2021 Q1` = "bed.x.x.x",
          `2020 Q4` = "bed.y.y.y", `2020 Q3` = "bed.x.x.x.x", `2020 Q2` = "bed.y.y.y.y",`2020 Q1` = "bed.x.x.x.x.x",
          `2019 Q4` = "bed.y.y.y.y.y", `2019 Q3` = "bed.x.x.x.x.x.x", `2019 Q2` = "bed.y.y.y.y.y.y", `2019 Q1` = "bed.x.x.x.x.x.x.x",
          `2018 Q4` = "bed.y.y.y.y.y.y.y", `2018 Q3` = "bed.x.x.x.x.x.x.x.x", `2018 Q2` = "bed.y.y.y.y.y.y.y.y", `2018 Q1` = "bed.x.x.x.x.x.x.x.x.x",
          `2017 Q4` = "bed.y.y.y.y.y.y.y.y.y", `2017 Q3` = "bed.x.x.x.x.x.x.x.x.x.x", `2017 Q2` = "bed.y.y.y.y.y.y.y.y.y.y", `2017 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x",
          `2016 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y", `2016 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x", `2016 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y", `2016 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x",
          `2015 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y",`2015 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x",`2015 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y",`2015 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
          `2014 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2014 Q3` ="bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2014 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2014 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
          `2013 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2013 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",`2013 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2013 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
          `2012 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2012 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2012 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2012 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
          `2011 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2011 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2011 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y"   
) 

# Data prep for python

#bed <- kay |> select(`Org Code`,
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

bed[bed==0] <- NA


bed_time <- bed |> 
        drop_na()|>
        pivot_longer(cols = !c(`Org Code`)) |>
        rename(Quarter = name)|>
        mutate(Quarter = yearquarter(Quarter))


bed_create_ts <- tsibble(
  quarter = bed_time$Quarter,
  org_code = bed_time$`Org Code`,
  value = bed_time$value,
  key = org_code,
  index = quarter
)

autoplot(bed_create_ts, value)

# THE ABOVE CODE WORKED
#PUSH



beds <- bed |> 
        drop_na()|>
        pivot_longer(cols = !c(`Org Code`)) |>
        pivot_wider(names_from = `Org Code`) |>
        rename(Quarter = name)

view(beds)

bed_ |> filter(`Org Code` == "RAL") |>
ggplot(aes(x= Quarter, y=value, color = `Org Code`, group=1)) +
  geom_line() + theme(axis.text.x = element_text(angle = 90, hjust = 0))

library(tsibble)
bed_ts <- beds |>
            mutate(Quarter = yearquarter(Quarter)) |>
            as_tsibble(key = !c(Quarter),
             index = Quarter)

bed_ts |> dplyr::select(c('Quarter', 'RAL'))
bed_ts |>filter(c('Quarter', 'RAL'))
  

install.packages("ggfortify")
library(ggfortify)

bed_t <- bed_ |>
            mutate(Quarter = yearquarter(Quarter)) |>
            as_tsibble(key = c(`Org Code`),
             index = Quarter) |>


be <- tsibble(bed_t)

feasts::autoplot(bed_t, value) +
      labs(y = ")",
       title = "Australian antidiabetic drug sales")
class(bed_t)

write.csv(bed,"C:\\Users\\de\\Documents\\HDR TP\\beds.csv", row.names = TRUE)

ced <- read_csv('beds.csv') |> select(-c('...1'))
cec <- read_csv('bed.csv') |> select(-c('...1'))

ced[ced==0]<- NA


ced_ <- ced |> 
        drop_na()|>
        pivot_longer(cols = !c(`Org Code`)) |>
        rename(Quarter = name)


ced_ts <- ced_ |>
            mutate(Quarter = yearquarter(Quarter)) |>
            as_tsibble(key = !c(Quarter),
             index = Quarter) |>
             as_tsibble()

ced_t2 <- ced_ts |>
  filter(`Org Code` == "R1C") |>
  select(-c(`Org Code`)) |>
  as_tsibble()



feasts::autoplot(ced_t2, value)

autoplot(ced_t2, value)


cedd <- ced_ tsibble(ced_, index = Quarter)
