library(tidyverse)
library(httr)
library(readxl)
library(corrr)
library(janitor)
library(dplyr)
library(skimr)


library(forecast)
library(tsibble)
library(feasts)
library(fable)


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

q4_21_22 <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2022/05/Beds-Open-Overnight-Web_File-Q4-2021-22-Final-OIUJK.xlsx",


GET(
    q4_21_22,
    write_disk(bed_night <- tempfile(fileext = ".xlsx"))  ) 



bd <- read_excel(
    bed_night,
    sheet = "NHS Trust by Sector",
    skip = 14) |>
    slice(-(1:2))


beds_night_sliced <-
  bed_night |>
  slice(-(1:2))

  

view(bd)

bdss <- bd |> 
      mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
      
nn <- bdss |>      
    select(
    `Org Code`, `Org Name`,
    `bed` = `General & Acute...13`,
    )
      
b      
      
#str_extract(bed_night_1, 'Q[1-4]')

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


#file_ <- list()

#i=1

#for(qu in url_x) { 
#GET(
#    qu,
#    write_disk(bed_night <- tempfile(fileext = ".xlsx"))
#  ) 
#  file_[[i]] <-
#   read_excel(
#    bed_night,
#    sheet = "NHS Trust by Sector",
#    skip = 14) |>
#    slice(-(1:2)) |>
#    mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
#    select(
#    `Org Code`,
#    `bed` = `bed_perc`,
#    
#    )
   #mutate(quater=str_extract(qu, 'Q[1-4]'))
#  i=i+1
#}

#for(qu in url_s) { 
#GET(
#    qu,
#    write_disk(bed_night <- tempfile(fileext = ".xls"))
#  ) 
#  file_[[i]] <-
#   read_excel(
#    bed_night,
#    sheet = "NHS Trust by Sector",
#    skip = 14) |>
#    mutate(`bed_perc` = (`General & Acute...13`/`General & Acute...7`) * 100 ) |>
#    select(
#    `Org Code`,
#    `bed` = `bed_perc`,
    
#    )
   #mutate(quater=str_extract(qu, 'Q[1-4]'))
#  i=i+1
#}

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


bed[bed==0]<- NA
bed|> drop_na()

beds <- bed |> 
        drop_na()|>
        pivot_longer(cols = !c(`Org Code`)) |>
        pivot_wider(names_from = `Org Code`) |>
        rename(Quarter = name) |>
        mutate(Quarter = yearquarter(Quarter))


beds_ <- bed |> 
        drop_na()|>
        pivot_longer(cols = !c(`Org Code`)) |>
        rename(Quarter = name)




view(beds)


bed_ts <- beds |>
            mutate(Quarter = yearquarter(Quarter)) |>
            as_tsibble(key = !c(Quarter),
             index = Quarter)

bed_t <- beds_ |>
            mutate(Quarter = yearquarter(Quarter)) |>
            as_tsibble(key = c(`Org Code`),
             index = Quarter)

bed_t |> autoplot(bed_t)
class(beds$Quarter)
scan_gaps(bed_ts)

fit <- bed_ts |>
    fill_gaps() |>
    model(
    snaive = SNAIVE(RAN ~ lag("Quarter")),
    ets = ETS(RAN),
    arima = ARIMA(RAN))




class(bed_ts)

library(feasts)

autoplot(bed_ts)

bed_ts |> autoplot(RAL)

plot(bed_ts)


kay_ <-  rename(kay, starts_with("bed"))
j=2

setNames(kay, old = !c(`Org Code`),
        new = c("q4_21_22", "q3_21_22", "q2_21_22", "q1_21_22") )


names(kay)[j] <- ("q4_21_22")


quaters <- list("q4_21_22", "q3_21_22", "q2_21_22", "q1_21_22")

 kay1 <- kay |> select(
    `Org Code`,
    for(x in quaters) { print(x)} = `4_general_acute_night_beds_occupied.x`)

file_ <- file_[-1]

df <- file_[['4']]
dp <- file_[[3]]


bed_occupancy_night <-
  beds_night_vars |>
  left_join(
    beds_night_vars_3,
    by = "Org Code") |>
  left_join(
    beds_night_vars_2,
    by = "Org Code") |>
  left_join(
    beds_night_vars_1,
    by = "Org Code")
view(raw_)


for(qu in url) {
  GET(
    qu,
    write_disk(bed_night <- tempfile(fileext = ".xlsx")|>
                 read_excel(
                   bed_night,
                   sheet = "NHS Trust by Sector",
                   skip = 14) |>
                 slice(-(1:2)) |>
                 select(
                   `Org Code`,
                   `4_total_beds_available_night`= `Total...6`,
                   `4_general_acute_night_beds_occupied` = `General & Acute...13`,
                   `4_occupied_night_total` = `Total...12`)))

}
# Loading the data into our enviroment and selecting the neccessary rows
clean <- function(x) {

  x |>
    slice(-(1:2)) |>
    select(
    `Org Code`,
    `4_total_beds_available_night`= `Total...6`,
    `4_general_acute_night_beds_occupied` = `General & Acute...13`,
    `4_occupied_night_total` = `Total...12`
  )
  }

clean(raw_night_3)

view(raw_night_3)

GET(
    bed_night_3,
    write_disk(bed_night <- tempfile(fileext = ".xlsx"))
  )
  

# Loading the data into our enviroment and selecting the neccessary rows
raw_night_2 <-
  read_excel(
    bed_night,
    sheet = "NHS Trust by Sector",
    skip = 14)|>
    slice(-(1:2)) |>
    select(
    `Org Code`,
    `4_total_beds_available_night`= `Total...6`,
    `4_general_acute_night_beds_occupied` = `General & Acute...13`,
    `4_occupied_night_total` = `Total...12`
  )
view(raw_night_2)
# remove first two entries (one is totals, other is blank)
beds_night_sliced <-
  raw_night |>
  

# Select cols
beds_night_vars <-
  beds_night_sliced |>
  
}

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
    `4_total_beds_available_night`= `Total...6`,
    `4_general_acute_night_beds_occupied` = `General & Acute...13`,
    `4_occupied_night_total` = `Total...12`
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
    `3_general_acute_night_beds_occupied` = `General & Acute...13`,
    `3_occupied_night_total` = `Total...12`
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
    `2_general_acute_night_beds_occupied` = `General & Acute...13`,
    `2_occupied_night_total` = `Total...12`
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
    `1_general_acute_night_beds_occupied` = `General & Acute...13`,
    `1_occupied_night_total` = `Total...12`
  )

#Merging day and night bed together for all 4 quaters
bed_occupancy_night <-
  beds_night_vars |>
  left_join(
    beds_night_vars_3,
    by = "Org Code") |>
  left_join(
    beds_night_vars_2,
    by = "Org Code") |>
  left_join(
    beds_night_vars_1,
    by = "Org Code")

view(bed_occupancy_night)


bed_occupancy_select <- bed_occupancy_night |>
        select(`Org Code`, 
        `2021 Q4` = `4_general_acute_night_beds_occupied`,
        `2021 Q3` = `3_general_acute_night_beds_occupied`,
        `2021 Q2` = `2_general_acute_night_beds_occupied`,
        `2021 Q1` = `1_general_acute_night_beds_occupied`)

install.packages("tsibble")
library(tsibble)
bed_pivot <- bed_occupancy_select |> 
            pivot_longer(cols = !c(`Org Code`)) |>
            mutate(Quarter = yearquarter(name)) |>
             select(-name) |>
            as_tsibble(key = c(`Org Code`),
             index = Quarter)


write.csv(beds,"C:\\Users\\de\\Documents\\HDR TP\\bedds.csv", row.names = TRUE)







########

view(bed_pivot)

bed_pivot <- bed_occupancy_night |> 
            pivot_longer(cols = !c(`Org Code`)) |>
            group_by(`Org Code`) 
