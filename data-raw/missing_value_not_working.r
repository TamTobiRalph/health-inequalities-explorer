bed_na <- bed_join |> select(`Org Code`,
                                `2019 Q3`= "bed.x", `2019 Q2` = "bed.y", `2019 Q1` = "bed.x.x", `2018 Q4` = "bed.y.y", `2018 Q3` = "bed.x.x.x",
                                `2018 Q2` = "bed.y.y.y", `2018 Q1` = "bed.x.x.x.x", `2017 Q4` = "bed.y.y.y.y",`2017 Q3` = "bed.x.x.x.x.x",
                                `2017 Q2` = "bed.y.y.y.y.y", `2017 Q1` = "bed.x.x.x.x.x.x", `2016 Q4` = "bed.y.y.y.y.y.y", `2016 Q3` = "bed.x.x.x.x.x.x.x",
                                `2016 Q2` = "bed.y.y.y.y.y.y.y", `2016 Q1` = "bed.x.x.x.x.x.x.x.x", `2015 Q4` = "bed.y.y.y.y.y.y.y.y", `2015 Q3` = "bed.x.x.x.x.x.x.x.x.x",
                                `2015 Q2` = "bed.y.y.y.y.y.y.y.y.y", `2015 Q1` = "bed.x.x.x.x.x.x.x.x.x.x", `2014 Q4` = "bed.y.y.y.y.y.y.y.y.y.y", `2014 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x",
                                `2014 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y", `2014 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x", `2013 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y", `2013 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x",
                                `2013 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y",`2013 Q1` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x",`2012 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y",`2012 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
                                `2012 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2012 Q1` ="bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x", `2011 Q4` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2011 Q3` = "bed.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x.x",
                                `2011 Q2` = "bed.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y.y", `2011 Q1` = "bed")

 xy <- bed_na

 xy[xy==NaN] <- NA
 
# Join it without removing the missing values
install.packages("naniar")
library(naniar)
  
eg <- nhanes


# Are there missing values in the dataset?
any_na(bed_na)
# How many?
n_miss(bed_na)
# What is the percentage of missing data
prop_miss(bed_na)
# Which variables are affected?
bed_na %>% is.na() %>% colSums()


# Get number of missings per variable (n and %)
miss_var_summary(bed_na)

miss_var_table(bed_na)
# Get number of missings per participant (n and %)
miss_case_summary(bed_na)
miss_case_table(bed_na)

# Which variables contain the most missing variables?
gg_miss_var(bed_na)

sum(is.na(bed_na))
mean(is.na(bed_na))
colsum(is.na(bed_na))

sum(is.na(xy))
mean(is.na(xy))
colsum(is.na(xy))

install.packages("mice")
library(mice)

bed_nas <- bed_na |>
  select(-`Org Code`)
  
imputed_Data <- mice(eg, m=5, maxit = 50, method = 'pmm', seed = 500)



summary(imputed_Data)

#mice
mice_bed_na <- mice(iris, m=10,meth='pmm', seed = 5)

mice_bed_na <- mice(bed_m, m=10,meth='pmm', seed = 5)
iris

md.pattern(bed_m) 
md.pattern(xy) 


bed_m |> group_by(`Org Code`)|> mean(value, na.rm =TRUE)

bed_m <- bed_na |> 
  pivot_longer(cols = !c(`Org Code`))|>
  rename(quarter = name)

ef <- bed_m
ef[ef==NaN] <- NA


|>
  mutate(quarter = yearquarter(quarter))

#missForest
install.packages("missForest")
library(missForest)

ef.imp <- missForest(ef$value)
