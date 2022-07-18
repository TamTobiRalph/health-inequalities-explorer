library(tidyverse)
library(geographr)
library(sf)

hospitals_england <- points_hospitals22

usethis::use_data(hospitals_england, overwrite = TRUE)