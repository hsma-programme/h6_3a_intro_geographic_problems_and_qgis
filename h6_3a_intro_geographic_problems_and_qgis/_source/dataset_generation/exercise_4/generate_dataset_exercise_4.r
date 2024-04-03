install.packages("stats19")
install.packages("dplyr")
install.packages("purrr")
install.packages("tidyr")
install.packages("stringr")
install.packages("lubridate")
install.packages("readr")

library(stats19)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)


# Download lookup from https://geoportal.statistics.gov.uk/datasets/b6ea48b4d3bc40e5b33c1580e686d254/about
# Not included in repo as large size (>400mb)

lsoa_lookup <- readr::read_csv("/home/sammi/HSMA/h6_3a_intro_geographic_problems_and_qgis/h6_3a_intro_geographic_problems_and_qgis/_source/dataset_generation/helpers/PCD_OA_LSOA_MSOA_LAD_FEB19_UK_LU.csv") %>%
    distinct(msoa11cd, lsoa11cd)

all_collisions_2018_2022 <- purrr::map_dfr(
    .x = seq(2018, 2022, 1),
    .f = ~ get_stats19(year=.x, type = "collision")
    )

counts_collisions_2018_2022_by_msoa <- all_collisions_2018_2022 %>%
    distinct(accident_index, accident_year, accident_reference, lsoa_of_accident_location) %>%
    filter(lsoa_of_accident_location != -1) %>%
    dplyr::full_join(lsoa_lookup, by=c("lsoa_of_accident_location" = "lsoa11cd")) %>%
    count(msoa11cd, sort=TRUE)

cas_2018 <- get_stats19(year=2018, type = "casualty") %>% dplyr::mutate(lsoa_of_casualty = as.character(lsoa_of_casualty))
cas_2019 <- get_stats19(year=2019, type = "casualty")  %>% dplyr::mutate(lsoa_of_casualty = as.character(lsoa_of_casualty))
cas_2020 <- get_stats19(year=2020, type = "casualty")  %>% dplyr::mutate(lsoa_of_casualty = as.character(lsoa_of_casualty))
cas_2021 <- get_stats19(year=2021, type = "casualty")  %>% dplyr::mutate(lsoa_of_casualty = as.character(lsoa_of_casualty))
cas_2022 <- get_stats19(year=2022, type = "casualty")  %>% dplyr::mutate(lsoa_of_casualty = as.character(lsoa_of_casualty))

all_casualties_2018_2022 <- bind_rows(cas_2018, cas_2019, cas_2020, cas_2021, cas_2022)

counts_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    count(accident_reference, lsoa_of_casualty, accident_year) %>%
    group_by(lsoa_of_casualty) %>%
    summarize(n = sum(n, na.rm=TRUE)) %>%
    dplyr::full_join(lsoa_lookup, by=c("lsoa_of_casualty" = "lsoa11cd")) %>%
    filter(lsoa_of_casualty != -1) %>%
    group_by(msoa11cd) %>%
    summarize(n = sum(n, na.rm=TRUE))

get_counts_collision_df <- function(filtered_df) {
    filtered_df %>%
        distinct(accident_index, accident_year, accident_reference, lsoa_of_accident_location) %>%
        filter(lsoa_of_accident_location != -1) %>%
        dplyr::full_join(lsoa_lookup, by=c("lsoa_of_accident_location" = "lsoa11cd")) %>%
        count(msoa11cd, sort=TRUE)
}

get_counts_casualty_df <- function(filtered_df) {
    filtered_df %>%
        count(accident_reference, lsoa_of_casualty, accident_year) %>%
        group_by(lsoa_of_casualty) %>%
        summarize(n = sum(n, na.rm=TRUE)) %>%
        dplyr::full_join(lsoa_lookup, by=c("lsoa_of_casualty" = "lsoa11cd")) %>%
        filter(lsoa_of_casualty != -1) %>%
        group_by(msoa11cd) %>%
        summarize(n = sum(n, na.rm=TRUE))
}

counts_fatal_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    filter(casualty_severity=="Fatal") %>%
    get_counts_casualty_df() %>%
    rename("fatal_casualties_2018_2022" = "n")

counts_cyclist_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    filter(casualty_type=="Cyclist") %>%
    get_counts_casualty_df() %>%
    rename("cyclist_casualties_2018_2022" = "n")

counts_pedestrian_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    filter(casualty_type=="Pedestrian") %>%
    get_counts_casualty_df() %>%
    rename("pedestrian_casualties_2018_2022" = "n")

counts_car_occupant_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    filter(casualty_type=="Car occupant") %>%
    get_counts_casualty_df() %>%
    rename("car_occupant_casualties_2018_2022" = "n")

counts_motorcycle_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    filter(stringr::str_detect(casualty_type, "Motorcycle")) %>%
    get_counts_casualty_df() %>%
    rename("car_occupant_casualties_2018_2022" = "n")

counts_horse_rider_casualties_2018_2022_by_msoa <- all_casualties_2018_2022 %>%
    filter(casualty_type=="Horse rider") %>%
    get_counts_casualty_df() %>%
    rename("horse_rider_casualties_2018_2022" = "n")

counts_snow <- all_collisions_2018_2022 %>%
    filter(stringr::str_detect(weather_conditions, "Snow")) %>%
    get_counts_collision_df() %>%
    rename("snow_collisions_2018_2022" = "n")

counts_fog <- all_collisions_2018_2022 %>%
    filter(stringr::str_detect(weather_conditions, "Fog")) %>%
    get_counts_collision_df() %>%
    rename("fog_or_mist_collisions_2018_2022" = "n")

counts_darkness <- all_collisions_2018_2022 %>%
    filter(stringr::str_detect(light_conditions, "Darkness")) %>%
    get_counts_collision_df() %>%
    rename("darkness_collisions_2018_2022" = "n")

counts_daylight <- all_collisions_2018_2022 %>%
    filter(stringr::str_detect(light_conditions, "Daylight")) %>%
    get_counts_collision_df() %>%
    rename("daylight_collisions_2018_2022" = "n")


counts_summer <- all_collisions_2018_2022 %>%
    mutate(month = lubridate::month(date)) %>%
    filter(month==6 | month==7 | month==8) %>%
    get_counts_collision_df() %>%
    rename("summer_collisions_2018_2022" = "n")

counts_winter <- all_collisions_2018_2022 %>%
    mutate(month = lubridate::month(date)) %>%
    filter(month==12 | month==1 | month==2) %>%
    get_counts_collision_df() %>%
    rename("summer_collisions_2018_2022" = "n")


all_counts <- counts_collisions_2018_2022_by_msoa %>%
    rename("total_number_of_collisions_2018_2022" = "n") %>%
    left_join(counts_casualties_2018_2022_by_msoa %>% rename("total_number_of_casualties_2018_2022" = "n"), by="msoa11cd") %>%
    mutate(across(where(is.numeric), \(x) tidyr::replace_na(x, 0))) %>%
    left_join(counts_fatal_casualties_2018_2022_by_msoa) %>%
    left_join(counts_cyclist_casualties_2018_2022_by_msoa) %>%
    left_join(counts_pedestrian_casualties_2018_2022_by_msoa) %>%
    left_join(counts_car_occupant_casualties_2018_2022_by_msoa) %>%
    left_join(counts_motorcycle_casualties_2018_2022_by_msoa) %>%
    left_join(counts_horse_rider_casualties_2018_2022_by_msoa) %>%
    left_join(counts_snow) %>%
    left_join(counts_fog) %>%
    left_join(counts_darkness) %>%
    left_join(counts_daylight) %>%
    left_join(counts_summer) %>%
    left_join(counts_winter)


all_counts %>% readr::write_csv("stats19_counts_by_msoa_2018_2022.csv")
