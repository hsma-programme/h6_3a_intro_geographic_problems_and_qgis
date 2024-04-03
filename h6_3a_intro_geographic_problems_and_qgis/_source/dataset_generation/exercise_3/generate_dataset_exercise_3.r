install.packages("stats19")

library(stats19)
library(dplyr)

crashes = get_stats19(year = 2022, type = "collision")

counts_breakdown <- crashes %>%
    count(police_force, accident_severity, day_of_week, date, lsoa_of_accident_location, urban_or_rural_area)

counts_breakdown %>%
    readr::write_csv("/datasets_export/stats19_collision_dataset.csv")
