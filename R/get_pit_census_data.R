library(tidyverse)
library(tidycensus)
library(readxl)

get_pit <- function(pit_year){
  print(paste("getting", pit_year, "data"))
  read_excel("data/2007-2024-PIT-Counts-by-State.xlsx", sheet = pit_year) |> 
    select(State, starts_with("Sheltered Total Homeless -"), starts_with("Unsheltered Homeless -")) |>
    select(-contains("Age 18"), -contains("Under 18"), -contains("24"), -contains("Hispanic")) |>
    rename_with(.fn = ~paste0(., "_", pit_year), .cols = -State)
}

pit <- map(c("2018", "2019", "2020", "2021", "2022", "2023"), get_pit) |>
  reduce(left_join, by = "State")

pit_colnames <- data.frame(pit_names = colnames(pit))

get_race <- function(cen_year){
  get_acs(geography = "state", 
          variables = c("C02003_003", 
                        "C02003_004", 
                        "C02003_005", 
                        "C02003_006", 
                        "C02003_007", 
                        "C02003_009"),
          year = cen_year) |>
    rename_with(.fn = ~paste0(., "_", cen_year), .cols = estimate:moe)
}

get_gender <- function(cen_year){
  get_acs(geography = "state", 
          variables = c("B01001_026", 
                        "B01001_002"),
          year = cen_year) |>
    rename_with(.fn = ~paste0(., "_", cen_year), .cols = estimate:moe)
}

race <- map(c(2018:2023), get_race) |> 
  reduce(left_join, by = c("GEOID", "NAME", "variable"))
gender <- map(c(2018:2023), get_gender) |> 
  reduce(left_join, by = c("GEOID", "NAME", "variable"))

race$variable <-
  recode(race$variable,
         C02003_003 = "White",
         C02003_004 = "Black or African American",
         C02003_005 = "American Indian or Alaska Native",
         C02003_006 = "Asian",
         C02003_007 = "Native Hawaiian or Other Pacific Islander",
         C02003_009 = "Multiple Races")

gender$variable <-
  recode(gender$variable,
         B01001_026 = "Female",
         B01001_002 = "Male")

write_csv(pit, "data/pit_2018_2023.csv")
write_csv(gender, "data/gender_state_2018_2023.csv")
write_csv(race, "data/race_state_2018_2023.csv")