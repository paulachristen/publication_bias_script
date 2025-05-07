library(tidyverse)

# Read and clean data ----------------------------------------------------------
# Read in data - download from Hackathon OneDrive
# Assumes wd is set to where this script is
dat <- read.csv(file.path("/Users/paulachristen/Documents/publication bias manuscript/shared", "clean_data_final.csv"),
                stringsAsFactors = FALSE, encoding = "UTF-8") |>
  mutate(date = ymd(date),
         time = case_when(year %in% c(2014:2015) ~ "2014-2015",
                          year %in% c(2016:2017) ~ "2016-2017",
                          year %in% c(2018:2019) ~ "2018-2019",
                          year %in% c(2020:2021) ~ "2020-2021",
                          year %in% c(2022:2023) ~ "2022-2023"),
         pandemic = case_when(date >= "2020-03-01" & date <= "2022-02-28" ~ "pandemic",
                              # use year when date variable is empty
                              is.na(date) & year %in% c(2020:2021) ~ "pandemic",
                              TRUE ~ "not pandemic")) |>
  ungroup()

# creating factors
dat <- dat %>% mutate(
  race = factor(race, levels = c("asian", "black", "hispanic", "white")),
  pandemic = factor(pandemic, levels = c("not pandemic", "pandemic")),
  time = factor(time, levels = c("2014-2015", "2016-2017", "2018-2019",
                                 "2020-2021", "2022-2023")),
  job_cat_old = factor(job_cat_old, levels = c("Student",
                                               "Research Assistant",
                                               "Research Associate",
                                               "Research Fellow",
                                               "Lecturer / Senior Lecturer",
                                               "Reader / Professor",
                                               "Other")),
  job_cat_simplified = factor(job_cat_simplified,
                              levels = c("Student",
                                         "Research Assistant / Associate",
                                         "Research Fellow",
                                         "Lecturer",
                                         "Senior Lecturer / Reader",
                                         "Professor",
                                         "Other")),
  job_cat_simplified2 = factor(job_cat_simplified2,
                               levels = c("Student / Research Assistant / Associate / Fellow", "Lecturer / Reader",
                                          "Professor",
                                          "Other")),
  job_cat_simplified3 = fct_collapse(job_cat, "Student / Research Assistant" = c("Student", "Research Assistant"),
                                     "Senior Lecturer / Reader" = c("Senior Lecturer", "Reader"),
                                     "Other" = c("Teaching Fellow", "Other")),
  job_cat_simplified3 = factor(job_cat_simplified3,
                               levels = c("Student / Research Assistant",
                                          "Research Associate",
                                          "Research Fellow",
                                          "Lecturer",
                                          "Senior Lecturer / Reader",
                                          "Professor",
                                          "Other")),
  job_cat_broad = fct_collapse(job_cat_simplified,
                               "Nontenure" = c("Student","Research Assistant / Associate","Research Fellow"),
                               "Tenure" = c("Lecturer","Senior Lecturer / Reader","Professor"),
                               "Other" = "Other"))
