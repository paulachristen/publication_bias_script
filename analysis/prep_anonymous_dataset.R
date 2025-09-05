
library(dplyr)
################################################################################
######################## Publication Data ######################################
# Get Publication data and anonymize --------------------------------------
dat <- read.csv("data_cleaning/manuscript_data/clean_data_final.csv",
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
  job_cat_simplified4 = factor(job_cat_simplified3,
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

dat_select <- dat |>
  dplyr::select("Title","Username","date","year","time","pandemic","gender","race","race_group","job_cat","job_cat_old","job_cat_broad","job_cat_simplified","job_cat_simplified2","job_cat_simplified3","job_cat_simplified4","author_position","N_authors","individual_position")

# Anonymize Title and Username
dat_select_anon <- dat_select %>%
  # Replace Title with Pub1, Pub2, etc., based on unique titles
  mutate(Title = paste0("Pub", as.integer(factor(Title)))) %>%
  # Replace Username with ID1, ID2, etc., based on unique usernames
  mutate(anon_id = paste0("ID", as.integer(factor(Username))))

write.csv(dat_select_anon, "data_cleaning/manuscript_data/clean_data_final_anon.csv",row.names = FALSE)

# Read in nw_dat
nw_dat <- read.csv("data_cleaning/shared/centrality_measures_year.csv", stringsAsFactors = FALSE)

# Create mapping from Username to anon_id
mapping <- dat_select_anon %>%
  dplyr::select(Username, anon_id) %>%
  distinct()

# Anonymize nw_dat by replacing username with anon_id
nw_dat_anon <- nw_dat %>%
  left_join(mapping, by = c("username" = "Username")) %>%
  mutate(username = anon_id) %>%
  dplyr::select(-anon_id)  # Remove the extra anon_id column

# Write the anonymized nw_dat to a new file
write.csv(nw_dat_anon, "data_cleaning/manuscript_data/centrality_measures_year_anon.csv", row.names = FALSE)
