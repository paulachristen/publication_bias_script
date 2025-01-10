library(tidyverse)
library(lubridate)
library(wru)
library(patchwork)

dat <- read.csv("../interim/clean_data_job_title_author_pos.csv",
                stringsAsFactors = FALSE, encoding = "UTF-8")


# clean up article type & removes editorial articles
dat$Sub.types[grep("rticle",dat$Sub.types)]<-"Journal Article"
dat$Sub.types[grep("Review",dat$Sub.types)]<-"Review"
dat$Sub.types[grep("Letter",dat$Sub.types)]<-"Letter"
dat<-dat[dat$Sub.types %in% c("Journal Article","Review","Letter"),] # 26506 to 22440
table(dat$Sub.types)

# remove honorary positions
dat <- dat |>
  dplyr::filter(!grepl("hono", job_title, ignore.case = T)) |>
  dplyr::filter(!grepl("visit", job_title, ignore.case = T)) |>
  dplyr::filter(!grepl("WHITTAKER, Charles F", Name))

# assign ethnicity using wru package -- method no longer in use as changed to NamePrism below
# create race/ethnicity variable
# need a variable called 'surname' to work with the ethnicities package
# dat <- dat |>
#   mutate(surname = last_name)
#
# dat <- wru::predict_race(voter.file = dat, surname.only = T, year = "2020")
#
# likely_race <- function(vec_probs) {
#   which(vec_probs==max(vec_probs))
# }
# cols <- grep("pred",names(dat))
#
# dat$race_num <- apply(dat[,cols],1,likely_race)
# dat <- dat |>
#   dplyr::mutate(race=race_num)
# dat$race<-recode(dat$race,`1`="white",`2`="black",`3`="Hispanic",`4`="Asian",`5`="other")
# table(dat$race,useNA="a")

# assign ethnicity using NamePrism
dat <- dat |>
  # create variable name_new in dat to match name_new in ethnicity file
  separate(Name, into = c("last_temp", "first_temp"), sep = ",\\s+", remove=FALSE) |>
  mutate(name_new = paste0(first_temp," ", str_to_title(last_temp))) |>
  # join ethnicity data
  left_join(read.csv("../interim/names_ethnicity_20230605.csv") |>
              dplyr::select(name_new, race), 
            by = "name_new") |>
  dplyr::select(!c(first_temp, last_temp, name_new)) |>
  mutate(race = factor(race, levels = c("API", "Black", "Hispanic", "White"),
                       labels = c("asian", "black", "hispanic", "white")),
         race_group = fct_collapse(race, "non-white" = c("asian","black","hispanic")),
         race_group = fct_relevel(race_group, "non-white"))

table(dat$race)
table(dat$race_group)

# compare wru and nameprism
# dat |>
#   # standardize variable names
#   mutate(race = str_to_lower(race),
#          race_nameprism = str_to_lower(race_nameprism),
#          race_nameprism = case_when(race_nameprism == "api" ~ "asian", TRUE ~ race_nameprism)) |>
#   # identify conflicts
#   mutate(race_conflict = case_when(race != race_nameprism ~ 1, TRUE ~ 0 )) |>
#   dplyr::select(Name, race, race_nameprism, race_conflict) |>
#   unique() |> # View()
#   filter(race_conflict == 1) |>
#   arrange(race)

# fix variable for number of authors
dat <- dat |>
  mutate(N_authors = ifelse(N_authors == 0, NA, N_authors))

summary(dat$N_authors)

# Create variable for covid-19 publication
dat <- dat |>
  rename(Title2 = Book.title.OR.Report.title.OR.Preprint.server) |>
  mutate(covid = case_when(grepl("COVID", Title, ignore.case = TRUE) ~ 1,
                           grepl("COVID", Title2, ignore.case = TRUE) ~ 1,
                           grepl("SARS-CoV-2", Title, ignore.case = TRUE) ~ 1,
                           grepl("SARS-CoV-2", Title2, ignore.case = TRUE) ~ 1,
                           TRUE ~ 0))

table(dat$covid, dat$year, useNA = "always")

# Create variable for pre/post pandemic
dat <- dat |>
  mutate(date = ymd(Publication.date.OR.Presented.date.OR.Date.awarded.OR.Presentation.date,  truncated = 2)) |>
  mutate(pandemic = case_when(date < "2020-03-01" ~ "pre",
                              date >= "2020-03-01" ~ "post"))

table(dat$pandemic, dat$year)


# Add simplified job categories
dat <- dat |> mutate(job_cat_f = factor(job_cat, levels = c("Student","Research Assistant","Research Associate","Research Fellow","Teaching Fellow","Lecturer","Senior Lecturer","Reader","Professor","Other"))) |>

  mutate(job_cat_simplified =
           fct_collapse(job_cat_f,
                        "Research Assistant / Associate" = c("Research Assistant","Research Associate"),
                        "Senior Lecturer / Reader" = c("Senior Lecturer", "Reader"),
                        "Other" = c("Teaching Fellow","Other"))) |>

  mutate(job_cat_simplified = factor(job_cat_simplified, levels = c("Student", "Research Assistant / Associate", "Research Fellow", "Lecturer", "Senior Lecturer / Reader", "Professor", "Other"))) |>

  mutate(job_cat_old =
           fct_collapse(job_cat_f,
                        "Lecturer / Senior Lecturer" = c("Senior Lecturer", "Lecturer"),
                        "Reader / Professor" = c("Reader","Professor"))) |>

  mutate(job_cat_old = factor(job_cat_old, levels = c("Student", "Research Assistant", "Research Associate", "Research Fellow", "Lecturer / Senior Lecturer", "Reader / Professor", "Other"))) |>

  mutate(job_cat_simplified2 =
           fct_collapse(job_cat_simplified,
                        "Student / Research Assistant / Associate / Fellow" = c("Student", "Research Assistant / Associate", "Research Fellow"),
                        "Lecturer / Reader" = c("Lecturer", "Senior Lecturer / Reader"))) |>

  mutate(job_cat_simplified2 = factor(job_cat_simplified2, levels = c("Student / Research Assistant / Associate / Fellow", "Lecturer / Reader", "Professor", "Other")))

table(dat$job_cat, dat$job_cat_simplified, useNA = "always")
table(dat$job_cat_simplified, dat$job_cat_simplified2, useNA = "always")
table(dat$job_cat, dat$job_cat_old, useNA = "always")


# Adding a clinical data_flag
dat <- dat %>%
  mutate(job_cat_clinical = ifelse(grepl(pattern = "clinical", x = tolower(job_title)),
                                   paste0(job_cat, " (clinical)"), job_cat))

dat <- dat %>%
  mutate(job_cat_clinical_flag = grepl(pattern = "clinical", x = tolower(job_title)))

dat %>%
  write.csv("../interim/clean_data_job_title_author_pos_ethn.csv", row.names = FALSE)
