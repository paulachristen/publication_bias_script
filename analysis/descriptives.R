
# Install R packages if not installed -------------------------------------

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("MASS")) install.packages("MASS")
if (!require("broom")) install.packages("broom")
if (!require("dplyr")) install.packages("dplyr")

# Load R packages ---------------------------------------------------------

library(tidyverse)
library(lubridate)
library(MASS)
library(broom)
library(dplyr)

################################################################################
######################## Publication Data ######################################
# Get Publication data (not in repository due to identifiable data) ------------
source("./load_publication_data.R",
       chdir=TRUE)

dat <- dat |>
  dplyr::select(Name, Username, Publication.type, Publication.date.OR.Presented.date.OR.Date.awarded.OR.Presentation.date,
                Title, Title2, JCR.rank:SNIP.rank, first_name:job_cat,
                Name, gender, race, race_group, job_title, job_cat, job_cat_clinical,
                job_cat_clinical_flag, job_cat_old, job_cat_f,
                job_cat_simplified, job_cat_simplified2, job_cat_simplified3, job_cat_broad,
                year, Sub.types, time, pandemic, author_position, covid, citations, h_index,
                pandemic, time, date)

# Read in network data (not in repository due to identifiable data) ------------
nw_dat <- read.csv("centrality_measures_year.csv")

# Create aggregated data by author ---------------------------------------------
dat2 <- dat |> group_by(Name, gender, race, race_group, job_title, job_cat, time,
                        job_cat_old, job_cat_f, job_cat_simplified, job_cat_simplified2, job_cat_simplified3) |> # author_position
  summarize(N_publications = n(),
            N_authors_median = median(N_authors, na.rm = TRUE),
            individual_position_median = median(individual_position, na.rm = TRUE))

table(dat2$job_cat_f, useNA = "always")
table(dat2$job_cat_simplified, useNA = "always")
table(dat2$job_cat_simplified2, useNA = "always")
table(dat2$time,dat2$gender, useNA = "always")

table(dat2$gender, useNA = "always")

cats <- c("Research Associate",
                    "Research Fellow" , 
                    "Lecturer / Senior Lecturer", 
                    "Reader / Professor")

# Misc stats -------------------------------------------------------------------
# number records
nrow(dat)

# number publications
length(unique(dat$Title))

# number staff
length(unique(dat$Name))

# number of male and female staff
dat |> group_by(gender) |> distinct(Name) |> summarize(n = n()) |> ungroup() |> mutate(t = sum(n), p = n/t)

# number staff by ethnicity
dat |> group_by(race) |> distinct(Name) |> summarize(n = n()) |> ungroup() |> mutate(t = sum(n), p = n/t)
dat |> group_by(race_group) |> distinct(Name) |> summarize(n = n()) |> ungroup() |> mutate(t = sum(n), p = n/t)

# number of male and female staff by ethnicity
dat |> group_by(gender, race) |> distinct(Name) |> summarize(n = n()) |> group_by(gender) |> mutate(t = sum(n), p = n/t)
dat |> group_by(gender, race_group) |> distinct(Name) |> summarize(n = n()) |> group_by(gender) |> mutate(t = sum(n), p = n/t)

# number of staff by ethnicity and job category
dat |> group_by(job_cat, race) |> distinct(Name) |> summarize(n = n()) |> group_by(job_cat) |> mutate(t = sum(n), p = n/t) |> print(n = 31)
dat |> group_by(job_cat, race_group) |> distinct(Name) |> summarize(n = n()) |> group_by(job_cat) |> mutate(t = sum(n), p = n/t) |> print(n = 20)
dat |> group_by(job_cat_simplified, race_group) |> distinct(Name) |> summarize(n = n()) |> group_by(job_cat_simplified) |> mutate(t = sum(n), p = n/t) |> print(n = 20)

# number publications by gender by year
table(dat$gender, dat$year, useNA = "always")

# publications by gender
dat |>
  group_by(gender) |>
  summarize(n = n()) |>
  mutate(t = sum(n),
         p = n / t * 100)

# number of publications per individual by gender
dat2 |> group_by(gender) |>
  summarize(n = n(),
            npub_median = median(N_publications, na.rm = T),
            npub_q25 = quantile(N_publications, probs = 0.25, na.rm = T),
            npub_q75 = quantile(N_publications, probs = 0.75, na.rm = T),
  )


# < pubs per year by ethnicity -------------------------------------------------
datb <- dat |> 
  group_by(year, race) |>
  summarize(n = n()) |>
  group_by(year) |>
  mutate(t = sum(n),
         p = n / t, 
         pos = ifelse(race == "white", n / 2, NA_real_))

table(dat$race, dat$year, useNA = "always")

dat |> 
  group_by(race) |>
  summarize(n = n()) |>
  mutate(t = sum(n),
         p = n / t)


# < co-authors by gender ----
datb <- dat |> 
  group_by(gender) |>
  summarize(med = median(N_authors, na.rm = T),
            q25 = quantile(N_authors, probs = 0.25, na.rm = T), 
            q75 = quantile(N_authors, probs = 0.75, na.rm = T),
            min = min(N_authors, na.rm = T),
            max = max(N_authors, na.rm = T))

datb; summary(dat$N_authors)


# < author position by job category and gender ----
datb <- dat |> filter(!is.na(author_position) & N_authors > 0) |>
  group_by(Name, gender, author_position) |>
  summarize(N_publications = n())

datc <- datb |> group_by(gender, author_position) |>
  summarize(median = round(median(N_publications)),
            q25 = round(quantile(N_publications, probs = 0.25)),
            q75 = round(quantile(N_publications, probs = 0.75))) |>
  mutate(label = paste0(median, " (", q25, ", ", q75, ")"))

table(dat$author_position, useNA = "always")
table(dat$author_position, dat$gender, useNA = "always")
summary(dat$N_authors)

dat |> group_by(gender, author_position) |>
  summarize(n = n()) |>
  group_by(author_position) |>
  mutate(t = sum(n), p = n / t * 100)


# < author position  by job category and gender ----
datb <- dat |> filter(!is.na(author_position) & N_authors > 0) |>
  group_by(Name, gender, job_cat_simplified, author_position) |>
  summarize(N_publications = n())

table(dat$author_position, useNA = "always")
table(dat$author_position, dat$gender, useNA = "always")

# < pubs by job category ----
dat2b <- dat2 |> group_by(gender, job_cat_old) |> 
  dplyr::filter(job_cat_old %in% cats) |>
  summarize(median = round(median(N_publications)),
            q25 = round(quantile(N_publications, probs = 0.25)),
            q75 = round(quantile(N_publications, probs = 0.75)),
            mean=round(mean(N_publications))) |>
  mutate(label = paste0(median, " (", q25, ", ", q75, ")"))


# < job category by gender ----
dat2b <- dat2 |> group_by(job_cat_simplified, gender) |>
  summarize(n = n())

table(dat2$gender, dat2$job_cat_simplified, useNA = "always")

dat2 |> group_by(gender, job_cat_simplified) |>
  summarize(n = n()) |> group_by(job_cat_simplified) |>
  mutate(t = sum(n), p = n / t * 100)

