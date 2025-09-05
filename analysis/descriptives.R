
# Install R packages if not installed -------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("RColorBrewer")) install.packages("RColorBrewer")

# Load R packages ---------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)

# Figure settings (for combined plots)
imperial_grey <- rgb(235/256, 238/256, 238/256)

theme1 <- theme(
  panel.border = element_blank(),
  axis.line = element_line(color = "black"),
  legend.title = element_text(size = 11),
  legend.text = element_text(size = 10),
  legend.key.size = unit(0.4, "cm"),
  panel.grid.major = element_line(colour = imperial_grey, linewidth = 0.3),
  panel.grid.minor = element_line(colour = imperial_grey, linewidth = 0.3),
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 11),
  strip.background = element_rect(fill = "white", linewidth = 1),
  strip.text.x = element_text(size = 11)
)

################################################################################
######################## Publication Data ######################################
# Get Publication data and anonymize --------------------------------------
dat <- read.csv("data_cleaning/manuscript_data/clean_data_final_anon.csv",
                stringsAsFactors = FALSE, encoding = "UTF-8")
# Create aggregated data by author ---------------------------------------------
dat2 <- dat_anon |> 
  group_by(anon_id, gender, race, race_group, job_cat_simplified, job_cat_old) |> 
  summarize(N_publications = n(),
            N_authors_median = median(N_authors, na.rm = TRUE),
            individual_position_median = median(individual_position, na.rm = TRUE))

table(dat2$job_cat_simplified, useNA = "always")
table(dat2$job_cat_old, useNA = "always")
table(dat2$gender, useNA = "always")

cats <- c("Research Associate",
          "Research Fellow", 
          "Lecturer / Senior Lecturer", 
          "Reader / Professor")

# Misc stats -------------------------------------------------------------------
# number records
nrow(dat_anon)

# number publications
length(unique(dat_anon$Title))

# number staff
length(unique(dat_anon$anon_id))

# number of male and female staff
dat_anon |> 
  group_by(gender) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  mutate(t = sum(n), p = n/t)

# number staff by ethnicity
dat_anon |> 
  group_by(race) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  mutate(t = sum(n), p = n/t)

dat_anon |> 
  group_by(race_group) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  ungroup() |> 
  mutate(t = sum(n), p = n/t)

# number of male and female staff by ethnicity
dat_anon |> 
  group_by(gender, race) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  group_by(gender) |> 
  mutate(t = sum(n), p = n/t)

dat_anon |> 
  group_by(gender, race_group) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  group_by(gender) |> 
  mutate(t = sum(n), p = n/t)

# number of staff by ethnicity and job category
dat_anon |> 
  group_by(job_cat_simplified, race) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  group_by(job_cat_simplified) |> 
  mutate(t = sum(n), p = n/t) |> 
  print(n = 20)

dat_anon |> 
  group_by(job_cat_simplified, race_group) |> 
  distinct(anon_id) |> 
  summarize(n = n()) |> 
  group_by(job_cat_simplified) |> 
  mutate(t = sum(n), p = n/t) |> 
  print(n = 20)

# number publications by gender by year
table(dat_anon$gender, dat_anon$year, useNA = "always")

# publications by gender
dat_anon |>
  group_by(gender) |>
  summarize(n = n()) |>
  mutate(t = sum(n),
         p = n / t * 100)

# number of publications per individual by gender
dat2 |> 
  group_by(gender) |>
  summarize(n = n(),
            npub_median = median(N_publications, na.rm = TRUE),
            npub_q25 = quantile(N_publications, probs = 0.25, na.rm = TRUE),
            npub_q75 = quantile(N_publications, probs = 0.75, na.rm = TRUE))

# < pubs per year by ethnicity -------------------------------------------------
datb <- dat_anon |> 
  group_by(year, race) |>
  summarize(n = n()) |>
  group_by(year) |>
  mutate(t = sum(n),
         p = n / t, 
         pos = ifelse(race == "white", n / 2, NA_real_))

table(dat_anon$race, dat_anon$year, useNA = "always")

dat_anon |> 
  group_by(race) |>
  summarize(n = n()) |>
  mutate(t = sum(n),
         p = n / t)

# < co-authors by gender ----
datb <- dat_anon |> 
  group_by(gender) |>
  summarize(med = median(N_authors, na.rm = TRUE),
            q25 = quantile(N_authors, probs = 0.25, na.rm = TRUE), 
            q75 = quantile(N_authors, probs = 0.75, na.rm = TRUE),
            min = min(N_authors, na.rm = TRUE),
            max = max(N_authors, na.rm = TRUE))

datb
summary(dat_anon$N_authors)

# < author position by job category and gender ----
datb <- dat_anon |> 
  filter(!is.na(author_position) & N_authors > 0) |>
  group_by(anon_id, gender, author_position) |>
  summarize(N_publications = n())

datc <- datb |> 
  group_by(gender, author_position) |>
  summarize(median = round(median(N_publications)),
            q25 = round(quantile(N_publications, probs = 0.25)),
            q75 = round(quantile(N_publications, probs = 0.75))) |>
  mutate(label = paste0(median, " (", q25, ", ", q75, ")"))

table(dat_anon$author_position, useNA = "always")
table(dat_anon$author_position, dat_anon$gender, useNA = "always")
summary(dat_anon$N_authors)

dat_anon |> 
  group_by(gender, author_position) |>
  summarize(n = n()) |>
  group_by(author_position) |>
  mutate(t = sum(n), p = n / t * 100)

# < pubs by job category ----
dat2b <- dat2 |> 
  group_by(gender, job_cat_old) |> 
  dplyr::filter(job_cat_old %in% cats) |>
  summarize(median = round(median(N_publications)),
            q25 = round(quantile(N_publications, probs = 0.25)),
            q75 = round(quantile(N_publications, probs = 0.75)),
            mean = round(mean(N_publications))) |>
  mutate(label = paste0(median, " (", q25, ", ", q75, ")"))

# < job category by gender ----
dat2b_job_gender <- dat2 |> 
  group_by(job_cat_simplified, gender) |>
  summarize(n = n())

table(dat2$gender, dat2$job_cat_simplified, useNA = "always")

