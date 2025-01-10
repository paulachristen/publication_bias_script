

# Install R packages if not installed -------------------------------------

if (!require("glmmTMB")) install.packages("glmmTMB")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("MASS")) install.packages("MASS")
if (!require("broom")) install.packages("broom")
if (!require("dplyr")) install.packages("dplyr")

# Load R packages ---------------------------------------------------------

library(glmmTMB)
library(tidyverse)
library(lubridate)
library(MASS)
library(broom)
library(dplyr)

############################################################################
######################## Publication Data ##################################
# Get Publication data (not in repository due to identifiable data) --------
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

# Read in network data (not in repository due to identifiable data) --------
nw_dat <- read.csv("centrality_measures_year.csv")

# Models -----------------------------------------------------------------------

# < data: annual pubs and author position --------------------------------------
dat_annual <- dat |>
  group_by(Username, year, time, pandemic, gender, race_group, 
           job_cat_broad, job_cat_simplified3, author_position) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(apos_firstlast = case_when(author_position %in% c("first","last") ~ n),
         apos_middle = case_when(author_position == "middle" ~ n)) |> 
  group_by(Username, year, time, pandemic, gender, race_group, 
           job_cat_broad, job_cat_simplified3) |>
  summarize(across(c(n, apos_firstlast:apos_middle), \(x) sum(x, na.rm = TRUE))) |>
  rename(N_pub = n)

# merge formatted data with network data ---------------------------------------
dat_annual <- merge(dat_annual, nw_dat[,c("username","year","deg")], 
                    by.x = c("Username","year"), by.y = c("username","year"), 
                    all.x = TRUE)

# Add rows with 0 outcomes for individuals with missing years 
# (between min year and max year)

dat_annual <- dat_annual |>
  arrange(Username, year) |>
  group_by(Username) %>%
  complete(year = seq(min(year), max(year))) |>
  fill(gender:job_cat_simplified3) |>
  mutate(across(c(N_pub:deg), ~ifelse(is.na(.), 0, .)))

# Year regression variable centered at 2019 ------------------------------------
dat_annual <- dat_annual |>
  mutate(year_reg = year - 2019)

model_function <- function(model = "m_1",
                            data = dat_annual,
                            ref_group_race = "non-white", 
                            ref_group_gender = "male", 
                            ref_group_time = "2014-2015", 
                            ref_group_jobcat = "Student / Research Assistant") {
  
  if(model == "m_1"){
    m <- "~ time + gender + race_group + job_cat_simplified3 + (1 | Username)"
  }
  
  if(model == "m_2"){
    m <- "~ time + gender*race_group + job_cat_simplified3 + (1 | Username)"
  }
  
  if(model == "m_3"){
    m <- "~ time*gender + race_group + job_cat_simplified3 + (1 | Username)"
  }
  
  if(model == "m_4"){
    m <- "~ job_cat_simplified3*gender + race_group + time + (1 | Username)"
  }
  
  ### Change reference group for easier comparison -----------------------------
  data <- within(data, race_group <- relevel(race_group, ref = ref_group_race))
  data <- within(data, gender <- relevel(gender, ref = ref_group_gender))
  data <- within(data, time <- relevel(time, ref = ref_group_time))
  data <- within(data, job_cat_simplified3 <- relevel(job_cat_simplified3, ref = ref_group_jobcat))
  
  m1 <- glmmTMB(as.formula(paste0("N_pub",m)), 
                data = data, family = nbinom2)
  
  m2 <- glmmTMB(as.formula(paste0("apos_firstlast",m)),
                data = data, family = nbinom2)
  
  m3 <- glmmTMB(as.formula(paste0("apos_middle",m)),
                data = data, family = nbinom2)
  
  m4 <- glmmTMB(as.formula(paste0("deg",m)),
                data = data, family = nbinom2)
  
  title <- case_when(model == "m_1" ~
                       "Model 1",
                     model == "m_2" ~
                       "Model 2",
                     model == "m_3" ~
                       "Model 3", 
                     model == "m4" ~
                       "Model 4")
  
  job_cat <- strsplit(ref_group_jobcat, " ")[[1]][1]
  html_filepath <- paste0("./regression_results/",model,"_",ref_group_race,"_",ref_group_gender,"_",ref_group_time,"_",job_cat,".html")
  print(sjPlot::tab_model(m1, m2, m3, m4,
                          dv.labels = c("N_pub", "apos_firstlast", "apos_middle", "deg"),
                          wrap.labels = 50,
                          title = title,
                          show.aic = TRUE,
                          p.style = "stars", 
                          file = html_filepath))
}

#run model with all possible combinations -- apart from time -- keep 2014/15 as reference year
combinations <- expand.grid(m = c("m_1", "m_2", "m_3", "m_4"),
                            ref_group_race = unique(dat_annual$race_group),
                            ref_group_gender = unique(dat_annual$gender),
                            ref_group_time = unique(dat_annual$time),
                            ref_group_jobcat = unique(dat_annual$job_cat_simplified3))

#for loop over each iteration
for (i in 1:nrow(combinations)) {
  m_ <- as.character(combinations$m[[i]])
  r <- as.character(combinations$ref_group_race[[i]])
  g <- as.character(combinations$ref_group_gender[[i]])
  t <- as.character(combinations$ref_group_time[[i]])
  j <- as.character(combinations$ref_group_jobcat[[i]])
  
  mod_foc_outcome(model = m_,
                  dat_annual,
                  ref_group_race = r, 
                  ref_group_gender = g,
                  ref_group_time = t, 
                  ref_group_jobcat = j)
}