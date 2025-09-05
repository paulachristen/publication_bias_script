# Install R packages if not installed -------------------------------------
if (!require("glmmTMB")) install.packages("glmmTMB")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("MASS")) install.packages("MASS")
if (!require("broom")) install.packages("broom")
if (!require("dplyr")) install.packages("dplyr")
if (!require("sjPlot")) install.packages("sjPlot")

# Load R packages ---------------------------------------------------------
library(glmmTMB)
library(tidyverse)
library(lubridate)
library(MASS)
library(broom)
library(dplyr)
library(sjPlot)

############################################################################
######################## Publication Data ##################################
# Get Publication data (not in repository due to identifiable data) --------
dat <- read.csv("data_cleaning/manuscript_data/clean_data_final_anon.csv",
                stringsAsFactors = FALSE, encoding = "UTF-8")

# Read in network data (not in repository due to identifiable data) --------
nw_dat <- read.csv("data_cleaning/manuscript_data/centrality_measures_year_anon.csv")

# Models -----------------------------------------------------------------------

# < data: annual pubs and author position --------------------------------------
dat_annual <- dat |>
  group_by(anon_id, year, time, pandemic, gender, race_group, 
           job_cat_broad, job_cat_simplified3, author_position) |>
  summarize(n = n()) |>
  ungroup() |>
  mutate(apos_firstlast = case_when(author_position %in% c("first","last") ~ n),
         apos_middle = case_when(author_position == "middle" ~ n)) |> 
  group_by(anon_id, year, time, pandemic, gender, race_group, 
           job_cat_broad, job_cat_simplified3) |>
  summarize(across(c(n, apos_firstlast:apos_middle), \(x) sum(x, na.rm = TRUE))) |>
  rename(N_pub = n)

# merge formatted data with network data ---------------------------------------
dat_annual <- merge(dat_annual, nw_dat[,c("username","year","deg")], 
                    by.x = c("anon_id","year"), by.y = c("username","year"), 
                    all.x = TRUE)

# Add rows with 0 outcomes for individuals with missing years 
# (between min year and max year)
dat_annual <- dat_annual |>
  arrange(anon_id, year) |>
  group_by(anon_id) %>%
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
  
  if (model == "m_1") {
    m <- "~ time + gender + race_group + job_cat_simplified3 + (1 | anon_id)"
  } else if (model == "m_2") {
    m <- "~ time + gender*race_group + job_cat_simplified3 + (1 | anon_id)"
  } else if (model == "m_3") {
    m <- "~ time*gender + race_group + job_cat_simplified3 + (1 | anon_id)"
  } else if (model == "m_4") {
    m <- "~ job_cat_simplified3*gender + race_group + time + (1 | anon_id)"
  }
  
  # Change reference groups
  data <- within(data, race_group <- relevel(as.factor(race_group), ref = ref_group_race))
  data <- within(data, gender <- relevel(as.factor(gender), ref = ref_group_gender))
  data <- within(data, time <- relevel(as.factor(time), ref = ref_group_time))
  data <- within(data, job_cat_simplified3 <- relevel(as.factor(job_cat_simplified3), ref = ref_group_jobcat))
  
  # Fit models
  m1 <- glmmTMB(as.formula(paste0("N_pub", m)), data = data, family = nbinom2)
  m2 <- glmmTMB(as.formula(paste0("apos_firstlast", m)), data = data, family = nbinom2)
  m3 <- glmmTMB(as.formula(paste0("apos_middle", m)), data = data, family = nbinom2)
  m4 <- glmmTMB(as.formula(paste0("deg", m)), data = data, family = nbinom2)
  
  # Title
  title <- case_when(
    model == "m_1" ~ "Model 1",
    model == "m_2" ~ "Model 2",
    model == "m_3" ~ "Model 3",
    model == "m_4" ~ "Model 4"
  )
  
  # Output file paths
  job_cat <- strsplit(ref_group_jobcat, " ")[[1]][1]
  file_stub <- paste0(model, "_", ref_group_race, "_", ref_group_gender, "_", ref_group_time, "_", job_cat)
  html_filepath <- paste0("./regression_results/", file_stub, ".html")
  png_filepath <- paste0("./regression_plots/", file_stub, ".png")
  
  # Create output directories if they don't exist
  dir.create("./regression_results", showWarnings = FALSE)
  dir.create("./regression_plots", showWarnings = FALSE)
  
  # Save regression table
  sjPlot::tab_model(m1, m2, m3, m4,
                    dv.labels = c("N_pub", "apos_firstlast", "apos_middle", "deg"),
                    wrap.labels = 50,
                    title = title,
                    show.aic = TRUE,
                    p.style = "stars", 
                    file = html_filepath)
  
  # Dynamic axis labels -------------------------------------------------------
  base_label <- c(
    "Time: 2016 - 2017", "Time: 2018 - 2019", "Time: 2020 - 2021", "Time: 2022 - 2023",
    "Gender: Male", "Ethnicity: Non-minoritized",
    "Job Category: Research Associate", "Job Category: Research Fellow",
    "Job Category: Lecturer", "Job Category: Senior Lecturer / Reader",
    "Job Category: Professor",
    "Job Category: Other"
  )
  
  if (model == "m_1") {
    axis_labels <- base_label
  } else if (model == "m_2") {
    axis_labels <- c(base_label, "Gender: Male x Ethnicity: Non-minoritized")
  } else if (model == "m_3") {
    axis_labels <- c(
      base_label,
      "Time: 2016 - 2017 x Gender: Male",
      "Time: 2018 - 2019 x Gender: Male",
      "Time: 2020 - 2021 x Gender: Male",
      "Time: 2022 - 2023 x Gender: Male"
    )
  } else if (model == "m_4") {
    jobcats <- c("Research Associate", "Research Fellow", "Lecturer", "Senior Lecturer / Reader", "Professor", "Other")
    interaction_labels <- paste0("Job Category: ", jobcats, " x Gender: Male")
    axis_labels <- c(base_label, interaction_labels)
  }
  
  # Plot and save
  p <- sjPlot::plot_models(m1, m2, m3, m4,
                           axis.labels = rev(axis_labels),
                           m.labels = c("Annual number of publications per person",
                                        "Annual number of first or last author publications per person",
                                        "Annual number of middle author publications per person", 
                                        "Annual internal co-authors per person"), 
                           colors = "Dark2",
                           p.shape = TRUE) + 
    labs(caption = paste("Reference:", ref_group_time, ", ", ref_group_race, ", ", ref_group_gender)) +
    theme_minimal(base_size = 14) +  # Base font size (titles, axes, etc.)
    theme(
      axis.text = element_text(size = 18),        # Axis tick labels
      axis.title = element_text(size = 18),       # Axis titles
      legend.text = element_text(size = 16),      # Legend labels
      plot.caption = element_text(size = 12),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    guides(color = guide_legend(nrow = 2))
  
  ggsave(png_filepath, p, width = 12, height = 12, dpi = 300, units = "in")
}

# Run model with all possible combinations -- apart from time -- keep 2014/15 as reference year
combinations <- expand.grid(m = c("m_1", "m_2", "m_3", "m_4"),
                            ref_group_race = "non-white",
                            ref_group_gender = "female",
                            ref_group_time = "2014-2015",
                            ref_group_jobcat = "Student / Research Assistant")

# For loop over each iteration
for (i in 1:nrow(combinations)) {
  m_ <- as.character(combinations$m[[i]])
  r <- as.character(combinations$ref_group_race[[i]])
  g <- as.character(combinations$ref_group_gender[[i]])
  t <- as.character(combinations$ref_group_time[[i]])
  j <- as.character(combinations$ref_group_jobcat[[i]])
  
  model_function(model = m_,
                 data = dat_annual,
                 ref_group_race = r, 
                 ref_group_gender = g,
                 ref_group_time = t, 
                 ref_group_jobcat = j)
}
