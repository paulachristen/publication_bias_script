#Plots

# Install R packages if not installed -------------------------------------

if (!require("glmmTMB")) install.packages("glmmTMB")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("MASS")) install.packages("MASS")
if (!require("broom")) install.packages("broom")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggtext")) install.packages("ggtext")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("grid")) install.packages("grid")
if (!require("graphlayouts")) install.packages("graphlayouts")

devtools::install_github("BlakeRMills/MetBrewer")
remotes::install_github("BlakeRMills/MetBrewer")

# Load R packages ---------------------------------------------------------

library(glmmTMB)
library(tidyverse)
library(lubridate)
library(MASS)
library(broom)
library(dplyr)
library(ggtext)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
require(grid)
library(graphlayouts)
library(MetBrewer)

# Figure settings
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
  #plot.margin = margin(10, 30, 10, 10),
  strip.background = element_rect(fill = "white", linewidth = 1),
  strip.text.x = element_text(size = 11)
)

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
                job_cat_simplified, job_cat_simplified2, job_cat_simplified3,job_cat_simplified4, job_cat_broad,
                year, Sub.types, time, pandemic, author_position, covid, citations, h_index,
                pandemic, time, date)

dat$race_group <- ifelse(dat$race_group == "white", 
                         "Non-minoritized", 
                         "Minoritized")

dat$gender <- ifelse(dat$gender == "male", "Man", "Woman")

dat$author_position <- ifelse((dat$author_position == "first" | 
                                 dat$author_position == "last"), 
                              "first/last", dat$author_position)

groups_to_plot <- c("Student / Research Assistant",
                    "Research Associate",
                    "Research Fellow", 
                    "Lecturer / Senior Lecturer", 
                    "Reader / Professor")

# Read in network data (not in repository due to identifiable data) --------
nw_dat <- read.csv("centrality_measures_year.csv")

# Create aggregated data by author
dat2 <- dat |> group_by(Name, gender, race, race_group, job_title, job_cat,
                        job_cat_old, job_cat_f, job_cat_simplified3,job_cat_simplified4) |> # author_position
  summarize(N_publications = n(),
            N_authors_median = median(N_authors, na.rm = TRUE),
            individual_position_median = median(individual_position, na.rm = TRUE))

# < data: annual pubs and author position (without aggregation of first / last) ----
dat_annual <- dat |>
  group_by(Username, year, time, pandemic, gender, race_group, 
           job_cat_broad, job_cat_simplified3, job_cat_simplified4, author_position) |>
  summarize(n = n()) |>
  ungroup() |>
  group_by(Username, year, time, pandemic, gender, race_group, 
           job_cat_broad, job_cat_simplified3,job_cat_simplified4, author_position) |>
  summarize(across(c(n), \(x) sum(x, na.rm = TRUE))) |>
  rename(N_pub = n)

# Add rows with 0 outcomes for individuals with missing years 
# (between min year and max year) 
dat_annual <- dat_annual |>
  arrange(Username, year) |>
  group_by(Username) %>%
  complete(year = seq(min(year), max(year))) |>
  fill(gender:job_cat_simplified4) |>
  mutate(across(c(N_pub), ~ifelse(is.na(.), 0, .)))

# Year regression variable centered at 2019
dat_annual <- dat_annual |>
  mutate(year_reg = year - 2019)

dat_annual$author_position <- ifelse((dat_annual$author_position == "first" | 
                                        dat_annual$author_position == "last"), 
                                     "first/last", dat_annual$author_position)
temp <- dat_annual
datD <- dat_annual
datE <- dat_annual

# Figures ----------------------------------------------------------------------

# < Figure 1 - number people per gender, ethnicity, jobcat ----

datplot_all <- dat |>
  group_by(job_cat_simplified3, gender) |>
  summarize(n = n_distinct(Name))

datplot_all_all <- dat |>
  group_by(gender) |>
  summarize(n = n_distinct(Name))

datplot_all_all$job_cat_simplified3 <- c("All")

datplot_all_all <- datplot_all_all[, colnames(datplot_all_all)[c(3,1,2)]]

datplot_all <- rbind(datplot_all_all, datplot_all)

datplot_white <- dat |>
  filter(race_group == "Non-minoritized") |>
  group_by(job_cat_simplified3, gender) |>
  summarize(n = n_distinct(Name))

datplot_white_All <- dat |>
  filter(race_group == "Non-minoritized") |>
  group_by(gender) |>
  summarize(n = n_distinct(Name))

datplot_white_All$job_cat_simplified3 <- c("All")

datplot_white_All <- datplot_white_All[, colnames(datplot_white_All)[c(3,1,2)]]

datplot_white <- rbind(datplot_white_All, datplot_white)

p1 <- ggplot() +
  # plot everyone with low alpha value
  geom_col(data = datplot_all,
           aes(x = job_cat_simplified3, y = n, fill = gender, alpha = "Minoritized"),
           position = position_dodge(width = 0.9)) +
  # overlay with white population
  geom_col(data = datplot_white,
           aes(x = job_cat_simplified3, y = n, fill = gender, alpha = "Non-minoritized"),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(breaks = c("Woman", "Man"),
                    labels = c("Woman", "Man"),
                    values = c( "#000080", "#FF9933")) +
  scale_alpha_manual(breaks = c("Minoritized", "Non-minoritized"),
                     labels = c("Minoritized", "Non-minoritized"),
                     values = c(0.4, 1)) +
  scale_x_discrete(limits = c("All", "Student / Research Assistant", "Research Associate", "Research Fellow", "Lecturer", "Senior Lecturer / Reader", "Professor", "Other"),
                   labels = scales::label_wrap(20)) +
  guides(fill = guide_legend(order = 1), alpha = guide_legend(order = 2)) +
  theme1+
  theme(legend.position = "bottom")+
  labs(x = "", y = "Number of people", fill = "Gender (perceived)", alpha = "Ethnicity (perceived)")

ggsave("./Figures/number_of_people_by_job_rg.png", plot = p1, 
       width = 12, height = 4.68, dpi = 300, units = "in")

# < Figure 2.1 Publication rates per person by job level and gender ----

dat_count <- dat |> 
  group_by(job_cat_simplified3, gender) |>
  summarize(count = n_distinct(Username))

dat_count_all <- dat |> 
  group_by(gender) |>
  summarize(count = n_distinct(Username))

dat_annual <- merge(dat_annual, dat_count, by = c("job_cat_simplified3", "gender"), all.x = TRUE)

pr_gender <- dat_annual |>
  group_by(gender) %>%
  summarize(mean = mean(N_pub),
            median = median(N_pub),
            q25 = quantile(N_pub, probs = 0.25, digits = 7),
            q75 = quantile(N_pub, probs = 0.75)) 

pr_gender <- merge(pr_gender, dat_count_all, by = "gender", all.x = T)

pr_gender <- pr_gender |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

pr_gender$job_cat_simplified3 <- "All"

pr_gender <- pr_gender[, colnames(pr_gender)[c(8,1:7)]]

pr_job_gender <- dat_annual |>
  group_by(job_cat_simplified3, gender) %>%
  summarize(mean = round(mean(N_pub), 4),
            median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

dat_annual$all <- "All"

all <- ggplot(data = dat_annual) + 
  geom_boxplot(aes(x = gender, y = N_pub, fill = gender, color = gender), 
               alpha = 0.4, show.legend = F) +
  geom_jitter(aes(x = gender, y = N_pub, color = gender), 
              show.legend = F, alpha = 0.1, height = 0, size = .8) +
  geom_text(data = pr_gender, aes(y = 68, x = gender, label = label, 
                                      group = gender, color = gender), 
            size = 3, position = position_dodge(width = .9), show.legend = F) + 
  facet_grid(cols = vars(all),
             labeller = labeller(all = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("#FF9933", "#000080")) + 
  scale_color_manual(values = c("#FF9933", "#000080")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Gender (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill= "white"))

grid <- ggplot(data = dat_annual) + 
  geom_boxplot(aes(x = gender, y = N_pub, fill = gender, color = gender), 
               alpha = 0.4, show.legend = F) + 
  geom_jitter(aes(x = gender, y = N_pub, color = gender), 
              show.legend = F, alpha = 0.1, height = 0, size = .8) +
  geom_text(data = pr_job_gender, aes(y = 68, x = gender, label = label, 
                                      group = gender, color = gender), 
            size = 3, position = position_dodge(width = .9), show.legend = F) + 
  facet_grid(cols = vars(job_cat_simplified3),
             labeller = labeller(job_cat_simplified3 = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("#FF9933", "#000080")) + 
  scale_color_manual(values = c("#FF9933", "#000080")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Gender (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill= "white"), 
                 axis.text.y=element_blank(), 
                 axis.ticks.y=element_blank(), 
                 axis.line.y=element_blank())

figure <- ggarrange(all + rremove("xlab"), 
                    grid + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    labels = NULL,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    widths = c(0.2, 1),
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(figure, 
                bottom = textGrob("Gender (perceived)", gp = gpar(cex = 1)))

ggsave("./Figures/pr_by_job_gender.png", 
       width = 9.4, height = 4, dpi = 300, units = "in")

# < Figure 2.2 Publication rates per person by author position and gender ----

dat_count <- dat |> 
  group_by(author_position, gender) |>
  summarize(count = n_distinct(Username))

temp <- merge(temp, dat_count, by = c("author_position", "gender"), all.x = TRUE)

date <- temp |> group_by(gender, author_position) |>
  summarize(median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

date <- date[!is.na(date$author_position),]
temp <- temp[!is.na(temp$author_position),]

grid <- ggplot(data = temp) + 
  geom_boxplot(aes(x = gender, y = N_pub, fill = gender, color = gender), alpha = 0.4, show.legend = F) +
  geom_jitter(aes(x = gender, y = N_pub, color = gender), 
              show.legend = F, alpha = 0.1, height = 0, size = .8) +
  geom_text(data = date, aes(y = 68, x = gender, label = label, group = gender, color = gender), size = 3, position = position_dodge(width = .9), show.legend = F) + 
  facet_grid(cols = vars(author_position),
             labeller = labeller(author_position = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("#FF9933", "#000080")) + 
  scale_color_manual(values = c("#FF9933", "#000080")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Gender (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill= "white"), 
                 axis.text.y=element_blank(), 
                 axis.ticks.y=element_blank(), 
                 axis.line.y=element_blank())

figure <- ggarrange(all + rremove("xlab"), 
                    grid + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    labels = NULL,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    widths = c(0.4, 1),
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(figure, 
                bottom = textGrob("Gender (perceived)", gp = gpar(cex = 1)))

ggsave("./Figures/authorposition_annual_gender.png", 
       width = 9.4, height = 4, dpi = 300, units = "in")


# < Figure 3.1 Publication rates per person by job level and gender ----

dat_count <- dat |> 
  group_by(job_cat_simplified4, race_group) |>
  summarize(count = n_distinct(Username))

dat_count_all <- dat |> 
  group_by(race_group) |>
  summarize(count = n_distinct(Username))

dat_annual <- merge(datD, dat_count, by = c("job_cat_simplified4", "race_group"), all.x = TRUE)

pr_race_group <- dat_annual |>
  group_by(race_group) %>%
  summarize(mean = round(mean(N_pub), 4),
            median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75))) 

pr_race_group <- merge(pr_race_group, dat_count_all, by = "race_group", all.x = T)

pr_race_group <- pr_race_group |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

pr_race_group$job_cat_simplified4 <- "All"

pr_race_group <- pr_race_group[, colnames(pr_race_group)[c(8,1:7)]]

pr_job_race_group <- dat_annual |>
  group_by(job_cat_simplified4, race_group) %>%
  summarize(mean = round(mean(N_pub), 4),
            median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

dat_annual$all <- "All"

all <- ggplot(data = dat_annual) + 
  geom_boxplot(aes(x = race_group, y = N_pub, fill = race_group, color = race_group), 
               alpha = 0.4, show.legend = F) + 
  geom_jitter(aes(x = race_group, y = N_pub, color = race_group), 
              show.legend = F, alpha = 0.1, height = 0, size = .8) +
  geom_text(data = pr_race_group, aes(y = 68, x = race_group, label = label, 
                                  group = race_group, color = race_group), 
            size = 3, position = position_dodge(width = .9), show.legend = F) + 
  facet_grid(cols = vars(all),
             labeller = labeller(all = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("maroon","#0f7ba2")) + 
  scale_color_manual(values = c("maroon","#0f7ba2")) +
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Ethnicity (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill= "white"), 
                 axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))


datD <- merge(datD, dat_count, by = c("job_cat_simplified4", "race_group"), all.x = TRUE)

pr_job_race_group <- datD |>
  group_by(job_cat_simplified4, race_group) %>%
  summarize(median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

grid <- ggplot(data = datD) + 
  geom_boxplot(aes(x = race_group, y = N_pub, fill = race_group, color = race_group), alpha = 0.4, show.legend = F) + 
  geom_jitter(aes(x = race_group, y = N_pub, color = race_group), 
              show.legend = F, alpha = 0.1, height = 0, size = .8) +
  geom_text(data = pr_job_race_group, aes(y = 68, x = race_group, label = label, group = race_group, color = race_group), size = 3, position = position_dodge(width = .9), show.legend = F) + 
  facet_grid(cols = vars(job_cat_simplified4),
             labeller = labeller(job_cat_simplified4 = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("maroon","#0f7ba2")) + 
  scale_color_manual(values = c("maroon","#0f7ba2")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Ethnicity (perceived)") + 
  theme1 + 
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.background = element_rect(fill= "white"), 
               axis.text.y=element_blank(), 
               axis.ticks.y=element_blank(), 
               axis.line.y=element_blank())

figure <- ggarrange(all + rremove("xlab"), 
                    grid + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    labels = NULL,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    widths = c(0.2, 1),
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(figure, 
                bottom = textGrob("Ethnicity (perceived)", gp = gpar(cex = 1)))

ggsave("pr_by_job_rg.png", 
       width = 9.4, height = 4, dpi = 300, units = "in")

# < Figure 3.2 Number of annual publications rates by author position and ethnicity  ----

dat_count <- dat |> 
  group_by(author_position, race_group) |>
  summarize(count = n_distinct(Username))

datE <- merge(datE, dat_count, by = c("author_position", "race_group"), all.x = TRUE)

date <- datE |> group_by(race_group, author_position) |>
  summarize(median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ",count ,",\n", median, " (", q25, ", ", q75, ")"))

date <- date[!is.na(date$author_position),]
datE <- datE[!is.na(datE$author_position),]

grid <- ggplot(data = datE) + 
  geom_boxplot(aes(x = race_group, y = N_pub, fill = race_group, color = race_group), alpha = 0.4, show.legend = F) + 
  geom_jitter(aes(x = race_group, y = N_pub, color = race_group), 
              show.legend = F, alpha = 0.1, height = 0, size = .8) +
  geom_text(data = date, aes(y = 68, x = race_group, label = label, group = race_group, color = race_group), size = 3, position = position_dodge(width = .9), show.legend = F) + 
  facet_grid(cols = vars(author_position),
             labeller = labeller(author_position = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("maroon","#0f7ba2")) + 
  scale_color_manual(values = c( "maroon","#0f7ba2")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Ethnicity (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill= "white")) + 
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.background = element_rect(fill= "white"), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(), 
        axis.line.y=element_blank())

figure <- ggarrange(all + rremove("xlab"), 
                    grid + rremove("ylab") + rremove("xlab"), # remove axis labels from plots
                    labels = NULL,
                    ncol = 2, nrow = 1,
                    common.legend = TRUE, legend = "bottom",
                    align = "hv", 
                    widths = c(0.4, 1),
                    font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(figure, 
                bottom = textGrob("Ethnicity (perceived)", gp = gpar(cex = 1)))

ggsave("./Figures/authorposition_annual_race.png", 
       width = 9.4, height = 4, dpi = 300, units = "in")

