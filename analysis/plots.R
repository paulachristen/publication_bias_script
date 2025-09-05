
# Install R packages if not installed -------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggtext")) install.packages("ggtext")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("ggpubr")) install.packages("ggpubr")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("grid")) install.packages("grid")

# Load R packages ---------------------------------------------------------
library(tidyverse)
library(dplyr)
library(ggtext)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(grid)

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
  strip.background = element_rect(fill = "white", linewidth = 1),
  strip.text.x = element_text(size = 11)
)

############################################################################
######################## Publication Data ##################################
# Get Publication data and anonymize --------------------------------------
dat_anon <- read.csv("/Users/paulachristen/SPH Pub Bias/data_cleaning/manuscript_data/clean_data_final_anon.csv")

# Transform variables as in original script
dat_anon$race_group <- ifelse(dat_anon$race_group == "white", 
                              "Non-minoritized", 
                              "Minoritized")

dat_anon$gender <- ifelse(dat_anon$gender == "male", "Man", "Woman")

dat_anon$author_position <- ifelse((dat_anon$author_position == "first" | 
                                      dat_anon$author_position == "last"), 
                                   "first/last", dat_anon$author_position)

# Create dat_annual -------------------------------------------------------
dat_annual <- dat_anon |>
  group_by(anon_id, year, gender, race_group, job_cat_simplified3, job_cat_simplified4, author_position) |>
  summarize(N_pub = n()) |>
  ungroup()

# Add rows with 0 outcomes for individuals with missing years 
dat_annual <- dat_annual |>
  arrange(anon_id, year) |>
  group_by(anon_id) %>%
  complete(year = seq(min(year), max(year))) |>
  fill(gender:job_cat_simplified4) |>
  mutate(across(c(N_pub), ~ifelse(is.na(.), 0, .)))

# Figures ----------------------------------------------------------------

# < Figure 1 - number people per gender, ethnicity, jobcat ----
datplot_all <- dat_anon |>
  group_by(job_cat_simplified3, gender) |>
  summarize(n = n_distinct(anon_id))

datplot_all_all <- dat_anon |>
  group_by(gender) |>
  summarize(n = n_distinct(anon_id))

datplot_all_all$job_cat_simplified3 <- "All"

datplot_all_all <- datplot_all_all[, c("job_cat_simplified3", "gender", "n")]

datplot_all <- rbind(datplot_all_all, datplot_all)

datplot_white <- dat_anon |>
  filter(race_group == "Non-minoritized") |>
  group_by(job_cat_simplified3, gender) |>
  summarize(n = n_distinct(anon_id))

datplot_white_All <- dat_anon |>
  filter(race_group == "Non-minoritized") |>
  group_by(gender) |>
  summarize(n = n_distinct(anon_id))

datplot_white_All$job_cat_simplified3 <- "All"

datplot_white_All <- datplot_white_All[, c("job_cat_simplified3", "gender", "n")]

datplot_white <- rbind(datplot_white_All, datplot_white)

p1 <- ggplot() +
  geom_col(data = datplot_all,
           aes(x = job_cat_simplified3, y = n, fill = gender, alpha = "Minoritized"),
           position = position_dodge(width = 0.9)) +
  geom_col(data = datplot_white,
           aes(x = job_cat_simplified3, y = n, fill = gender, alpha = "Non-minoritized"),
           position = position_dodge(width = 0.9)) +
  scale_fill_manual(breaks = c("Woman", "Man"),
                    labels = c("Woman", "Man"),
                    values = c("#000080", "#FF9933")) +
  scale_alpha_manual(breaks = c("Minoritized", "Non-minoritized"),
                     labels = c("Minoritized", "Non-minoritized"),
                     values = c(0.4, 1)) +
  scale_x_discrete(limits = c("All", "Student / Research Assistant", "Research Associate", 
                              "Research Fellow", "Lecturer", "Senior Lecturer / Reader", 
                              "Professor", "Other"),
                   labels = scales::label_wrap(20)) +
  guides(fill = guide_legend(order = 1), alpha = guide_legend(order = 2)) +
  theme1 +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Number of people", fill = "Gender (perceived)", alpha = "Ethnicity (perceived)")

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/number_of_people_by_job_rg.png", 
       plot = p1, width = 12, height = 4.68, dpi = 600, units = "in", bg = "white")

# < Figure 2.1 Publication rates per person by job level and gender ----
dat_count <- dat_anon |> 
  group_by(job_cat_simplified3, gender) |>
  summarize(count = n_distinct(anon_id))

dat_count_all <- dat_anon |> 
  group_by(gender) |>
  summarize(count = n_distinct(anon_id))

dat_annual <- merge(dat_annual, dat_count, by = c("job_cat_simplified3", "gender"), all.x = TRUE)

pr_gender <- dat_annual |>
  group_by(gender) %>%
  summarize(mean = mean(N_pub),
            median = median(N_pub),
            q25 = quantile(N_pub, probs = 0.25, digits = 7),
            q75 = quantile(N_pub, probs = 0.75)) 

pr_gender <- merge(pr_gender, dat_count_all, by = "gender", all.x = TRUE)

pr_gender <- pr_gender |>
  mutate(label = paste0("n = ", count, ",\n", median, " (", q25, ", ", q75, ")"))

pr_gender$job_cat_simplified3 <- "All"

pr_gender <- pr_gender[, c("job_cat_simplified3", "gender", "mean", "median", "q25", "q75", "count", "label")]

pr_job_gender <- dat_annual |>
  group_by(job_cat_simplified3, gender) %>%
  summarize(mean = round(mean(N_pub), 4),
            median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ", count, ",\n", median, " (", q25, ", ", q75, ")"))

dat_annual$all <- "All"

all_2_1 <- ggplot(data = dat_annual) + 
  geom_boxplot(aes(x = gender, y = N_pub, fill = gender, color = gender), 
               alpha = 0.4, show.legend = FALSE) +
  geom_jitter(aes(x = gender, y = N_pub, color = gender), 
              show.legend = FALSE, alpha = 0.1, height = 0, size = 0.8) +
  geom_text(data = pr_gender, aes(y = 68, x = gender, label = label, 
                                  group = gender, color = gender), 
            size = 3, position = position_dodge(width = 0.9), show.legend = FALSE) + 
  facet_grid(cols = vars(all),
             labeller = labeller(all = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("#FF9933", "#000080")) + 
  scale_color_manual(values = c("#FF9933", "#000080")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Gender (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill = "white"))

grid_2_1 <- ggplot(data = dat_annual) + 
  geom_boxplot(aes(x = gender, y = N_pub, fill = gender, color = gender), 
               alpha = 0.4, show.legend = FALSE) + 
  geom_jitter(aes(x = gender, y = N_pub, color = gender), 
              show.legend = FALSE, alpha = 0.1, height = 0, size = 0.8) +
  geom_text(data = pr_job_gender, aes(y = 68, x = gender, label = label, 
                                      group = gender, color = gender), 
            size = 3, position = position_dodge(width = 0.9), show.legend = FALSE) + 
  facet_grid(cols = vars(job_cat_simplified3),
             labeller = labeller(job_cat_simplified3 = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("#FF9933", "#000080")) + 
  scale_color_manual(values = c("#FF9933", "#000080")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Gender (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill = "white"), 
                 axis.text.y = element_blank(), 
                 axis.ticks.y = element_blank(), 
                 axis.line.y = element_blank())

figure_2_1 <- ggarrange(all_2_1 + rremove("xlab"), 
                        grid_2_1 + rremove("ylab") + rremove("xlab"),
                        labels = NULL,
                        ncol = 2, nrow = 1,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv", 
                        widths = c(0.2, 1),
                        font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

annotate_figure(figure_2_1, 
                bottom = textGrob("Gender (perceived)", gp = gpar(cex = 1)))

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/pr_by_job_gender.png", 
       width = 9.4, height = 4, dpi = 600, units = "in", bg = "white")

# < Figure 2.2 Publication rates per person by author position and gender ----
dat_count <- dat_anon |> 
  group_by(author_position, gender) |>
  summarize(count = n_distinct(anon_id))

dat_annual_temp <- merge(dat_annual, dat_count, by = c("author_position", "gender"), all.x = TRUE)

date <- dat_annual_temp |> 
  group_by(gender, author_position) |>
  summarize(median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ", count, ",\n", median, " (", q25, ", ", q75, ")"))

date <- date[!is.na(date$author_position),]
dat_annual_temp <- dat_annual_temp[!is.na(dat_annual_temp$author_position),]

grid_2_2 <- ggplot(data = dat_annual_temp) + 
  geom_boxplot(aes(x = gender, y = N_pub, fill = gender, color = gender), 
               alpha = 0.4, show.legend = FALSE) +
  geom_jitter(aes(x = gender, y = N_pub, color = gender), 
              show.legend = FALSE, alpha = 0.1, height = 0, size = 0.8) +
  geom_text(data = date, aes(y = 68, x = gender, label = label, group = gender, color = gender), 
            size = 3, position = position_dodge(width = 0.9), show.legend = FALSE) + 
  facet_grid(cols = vars(author_position),
             labeller = labeller(author_position = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("#FF9933", "#000080")) + 
  scale_color_manual(values = c("#FF9933", "#000080")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Gender (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill = "white"), 
                 axis.text.y = element_blank(), 
                 axis.ticks.y = element_blank(), 
                 axis.line.y = element_blank())

figure_2_2 <- ggarrange(all_2_1 + rremove("xlab"), 
                        grid_2_2 + rremove("ylab") + rremove("xlab"),
                        labels = NULL,
                        ncol = 2, nrow = 1,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv", 
                        widths = c(0.4, 1),
                        font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure_2_2 <- annotate_figure(figure_2_2, 
                              bottom = textGrob("Gender (perceived)", gp = gpar(cex = 1)))

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/authorposition_annual_gender.png", 
       width = 9.4, height = 4, dpi = 300, units = "in", bg = "white")

# < Figure 3.1 Publication rates per person by job level and race_group ----
dat_count <- dat_anon |> 
  group_by(job_cat_simplified4, race_group) |>
  summarize(count = n_distinct(anon_id))

dat_count_all <- dat_anon |> 
  group_by(race_group) |>
  summarize(count = n_distinct(anon_id))

datD <- merge(dat_annual, dat_count, by = c("job_cat_simplified4", "race_group"), all.x = TRUE)

pr_race_group <- datD |>
  group_by(race_group) %>%
  summarize(mean = round(mean(N_pub), 4),
            median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75))) 

pr_race_group <- merge(pr_race_group, dat_count_all, by = "race_group", all.x = TRUE)

pr_race_group <- pr_race_group |>
  mutate(label = paste0("n = ", count, ",\n", median, " (", q25, ", ", q75, ")"))

pr_race_group$job_cat_simplified4 <- "All"

pr_race_group <- pr_race_group[, c("job_cat_simplified4", "race_group", "mean", "median", "q25", "q75", "count", "label")]

pr_job_race_group <- datD |>
  group_by(job_cat_simplified4, race_group) %>%
  summarize(mean = round(mean(N_pub), 4),
            median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ", count, ",\n", median, " (", q25, ", ", q75, ")"))

datD$all <- "All"

all_3_1 <- ggplot(data = datD) + 
  geom_boxplot(aes(x = race_group, y = N_pub, fill = race_group, color = race_group), 
               alpha = 0.4, show.legend = FALSE) + 
  geom_jitter(aes(x = race_group, y = N_pub, color = race_group), 
              show.legend = FALSE, alpha = 0.1, height = 0, size = 0.8) +
  geom_text(data = pr_race_group, aes(y = 68, x = race_group, label = label, 
                                      group = race_group, color = race_group), 
            size = 3, position = position_dodge(width = 0.9), show.legend = FALSE) + 
  facet_grid(cols = vars(all),
             labeller = labeller(all = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("maroon", "#0f7ba2")) + 
  scale_color_manual(values = c("maroon", "#0f7ba2")) +
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Ethnicity (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill = "white"), 
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

grid_3_1 <- ggplot(data = datD) + 
  geom_boxplot(aes(x = race_group, y = N_pub, fill = race_group, color = race_group), 
               alpha = 0.4, show.legend = FALSE) + 
  geom_jitter(aes(x = race_group, y = N_pub, color = race_group), 
              show.legend = FALSE, alpha = 0.1, height = 0, size = 0.8) +
  geom_text(data = pr_job_race_group, aes(y = 68, x = race_group, label = label, 
                                          group = race_group, color = race_group), 
            size = 3, position = position_dodge(width = 0.9), show.legend = FALSE) + 
  facet_grid(cols = vars(job_cat_simplified4),
             labeller = labeller(job_cat_simplified4 = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("maroon", "#0f7ba2")) + 
  scale_color_manual(values = c("maroon", "#0f7ba2")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Ethnicity (perceived)") + 
  theme1 + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        strip.background = element_rect(fill = "white"), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.line.y = element_blank())

figure_3_1 <- ggarrange(all_3_1 + rremove("xlab"), 
                        grid_3_1 + rremove("ylab") + rremove("xlab"),
                        labels = NULL,
                        ncol = 2, nrow = 1,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv", 
                        widths = c(0.2, 1),
                        font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure_3_1 <- annotate_figure(figure_3_1, 
                              bottom = textGrob("Ethnicity (perceived)", gp = gpar(cex = 1)))

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/pr_by_job_rg.png", 
       width = 9.4, height = 4, dpi = 600, units = "in", bg = "white")

# < Figure 3.2 Number of annual publications rates by author position and ethnicity ----
dat_count <- dat_anon |> 
  group_by(author_position, race_group) |>
  summarize(count = n_distinct(anon_id))

datE <- merge(dat_annual, dat_count, by = c("author_position", "race_group"), all.x = TRUE)

date <- datE |> 
  group_by(race_group, author_position) |>
  summarize(median = round(median(N_pub)),
            q25 = round(quantile(N_pub, probs = 0.25)),
            q75 = round(quantile(N_pub, probs = 0.75)), 
            count = mean(count)) |>
  mutate(label = paste0("n = ", count, ",\n", median, " (", q25, ", ", q75, ")"))

date <- date[!is.na(date$author_position),]
datE <- datE[!is.na(datE$author_position),]

grid_3_2 <- ggplot(data = datE) + 
  geom_boxplot(aes(x = race_group, y = N_pub, fill = race_group, color = race_group), 
               alpha = 0.4, show.legend = FALSE) + 
  geom_jitter(aes(x = race_group, y = N_pub, color = race_group), 
              show.legend = FALSE, alpha = 0.1, height = 0, size = 0.8) +
  geom_text(data = date, aes(y = 68, x = race_group, label = label, 
                             group = race_group, color = race_group), 
            size = 3, position = position_dodge(width = 0.9), show.legend = FALSE) + 
  facet_grid(cols = vars(author_position),
             labeller = labeller(author_position = label_wrap_gen(width = 15)),
             scales = "free") + 
  scale_fill_manual(values = c("maroon", "#0f7ba2")) + 
  scale_color_manual(values = c("maroon", "#0f7ba2")) + 
  coord_cartesian(ylim = c(0, 70)) + 
  labs(y = "Number of annual publications", x = "Ethnicity (perceived)") + 
  theme1 + theme(strip.background = element_rect(fill = "white"),
                 axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                 axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
                 axis.text.y = element_blank(), 
                 axis.ticks.y = element_blank(), 
                 axis.line.y = element_blank())

figure_3_2 <- ggarrange(all_3_1 + rremove("xlab"), 
                        grid_3_2 + rremove("ylab") + rremove("xlab"),
                        labels = NULL,
                        ncol = 2, nrow = 1,
                        common.legend = TRUE, legend = "bottom",
                        align = "hv", 
                        widths = c(0.4, 1),
                        font.label = list(size = 10, color = "black", face = "bold", family = NULL, position = "top"))

figure_3_2 <- annotate_figure(figure_3_2, 
                              bottom = textGrob("Ethnicity (perceived)", gp = gpar(cex = 1)))

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/authorposition_annual_race.png", 
       width = 9.4, height = 4, dpi = 600, units = "in", bg = "white")

# < Combined Figures: Stack authorposition_annual_gender and authorposition_annual_race ----
combined_authorposition <- ggarrange(figure_2_2, figure_3_2,
                                     labels = c("A", "B"),
                                     ncol = 1, nrow = 2,
                                     common.legend = TRUE, legend = "bottom",
                                     align = "hv")

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/authorposition_annual_combined.png", 
       plot = combined_authorposition, width = 9.4, height = 8, dpi = 1200, units = "in", bg = "white")

# < Combined Figures: Stack pr_by_job_gender and pr_by_job_rg ----
combined_pr_by_job <- ggarrange(figure_2_1, figure_3_1,
                                labels = c("A", "B"),
                                ncol = 1, nrow = 2,
                                common.legend = TRUE, legend = "bottom",
                                align = "hv")

ggsave("/Users/paulachristen/SPH Pub Bias/Figures/pr_by_job_combined.png", 
       plot = combined_pr_by_job, width = 9.4, height = 8, dpi = 1200, units = "in", bg = "white")

