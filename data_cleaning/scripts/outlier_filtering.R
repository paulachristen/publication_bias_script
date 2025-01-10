# imports
library(tidyverse)


# creating output for manual outlier checking:
dat <- read.csv(file.path("..", "interim", "clean_data_job_title_author_pos_ethn_sjr.csv"),
                stringsAsFactors = FALSE, encoding = "UTF-8")

dat_summarised <- dat %>%
  group_by(Username, Name, gender, job_cat_simplified, job_title) %>%
  summarize(n_pub = n(),
            n_citations = sum(citations,
                              na.rm = TRUE))

# Looking at outliers visually
dat_summarised %>%
  ggplot(aes(x=job_cat_simplified, y=n_pub, fill=gender)) +
  geom_boxplot(position = "dodge") +
  labs(y="Publication count", x = "Job category") +
  theme_bw()

dat_summarised %>%
  filter(!is.na(n_citations)) %>%
  ggplot(aes(x=job_cat_simplified, y=n_citations, fill=gender)) +
  geom_boxplot(position = "dodge") +
  labs(y="Citation count", x = "Job category") +
  theme_bw()

# Exporting rows with low publication counts across all categories
dat_summarised <- dat_summarised %>%
  group_by(job_cat_simplified, gender) %>%
  mutate(pub_quartile = ntile(n_pub, 4),
         citation_quartile = ntile(n_citations, 4))

dat_summarised %>% write.csv(file.path("..",
                                       "manual_checking",
                                       "outlier_detection.csv"),
                             row.names = FALSE)

# Getting a list of usernames of outliers to exclude from the final dataset
# Notes:
# In total there are:
# 6 Emeritus professors: 5 in Q1 and 1 in Q2
# 2 Emeritus Readers: 1 in Q1 and 1 in Q2
dat_summarised %>%
  filter(grepl("Emeritus", job_title))

# Exclude Emeritus in Q1:
usernames_to_exclude <- dat_summarised %>%
  filter((grepl("Emeritus", job_title) & pub_quartile==1)) %>%
  pull(Username)

# Total number of rows
dat_summarised %>% NROW()

# Remove Emeritus Professors in Q1:
dat_summarised <- dat_summarised %>%
  filter(!(grepl("Emeritus", job_title) & pub_quartile==1))

# Removes 6 rows: 524 row to 518
dat_summarised %>% NROW()

# Recalculate quartiles with Emeritus removed
dat_summarised <- dat_summarised %>%
  group_by(job_cat_simplified, gender) %>%
  mutate(pub_quartile = ntile(n_pub, 4),
         citation_quartile = ntile(n_citations, 4))

dat_summarised %>% filter(job_cat_simplified == "Professor",
                          pub_quartile == 1)  %>%
  arrange(n_pub)

# Checking if we can use mean + 2sd or Q1 - 1.5IQR for an outlier cutoff point
dat_summarised %>% group_by(job_cat_simplified, gender) %>%
  summarise(mean=mean(n_pub),
            two_sd=2*sd(n_pub),
            sd_cutoff = mean - two_sd,
            iqr = IQR(n_pub),
            iqr_cutoff = quantile(n_pub, probs=c( .25), na.rm = FALSE)-1.5*iqr)

# Above cut-offs are too low so look at histogram to get a visual cut-off (not ideal)
dat_summarised %>%
  filter(job_cat_simplified=="Professor" & gender=="male") %>%
  ggplot(aes(x=n_pub)) +
  geom_histogram() +
  geom_vline(xintercept=25, colour="red", linetype="longdash") +
  theme_bw()

# From the above a cut point of 25 looks reasonable; male Profs to exclude are below:
dat_summarised %>%
  filter(job_cat_simplified=="Professor" & gender=="male" & n_pub <= 25) %>%
  ungroup() %>%
  arrange(n_pub) %>%
  select(Username, Name, n_pub, n_citations)

profs_to_exlcude <- dat_summarised %>%
  filter(job_cat_simplified=="Professor" & gender=="male" & n_pub <= 25) %>%
  ungroup() %>%
  pull(Username)

usernames_to_exclude <- c(usernames_to_exclude, profs_to_exlcude)

# 11 users to exclude
usernames_to_exclude %>% NROW()

dat <- dat %>%
  filter(! Username %in% usernames_to_exclude)

# Check that filtering has worked: we should now have 513 rows (524 - 11)
dat %>% distinct(Username) %>% NROW()

dat %>%
  write.csv(file.path("..", "shared", "clean_data_final.csv"),
            row.names = FALSE)

# -------------------------------- Manual Check --------------------------------
# An example of how to manually check whether the final pub totals are correct
dat_raw <- read.csv(file.path("..",
                              "raw",
                              "Publications_UserObjectPairs_From20140101_To20230605_School of Public Health_20230605_gender.csv"))

# Username from usernames_to_exclude; e.g. dewolf
username <- "dewolf"

# The table below lists all article sub-types; the sum of the sub-types we're
# interested in should sum to the total number of publications listed for the
# author in dat_summarise
dat_raw %>%
  filter(Username==username) %>%
  distinct(ID, .keep_all = TRUE) %>%
  group_by(Sub.types) %>%
  summarise(n=n())
