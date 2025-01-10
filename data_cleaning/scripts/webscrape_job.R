# Load packages

library(rvest)
library(dplyr)

# See example of using rvest here: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

# Read in clean_data.csv created through Data_cleaning.Rmd
dat <- read.csv("../raw/clean_data.csv")

# Extract first name
name_split <- strsplit(dat$Name,", ")

dat$first_name <- unlist(lapply(name_split, function(x){
  temp <- x[[2]] # extract first name & middle initial
  temp <- strsplit(temp," ")[[1]][1] # pull just first name
  return(temp)
}))

# Extract last name
dat$last_name <- unlist(lapply(name_split, function(x){
  temp <- x[[1]] # extract last name
  return(temp)
}))

# Extract username from email address
dat$webname <- gsub("@imperial.ac.uk", "", dat$Email)

# Creating new variables for job title
names <- dat[, c("Name", "webname", "first_name", "last_name")]
names <- names[!duplicated(names), ]
names$job_title <- NA  # full title on website
names$job_title2 <- NA  # full title on website 2 (some sites have different labeling)
names$short_job_title <- NA  # title before name e.g. "professor"

# Scrape each person's individual PWP
for(i in 1:nrow(names)) {
  
  # create URL of PWP
  url <- paste0("https://www.imperial.ac.uk/people/", names$webname[i])

  # read the HTML code from the website
  webpage <- rvest::read_html(url)

  # use CSS selectors to scrape the rankings section
  # name_data_html <- html_nodes(webpage,".ac_info")
  job_data_html <- html_nodes(webpage, "#titlepart6")
  short_job_data_html <- html_nodes(webpage, "#titlepart1")
  banner_html <-html_nodes(webpage, "#bannername")  # "titlepart1 html node doesn"t work for everyone

  # convert data to text
  # name_data <- html_text(name_data_html)
  if(length(html_text(job_data_html)) > 0) {
    names$job_title[i] <- html_text(job_data_html)
  }
  if(length(html_text(short_job_data_html)) > 0) {
    names$short_job_title[i] <- html_text(short_job_data_html)
  }
  if(length(html_text(banner_html)) > 0) {
    names$job_title2[i] <- html_text(banner_html)
  }

}

# glance
table(names$job_title, useNA = "always")
table(names$job_title2, useNA = "always")
table(names$short_job_title, useNA = "always")
names0 <- names  #(store to avoid redoing)

# Scrape the ICL "people" site, as everyone does not have PWPs
get_deets <- function(node){
  
  # extract name and job title
  r.name <- html_nodes(node, ".person-name") |> rvest::html_text()
  r.job_title <- html_nodes(node, ".job-title") |> rvest::html_text()

  data.frame(
    name = ifelse(length(r.name) == 0, NA, r.name),
    job_title = ifelse(length(r.job_title) == 0, NA, r.job_title), stringsAsFactors = F)
}

# loop through the A-Z people pages
names2 <- NULL  # set up another names dataset

for(i in 1:58) {  ## there are 58 pages of names at the time of scraping (see URL below)
  if(i == 1) url <- "https://www.imperial.ac.uk/school-public-health/about-us/a-z-people/"
  if(i > 1) url <- paste0("https://www.imperial.ac.uk/school-public-health/about-us/a-z-people/?instanceid=%2fmedicine%2fpublichealth%2faboutus%2fazpeople&pplist-action=people.html&page=", i)

  # read the HTML code from the website
  webpage <- read_html(url)

  # use CSS selectors to scrape the rankings section
  combined <- html_nodes(webpage,".name-wrapper")
  curr_names <- lapply(combined, get_deets) |> bind_rows()

  names2 <- rbind(names2, curr_names)

}

head(names2)

# Process names2 to get first name, last name
# Extract first name
name_split <- strsplit(names2$name," ")
names2$first_name <- unlist(lapply(name_split, function(x){
  temp <- x[[2]] # extract first name & middle initial
  return(temp)
}))

# Extract last name
names2$last_name <- unlist(lapply(name_split, function(x){
  temp <- x[[length(x)]] # extract last name
  return(temp)
}))

# Rearrange 
names2 <- names2 |>
  mutate(first_last_name = paste0(tolower(first_name), " ", tolower(last_name))) |>
  select(first_last_name, job_title) |>
  rename(job_title_az = job_title)


# Merge all names and job titles data
names$first_last_name <- paste0(tolower(names$first_name)," ",tolower(names$last_name))
names <- left_join(names, names2, by = "first_last_name", multiple = "first")

# Save file
write.csv(names,"../interim/job_titles_scraped0.csv", row.names = F)

# Categorize jobs - find index of job titles which match and assign titles
# glance
table(names$job_title, useNA = "always")
table(names$job_title2, useNA = "always")
table(names$short_job_title, useNA = "always")

names$job_cat <- NA

# Assign categories from PWP pages
inds <- grep("Professor",names$short_job_title, ignore.case = T)
names$job_cat[inds] <- "Professor"
inds <- grep("Professor", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Professor"
inds <- grep("Professor", names$job_title2, ignore.case = T)
names$job_cat[inds] <- "Professor"
inds <- grep("Reader", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Reader"
inds <- grep("Senior Lecturer", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Senior Lecturer"
inds <- grep("Research Fellow", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Research Fellow"
inds <- grep("Sir Henry Dale Fellow", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Research Fellow"
inds <- grep("MRC Skills Development Fellow", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Research Fellow"
inds <- grep("Research Associate", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Research Associate"
inds <- grep("Research Assistant", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Research Assistant"
inds <- grep("Teaching Fellow", names$job_title, ignore.case = T)
names$job_cat[inds] <- "Teaching Fellow"

# glance
table(names$job_cat, useNA = "always")

# create variable with the remaining unmatched job titles
# fill in data from people A-Z pages
names$unmatched_job_title <- names$job_title
names$unmatched_job_title[which(!is.na(names$job_cat))] <- NA
names$unmatched_job_title_az <- names$job_title_az
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Lecturer", names$unmatched_job_title, ignore.case = T)
names$job_cat[inds] <- "Lecturer"
names$unmatched_job_title <- names$job_title
names$unmatched_job_title[which(!is.na(names$job_cat))] <- NA
inds <- grep("Chair",names$unmatched_job_title, ignore.case = T)
names$job_cat[inds] <- "Professor"
inds <- grep("Research Fellow", names$unmatched_job_title_az, ignore.case = T)
names$job_cat[inds] <- "Research Fellow"
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Reader",names$unmatched_job_title_az, ignore.case = T)
names$job_cat[inds] <- "Reader"
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Research Associate", names$unmatched_job_title_az, ignore.case = T)
names$job_cat[inds] <- "Research Associate"
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Professor", names$unmatched_job_title_az, ignore.case = T)
names$job_cat[inds] <- "Professor"
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Research Assistant", names$unmatched_job_title_az, ignore.case = T)
names$job_cat[inds] <- "Research Assistant"
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Lecturer", names$unmatched_job_title_az, ignore.case = T)
names$job_cat[inds] <- "Lecturer"
names$unmatched_job_title_az[which(!is.na(names$job_cat))] <- NA
inds <- grep("Research Postgraduate", names$unmatched_job_title, ignore.case = T)
names$job_cat[inds] <- "Research Associate"
names$unmatched_job_title[which(!is.na(names$job_cat))] <- NA

# Glace at remaining unclassified job titles
table(names$unmatched_job_title)
table(names$unmatched_job_title_az)

# these can be labeled as "other" for now, to revisit later
inds <- which(!is.na(names$unmatched_job_title_az))
names$job_cat[inds] <- "Other"
inds <- which(!is.na(names$unmatched_job_title))
names$job_cat[inds] <- "Other"

# look through unknowns manually
test <- subset(names, is.na(job_cat))
test <- arrange(test, first_last_name)
head(test)

# print out to csv and revise
write.csv(test, file = "../interim/names_unknown_job.csv", row.names = F)

# read in manually reviewed jobs
unknown_fix <- read.csv("../interim/names_unknown_job_revised.csv") |>
  select(Name, job_cat) |>
  rename(job_cat2 = job_cat)

# merge fixes
names <- names |> 
  left_join(unknown_fix, by = c("Name")) |>
  mutate(job_cat1 = job_cat,
         job_cat = case_when(is.na(job_cat) ~ job_cat2,
                             TRUE ~ job_cat1)) |>
  select(-job_cat1, -job_cat2)

table(names$job_cat, useNA = "always") # 0 entries with NA = fixed!

# Merge job category into overall dataset
names_jobs_only <- names |> select(Name, job_title:job_cat, -first_last_name)

dat2 <- dat |> left_join(names_jobs_only, by = "Name")

table(dat2$job_cat, useNA = "always")

# Save data
write.csv(dat2, file = "../interim/clean_data_job_title.csv", row.names = F)

