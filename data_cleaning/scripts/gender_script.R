# Install packages

library(gender)     # devtools::install_github("ropensci/gender") 
library(genderdata) # devtools::install_github("lmullen/genderdata")

# genderdata will take a few minutes to load, it is a large file

# Read in data - download from Hackathon OneDrive
dat <- read.csv("../raw/Publications_UserObjectPairs_From20140101_To20230605_School of Public Health_20230605.csv",
              stringsAsFactors = FALSE, encoding = "UTF-8")

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

# Extract additional author data
dat$N_authors <- NA 
dat$individual_position <- NA

dat[, c("N_authors", "individual_position")] <- t(
  # loop through all publications to find the number of authors and their author position
  sapply(1:nrow(dat), function(e){
    # create list where each element is one author
    author_list <- strsplit(dat$Authors.OR.Authors.Contributors[e],split = ",")[[1]]
    # count number of authors
    N_authors <- length(author_list)
    # extract department author first initial and last name
    author_first_init <- substr(strsplit(dat$Name[e], split = ",")[[1]][2],2,2)
    author_last_name <- strsplit(dat$Name[e], split = ",")[[1]][1]
    # find position of department author in the author list
    position <- grep(paste0(author_last_name, " ", author_first_init), toupper(author_list))
    # return the number of authors and the department author position
    if (length(position) == 0) position <- NA
    if (length(position) > 1) position <- position[1]
    return(c(N_authors, position))
  })
)

# Glance at distribution - some are in the 1,000s = group authorship
summary(dat$N_authors); summary(dat$individual_position)
test <- dat[dat$N_authors > 2000, c(1:5, grep("DOI", colnames(dat)), 150:151)]


# Assign gender
gender_author <- gender::gender(unique(dat$first_name), method = "ssa", years = c(1940, 2000)) # takes a while to run; genderize to many requests to api

# glance - most seem to have a good match based on median value
summary(gender_author$proportion_female)

# Match gender back to publications data
temp <- t(sapply(1:nrow(dat),function(e){
  # find position of matched name
  x <- which(gender_author$name %in% dat$first_name[e])
  if (length(x) == 0) res <- c(NA,NA) else {
    # select assigned gender and the likelihood of being male
    res <- c(gender_author$gender[x], as.numeric(gender_author$proportion_male[x]))
  }
  return(res)
}))

temp <- as.data.frame(temp)
names(temp) <- c("gender", "prob_male")
dat <- cbind(dat, temp) # attach back to data

table(dat$gender, useNA = "always") # 4,563 entries with NA

# Fix names which have no match
n <- which(is.na(dat$gender) | (dat$prob_male < 0.8 & dat$prob_male > 0.2))

n1 <- which(is.na(dat$gender))
n2 <- which((dat$prob_male < 0.8 & dat$prob_male > 0.2))

# no match due to missing assignment
u1 <- data.frame(firstname = unique(dat[n1, c("first_name","last_name")]), 
                        gender = NA)
# no match due to probability <0.8 >0.2
u2 <- data.frame(firstname = unique(dat[n2, c("first_name","last_name")]), 
                 gender = NA)

# no match total
unknown <- data.frame(firstname = unique(dat[n, c("first_name","last_name")]), 
                      gender = NA)

# print out to csv and revise
write.csv(unknown, file = "../interim/unknown_gender.csv", row.names = FALSE)

# read in manually reviewed genders
unknown_fix <- read.csv("../interim/unknown_gender_reviewed.csv")

# merge fixes
for (j in 1:nrow(unknown_fix)){
  dat$gender[which(dat$first_name %in% unknown_fix$firstname.first_name[j] &
                     dat$last_name %in% unknown_fix$firstname.last_name)] <- unknown_fix$gender[j]
}

table(dat$gender, useNA = "always") # 0 entries with NA = fixed!

# Create author position variable
dat$author_position <- "middle"
dat$author_position[dat$individual_position == 1 ] <- "first"
dat$author_position[dat$individual_position ==  dat$N_authors] <- "last"
dat$author_position[is.na(dat$individual_position)] <- NA

table(dat$author_position, useNA = "always") # NAs are group authors or misclassifications

# Save data
write.csv(dat, file = "../raw/Publications_UserObjectPairs_From20140101_To20230605_School of Public Health_20230605_gender.csv", row.names = FALSE)

# glance at results
table(dat$gender, useNA = "always") # gender
dat_u <- unique(dat[ , c("first_name", "last_name", "gender")]) # unique authors
table(dat_u$gender)

tt <- table(dat$gender, dat$author_position) # authors position by gender
tt[1, ] / (tt[2, ] + tt[1, ]) # author position by percentage female

