# Publications

Cleaning and analyzing publications data January 2014 to March 2023.

## Directory - run scripts in this order

```
.
├── gender_script.R                     # Assigns gender to each individual based on first name
├── Data_cleaning.RMd                   # Cleaning (removes non-standard publications and IDs group authorship)
├── webscrape_job.R                     # Webscrapes each individual's job
├── ethnicity.R                         # Ethnicity
├── summary_with_job_level.RDM          # Old analysis
├── descriptives.R                      # Run descriptive stats underlying the manuscript
├── models.R                            # Run models presented in manuscript
├── plots.R                             # Create plots for paper
└── README.md                         	# Overview

```
## Analysis
All analysis code can be found in the `analysis` subfolder.

## Data
Data is kept in the `data_cleaning` folder. Scripts used to clean the raw data are kept in this subfolder.
There are 4 data types in this subfolder: `raw`, `interim`, `manual_checking`, and `shared`.
`raw` contains the raw data used. `interim` contains the interim datasets created while cleaning the raw data.
Estimating the missing author positions requires some manual checking, the files used to do this are stored in 
`manual_checking`. The final dataset is stored in `shared`; all publication data shared across analyses should be stored here. 

For data up to 5 June 2023 downloaded from Symplectic, find on the Hackathon OneDrive or on Github. 
The file is entitled: Publications_UserObjectPairs_From20140101_To20230605_School of Public Health_20230605.csv

Other data files:

```
# Symplectic data
.
├── Journal articles_UserObjectPairs_From20141217_To20191217_School of Public Health_20191217.csv         # Original file
├── Journal articles_UserObjectPairs_From20141217_To20191217_School of Public Health_20191217_gender.csv  # With gender variable added
├── Publications_UserObjectPairs_From20140101_To20230605_School of Public Health_20230605.csv             # Original file
└── Publications_UserObjectPairs_From20140101_To20230605_School of Public Health_20230605_gender.csv      # With gender variable added

# In-between files: gender_script.R
.
├── unknown_gender.csv            # Names with unmatched gender
└── unknown_gender_reviewed.csv   # Manually matched genders (by hand)

# In-between files: Data_cleaning.Rmd
.
└── clean_data.csv   # Cleaning out non-journal articles and assigning alphabetical authorship

# In-between files: webscrape_job.R
. 
├── job_titles_scraped0.csv        # Data with job titles scraped from PWP and ICL A-Z staff directory
├── names_unknown_job.csv          # Names with unknown job categories
├── names_unknown_job_revised.csv  # Manually matched job categories (by hand)
└── clean_data_job_title.csv       # Clean data with gender and job category

# In-between files: author_position_update.Rmd
. 
└── clean_data_job_title_author_pos.csv  # Estimates NA author positions

# In-between files: Analysis_gender_ethnicity_cleaning.R
. 
└── clean_data_job_title_author_pos_ethn.csv  # Clean dataset with ethnicity, simplified job categories, and covid time + search term

# In-between files: adding_sjr_data.R
.
└── clean_data_job_title_author_pos_ethn_sjr.cs  # Clean dataset with sjr impact factor variables

# In-between files: outlier_filtering.R
.
└── clean_data_final.csv  # Final clean dataset with lower outlier Professors and Emeritus Readers/Professors removed


```

## R package versions

gender: v0.6.0

genderdata: v0.6.0

rvest: v1.0.3

glmmTMB: v1.1.9

tidyverse: v2.0.0

lubridate: v1.9.3

MASS: v7.3.60.2

broom: v1.0.5

dplyr: v1.1.4

