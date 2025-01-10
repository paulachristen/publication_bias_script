# merge publication data with sjr dataset ----------
clean_data_job_title <- read.csv(file.path("..", "interim",
                                           "clean_data_job_title_author_pos_ethn.csv"),
                                 stringsAsFactors = FALSE, encoding = "UTF-8")

d_convert_names_to_sjr <- readRDS(file.path("..", "..", "..",
                                            "impact_factor", "rds",
                                            "d_convert_names_to_sjr.rds")) %>%
  dplyr::select(old_journal_name, new_sjr_journal_name) %>%
  dplyr::rename(SJR.journal.name = old_journal_name)  # rename as in the publication dataset

sjr_rank_data <- readRDS(file.path("..", "..", "..",
                                   "impact_factor", "rds",
                                   "sjr_rank_data.rds")) %>%
  dplyr::rename(new_sjr_journal_name = title)

# add names as in the sjr dataset and merge sjr data
pub_convert_name <- clean_data_job_title %>%
  left_join(d_convert_names_to_sjr, by = "SJR.journal.name") %>%
  left_join(sjr_rank_data, by = "new_sjr_journal_name")

multiple_matches <- pub_convert_name %>%
  group_by(new_sjr_journal_name) %>%
  filter(n_distinct(sjr) > 1)

multiple_matches # shows rows with multiple matches

mult_match_drop <- multiple_matches %>% dplyr::filter(sjr_best_quartile %in% c("Q4")) # select matches to drop

pub_convert_name_filter <- anti_join(pub_convert_name, mult_match_drop, # drop wrong duplicate
                                     by = names(multiple_matches))

sum(is.na(pub_convert_name$new_sjr_journal_name)) # n of missing obs


# adding citations
pub_convert_name_filter <- pub_convert_name_filter %>%
  mutate(citations = pmax(Times.cited..Web.of.Science., Times.cited..Scopus.,
                          Times.cited..Europe.PubMed.Central., na.rm = T))

pub_convert_name_filter %>%
  write.csv(file.path("..", "interim", "clean_data_job_title_author_pos_ethn_sjr.csv"), row.names = FALSE)
