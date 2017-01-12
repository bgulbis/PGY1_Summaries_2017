# tidy data

library(tidyverse)
library(stringr)
library(lubridate)

raw_applicants <- read_csv("data/raw/API_Applicants.csv")
raw_scores <- read_csv("data/raw/API_Application_Scores.csv")
raw_extracts <- read_csv("data/raw/API_Extraction.csv")
raw_references <- read_csv("data/raw/API_References.csv")
raw_vidyo <- read_csv("data/raw/API_Vidyo.csv")

interest_areas <- c("Not Specified", "Ambulatory Care", "Cardiology",
                    "Critical Care", "Emergency Medicine",
                    "Infectious Diseases", "Informatics", "Internal Medicine",
                    "Management", "Oncology", "Pediatrics", "Psychiatry",
                    "Transplant", "Other")

# applicant info ---------------------------------------
data_applicants <- raw_applicants %>%
    mutate(mhtmc_rec = `custom_field_mh-tmc_rec` == "Yes") %>%
    select(-starts_with("custom_field"), -starts_with("pharmacy_school"))

data_schools <- raw_applicants %>%
    select(cas_id, starts_with("pharmacy_school")) %>%
    mutate(school = if_else(pharmacy_school_name_0 == "NON-US/CANADIAN (FOREIGN) INSTITUTION",
                            pharmacy_school_name_1, pharmacy_school_name_0),
           gpa = if_else(pharmacy_school_name_0 == "NON-US/CANADIAN (FOREIGN) INSTITUTION",
                         pharmacy_school_gpa_1, pharmacy_school_gpa_0),
           grad_date = if_else(pharmacy_school_name_0 == "NON-US/CANADIAN (FOREIGN) INSTITUTION",
                               pharmacy_school_graduation_date_1, pharmacy_school_graduation_date_0)) %>%
    select(-starts_with("pharmacy_school"))

data_applicants <- left_join(data_applicants, data_schools, by = "cas_id")

write_rds(data_applicants, "data/tidy/data_applicants.Rds", "gz")

# areas of interests -----------------------------------
data_interests <- raw_applicants %>%
    select(cas_id, starts_with("custom_field_interest")) %>%
    gather(interest_num, interest, starts_with("custom_field_interest"), na.rm = TRUE) %>%
    mutate(interest_num = as.numeric(str_extract(interest_num, "[0-9]")))

write_rds(data_interests, "data/tidy/data_interests.Rds", "gz")

# reference names --------------------------------------
data_references <- raw_references %>%
    select(cas_id:evaluator_id_for_references_4) %>%
    gather(ref_num, writer_id, evaluator_id_for_references_0:evaluator_id_for_references_4, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id, writer_id)

tmp_ref_name_first <- raw_references %>%
    select(cas_id, reference_first_name_0:reference_first_name_4) %>%
    gather(ref_num, ref_first_name, reference_first_name_0:reference_first_name_4, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id) %>%
    arrange(ref_num)

tmp_ref_name_last <- raw_references %>%
    select(cas_id, reference_last_name_0:reference_last_name_4) %>%
    gather(ref_num, ref_last_name, reference_last_name_0:reference_last_name_4, na.rm = TRUE) %>%
    mutate(ref_num = as.numeric(str_extract(ref_num, "[0-9]"))) %>%
    group_by(cas_id) %>%
    arrange(ref_num)

tmp_ref_name <- inner_join(tmp_ref_name_first, tmp_ref_name_last, by = c("cas_id", "ref_num")) %>%
    filter(ref_last_name != "")

data_references <- inner_join(data_references, tmp_ref_name, by = c("cas_id", "ref_num"))

write_rds(data_references, "data/tidy/data_references.Rds", "gz")

# reference ratings ------------------------------------

# gather the ratings into long data format
tmp_ratings <- raw_references %>%
    select(cas_id, matches("reference_(.*)_rating_[0-4]$")) %>%
    gather(ref_num, rating, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_rating_([0-4])$") %>%
    dmap_at("ref_num", as.numeric)

# reference comments -----------------------------------

# gather the comments into long data format
tmp_comments <- raw_references %>%
    select(cas_id, matches("reference_(.*)_comments_[0-4]$")) %>%
    gather(ref_num, comment, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_comments_([0-4])$") %>%
    dmap_at("ref_num", as.numeric) %>%
    dmap_at("quality", str_replace, pattern = "_rating", replacement = "") %>%
    dmap_at("comment", str_replace_all, pattern = "(\\n|\\t)", replacement = "") %>%
    dmap_at("comment", str_trim, side = "both")

# combine rating and comments
quality_names <- c("assertiveness" = "Assertive",
                   "clinical_problem_solving_skills" = "Problem Solving",
                   "constructive_criticism" = "Criticism",
                   "dependability" = "Dependable",
                   "emotional_maturity" = "Maturity",
                   "independence" = "Independence",
                   "leadership" = "Leadership",
                   "oral_communication" = "Oral Comm",
                   "overall_code" = "Overall",
                   "patient_interactions" = "Patient Interact",
                   "peer_communication" = "Peer Comm",
                   "professionalism" = "Professional",
                   "time_management" = "Time Management",
                   "written_communication" = "Written Comm")

data_qualities <- full_join(tmp_ratings, tmp_comments, by=c("cas_id", "quality", "ref_num")) %>%
    arrange(cas_id, quality, ref_num) %>%
    dmap_at("quality", str_replace_all, pattern = quality_names)

write_rds(data_qualities, "data/tidy/data_qualities.Rds", "gz")

# program comments -------------------------------

# description, strengths, weakness, and other all contain no data

data_program_comments <- raw_references %>%
    select(cas_id,
           contains("improvement_areas"),
           contains("other_observances"),
           contains("description"),
           contains("reference_comments"),
           contains("strength")) %>%
    gather(ref_num, comment, starts_with("reference_"), na.rm = TRUE) %>%
    extract(ref_num, c("quality", "ref_num"), "reference_(.*)_([0-4])$") %>%
    dmap_at("ref_num", as.numeric) %>%
    dmap_at("quality", str_replace, pattern = "_rating", replacement = "") %>%
    dmap_at("comment", str_replace_all, pattern = "(\\n|\\t)", replacement = "") %>%
    dmap_at("comment", str_trim, side = "both")

write_rds(data_program_comments, "data/tidy/data_program_comments.Rds", "gz")

# LOI extraction --------------------------------------

question_names <- c("(.*)other_findings_in_cv(.*)" = "other_cv",
                    "(.*)other_statements(.*)" = "other_letter",
                    "(.*)what_are_your_goals(.*)" = "goals",
                    "(.*)what_can_you_bring(.*)" = "contributions",
                    "(.*)why_do_you_want(.*)" = "motivation",
                    "(.*)what_are_you_expecting(.*)" = "expectations",
                    "(.*)extraction_comments(.*)" = "reviewer_comments")

data_intent <- raw_extracts %>%
    select(cas_id, contains("comments")) %>%
    mutate_each(funs(as.character(.)), contains("comments")) %>%
    gather(question, response, contains("comments"), na.rm = TRUE) %>%
    dmap_at("question", str_replace_all, pattern = question_names) %>%
    dmap_at("response", str_replace_all, pattern = "(\\n|\\t)", replacement = "") %>%
    dmap_at("response", str_trim, side = "both") %>%
    dmap_at("response", str_replace_all, pattern = "â\u0080\u0099", replacement = "'") %>%
    dmap_at("response", str_replace_all, pattern = "â\u0080\u0093", replacement = "-") %>%
    spread(question, response)

write_rds(data_intent, "data/tidy/data_intent.Rds", "gz")

# CV extraction ----------------------------------------

data_cv <- raw_extracts %>%
    select(cas_id, contains("score"))

cv_names <- c("assignments_data_extraction_question_number_of_" = "",
              "_score" = "",
              "rotations_(.*)_centers" = "academic_rotations",
              "assignments_(.*)_rotations" = "num_rotations",
              "peer(.*)_publications" = "publications",
              "state_national_" = "")

names(data_cv) <- str_replace_all(names(data_cv), cv_names)

write_rds(data_cv, "data/tidy/data_cv.Rds", "gz")

# application scores -----------------------------------
data_scores <- raw_scores %>%
    dmap_if(is.character, str_replace_all, pattern = "(\\n|\\t)", replacement = "") %>%
    dmap_if(is.character, str_trim, side = "both") %>%
    dmap_if(is.character, str_replace_all, pattern = "Application Scoring: | - Poor Fit", "") %>%
    dmap_at(c("assignments_application_scoring_remark_0", "assignments_application_scoring_remark_1"), as.numeric)

scores_names <- c("assignments_application_scoring_question_" = "",
                  "_(.?[0-9])_to_[0-9]" = "",
                  "assignment(s)?_application_scoring" = "application",
                  "contributions_(.*)_patients" = "contributions",
                  "expecting_(.*)_residency" = "expectations",
                  "motivation_(.*)_residency" = "motivation",
                  "followed_(.*)_instructions" = "instructions",
                  "any_spelling_(.*)_application" = "spelling",
                  "other_(.*)_awarded" = "other",
                  "recommender_(.*)_us" = "known_recommender",
                  "review_(.*)_recommendations" = "recs",
                  "clinical_(.*)_competition" = "clin_skills",
                  "lcep(.*)_program" = "lcep",
                  "leadership_experience" = "leadership",
                  "long(.*)_goals" = "goals")

names(data_scores) <- str_replace_all(names(data_scores), scores_names)

write_rds(data_scores, "data/tidy/data_scores.Rds", "gz")