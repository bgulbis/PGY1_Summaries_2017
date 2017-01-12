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
    select(cas_id, starts_with("pharmacy_school"), contains("school_score")) %>%
    mutate(school = if_else(pharmacy_school_name_0 == "NON-US/CANADIAN (FOREIGN) INSTITUTION",
                            pharmacy_school_name_1, pharmacy_school_name_0),
           gpa = if_else(pharmacy_school_name_0 == "NON-US/CANADIAN (FOREIGN) INSTITUTION",
                         pharmacy_school_gpa_1, pharmacy_school_gpa_0),
           grad_date = if_else(pharmacy_school_name_0 == "NON-US/CANADIAN (FOREIGN) INSTITUTION",
                               pharmacy_school_graduation_date_1, pharmacy_school_graduation_date_0)) %>%
    select(-starts_with("pharmacy_school")) %>%
    rename(school_score = custom_field_school_score)

data_applicants <- left_join(data_applicants, data_schools, by = "cas_id")

write_rds(data_applicants, "data/tidy/data_applicants.Rds", "gz")

# areas of interests -----------------------------------
data_interests <- raw_applicants %>%
    select(cas_id, starts_with("custom_field_interest")) %>%
    gather(interest_num, interest, starts_with("custom_field_interest"), na.rm = TRUE) %>%
    dmap_at("interest_num", str_replace_all, pattern = "custom_field_", replacement = "") %>%
    spread(interest_num, interest)

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
tmp_remarks <- c("assignments_application_scoring_remark_0", "assignments_application_scoring_remark_1")

scores_names <- c("application_scoring_question_|any_scores_of_|_for_preceptors|_as_a_licensed" = "",
                  "_(.?[0-9]*)_to_[0-9]|_(-1)" = "",
                  "application_scoring" = "application",
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

tmp_scores_num <- raw_scores %>%
    select(cas_id, matches("assignments_(.*)_score_[0-1]$")) %>%
    gather(review_num, score, starts_with("assignments_"), na.rm = TRUE) %>%
    extract(review_num, c("item", "review_num"), "assignments_(.*)_score_([0-1])$") %>%
    dmap_at("item", str_replace_all, pattern = scores_names) %>%
    dmap_at("review_num", as.numeric) %>%
    spread(item, score)

tmp_scores_comment <- raw_scores %>%
    select(cas_id, matches("assignments_(.*)_comments_[0-1]$")) %>%
    gather(review_num, notes, starts_with("assignments_"), na.rm = TRUE) %>%
    extract(review_num, c("item", "review_num"), "assignments_(.*)_comments_([0-1])$") %>%
    dmap_at("item", str_replace_all, pattern = scores_names) %>%
    dmap_at("item", ~ paste0(.x, "_comments")) %>%
    dmap_at("review_num", as.numeric) %>%
    spread(item, notes)

data_scores <- raw_scores %>%
    select(cas_id, contains("reviewer"), contains("remark")) %>%
    gather(review_num, value, starts_with("assignment"), na.rm = TRUE) %>%
    dmap_at("review_num", str_replace_all, pattern = "assignments", replacement = "assignment") %>%
    extract(review_num, c("item", "review_num"), "(reviewer|remark)_([0-1])$") %>%
    spread(item, value) %>%
    dmap_at("remark", str_extract, pattern = "[0-9]") %>%
    dmap_at(c("review_num", "remark"), as.numeric) %>%
    left_join(tmp_scores_num, by = c("cas_id", "review_num")) %>%
    left_join(tmp_scores_comment, by = c("cas_id", "review_num")) %>%
    rename(remark_application = remark,
           score_application = application)

write_rds(data_scores, "data/tidy/data_scores.Rds", "gz")

data_scores_total <- data_scores %>%
    mutate(review_group = if_else(reviewer == "Gulbis, Brian" | reviewer == "Schepcoff, Sara", 1, 2)) %>%
    select_if(is.numeric) %>%
    group_by(cas_id, review_group) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    select(-review_num)

write_rds(data_scores_total, "data/tidy/data_scores_total.Rds", "gz")

# vidyo interviews -------------------------------------
data_vidyo <- raw_vidyo %>%
    select(-designation_program_lookup_id) %>%
    dmap_if(is.character, str_replace_all, pattern = "(\\n|\\t)", replacement = "") %>%
    dmap_if(is.character, str_trim, side = "both") %>%
    dmap_at("interview_vidyo_interview_remarks", str_extract, pattern = "[0-9]") %>%
    dmap_at("interview_vidyo_interview_remarks", as.numeric)

vidyo_names <- c("interview(s)?_vidyo_interview_(question_)?" = "",
                 "critical_(.*)_task" = "crit_think",
                 "time_(.*)_assignments" = "time_mgmt",
                 "self(.*)_obstacle" = "prob_solve",
                 "integrity_(.*)_harmed" = "integrity")

names(data_vidyo) <- str_replace_all(names(data_vidyo), vidyo_names)

data_vidyo <- data_vidyo %>%
    rename(remark_vidyo = remarks,
           score_vidyo = score,
           comments_vidyo = comments)

write_rds(data_vidyo, "data/tidy/data_vidyo.Rds", "gz")

# application summary ----------------------------------
results_application <- select(data_applicants, cas_id:last_name, school_score:grad_date) %>%
    left_join(data_interests, by = "cas_id") %>%
    left_join(data_scores_total, by = "cas_id") %>%
    left_join(data_cv, by = "cas_id") %>%
    left_join(select_if(data_vidyo, is.numeric), by = "cas_id") %>%
    group_by(cas_id) %>%
    mutate(overall.score = sum(school_score, score_application, score_vidyo, na.rm = TRUE),
           avg.fit = mean(c(remark_application, remark_vidyo), na.rm = TRUE),
           low.fit = remark_application < 3 | remark_vidyo < 3,
           low.school = school_score < 2) %>%
    ungroup %>%
    arrange(desc(overall.score), desc(avg.fit)) %>%
    select(cas_id, last_name, first_name, school, overall.score, avg.fit, low.fit, low.school, everything())

write_csv(results_application, "data/final/application_scoring.csv")

results_application_summary <- results_application %>%
    select(last_name:review_group)

write_csv(results_application_summary, "data/final/application_scoring_summary.csv")
