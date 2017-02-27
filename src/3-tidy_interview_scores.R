# on-site interview scores

library(tidyverse)
library(stringr)

raw_scores <- read_csv("data/raw/API_Interviews.csv")
results_application <- read_rds("data/final/application_scoring.Rds")

# fit scores -------------------------------------------
interview_remarks <- raw_scores %>%
    select(cas_id, contains("remarks")) %>%
    dmap_if(is.character, str_extract, pattern = ": ([0-9])") %>%
    gather(interviewer, score, -cas_id) %>%
    dmap_at("interviewer", str_replace_all, pattern = "1-2", replacement = "ec") %>%
    dmap_at("interviewer", str_replace_all, pattern = "3-4", replacement = "to") %>%
    dmap_at("score", str_extract, pattern = "([0-9])") %>%
    dmap_at("score", as.numeric) %>%
    dmap_at("interviewer", str_replace_all, pattern = "interview_|_remarks_[0-1]|traditional_", replacement = "") %>%
    dmap_at("interviewer", str_replace_all, pattern = "coachability", replacement = "rpd") %>%
    group_by(cas_id, interviewer) %>%
    summarize_at("score", mean, na.rm = TRUE) %>%
    spread(interviewer, score) %>%
    mutate(interview_fit_mean = mean(mmi_ec, mmi_to, preceptor, residents, rpd),
           interview_fit_total = sum(mmi_ec, mmi_to, preceptor, residents, rpd))

# interview scores -------------------------------------

interview_results <- raw_scores %>%
    select(cas_id, contains("score")) %>%
    select(cas_id, contains("question")) %>%
    gather(interviewer, score, -cas_id) %>%
    dmap_at("interviewer", str_replace_all, pattern = "1-2", replacement = "ec") %>%
    dmap_at("interviewer", str_replace_all, pattern = "3-4", replacement = "to") %>%
    dmap_at("interviewer", str_replace_all, pattern = "(interview|preceptor|question)_|-_|_score_[0-1]", replacement = "") %>%
    dmap_at("interviewer", str_replace_all, pattern = "_mmi_[0-9]", replacement = "") %>%
    dmap_at("interviewer", str_replace_all, pattern = "time_management", replacement = "timeMgmt") %>%
    dmap_at("interviewer", str_replace_all, pattern = "critical_thinking", replacement = "critThink") %>%
    dmap_at("interviewer", str_replace_all, pattern = "difficult_person", replacement = "dfcltPerson") %>%
    dmap_at("interviewer", str_replace_all, pattern = "_(difficult_decision|large_workload)", replacement = "A") %>%
    dmap_at("interviewer", str_replace_all, pattern = "_(short_deadline|unclear_expectations)", replacement = "B") %>%
    dmap_at("interviewer", str_replace_all, pattern = "(critical|new_drug_disease_state|stressful_situation)_", replacement = "") %>%
    group_by(cas_id, interviewer) %>%
    summarize_at("score", mean, na.rm = TRUE)

interview_questions <- spread(interview_results, interviewer, score)

interview_totals <- interview_results %>%
    group_by(cas_id) %>%
    summarize_at("score", funs(mean, sum)) %>%
    rename(interview_mean = mean,
           interview_total = sum)

results_interviews <- interview_totals %>%
    left_join(interview_questions, by = "cas_id") %>%
    left_join(interview_remarks, by = "cas_id") %>%
    left_join(results_application, by = "cas_id") %>%
    select(cas_id, last_name:school, interview_mean, interview_total,
           interview_fit_mean, interview_fit_total, score_application,
           score_vidyo, remark_vidyo, interest_1, interest_2, interest_3,
           everything()) %>%
    arrange(desc(interview_fit_total), desc(interview_total))

results_summary <- select(results_interviews, cas_id:interest_3)

write_rds(results_interviews, "data/final/results_interviews.rds", "gz")
write_csv(results_interviews, "data/final/results_interviews.csv")
write_rds(results_summary, "data/final/results_summary.rds", "gz")
write_csv(results_summary, "data/final/results_summary.csv")
