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

# determine how many references there were for each candidate, remove duplicates
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
