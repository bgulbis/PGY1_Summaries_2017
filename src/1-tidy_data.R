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
