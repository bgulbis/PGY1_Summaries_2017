# create applicant summaries

library(tidyverse)
library(stringr)
library(ReporteRs)

dirr::get_rds("data/tidy")

calc_scores <- data_scores_total %>%
    group_by(cas_id) %>%
    mutate(
        score_loi = sum(goals, contributions, expectations, motivation,
                        areas_of_interest, na.rm = TRUE),
        score_cv = sum(clin_skills, lcep, rotations, inpatient_rotations,
                       experience_pharmacist, additional_degree,
                       work_experience, publications,
                       poster_presentations, platform_presentation,
                       leadership, no_contact_information,
                       no_rotation_descriptions, na.rm = TRUE),
        score_ref = sum(recs, known_recommender, fails_to_meet,
                        na.rm = TRUE),
        score_other = sum(other, any_formatting_issues, instructions,
                          spelling, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(text_loi = paste0(score_loi, " (", round(percent_rank(score_loi) * 100, 0), "th)"),
           text_cv = paste0(score_cv, " (", round(percent_rank(score_cv) * 100, 0), "th)"),
           text_ref = paste0(score_ref, " (", round(percent_rank(score_ref) * 100, 0), "th)"),
           text_other = paste0(score_other, " (", round(percent_rank(score_other) * 100, 0), "th)"),
           text_application = paste0(score_application, " (", round(percent_rank(score_application) * 100, 0), "th)"))

calc_vidyo <- data_vidyo %>%
    mutate(text_vidyo = paste0(score_vidyo, " (", round(percent_rank(score_vidyo) * 100, 0), "th)"),
           interview_group = if_else(interviewer == "Gulbis, Brian", 1, 2))

calc_overall <- data_scores_overall %>%
    mutate(text_overall = paste0(score_overall, " (", round(percent_rank(score_overall) * 100, 0), "th)"))

hdr_application <- c("Letter of Intent", "Curriculum Vitae", "Letters of Reference", "Reviewer Fit", "Reviewer")
hdr_vidyo <- c("Application Score", "Vidyo Score", "Total Score", "Interviewer Fit", "Interviewer")
hdr_school <- c("GPA", "Grad Date", "School Score", "Known Rec", "")
hdr_intent <- c("Motivation for residency", "Expectating from residency", "Contributions to hospital", "Career goals", "Other statements")

# for (i in 1:nrow(data_applicants)) {
for (i in 1:5) {
    app.id <- data_applicants$cas_id[i]

    applicant <- data_applicants %>%
        filter(cas_id == app.id) %>%
        mutate(applicant_name = paste(last_name, first_name, sep = ", "))

    school <- data_schools %>%
        filter(cas_id == app.id) %>%
        dmap_at("grad_date", as.character)

    interests <- data_interests %>%
        filter(cas_id == app.id) %>%
        mutate(interest = str_c(interest_1, interest_2, interest_3, sep = ", "))

    letter_intent <- filter(data_intent, cas_id == app.id)

    cv <- filter(data_cv, cas_id == app.id)

    application_score <- filter(calc_scores, cas_id == app.id)

    reference <- data_references %>%
        filter(cas_id == app.id) %>%
        mutate(writer = paste(ref_last_name, ref_first_name, sep = ", "))

    vidyo <- filter(calc_vidyo, cas_id == app.id)

    overall <- filter(calc_overall, cas_id == app.id)

    # application summary ------------------------------
    col_application <- c(
        application_score$text_loi,
        application_score$text_cv,
        application_score$text_ref,
        application_score$remark_application,
        application_score$review_group
    )

    col_vidyo <- c(
        application_score$text_application,
        vidyo$text_vidyo,
        overall$text_overall,
        vidyo$remark_vidyo,
        vidyo$interview_group
    )

    col_school <- c(
        school$gpa,
        school$grad_date,
        school$school_score,
        applicant$mhtmc_rec,
        ""
    )

    col_writers <- vector("character", 5)
    for (j in 1:nrow(reference)) {
        col_writers[j] <- reference$writer[j]
    }

    df_scores <- tibble(
        Application = hdr_application,
        Scores = col_application,
        Interview = hdr_vidyo,
        Assessment = col_vidyo,
        School = hdr_school,
        Values = col_school,
        References = col_writers
    )

    tbl_scores <- vanilla.table(df_scores)
    tbl_scores[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    tbl_scores[] <- textProperties(font.size = 8, font.family = "Calibri")
    tbl_scores[3, 3:4] <- textProperties(font.size = 8, font.family = "Calibri", font.weight = "bold")
    tbl_scores[, c(2,4,6)] <- parCenter()

    # letter of intent summary -------------------------

    col_intent <- c(
        application_score$motivation,
        application_score$expectations,
        application_score$contributions,
        application_score$goals,
        NA
    )

    col_comment <- c(
        letter_intent$motivation,
        letter_intent$expectations,
        letter_intent$contributions,
        letter_intent$goals,
        letter_intent$other_letter
    )

    df_intent <- tibble(
        Attribute = hdr_intent,
        Score = col_intent,
        Statement = col_comment
    )

    tbl_intent <- vanilla.table(df_intent) %>%
        setFlexTableWidths(widths = c(1.5, 0.75, 5.25)) %>%
        setZebraStyle(odd = "#eeeeee", even = "white")

    tbl_intent[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    tbl_intent[, to = "header"] <- parCenter()
    tbl_intent[] <- textProperties(font.size = 8, font.family = "Calibri")
    tbl_intent[, 2] <- parCenter()
    tbl_intent[, 3] <- parLeft()

    # make Word document -------------------------------
    mydoc <- docx(template = "ref/template_applicant_summary.docx") %>%
    # styles(mydoc)
        map_title(stylenames = c("Heading1", "Heading2", "Heading3")) %>%
        addParagraph(paste0(applicant$applicant_name, " - ", school$school),
                     stylename = "Heading1", bookmark = "Start") %>%
        addParagraph(paste0("Interests: ", interests$interest),
                     stylename = "Heading2") %>%
        addTitle("Summary of Scores", level = 3) %>%
        addFlexTable(tbl_scores) %>%
        addTitle("Letter of Intent", level = 3) %>%
        addFlexTable(tbl_intent)

    file.name <- paste0("report/summaries/", applicant$last_name, "_", applicant$first_name, ".docx")
    writeDoc(mydoc, file = file.name)

}
