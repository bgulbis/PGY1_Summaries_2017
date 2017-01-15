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

cv_comments <- data_scores %>%
    dmap_at("cas_id", as.character) %>%
    select_if(is.character) %>%
    dmap_at("cas_id", as.numeric)

hdr_application <- c("Letter of Intent", "Curriculum Vitae", "Letters of Reference", "Reviewer Fit", "Reviewer")
hdr_vidyo <- c("Application Score", "Vidyo Score", "Total Score", "Interviewer Fit", "Interviewer")
hdr_school <- c("GPA", "Grad Date", "School Score", "Known Rec", "")
hdr_intent <- c("Motivation for residency", "Expectating from residency", "Contributions to hospital", "Career goals", "Other statements")
hdr_cv <- c("Leadership", "Posters / Platform Presentations", "Research / Publications", "Work Experience", "Rotations / Acute Care / Academic", "Longitudinal APPE Program", "Clinical Skills Winner")
hdr_vidyo_scores <- c("Critical Thinking", "Time Management", "Problem Solving", "Integrity")

for (i in 1:nrow(data_applicants)) {
# for (i in 1:5) { # for testing
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

    cv_notes <- cv_comments %>%
        filter(cas_id == app.id) %>%
        dmap_if(is.character, str_c, collapse = "; ") %>%
        distinct(.keep_all = TRUE)

    application_score <- filter(calc_scores, cas_id == app.id)

    reference <- data_references %>%
        filter(cas_id == app.id) %>%
        mutate(writer = paste(ref_last_name, ref_first_name, sep = ", "))

    qualities <- data_qualities %>%
        filter(cas_id == app.id) %>%
        filter((quality %in% c("Problem Solving", "Time Management", "Maturity", "Independence") |
                    rating == "Fails to Meet" |
                    str_detect(comment, regex("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improv(e|ement|ing)?", ignore_case = TRUE)) == TRUE |
                    str_detect(comment, regex("(struggle|difficult|deficien)", ignore_case = TRUE)) == TRUE)
               & comment != "") %>%
        select(-cas_id, -ref_num) %>%
        rename(Quality = quality,
               Rating = rating,
               Comment = comment)

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

    # cv summary ---------------------------------------
    col_numbers <- c(
        cv$leadership_positions,
        paste(cv$poster_presentations, cv$platform_presentations, sep = " / "),
        paste(cv$research_projects, cv$publications, sep = " / "),
        "",
        paste(cv$num_rotations, cv$acute_care_rotations, cv$academic_rotations, sep = " / "),
        "",
        ""
    )

    col_scores_cv <- c(
        application_score$leadership,
        application_score$poster_presentations + application_score$platform_presentation,
        application_score$publications,
        application_score$work_experience,
        application_score$rotations,
        application_score$lcep,
        application_score$clin_skills
    )

    col_comments_cv <- c(
        cv_notes$leadership_comments,
        cv_notes$poster_presentations_comments,
        cv_notes$publications_comments,
        cv_notes$work_experience_comments,
        cv_notes$rotations_comments,
        cv_notes$lcep_comments,
        cv_notes$clin_skills_comments)

    df_cv <- tibble(
        Attribute = hdr_cv,
        Numbers = col_numbers,
        Score = col_scores_cv,
        Comments = col_comments_cv)

    tbl_cv <- vanilla.table(df_cv) %>%
        setFlexTableWidths(widths = c(2, 0.75, 0.75, 4)) %>%
        setZebraStyle(odd = "#eeeeee", even = "white")

    tbl_cv[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    tbl_cv[, to = "header"] <- parCenter()
    tbl_cv[] <- textProperties(font.size = 8, font.family = "Calibri")
    tbl_cv[, 2:3] <- parCenter()
    tbl_cv[, 4] <- parLeft()

    # references summary -------------------------------
    if (nrow(qualities) == 0) {
        qualities <- add_row(qualities, Quality = "None", Rating = "None", Comment = "None")
    }

    need.improve <- str_detect(qualities$Comment, regex("(need|room|could|area)(s|ing)?( +[^ ]+){0,5} improv(e|ement|ing)?", ignore_case = TRUE))
    struggle <- str_detect(qualities$Comment, regex("(struggle|difficult|deficien)", ignore_case = TRUE))
    bold.rows <- c(which(qualities$Rating == "Fails to Meet"), which(need.improve == TRUE), which(struggle == TRUE))

    df_qualities <- vanilla.table(qualities) %>%
        setFlexTableWidths(widths = c(1.5, 1, 5)) %>%
        setZebraStyle(odd = "#eeeeee", even = "white")
    df_qualities[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    df_qualities[, to = "header"] <- parCenter()
    df_qualities[] <- textProperties(font.size = 8, font.family = "Calibri")
    df_qualities[, 1:2] <- parCenter()
    df_qualities[, 3] <- parLeft()

    # vidyo summary ------------------------------------
    col_vidyo_score <- c(
        vidyo$crit_think_score,
        vidyo$time_mgmt_score,
        vidyo$prob_solve_score,
        vidyo$integrity_score
    )

    col_vidyo_comment <- c(
        vidyo$crit_think_comments,
        vidyo$time_mgmt_comments,
        vidyo$prob_solve_comments,
        vidyo$integrity_comments
    )

    tbl_vidyo <- tibble(
        Attribute = hdr_vidyo_scores,
        Score = col_vidyo_score,
        Comment = col_vidyo_comment
    )

    df_vidyo <- vanilla.table(tbl_vidyo) %>%
        setFlexTableWidths(widths = c(1.5, 0.75, 5.25)) %>%
        setZebraStyle(odd = "#eeeeee", even = "white")
    df_vidyo[, to = "header"] <- textProperties(font.size = 10, font.family = "Calibri", font.weight = "bold")
    df_vidyo[, to = "header"] <- parCenter()
    df_vidyo[] <- textProperties(font.size = 8, font.family = "Calibri")
    df_vidyo[, 1:2] <- parCenter()
    df_vidyo[, 3] <- parLeft()

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
        addFlexTable(tbl_intent) %>%
        addTitle("Curriculum Vitae", level = 3) %>%
        addFlexTable(tbl_cv) %>%
        addTitle("Letters of Recommendation", level = 3) %>%
        addFlexTable(df_qualities) %>%
        addTitle("Vidyo Interviews", level = 3) %>%
        addFlexTable(df_vidyo) %>%
        addTitle("Overall Comments", level = 3) %>%
        addParagraph(paste0("Application Comments: ", cv_notes$application_comments), stylename = "Normal") %>%
        addParagraph(paste0("Vidyo Comments: ", vidyo$comments_vidyo), stylename = "Normal")

    file.name <- paste0("report/summaries/", applicant$last_name, "_", applicant$first_name, "_", applicant$cas_id, ".docx")
    writeDoc(mydoc, file = file.name)

}
