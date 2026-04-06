# ============================================================================
# PCOS IoT Study - Script 1: Data Cleaning
# Sandro Graca | University of Wolverhampton
# March 2026
#
# Purpose: Load raw survey data, recode all variables, calculate composite
#          scores, classify postcodes, and save analysis-ready dataset.
#
# Input:   IoT and PCOS UK92 - Claudio Antonio.xlsx (Sheet: Full with IPAQ-SF)
# Output:  data-processed/cleaned_data.rds
#          data-processed/cleaned_data.csv
# ============================================================================

library(tidyverse)
library(readxl)
library(PostcodesioR)


# ----------------------------------------------------------------------------
# 1. Load data
# ----------------------------------------------------------------------------

df_raw <- read_excel(
  "~/OneDrive - University of Wolverhampton/IoT-PCOS/raw_data/IoT and PCOS UK92 - Claudio Antonio.xlsx",
  sheet = "Full with IPAQ-SF",
  na    = ""
)

# n = 91, 116 columns expected
cat("Rows:", nrow(df_raw), "| Columns:", ncol(df_raw), "\n")


# ----------------------------------------------------------------------------
# 2. Rename columns
# ----------------------------------------------------------------------------

df <- df_raw

colnames(df)[1:8] <- c("ID", "start_time", "completion_time", "email",
                        "name", "last_modified", "consent_confirm", "consent_items")

colnames(df)[9:21] <- c("age", "years_diagnosed_raw", "comorbidities_raw",
                         "ethnicity", "relationship", "occupation", "education",
                         "children", "children_u5", "children_6_11",
                         "children_11_16", "children_o16", "postcode")

colnames(df)[22:56] <- paste0("pcosqol_", 1:35)
colnames(df)[57:66] <- paste0("iot_",     1:10)

colnames(df)[67:70] <- c("alcohol_Q1", "alcohol_Q2", "tobacco_Q1", "tobacco_Q2")
colnames(df)[71:76] <- paste0("cm_", 1:6)
colnames(df)[77]    <- "cm_other_text"

colnames(df)[78:85] <- c("ipaq_vig_yn", "ipaq_vig_days", "ipaq_vig_mins",
                          "ipaq_mod_days", "ipaq_mod_mins",
                          "ipaq_walk_days", "ipaq_walk_mins", "ipaq_sitting_raw")

colnames(df)[86:92]   <- paste0("ede_",     1:7)
colnames(df)[93:99]   <- paste0("isi_",     1:7)
colnames(df)[100:106] <- paste0("swemwbs_", 1:7)

colnames(df)[107:109] <- c("unique_keyword", "focus_group_consent", "phone_number")
colnames(df)[110:116] <- c("sitting_hours", "MET_vigorous", "MET_moderate",
                            "MET_walk", "MET_total", "PA_category_num", "PA_category")


# ----------------------------------------------------------------------------
# 3. Demographics
# ----------------------------------------------------------------------------

df <- df %>%
  mutate(
    age             = as.numeric(age),
    years_diagnosed = as.numeric(years_diagnosed_raw),

    education_cat = case_when(
      str_detect(education, "Secondary|GCSE|O-level")   ~ "Secondary",
      str_detect(education, "College|A-level|BTEC")     ~ "College",
      str_detect(education, "Undergraduate|BSc|BA")     ~ "Undergraduate",
      str_detect(education, "Postgraduate|MA|MSc|PG")   ~ "Postgraduate",
      str_detect(education, "Doctorate|PhD|MD")         ~ "Doctorate",
      str_detect(education, "prefer not|Prefer not")    ~ NA_character_,
      TRUE ~ "Other"
    ),
    education_cat = factor(education_cat,
                           levels = c("Secondary", "College", "Undergraduate",
                                      "Postgraduate", "Doctorate", "Other")),

    degree_level = case_when(
      education_cat %in% c("Undergraduate", "Postgraduate", "Doctorate") ~ 1,
      education_cat %in% c("Secondary", "College") ~ 0,
      TRUE ~ NA_real_
    ),

    # Ethnicity
    # Two free-text entries were manually reviewed and classified:
    #   "Spanish" (ID=32) -> White (Spanish is a White European ethnicity)
    #   "Mainly european, with some west asian and canadian indigenous" (ID=12) -> Mixed
    ethnicity_cat = case_when(
      str_detect(ethnicity, "White|Spanish")              ~ "White",
      str_detect(ethnicity, "Asian")                      ~ "Asian/Asian British",
      str_detect(ethnicity, "Black")                      ~ "Black/Black British",
      str_detect(ethnicity, "Mixed|european.*asian")      ~ "Mixed",
      str_detect(ethnicity, "Gypsy|Gipsy|Traveller")     ~ "Gypsy or Traveller",
      str_detect(ethnicity, "prefer not|Prefer not")      ~ NA_character_,
      TRUE ~ "Other"
    ),

    # Relationship status — grouped for analysis and table presentation
    relationship_grouped = case_when(
      str_detect(relationship, regex(
        "married|engaged|civil.partnership|co.habiting|cohabiting|living with partner",
        ignore_case = TRUE))                              ~ "Married/engaged/partnered",
      str_detect(relationship, regex("single", ignore_case = TRUE))   ~ "Single",
      str_detect(relationship, regex("divorced", ignore_case = TRUE)) ~ "Divorced",
      str_detect(relationship, regex("prefer not", ignore_case = TRUE)) ~ NA_character_,
      TRUE ~ NA_character_
    ),

    has_children = case_when(
      str_detect(children, "Yes")                   ~ 1,
      str_detect(children, "No")                    ~ 0,
      str_detect(children, "prefer not|Prefer not") ~ NA_real_,
      TRUE ~ NA_real_
    ),

    employed = case_when(
      str_detect(occupation, regex("full-time employed|part-time employed|self-employed",
                                   ignore_case = TRUE)) ~ 1,
      str_detect(occupation, regex("prefer not", ignore_case = TRUE)) ~ NA_real_,
      TRUE ~ 0
    )
  )


# ----------------------------------------------------------------------------
# 4. Occupation — detailed category flags
# ----------------------------------------------------------------------------
# Multiple selections were permitted; 5 respondents selected more than one
# category. Each flag is 0/1 per respondent. ID=48 appended free text
# ("full time employed and part time masters student") as a note alongside
# formal checkbox selections; only the formal selections are counted here.

df <- df %>%
  mutate(
    occ_fulltime     = as.integer(str_detect(occupation,
                         regex("full-time employed",  ignore_case = TRUE))),
    occ_parttime     = as.integer(str_detect(occupation,
                         regex("part-time employed",  ignore_case = TRUE))),
    occ_selfemployed = as.integer(str_detect(occupation,
                         regex("self.employed",        ignore_case = TRUE))),
    occ_student      = as.integer(str_detect(occupation,
                         regex("student",              ignore_case = TRUE))),
    occ_unemployed   = as.integer(str_detect(occupation,
                         regex("unemployed",           ignore_case = TRUE))),
    occ_houseperson  = as.integer(str_detect(occupation,
                         regex("house person",         ignore_case = TRUE))),
    occ_sahm         = as.integer(str_detect(occupation,
                         regex("stay at home",         ignore_case = TRUE))),
    occ_disabled     = as.integer(str_detect(occupation,
                         regex("disabled",             ignore_case = TRUE))),
    occ_prefernot    = as.integer(str_detect(occupation,
                         regex("prefer not",           ignore_case = TRUE)))
  )

# Verification — expected: FT=52, PT=16, SE=4, Stu=10, Unemp=10, HP=2, SAHM=1, Dis=1
cat("\nOccupation flag counts:\n")
cat("  Full-time employed:  ", sum(df$occ_fulltime,     na.rm = TRUE), "\n")
cat("  Part-time employed:  ", sum(df$occ_parttime,     na.rm = TRUE), "\n")
cat("  Self-employed:       ", sum(df$occ_selfemployed, na.rm = TRUE), "\n")
cat("  Student:             ", sum(df$occ_student,      na.rm = TRUE), "\n")
cat("  Unemployed:          ", sum(df$occ_unemployed,   na.rm = TRUE), "\n")
cat("  House person:        ", sum(df$occ_houseperson,  na.rm = TRUE), "\n")
cat("  Stay at home mother: ", sum(df$occ_sahm,         na.rm = TRUE), "\n")
cat("  Disabled:            ", sum(df$occ_disabled,     na.rm = TRUE), "\n")
cat("  Prefer not to answer:", sum(df$occ_prefernot,    na.rm = TRUE), "\n")


# ----------------------------------------------------------------------------
# 5. Postcode — nation and urban/rural classification
# ----------------------------------------------------------------------------
# Country (England/Scotland/Wales) is looked up via the PostcodesioR package
# (Postcodes.io API), which returns ONS-verified country for full valid
# postcodes. Urban/rural is classified from postcode area prefix using ONS
# postcode area definitions, as the bulk API does not return rural-urban codes.
# Partial postcodes (outward code only, n=7) are classified by prefix.
# One non-UK postcode (6036) and one "Prefer not to answer" are coded NA.
# Five postcodes appear for two respondents each; the lookup deduplicates
# before the API call and joins by postcode string.

df <- df %>%
  mutate(postcode_clean = str_to_upper(str_remove_all(postcode, " ")))

df <- df %>%
  mutate(
    postcode_type = case_when(
      str_detect(postcode_clean, "PREFER")                                    ~ "prefer_not",
      str_detect(postcode_clean, "^[0-9]")                                    ~ "non_uk",
      str_detect(postcode_clean, "^[A-Z]{1,2}[0-9][0-9A-Z]?[0-9][A-Z]{2}$") ~ "full",
      str_detect(postcode_clean, "^[A-Z]{1,2}[0-9][0-9A-Z]?$")               ~ "partial",
      TRUE ~ "unknown"
    )
  )

# Deduplicate before API call to avoid duplicate join rows
full_postcodes_unique <- df %>%
  filter(postcode_type == "full") %>%
  distinct(postcode_clean) %>%
  pull(postcode_clean)

cat("\nLooking up", length(full_postcodes_unique),
    "unique full postcodes via Postcodes.io...\n")

lookup_results <- bulk_postcode_lookup(list(postcodes = full_postcodes_unique))

api_data <- map_dfr(lookup_results, function(res) {
  tibble(
    postcode_clean = str_remove_all(res$query, " "),
    api_country    = if (!is.null(res$result$country)) res$result$country
                     else NA_character_
  )
})

cat("API lookup complete. Retrieved", nrow(api_data), "results.\n")

df <- df %>%
  left_join(api_data, by = "postcode_clean")

# Prefix-based classification
# Scotland and Wales areas per Royal Mail/ONS standard definitions.
# CH covers Chester (England) and Flintshire/NE Wales; all CH postcodes
# in this sample (CH5, CH7) are Flintshire — classified as Wales.
scotland_prefixes <- c("AB","DD","DG","EH","FK","G","HS","IV","KA","KW",
                       "KY","ML","PA","PH","TD","ZE")
wales_prefixes    <- c("CF","CH","LD","LL","NP","SA","SY")

# Rural areas in this sample: DG (Dumfries & Galloway) and DT (Dorset)
rural_prefixes <- c("DG", "DT")

df <- df %>%
  mutate(
    postcode_area = str_extract(postcode_clean, "^[A-Z]{1,2}"),

    nation = case_when(
      postcode_type %in% c("prefer_not", "non_uk")  ~ NA_character_,
      postcode_type == "full" & !is.na(api_country) ~ api_country,
      postcode_area %in% scotland_prefixes           ~ "Scotland",
      postcode_area %in% wales_prefixes              ~ "Wales",
      postcode_type %in% c("full", "partial")        ~ "England",
      TRUE                                           ~ NA_character_
    ),

    urban_rural = case_when(
      postcode_type %in% c("prefer_not", "non_uk") ~ NA_character_,
      postcode_area %in% rural_prefixes             ~ "Rural",
      !is.na(postcode_area)                         ~ "Urban",
      TRUE                                          ~ NA_character_
    )
  )

# Verification
cat("\nNation (expected: England=76, Scotland=8, Wales=5, NA=2):\n")
print(table(df$nation, useNA = "ifany"))
cat("\nUrban/Rural (expected: Urban=87, Rural=2, NA=2):\n")
print(table(df$urban_rural, useNA = "ifany"))


# ----------------------------------------------------------------------------
# 6. Comorbidity count
# ----------------------------------------------------------------------------

comorbidity_terms <- c(
  "Type 2 diabetes", "Pre-diabetes", "Insulin resistance",
  "High blood pressure", "High blood fat", "High LDL", "Low HDL",
  "Heart disease", "Liver Disease", "Kidney Disease",
  "Asthma", "Chronic Lung Disease", "Sleep Apnoea",
  "Depression", "Anxiety disorder", "Insomnia", "Chronic Pain", "Cancer"
)

df <- df %>%
  mutate(
    comorbidity_count = case_when(
      str_detect(comorbidities_raw, "prefer not|Prefer not") ~ NA_real_,
      is.na(comorbidities_raw) ~ 0,
      TRUE ~ as.numeric(
        rowSums(sapply(comorbidity_terms, function(x)
          str_detect(coalesce(comorbidities_raw, ""), fixed(x))))
      )
    ),
    has_depression  = as.numeric(str_detect(coalesce(comorbidities_raw, ""),
                                            "Depression")),
    has_anxiety     = as.numeric(str_detect(coalesce(comorbidities_raw, ""),
                                            "Anxiety disorder")),
    has_insulin_res = as.numeric(str_detect(coalesce(comorbidities_raw, ""),
                                            "Insulin resistance"))
  )


# ----------------------------------------------------------------------------
# 7. Recode scale items
# ----------------------------------------------------------------------------

pcosqol_recode <- function(x) {
  case_when(
    x == "Usually"        ~ 1,
    x == "Often"          ~ 2,
    x == "Sometimes"      ~ 3,
    x == "Occasionally"   ~ 4,
    x == "Rarely"         ~ 5,
    x == "Not at all"     ~ 6,
    x == "Does not apply" ~ 7,
    TRUE ~ NA_real_
  )
}

iot_recode <- function(x) {
  case_when(
    x == "Never"     ~ 0,
    x == "Seldom"    ~ 1,
    x == "Often"     ~ 2,
    x == "Regularly" ~ 3,
    TRUE ~ NA_real_
  )
}

ede_recode_days <- function(x) {
  case_when(
    x %in% c("No days",    "0") ~ 0,
    x %in% c("1-5 days",   "1") ~ 1,
    x %in% c("6-12 days",  "2") ~ 2,
    x %in% c("13-15 days", "3") ~ 3,
    x %in% c("16-22 days", "4") ~ 4,
    x %in% c("23-27 days", "5") ~ 5,
    x %in% c("Every day",  "6") ~ 6,
    TRUE ~ NA_real_
  )
}

ede_recode_degree <- function(x) {
  case_when(
    x %in% c("Not at all", "0")                               ~ 0,
    x %in% c("Slightly (almost, but not quite absent)", "1")  ~ 1,
    x %in% c("Slightly", "2")                                 ~ 2,
    x %in% c("Moderately (midway between low and high)", "3") ~ 3,
    x %in% c("Moderately", "4")                               ~ 4,
    x %in% c("Markedly", "5")                                 ~ 5,
    x %in% c("Markedly (present to an extreme degree)", "6")  ~ 6,
    TRUE ~ NA_real_
  )
}

isi_recode <- function(x) {
  case_when(
    x %in% c("None", "Not at all noticeable", "Not at all worried",
              "Not at all interfering", "Very satisfied",  "0") ~ 0,
    x %in% c("Mild", "A little", "Satisfied",                  "1") ~ 1,
    x %in% c("Moderate", "Somewhat", "Moderately satisfied",   "2") ~ 2,
    x %in% c("Severe", "Much", "Dissatisfied",                 "3") ~ 3,
    x %in% c("Very severe", "Very much noticeable", "Very much worried",
              "Very much interfering", "Very dissatisfied",     "4") ~ 4,
    TRUE ~ NA_real_
  )
}

swemwbs_recode <- function(x) {
  case_when(
    x %in% c("None of the time", "1") ~ 1,
    x %in% c("Rarely",           "2") ~ 2,
    x %in% c("Some of the time", "3") ~ 3,
    x %in% c("Often",            "4") ~ 4,
    x %in% c("All of the time",  "5") ~ 5,
    TRUE ~ NA_real_
  )
}

pcosqol_cols <- paste0("pcosqol_", 1:35)
iot_cols     <- paste0("iot_",     1:10)
ede_cols     <- paste0("ede_",     1:7)
isi_cols     <- paste0("isi_",     1:7)
swemwbs_cols <- paste0("swemwbs_", 1:7)
cm_cols      <- paste0("cm_",      1:6)

df <- df %>%
  mutate(
    across(all_of(pcosqol_cols), pcosqol_recode),
    across(all_of(iot_cols),     iot_recode),
    across(all_of(swemwbs_cols), swemwbs_recode),
    across(all_of(isi_cols),     isi_recode),
    ede_1 = ede_recode_days(ede_1),
    ede_2 = ede_recode_days(ede_2),
    ede_3 = ede_recode_days(ede_3),
    ede_4 = ede_recode_degree(ede_4),
    ede_5 = ede_recode_degree(ede_5),
    ede_6 = ede_recode_degree(ede_6),
    ede_7 = ede_recode_degree(ede_7),
    across(all_of(cm_cols), iot_recode)
  )


# ----------------------------------------------------------------------------
# 8. Composite scores
# ----------------------------------------------------------------------------

df <- df %>%
  mutate(

    # PCOSQOL (range 35-245; higher = better QoL)
    # Domain structure per Williams et al. (2018)
    pcosqol_total = rowSums(pick(all_of(pcosqol_cols)), na.rm = FALSE),
    pcosqol_impact = rowSums(pick(pcosqol_4, pcosqol_6, pcosqol_10, pcosqol_13,
                                  pcosqol_15, pcosqol_16, pcosqol_17, pcosqol_18,
                                  pcosqol_19, pcosqol_20, pcosqol_27, pcosqol_28,
                                  pcosqol_29, pcosqol_32, pcosqol_33, pcosqol_35),
                             na.rm = FALSE),
    pcosqol_infertility = rowSums(pick(pcosqol_1, pcosqol_8, pcosqol_9,
                                       pcosqol_23, pcosqol_25, pcosqol_31, pcosqol_34),
                                  na.rm = FALSE),
    pcosqol_hirsutism = rowSums(pick(pcosqol_7, pcosqol_21, pcosqol_22,
                                     pcosqol_24, pcosqol_26, pcosqol_30),
                                na.rm = FALSE),
    pcosqol_mood = rowSums(pick(pcosqol_2, pcosqol_3, pcosqol_5,
                                pcosqol_11, pcosqol_12, pcosqol_14),
                           na.rm = FALSE),

    # IoT (range 0-30; higher = greater use)
    iot_total      = rowSums(pick(all_of(iot_cols)), na.rm = FALSE),
    iot_engagement = rowSums(pick(iot_1, iot_2, iot_3, iot_4, iot_5), na.rm = FALSE),
    iot_selfmon    = rowSums(pick(iot_6, iot_7, iot_8, iot_9, iot_10), na.rm = FALSE),

    # IoT groups: sample-specific tertile cut-points
    iot_category = case_when(
      iot_total <= quantile(iot_total, 1/3, na.rm = TRUE) ~ "Low",
      iot_total <= quantile(iot_total, 2/3, na.rm = TRUE) ~ "Medium",
      iot_total >  quantile(iot_total, 2/3, na.rm = TRUE) ~ "High",
      TRUE ~ NA_character_
    ),
    iot_category = factor(iot_category,
                          levels = c("Low", "Medium", "High"), ordered = TRUE),

    # IPAQ (pre-scored in Excel; log-transform applied for regression analyses)
    sitting_hours = as.numeric(sitting_hours),
    MET_total     = as.numeric(MET_total),
    MET_vigorous  = as.numeric(MET_vigorous),
    MET_moderate  = as.numeric(MET_moderate),
    MET_walk      = as.numeric(MET_walk),
    MET_total_log = log(MET_total + 1),
    PA_category   = factor(PA_category,
                           levels = c("Low", "Moderate", "High"), ordered = TRUE),

    # EDE-Q7 (mean global score 0-6; higher = more eating concerns)
    # Scored as mean of three subscale means per Bang et al. (2023)
    ede_restraint       = rowMeans(pick(ede_1, ede_2, ede_3), na.rm = FALSE),
    ede_overvaluation   = rowMeans(pick(ede_4, ede_5),        na.rm = FALSE),
    ede_dissatisfaction = rowMeans(pick(ede_6, ede_7),        na.rm = FALSE),
    ede_q7_global = rowMeans(
      pick(ede_restraint, ede_overvaluation, ede_dissatisfaction),
      na.rm = FALSE),
    ede_q7_clinical = as.numeric(ede_q7_global > 3.64),

    # ISI (range 0-28; higher = worse insomnia)
    isi_total = rowSums(pick(all_of(isi_cols)), na.rm = FALSE),
    isi_category = case_when(
      isi_total <= 7  ~ "No insomnia",
      isi_total <= 14 ~ "Subthreshold insomnia",
      isi_total <= 21 ~ "Moderate insomnia",
      isi_total <= 28 ~ "Severe insomnia",
      TRUE ~ NA_character_
    ),
    isi_category = factor(isi_category,
                          levels = c("No insomnia", "Subthreshold insomnia",
                                     "Moderate insomnia", "Severe insomnia"),
                          ordered = TRUE),

    # SWEMWBS (range 7-35; higher = better wellbeing)
    swemwbs_total = rowSums(pick(all_of(swemwbs_cols)), na.rm = FALSE),

    # Complementary medicine total
    cm_total = rowSums(pick(all_of(cm_cols)), na.rm = FALSE)
  )


# ----------------------------------------------------------------------------
# 9. Missing data summary
# ----------------------------------------------------------------------------

key_vars <- c("age", "years_diagnosed", "comorbidity_count",
              "iot_total", "iot_engagement", "iot_selfmon",
              "MET_total", "sitting_hours",
              "isi_total", "ede_q7_global", "swemwbs_total", "pcosqol_total")

cat("\nMissing data (key variables):\n")
df %>%
  dplyr::select(all_of(key_vars)) %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  print(n = 20)


# ----------------------------------------------------------------------------
# 10. Save analysis-ready dataset
# ----------------------------------------------------------------------------

df_clean <- df %>%
  dplyr::select(
    ID, age, years_diagnosed, education_cat, degree_level, employed,
    occ_fulltime, occ_parttime, occ_selfemployed, occ_student,
    occ_unemployed, occ_houseperson, occ_sahm, occ_disabled, occ_prefernot,
    ethnicity_cat, relationship_grouped, has_children,
    postcode_area, nation, urban_rural,
    comorbidity_count, has_depression, has_anxiety, has_insulin_res,
    tobacco_Q1, tobacco_Q2, alcohol_Q1, alcohol_Q2, cm_total, all_of(cm_cols),
    all_of(iot_cols), iot_total, iot_engagement, iot_selfmon, iot_category,
    MET_total, MET_total_log, MET_vigorous, MET_moderate, MET_walk,
    sitting_hours, PA_category, PA_category_num,
    all_of(isi_cols), isi_total, isi_category,
    all_of(ede_cols), ede_restraint, ede_overvaluation, ede_dissatisfaction,
    ede_q7_global, ede_q7_clinical,
    all_of(swemwbs_cols), swemwbs_total,
    all_of(pcosqol_cols), pcosqol_total, pcosqol_impact, pcosqol_infertility,
    pcosqol_hirsutism, pcosqol_mood,
    focus_group_consent
  )

saveRDS(df_clean, "data-processed/cleaned_data.rds")
write_csv(df_clean, "data-processed/cleaned_data.csv")

cat("\nDone. n =", nrow(df_clean), "| Variables:", ncol(df_clean), "\n")

# Next: Script_2_DescriptivesAndReliability.R
