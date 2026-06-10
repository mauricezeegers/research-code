#ACKNOWLEDGEMENT INITIONAL CODE
#16-09-2024 Kirsten Reijbroek
#27-05-2026: Maurice Zeegers
#R-files are pulled and pushed toGithub
#Data-files are pulled from OSF (not yet)

# Install and load packages ---------------------------------------------


#Instal haven package
install.packages("haven")
library(haven)

# ensure the package 'pacman' is installed
if (!require("pacman"))install.packages("pacman")

# install and load packages for use 
pacman::p_load(rio,           # import/export of many types of data
               tidyverse,     # includes many packages for tidy data wrangling and presentation
               here,          # file paths relative to R project root folder
               openxlsx,      # special functions for handling Excel workbooks
               janitor,       # tables and data cleaning
               gtsummary,     # making descriptive and statistical tables
               rmarkdown,     # produce PDFs, Word Documents, Powerpoints, and HTML files
               lmtest,        # Package provides various diagnostic tests and additional functionalities for linear regression models
               skimr,         # Summarizing data-sets
               dplyr,
               car,           # Regression analysis and diagnostics
               flextable,     # for creating good looking tables
               wordcloud,     # for simple wordcloud
               hunspell,      # to clean common spelling mistakes
               MASS,          # for ordinal logistic regression
               brant,         # test the proportional odds assumption for ordinal logistic regression
               nnet,          # for multinominal logistic regression
               glmnet,        # functions for fitting regularized generalized linear models, including Lasso regression
               broom         # for tidying up model results
)          



# Set a Working Directory -------------------------------------------------

#Home Computer MZ
setwd("~/Library/Mobile Documents/3L68KQB4HG~com~readdle~CommonDocuments/Documents/To Analyse/SERCEA Survey/Github/SERCEA/survey/data")

# Data Importing Section --------------------------------------------------


#upload data into R and view data
#creating numeric values: Likert scales are transformed in numeric variables while importing the data
library(readxl)
Research_Integrity_and_Open_Science_Online_Survey_April_11_2025_07_43 <- read_excel("Research Integrity and Open Science Online Survey_April 11, 2025_07.43.xlsx", 
                                                                                    col_types = c("date", "date", "text", 
                                                                                                  "text", "text", "numeric", "text", "date",
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text",
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", 
                                                                                                  "numeric", "text", "text", "text", 
                                                                                                  "text", "numeric", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text",
                                                                                                  "text", "text", "numeric", "numeric",
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "numeric", "numeric", 
                                                                                                  "numeric", "text", "text", "text", 
                                                                                                  "text", "text", "text", "text", "text",
                                                                                                  "text", "text", "text"))                                                   
View(Research_Integrity_and_Open_Science_Online_Survey_April_11_2025_07_43)


# Remove the first row of the original data file and save as dummy_file_ri_os_survey
ri_os_survey <- Research_Integrity_and_Open_Science_Online_Survey_April_11_2025_07_43[-1,]
head(ri_os_survey)

# Remove all responses before 31st of October, these are all pilot test results
# Define the cutoff date
cutoff_date <- as.Date("2024-10-31")

# Filter rows with answer_date on or after the cutoff date 
ri_os_survey <- ri_os_survey %>%
  filter(StartDate >= cutoff_date)

# Set OFF for Descriptive Analysis 
# Remove all responses that are not complete
# ri_os_survey <- ri_os_survey[ri_os_survey$Finished != "False", ]

# Remove all responses where pariticpants don't give informed consent
ri_os_survey <- ri_os_survey[ri_os_survey$`Info0.0 Kenya` != "No" | is.na(ri_os_survey$`Info0.0 Kenya`), ]
ri_os_survey <- ri_os_survey[ri_os_survey$`Info0.0 Tanzania` != "No" | is.na(ri_os_survey$`Info0.0 Tanzania`), ]
ri_os_survey <- ri_os_survey[ri_os_survey$`Info0.0 Malawi` != "No" | is.na(ri_os_survey$`Info0.0 Malawi`), ]
ri_os_survey <- ri_os_survey[ri_os_survey$`Info0.0 Uganda` != "No" | is.na(ri_os_survey$`Info0.0 Uganda`), ]

# Remove all responses where country is other
ri_os_survey <- ri_os_survey[ri_os_survey$Info0.0 != "Other", ]

# Remove all responses after closing date
cutoff_date_malawi <- as.Date("2024-12-02")
ri_os_survey <- ri_os_survey %>%
  filter((Info0.0 == "Malawi" & StartDate <= cutoff_date_malawi | Info0.0 != "Malawi"))

cutoff_date_uganda <- as.Date("2025-01-22")
ri_os_survey <- ri_os_survey %>%
  filter((Info0.0 == "Uganda" & StartDate <= cutoff_date_uganda | Info0.0 != "Uganda"))

cutoff_date_kenya <- as.Date("2025-02-10")
ri_os_survey <- ri_os_survey %>%
  filter((Info0.0 == "Kenya" & StartDate <= cutoff_date_kenya | Info0.0 != "Kenya"))

cutoff_date_tanzania <- as.Date("2025-02-10")
ri_os_survey <- ri_os_survey %>%
  filter((Info0.0 == "Tanzania" & StartDate <= cutoff_date_tanzania | Info0.0 != "Tanzania"))

# TO DO: Decide what to do with role_other responses
# Remove all responses where role is "None" in combination with role_other is NA
# ri_os_survey <- ri_os_survey %>%
#  filter((Q1.1 == "None" & !is.na(ri_os_survey$Q1.1_6_TEXT) | Q1.1 !="None"))



# Data Exploration --------------------------------------------------------



#exploring data with summary function
summary(ri_os_survey)

#show a list of variables 
names(ri_os_survey)

# shows characters 
skim(ri_os_survey)



# Data Cleaning -----------------------------------------------------------



#clean the column names to lowercase and remove spaces
ri_os_survey <- clean_names(ri_os_survey)
names(ri_os_survey)


ri_os_survey <- ri_os_survey %>% 
  # manually re-name columns participant background information
  # NEW name         # OLD name
  rename(country = info0_0,
         ic_kenya = info0_0_kenya,
         ic_tanzania = info0_0_tanzania,
         ic_malawi = info0_0_malawi,
         ic_uganda = info0_0_uganda,
         role = q1_1,
         role_other = q1_1_6_text,
         involvement_research = q1_2,
         years_role_researcher = q1_3_26,
         years_role_nrec = q1_3_27,
         years_role_irec = q1_3_28,
         years_role_nra = q1_3_29,
         years_role_ra = q1_3_30,
         years_role_other = q1_3_31,
         expertise = q1_4,
         expertise_other_text = q1_4_6_text,
         academic_qual = q1_5,
         academic_qual_other_text = q1_5_4_text,
         gender = q1_6,
         involvement_ri = q1_7,
         involvement_ri_other_text = q1_7_6_text,
         knowledge_ri = q2_1_1,
         awareness_ri_national = q2_2,
         awareness_ri_institutional = q2_3,
         awareness_ri_collaborators = q2_4,
         awareness_ri_text = q2_5,
         knowledge_os = q2_6_1,
         awareness_os_institutional = q2_7,
         awareness_os_national = q2_8,
         awareness_os_collaborators = q2_9,
         awareness_os_text = q2_10,
         value1 = q3_1_4,
         value2 = q3_1_5,
         value3 = q3_1_6,
         value4 = q3_1_7,
         value5 = q3_1_8,
         bb_ri_faster_promotion = q4_1_1,
         bb_ri_more_paperwork = q4_2_1,
         bb_os_faster_promotion = q4_3_1,
         bb_os_more_paperwork = q4_4_1,
         nb_ri_peers_encourage_ri = q5_1_1,
         nb_ri_peers_dissaprove_not_acting_ri = q5_2_1,
         nb_ri_acting_like_collegues = q5_3_1,
         nb_os_peers_encourage_os = q5_4_1,
         nb_os_peers_dissaprove_not_acting_os = q5_5_1,
         nb_os_acting_like_collegues = q5_6_1,
         cb_ri_coc_prevents_misconduct = q6_1_1,
         cb_ri_training_improves_ri = q6_2_1,
         cb_ri_not_adequatly_trained = q6_3_1,
         cb_os_improves_transparency = q6_4_1,
         cb_os_training_improves_os = q6_5_1,
         cb_os_not_adequately_trained = q6_6_1,
         attitude_ri_coc_crucial_quality = q7_1_1,
         attitude_ri_feeling_implementing_coc = q7_2_1,
         attitude_os_crucial_quality = q7_3_1,
         pn_ri_pressure_adhering = q8_1_1,
         pn_ri_pressure_not_adhering = q8_2_1,
         pn_ri_institutions_responsibility = q8_3_1,
         pn_ri_adhere_when_mandatory = q8_4_1,
         pn_os_pressure_adhering = q8_5_1,
         pn_os_pressure_not_adhering = q8_6_1,
         pbc_ri_easy_adhering = q9_1_1,
         pbc_ri_difficult_adhering = q9_2_1,
         pbc_os_easy_adhering = q9_3_1,
         pbc_os_difficult_adhering = q9_4_1,
         ef_ri_qualified_person = q10_1_1,
         ef_ri_priority = q10_2_1,
         ef_ri_misconduct_sanctioned = q10_3_1,
         ef_ri_adequate_training = q10_4_1,
         ef_os_priority = q10_5_1,
         ef_os_policy_data_sharing = q10_6_1,
         ef_os_preprint = q10_7_1,
         ef_os_publishing_open_access = q10_8_1,
         ef_os_infrastructure_data_sharing = q10_9_1,
         ef_os_adequate_training = q10_10_1,
         intention_ri = q11_1,
         intention_ri_text = q11_1a,
         intention_os = q11_2,
         intention_os_text = q11_2a,
         intention_ri_training = q11_3,
         intention_ri_training_text = q11_3a,
         intention_os_training = q11_4,
         intention_os_training_text = q11_4a
  )

names(ri_os_survey)




# Study Population -----------------------------------



# create variable role_total
# This way people with multiple roles will be categorized under 1 role
ri_os_survey <- ri_os_survey %>% 
  mutate(role_total = case_when(
    role %in% c("IREC", "NREC", "Researcher,IREC", "Researcher,NREC", "Researcher,IREC,RA") ~ "REC",  # Merge "IREC" and "NREC" into "REC"
    role %in% c("NRA", "NREC,NRA", "Researcher,NRA", "Researcher,NREC,NRA") ~ "NRA",   # Merge "NRA" and "NREC,NRA" into "NRA"
    role %in% c("RA", "Researcher,RA") ~ "RA",   # Merge "RA" and "Researcher,RA" into "RA"
    role %in% c("Researcher") ~ "Researcher"   # Merge "Researcher" into "Researcher"
  ))


# create a summary table for role, split per participant group and per country
# does not need to be included in report
ri_os_survey %>% 
  dplyr::select(country, role) %>% 
  tbl_summary(by = country)

# create a summary table for role_other
# lists all open text answers for role_other
ri_os_survey %>% 
  dplyr::select(country, role_other) %>% 
  tbl_summary(by = country)



#TABLE 1 create a summary table for role_total, split per participant group and per country
ri_os_survey %>% 
  dplyr::select(country, role_total) %>% 
  tbl_summary(by = country)



# get idea of drop out rates
# TO DO: improve this table 
# Only 49% completed survey 100%

ri_os_survey <- ri_os_survey %>% 
  mutate(progress_total = case_when(
    progress %in% c("0.0", "1.0", "2.0", "8.0", "9.0") ~ ">10%",  # Merge less than 10% together
    progress %in% c("10.0", "11.0") ~ "10-19%",   # Merge 10-19% together
    progress %in% c("23.0", "24.0", "29.0") ~ "20-29%",   # Merge 20-29% together
    progress %in% c("30.0", "31.0", "36.0", "37.0", "39.0") ~ "30-39%",   # Merge 30-39% together
    progress %in% c("40.0", "45.0") ~ "40-49%",   # Merge 40-49% together
    progress %in% c("52.0", "53.0") ~ "50-59%",   # Merge 50-59% together
    progress %in% c("60.0", "64.0", "65.0") ~ "60-69%",   # Merge 60-69% together
    progress %in% c("73.0", "78.0") ~ "70-79%",   # Merge 70-79% together
    # 0 respondents finished between 80-89%
    progress %in% c("90.0") ~ "90-99%",   # Merge 90-99% together
    progress %in% c("100.0") ~ "100%"   # Merge 100% together
  ))

ri_os_survey %>% 
  dplyr::select(progress_total) %>% 
  tbl_summary()



# Creating Variables ----------------------------------------------------------



# create variable awareness_ri_total
# if answers to question awareness national/institutional/collaborators at 
# least one is yes, then categorize yes, otherwise categorize no
# ?ifelse # very versatile function
ri_os_survey$awareness_ri_total <- ifelse(ri_os_survey$awareness_ri_national=="Yes" | 
                                            ri_os_survey$awareness_ri_institutional=="Yes" | 
                                            ri_os_survey$awareness_ri_collaborators=="Yes","Yes","No") 

# create variable awareness_os_total
# if answer to question awareness national/institutional/collaborators at 
#least one is yes, then categorize yes, otherwise categorize no
?ifelse # very versitle function
ri_os_survey$awareness_os_total <- ifelse(ri_os_survey$awareness_os_national=="Yes" | 
                                            ri_os_survey$awareness_os_institutional=="Yes" | 
                                            ri_os_survey$awareness_os_collaborators=="Yes","Yes","No") 


# creating variable years_role_total
ri_os_survey <- ri_os_survey %>% 
  mutate(years_role_total = case_when(role_total == "Researcher" ~ years_role_researcher,
                                      role_total == "NRA" ~ years_role_nra,
                                      role_total == "REC" ~ years_role_irec,
                                      role_total == "RA" ~ years_role_ra))



# creating variable involvement_ri_total
# To DO: IN FINAL SCRIPT CHECK IF COMPLETE
ri_os_survey <- ri_os_survey %>% 
  mutate(involvement_ri_total = case_when(
    involvement_ri %in% c("formulating and implementing policies", 
                          "formulating and implementing policies,other",
                          "handling allegations",
                          "handling allegations,formulating and implementing policies",
                          "research", 
                          "research,formulating and implementing policies",
                          "research,handling allegations",
                          "research,handling allegations,formulating and implementing policies",
                          "research,handling allegations,other",
                          "research,other",
                          "teaching", 
                          "teaching,formulating and implementing policies",
                          "teaching,handling allegations",
                          "teaching,handling allegations,formulating and implementing policies",
                          "teaching,handling allegations,formulating and implementing policies, other",
                          "teaching,other",
                          "teaching,research", 
                          "teaching,research,formulating and implementing policies",
                          "teaching,research,handling allegations",
                          "teaching,research,handling allegations,formulating and implementing policies",
                          "teaching,research,handling allegations,formulating and implementing policies,other",
                          "teaching,research,other"
    ) ~ "Yes",  # Merge if involvement is Yes
    involvement_ri %in% c("not involved"
    ) ~ "No",   # Merge if involvement is No
    involvement_ri %in% c("other") ~ "Other",   # Merge if involvement is Other
  ))


#Checking if final script is complete
# ri_os_survey %>% 
#  dplyr::select(involvement_ri) %>% 
#   tbl_summary()


# Statistical Analysis Objective 1.1 ------------------------------------------

# Objective: To describe the participant background information, the six 
# predictor domains, the moderating domain and the outcome domain


# TABLE 1 To describe the participant background information
# TO DO: knowledge_ri and knowledge_os should be a scale from 1-5 so debug
# TO DO: figure out if awareness_ri_total is Yes or No group
ri_os_survey %>% 
  dplyr::select(country, role_total, years_role_total, expertise, academic_qual,
                gender, involvement_ri_total, knowledge_ri, awareness_ri_total, 
                knowledge_os, awareness_os_total) %>% 
  tbl_summary()

# To describe the participant background information by country
# TO DO: knowledge_ri and knowledge_os should be a scale from 1-5 so debug
# TO DO: figure out if awareness_ri_total is Yes or No group
ri_os_survey %>% 
  dplyr::select(country, role_total, years_role_total, expertise, academic_qual,
                gender, involvement_ri_total, knowledge_ri, awareness_ri_total, 
                knowledge_os, awareness_os_total) %>% 
  tbl_summary(by = country)


# To create a word cloud for the values 
# Clean names of the value columns to remove capital letters and spaces
ri_os_survey <- ri_os_survey %>%
  mutate(across(c(value1, value2, value3, value4, value5), ~ gsub(" ", "", tolower(.))))
# Combine all words into a single list and create a frequency table
# Flatten all columns into a single vector
values_list <- unlist(ri_os_survey %>% dplyr::select(value1, value2, value3, value4, value5))
# Create a data frame with word counts
values_frequency <- as.data.frame(table(values_list))
# Rename columns to match 'wordcloud' requirements
colnames(values_frequency) <- c("word", "freq")
# Basic word cloud
set.seed(123)  # For reproducibility
wordcloud(
  words = values_frequency$word,             # Words to be displayed
  freq = values_frequency$freq,              # Frequency of each word
  min.freq = 10,                       # Minimum frequency of words to include
  max.words = 150,                    # Maximum number of words to display
  scale = c(1.5, 0.5),
  colors = brewer.pal(8, "Dark2")     # Color palette
)

# To DO: COULD ALSO CREATE FREQUENCY GRAPH/TABLE HERE

# To describe the six predictor domains and the moderating domain
ri_os_survey %>% 
  dplyr::select(bb_ri_faster_promotion, bb_ri_more_paperwork, 
                bb_os_faster_promotion, bb_os_more_paperwork,
                nb_ri_peers_encourage_ri, nb_ri_peers_dissaprove_not_acting_ri, nb_ri_acting_like_collegues,
                nb_os_peers_encourage_os, nb_os_peers_dissaprove_not_acting_os, nb_os_acting_like_collegues,
                cb_ri_coc_prevents_misconduct, cb_ri_training_improves_ri, cb_ri_not_adequatly_trained,
                cb_os_improves_transparency, cb_os_training_improves_os, cb_os_not_adequately_trained,
                attitude_ri_coc_crucial_quality, attitude_ri_feeling_implementing_coc,
                attitude_os_crucial_quality,
                pn_ri_pressure_adhering, pn_ri_pressure_not_adhering, pn_ri_institutions_responsibility, pn_ri_adhere_when_mandatory,
                pn_os_pressure_adhering, pn_os_pressure_not_adhering,
                pbc_ri_easy_adhering, pbc_ri_difficult_adhering,
                pbc_os_easy_adhering, pbc_os_difficult_adhering,
                ef_ri_qualified_person, ef_ri_priority, ef_ri_misconduct_sanctioned, ef_ri_adequate_training,
                ef_os_priority, ef_os_policy_data_sharing, ef_os_preprint, ef_os_publishing_open_access, ef_os_infrastructure_data_sharing, ef_os_adequate_training
  ) %>% 
  tbl_summary()


# To describe the six predictor domains and the moderating domain by country
ri_os_survey %>% 
  dplyr::select(country,
                bb_ri_faster_promotion, bb_ri_more_paperwork, 
                bb_os_faster_promotion, bb_os_more_paperwork,
                nb_ri_peers_encourage_ri, nb_ri_peers_dissaprove_not_acting_ri, nb_ri_acting_like_collegues,
                nb_os_peers_encourage_os, nb_os_peers_dissaprove_not_acting_os, nb_os_acting_like_collegues,
                cb_ri_coc_prevents_misconduct, cb_ri_training_improves_ri, cb_ri_not_adequatly_trained,
                cb_os_improves_transparency, cb_os_training_improves_os, cb_os_not_adequately_trained,
                attitude_ri_coc_crucial_quality, attitude_ri_feeling_implementing_coc,
                attitude_os_crucial_quality,
                pn_ri_pressure_adhering, pn_ri_pressure_not_adhering, pn_ri_institutions_responsibility, pn_ri_adhere_when_mandatory,
                pn_os_pressure_adhering, pn_os_pressure_not_adhering,
                pbc_ri_easy_adhering, pbc_ri_difficult_adhering,
                pbc_os_easy_adhering, pbc_os_difficult_adhering,
                ef_ri_qualified_person, ef_ri_priority, ef_ri_misconduct_sanctioned, ef_ri_adequate_training,
                ef_os_priority, ef_os_policy_data_sharing, ef_os_preprint, ef_os_publishing_open_access, ef_os_infrastructure_data_sharing, ef_os_adequate_training
  ) %>% 
  tbl_summary(by = country)


# To describe the outcome domain
ri_os_survey %>% 
  dplyr::select(intention_ri, intention_os, intention_ri_training, intention_os_training) %>% 
  tbl_summary()

# To describe the outcome domain by country
ri_os_survey %>% 
  dplyr::select(country, intention_ri, intention_os, intention_ri_training, intention_os_training) %>% 
  tbl_summary(by = country)



# Changing Constructs ---------------------------------------------------------



# recode values with opposite construct 
# example: adhering to a RI CoC is difficult - very negative is the positive answer

ri_os_survey$bb_ri_more_paperwork <- car::recode(ri_os_survey$bb_ri_more_paperwork, 
                                                 "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$bb_os_more_paperwork <- car::recode(ri_os_survey$bb_os_more_paperwork, 
                                                 "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$cb_ri_not_adequatly_trained <- car::recode(ri_os_survey$cb_ri_not_adequatly_trained, 
                                                        "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$cb_os_not_adequately_trained <- car::recode(ri_os_survey$cb_os_not_adequately_trained, 
                                                         "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$pn_ri_pressure_not_adhering <- car::recode(ri_os_survey$pn_ri_pressure_not_adhering, 
                                                        "1 = 5;  2= 4; 4 = 2; 5 = 1")

# Leaving pn_ri_institutions_responsibility out as we are not using it in the calculation of Simple Average
# ri_os_survey$pn_ri_institutions_responsibility <- car::recode(ri_os_survey$pn_ri_institutions_responsibility, 
#                                                            "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$pn_ri_adhere_when_mandatory <- car::recode(ri_os_survey$pn_ri_adhere_when_mandatory, 
                                                        "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$pn_os_pressure_not_adhering <- car::recode(ri_os_survey$pn_ri_pressure_not_adhering, 
                                                        "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$pbc_ri_difficult_adhering <- car::recode(ri_os_survey$pbc_ri_difficult_adhering, 
                                                      "1 = 5;  2= 4; 4 = 2; 5 = 1")

ri_os_survey$pbc_os_difficult_adhering <- car::recode(ri_os_survey$pbc_os_difficult_adhering, 
                                                      "1 = 5;  2= 4; 4 = 2; 5 = 1")



# Creating Simple Averages ----------------------------------------------------



# Simple averages Behavioral Beliefs RI
# ?apply - explanation apply function
ri_os_survey[,c("bb_ri_faster_promotion","bb_ri_more_paperwork")]
ri_os_survey$bb_ri<-apply(ri_os_survey[,c("bb_ri_faster_promotion","bb_ri_more_paperwork")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Behavioral Beliefs OS
ri_os_survey[,c("bb_os_faster_promotion","bb_os_more_paperwork")]
ri_os_survey$bb_os<-apply(ri_os_survey[,c("bb_os_faster_promotion","bb_os_more_paperwork")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Normative Beliefs RI
ri_os_survey[,c("nb_ri_peers_encourage_ri","nb_ri_peers_dissaprove_not_acting_ri", "nb_ri_acting_like_collegues")]
ri_os_survey$nb_ri<-apply(ri_os_survey[,c("nb_ri_peers_encourage_ri","nb_ri_peers_dissaprove_not_acting_ri", "nb_ri_acting_like_collegues")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Normative Beliefs OS
ri_os_survey[,c("nb_os_peers_encourage_os","nb_os_peers_dissaprove_not_acting_os", "nb_os_acting_like_collegues")]
ri_os_survey$nb_os<-apply(ri_os_survey[,c("nb_os_peers_encourage_os","nb_os_peers_dissaprove_not_acting_os", "nb_os_acting_like_collegues")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Control Beliefs RI
ri_os_survey[,c("cb_ri_coc_prevents_misconduct","cb_ri_training_improves_ri", "cb_ri_not_adequatly_trained")]
ri_os_survey$cb_ri<-apply(ri_os_survey[,c("cb_ri_coc_prevents_misconduct","cb_ri_training_improves_ri", "cb_ri_not_adequatly_trained")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Control Beliefs OS
ri_os_survey[,c("cb_os_improves_transparency","cb_os_training_improves_os", "cb_os_not_adequately_trained")]
ri_os_survey$cb_os<-apply(ri_os_survey[,c("cb_os_improves_transparency","cb_os_training_improves_os", "cb_os_not_adequately_trained")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Attitude RI
ri_os_survey[,c("attitude_ri_coc_crucial_quality","attitude_ri_feeling_implementing_coc")]
ri_os_survey$attitude_ri<-apply(ri_os_survey[,c("attitude_ri_coc_crucial_quality","attitude_ri_feeling_implementing_coc")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Perceived Norm RI
ri_os_survey[,c("pn_ri_pressure_adhering","pn_ri_pressure_not_adhering", "pn_ri_adhere_when_mandatory")]
ri_os_survey$pn_ri<-apply(ri_os_survey[,c("pn_ri_pressure_adhering","pn_ri_pressure_not_adhering", "pn_ri_adhere_when_mandatory")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Perceived Norm OS
ri_os_survey[,c("pn_os_pressure_adhering","pn_os_pressure_not_adhering")]
ri_os_survey$pn_os<-apply(ri_os_survey[,c("pn_os_pressure_adhering","pn_os_pressure_not_adhering")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Perceived Behavioral Control RI
ri_os_survey[,c("pbc_ri_easy_adhering","pbc_ri_difficult_adhering")]
ri_os_survey$pbc_ri<-apply(ri_os_survey[,c("pbc_ri_easy_adhering","pbc_ri_difficult_adhering")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Perceived Behavioral Control OS
ri_os_survey[,c("pbc_os_easy_adhering","pbc_os_difficult_adhering")]
ri_os_survey$pbc_os<-apply(ri_os_survey[,c("pbc_os_easy_adhering","pbc_os_difficult_adhering")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Environmental Factors RI
ri_os_survey[,c("ef_ri_qualified_person","ef_ri_priority", "ef_ri_misconduct_sanctioned", "ef_ri_adequate_training")]
ri_os_survey$ef_ri<-apply(ri_os_survey[,c("ef_ri_qualified_person","ef_ri_priority", "ef_ri_misconduct_sanctioned", "ef_ri_adequate_training")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values


# Simple averages Environmental Factors OS
ri_os_survey[,c("ef_os_priority","ef_os_policy_data_sharing", "ef_os_preprint", "ef_os_publishing_open_access", "ef_os_infrastructure_data_sharing", "ef_os_adequate_training")]
ri_os_survey$ef_os<-apply(ri_os_survey[,c("ef_os_priority","ef_os_policy_data_sharing", "ef_os_preprint", "ef_os_publishing_open_access", "ef_os_infrastructure_data_sharing", "ef_os_adequate_training")],1,mean, na.rm = TRUE) # na.rm=TRUE is to ignore missing values



# Statistical Analysis Objective 1.3 ------------------------------------------


mean_sd__ri_table <- ri_os_survey %>%
  group_by(country) %>%
  summarise(
    bb_ri_mean = mean(bb_ri, na.rm = TRUE),
    bb_ri_sd = sd(bb_ri, na.rm = TRUE),
    nb_ri_mean = mean(nb_ri, na.rm = TRUE),
    nb_ri_sd = sd(nb_ri, na.rm = TRUE),
    cb_ri_mean = mean(cb_ri, na.rm = TRUE),
    cb_ri_sd = sd(cb_ri, na.rm = TRUE),
    attitude_ri_mean = mean(attitude_ri, na.rm = TRUE),
    attitude_ri_sd = sd(attitude_ri, na.rm = TRUE),
    pn_ri_mean = mean(pn_ri, na.rm = TRUE),
    pn_ri_sd = sd(pn_ri, na.rm = TRUE),
    pbc_ri_mean = mean(pbc_ri, na.rm = TRUE),
    pbc_ri_sd = sd(pbc_ri, na.rm = TRUE),
    ef_ri_mean = mean(ef_ri, na.rm = TRUE),
    ef_ri_sd = sd(ef_ri, na.rm = TRUE)
  )


mean_sd__os_table <- ri_os_survey %>%
  group_by(country) %>%
  summarise(
    bb_os_mean = mean(bb_os, na.rm = TRUE),
    bb_os_sd = sd(bb_os, na.rm = TRUE),
    nb_os_mean = mean(nb_os, na.rm = TRUE),
    nb_os_sd = sd(nb_os, na.rm = TRUE),
    cb_os_mean = mean(cb_os, na.rm = TRUE),
    cb_os_sd = sd(cb_os, na.rm = TRUE),
    attitude_os_mean = mean(attitude_os_crucial_quality, na.rm = TRUE),
    attitude_os_sd = sd(attitude_os_crucial_quality, na.rm = TRUE),
    pn_os_mean = mean(pn_os, na.rm = TRUE),
    pn_os_sd = sd(pn_os, na.rm = TRUE),
    pbc_os_mean = mean(pbc_os, na.rm = TRUE),
    pbc_os_sd = sd(pbc_os, na.rm = TRUE),
    ef_os_mean = mean(ef_os, na.rm = TRUE),
    ef_os_sd = sd(ef_os, na.rm = TRUE)
  )


# Checking Assumptions for Objective 2 ----------------------------------------



# Trying list functions ----------------------------------------------------


# testing without Tanzania
df_without_tanzania <- 
  ri_os_survey %>%
  subset(country != "Tanzania")



# list of variables
vars_predictor_moderating_domains <- c("bb_ri",
                                       "nb_ri",
                                       "cb_ri",
                                       "attitude_ri",
                                       "pn_ri",
                                       "pbc_ri",
                                       "ef_ri")


simple_linear_regression_fun <- function(vars){
  # Create the model to see if there are differences in BB per country of work 
  # BB = β0 + β1 country + ε
  
  formula <- as.formula(paste(vars, "~as.factor(country)"))
  
  obj2_vars_country <- lm(formula, 
                          data = ri_os_survey)
  print(summary(obj2_vars_country))
  
  
  # Assumptions: Independence, Normality, Homoscedasticity, Linearity, 
  # No influential outliers 
  
  # Independence is checked through study design. Every subject is included once 
  # and the subjects are assumed to be independent, therefor the assumption of 
  # independence is not violated. 
  
  # Normality 
  #create histogram of residuals
  res_obj2_vars_country <- residuals(obj2_vars_country)
  hist(res_obj2_vars_country)
  #create QQ-plot
  plot(obj2_vars_country, 2)
  # If the histogram and QQ-plot show only a small deviation from normality,
  # taking a large sample size into account, we may conclude that the normality 
  # assumption is met.
  # HOW TO INTERPRET THE QQ PLOT SHAPE? IS NOT LINEAR BUT ALSO NOT_NOT LINEAR
  
  #Other way to make qq plot based on Franscesco's lecture
  rstandard(obj2_vars_country)
  qqPlot(rstandard(obj2_vars_country), dist="norm", ylab="standardized redisuals")
  hist(rstandard(obj2_vars_country))
  
  # Homoscedasticity
  #create the residuals plot 
  plot(obj2_vars_country, 1)
  # If this plot shows no clear increase or decrease in dispersion of residuals 
  # (y-direction), homoscedasticity can be assumed
  
  # Linearity
  # check this for continuous variables, in this case BB
  # partial residual plot
  car::crPlots(obj2_vars_country)
  # Partial plot should show no (clear) deviation from linearity. 
  # IS THIS THE RIGHT WAY?
  # 
  # Influential Outliers
  # Cook's distance > 1?
  # cooks.distance(obj2_vars_country)
  cooks_values <- cooks.distance(obj2_vars_country)
  print(cooks_values)
  hat_values <- hatvalues(obj2_vars_country)
  print(hat_values)
  max(cooks.distance(obj2_vars_country), na.rm = TRUE)
  # if max cooks distance < 1, there are no influential outliers
  
  # Multicollinearity
  # Does not need to be cheked for the simple linear regression model if outcome
  # variable is continuous but independent variable is categorical
}

--------------------------------------------------------------------
  
  dependent_vars_ri <- c("bb_ri",
                         "nb_ri",
                         "cb_ri",
                         "attitude_ri",
                         "pn_ri",
                         "pbc_ri",
                         "ef_ri")
#"intention_ri"

independent_vars_ri <- c("country",
                         # "role_total",
                         "expertise",
                         "academic_qual",
                         "gender",
                         # "involvement_ri_total",
                         # "knowledge_ri",
                         "awareness_ri_total")

ri_os_survey$country <- as.factor(ri_os_survey$country)
models <- lapply(dependent_vars_ri, function(dep) {
  lapply(independent_vars_ri, function(var) {
    formula <- as.formula(paste(dep, "~", var,
                                "+ factor(country)"))
    lm(formula, data = ri_os_survey)
  }) })

print(formula)

# Extract coefficients and p-values
results <- lapply(models, function(model) {
  coefs <- summary(model)$coefficients
  data.frame(
    Estimate = coefs[, "Estimate"],
    P_Value = coefs[, "Pr(>|t|)"]
  )
})

# Extract coefficients and p-values for all models
results <- do.call(rbind, lapply(1:length(dependent_vars_ri), function(i) {
  do.call(rbind, lapply(1:length(independent_vars_ri), function(j) {
    model_summary <- summary(models[[i]][[j]])$coefficients
    data.frame(
      Dependent = dependent_vars_ri[i],
      Independent = independent_vars_ri[j],
      Estimate = model_summary[2, "Estimate"],
      Std.Error = model_summary[2, "Std. Error"],
      P.value = model_summary[2, "Pr(>|t|)"]
    )
  }))
}))

# View the results table
print(results)
###

### attempt to create table - stuck because NA
# Create a list to hold the results
results <- lapply(dependent_vars_ri, function(dep) {
  lapply(independent_vars_ri, function(var) {
    # Build the formula
    formula <- as.formula(paste(dep, "~", var, "+ factor(country)"))
    
    # Fit the model
    model <- lm(formula, data = ri_os_survey)
    
    # Extract tidy results (estimate and p-value)
    tidy_result <- broom::tidy(model)
    
    # Extract the coefficient corresponding to the current independent variable
    result <- tidy_result %>%
      filter(term == var) %>%  
      dplyr::select(estimate, p.value)
    
    print(tidy_result)
    
    return(result)
  })
})



# Convert the results into a dataframe
table_results <- do.call(rbind, lapply(seq_along(dependent_vars_ri), function(i) {
  dep <- dependent_vars_ri[i]  # Current dependent variable
  
  do.call(rbind, lapply(seq_along(independent_vars_ri), function(j) {
    var <- independent_vars_ri[j]  # Current independent variable
    est <- results[[i]][[j]]$estimate
    pval <- results[[i]][[j]]$p.value
    
    # Format: "Estimate (P-Value)"
    result_formatted <- paste0(round(est, 3), " (", round(pval, 3), ")")
    
    # Return as dataframe row
    data.frame(
      Independent_Var = var,
      Dependent_Var = dep,
      Result = result_formatted
    )
  }))
}))

# Reshape the table to have dependent variables as columns
final_table <- table_results %>%
  pivot_wider(names_from = Dependent_Var, values_from = Result)

print(final_table)
###



# Create a list to hold results
results <- lapply(dependent_vars_ri, function(dep) {
  lapply(independent_vars_ri, function(var) {
    # Build the formula
    formula <- as.formula(paste(dep, "~", var, "+ factor(country)"))
    model <- tryCatch({
      lm(formula, data = ri_os_survey)
    }, error = function(e) return(NULL))  # Handle errors gracefully
    
    if (!is.null(model)) {
      tidy_result <- tidy(model)
      
      # Filter the term and handle empty results
      result <- tidy_result %>%
        filter(grepl(paste0("^", var, "$"), term)) %>%
        dplyr::select(estimate, p.value)
      
      if (nrow(result) == 0) {
        result <- data.frame(estimate = NA, p.value = NA)  # Safeguard against errors
      }
    } else {
      result <- data.frame(estimate = NA, p.value = NA)  # Model failed
    }
    
    return(result)
  })
})

# Transform results into a wide table
table_results <- do.call(rbind, lapply(seq_along(dependent_vars_ri), function(i) {
  dep <- dependent_vars_ri[i]
  do.call(rbind, lapply(seq_along(independent_vars_ri), function(j) {
    var <- independent_vars_ri[j]
    est <- results[[i]][[j]]$estimate
    pval <- results[[i]][[j]]$p.value
    
    # Format: "Estimate (P-Value)"
    result_formatted <- ifelse(!is.na(est),
                               paste0(round(est, 3), " (", round(pval, 3), ")"),
                               "NA")
    
    data.frame(
      Independent_Var = var,
      Dependent_Var = dep,
      Result = result_formatted
    )
  }))
}))

# Pivot into wide format
final_table <- table_results %>%
  pivot_wider(names_from = Dependent_Var, values_from = Result)

# View final table
print(final_table)



###
# Combine into a single data frame
table_rows <- rownames(results[[1]])  # Row names (independent variables)
final_table <- do.call(cbind, lapply(results, `[`, "Estimate"))
colnames(final_table) <- dependent_vars_ri
rownames(final_table) <- table_rows

# Round estimates
final_table <- round(final_table, 3)

# Optionally include p-values
final_table_pvals <- do.call(cbind, lapply(results, `[`, "P_Value"))
colnames(final_table_pvals) <- paste0(dependent_vars_ri, "_pval")
rownames(final_table_pvals) <- table_rows

# Combine estimates and p-values
full_table <- cbind(final_table, final_table_pvals)

print(full_table)

# Combine Estimate and P-value into one cell
formatted_results <- lapply(results, function(df) {
  paste0(round(df$Estimate, 3), " (", round(df$P_Value, 3), ")")
})

# Reshape the results into a table
final_table <- do.call(cbind, formatted_results)
colnames(final_table) <- dependent_vars_ri  # Add column names for dependent variables
rownames(final_table) <- rownames(results[[1]])  # Add row names for independent variables

print(final_table)

# Assuming final_table_pvals holds results you want to treat as new independent vars
confounders_as_independent_vars <- as.vector(final_table$variable_names)  # Make sure the variable names are accessible

# Create the formula dynamically and fit the regression
new_model <- lm(dependent_vars_ri ~ ., data = cbind(data, confounders_as_independent_vars))  # Use appropriate data structure
summary(new_model)


length(confounders_as_independent_vars)

#Checking code
#print(formula[1])
#print(dependent_vars_ri)
#colnames(ri_os_survey)
#anyNA(independent_vars_ri)
#is.nan(ri_os_survey$nb_ri)




# Simple Linear Regression --------------------------------------------------



# Create the model to see if there are differences in BB per country of work 
# BB = β0 + β1 country + ε

obj2_bb_ri_country <- lm(bb_ri ~  as.factor(country), 
                         data = ri_os_survey)
summary(obj2_bb_ri_country)


# Assumptions: Independence, Normality, Homoscedasticity, Linearity, 
# No influential outliers 

# Independence is checked through study design. Every subject is included once 
# and the subjects are assumed to be independent, therefor the assumption of 
# independence is not violated. 

# Normality 
#create histogram of residuals
res_obj2_bb_ri_country <- residuals(obj2_bb_ri_country)
hist(res_obj2_bb_ri_country)
#create QQ-plot
plot(obj2_bb_ri_country, 2)
# If the histogram and QQ-plot show only a small deviation from normality,
# taking a large sample size into account, we may conclude that the normality 
# assumption is met.
# HOW TO INTERPRET THE QQ PLOT SHAPE? IS NOT LINEAR BUT ALSO NOT_NOT LINEAR

#Other way to make qq plot based on Franscesco's lecture
rstandard(obj2_bb_ri_country)
qqPlot(rstandard(obj2_bb_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_country))

# Homoscedasticity
#create the residuals plot 
plot(obj2_bb_ri_country, 1)
# If this plot shows no clear increase or decrease in dispersion of residuals 
# (y-direction), homoscedasticity can be assumed

# Linearity
# check this for continuous variables, in this case BB
# partial residual plot
car::crPlots(obj2_bb_ri_country)
# Partial plot should show no (clear) deviation from linearity. 
# IS THIS THE RIGHT WAY?

# Influential Outliers 
# Cook's distance > 1?
cooks.distance(obj2_bb_ri_country)
max(cooks.distance(obj2_bb_ri_country))
# if max cooks distance < 1, there are no influential outliers

# Multicollinearity
# Does not need to be cheked for the simple linear regression model 



# Create the model to see if there are differences in nb_ri per country of work 
# nb_ri = β0 + β1 country + ε
obj2_nb_ri_country <- lm(nb_ri ~  as.factor(country), 
                         data = ri_os_survey)
summary(obj2_nb_ri_country)
# Normality (qq-plot and histogram)
#create histogram of residuals
rstandard(obj2_nb_ri_country)
qqPlot(rstandard(obj2_nb_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_country))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_country, 1)
# Linearity
car::crPlots(obj2_nb_ri_country)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_country)
max(cooks.distance(obj2_nb_ri_country))



# Create the model to see if there are differences in cb_ri per country of work 
# cb_ri = β0 + β1 country + ε
obj2_cb_ri_country <- lm(cb_ri ~  as.factor(country), 
                         data = ri_os_survey)
summary(obj2_cb_ri_country)
# Normality (qq-plot and histogram)
#create histogram of residuals
rstandard(obj2_cb_ri_country)
qqPlot(rstandard(obj2_cb_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_country))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_country, 1)
# Linearity
car::crPlots(obj2_cb_ri_country)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_country)
max(cooks.distance(obj2_cb_ri_country))


# Create the model to see if there are differences in attitude_ri per country of work 
# attitude_ri = β0 + β1 country + ε
obj2_attitude_ri_country <- lm(attitude_ri ~  as.factor(country), 
                               data = ri_os_survey)
summary(obj2_attitude_ri_country)
# Normality (qq-plot and histogram)
#create histogram of residuals
rstandard(obj2_attitude_ri_country)
qqPlot(rstandard(obj2_attitude_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_country))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_country, 1)
# Linearity
car::crPlots(obj2_attitude_ri_country)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_country)
max(cooks.distance(obj2_attitude_ri_country))



# Create the model to see if there are differences in pn_ri per country of work 
# pn_ri = β0 + β1 country + ε
obj2_pn_ri_country <- lm(pn_ri ~  as.factor(country), 
                         data = ri_os_survey)
summary(obj2_pn_ri_country)
# Normality (qq-plot and histogram)
#create histogram of residuals
rstandard(obj2_pn_ri_country)
qqPlot(rstandard(obj2_pn_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_country))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_country, 1)
# Linearity
car::crPlots(obj2_pn_ri_country)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_country)
max(cooks.distance(obj2_pn_ri_country))



# Create the model to see if there are differences in pbc_ri per country of work 
# pbc_ri = β0 + β1 country + ε
obj2_pbc_ri_country <- lm(pbc_ri ~  as.factor(country), 
                          data = ri_os_survey)
summary(obj2_pbc_ri_country)
# Normality (qq-plot and histogram)
#create histogram of residuals
rstandard(obj2_pbc_ri_country)
qqPlot(rstandard(obj2_pbc_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_country))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_country, 1)
# Linearity
car::crPlots(obj2_pbc_ri_country)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_country)
max(cooks.distance(obj2_pbc_ri_country))



# Create the model to see if there are differences in ef_ri per country of work 
# ef_ri = β0 + β1 country + ε
obj2_ef_ri_country <- lm(ef_ri ~  as.factor(country), 
                         data = ri_os_survey)
summary(obj2_ef_ri_country)
# Normality (qq-plot and histogram)
#create histogram of residuals
rstandard(obj2_ef_ri_country)
qqPlot(rstandard(obj2_ef_ri_country), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_country))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_country, 1)
# Linearity
car::crPlots(obj2_ef_ri_country)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_country)
max(cooks.distance(obj2_ef_ri_country))



# Multiple Linear Regression --------------------------------------------------



# Create the model to see if there are differences in bb_ri per role,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 role + β2 country + ε
obj2_bb_ri_role <- lm(bb_ri ~ as.factor(role_total) + as.factor(country), 
                      data = ri_os_survey)
summary(obj2_bb_ri_role)


# Assumptions: Independence, Normality, Homoscedasticity, Linearity, 
# No influential outliers, Multicollinearity 

# Independence is checked through study design. Every subject is included once 
# and the subjects are assumed to be independent, therefor the assumption of 
# independence is not violated. 

# Normality 
#create histogram of residuals
res_obj2_bb_ri_role <- residuals(obj2_bb_ri_role)
hist(res_obj2_bb_ri_role)
#create QQ-plot
plot(obj2_bb_ri_role, 2)
# If the histogram and QQ-plot show only a small deviation from normality,
# taking a large sample size into account, we may conclude that the normality 
# assumption is met.
# HOW TO INTERPRET THE QQ PLOT SHAPE? IS NOT LINEAR BUT ALSO NOT_NOT LINEAR

#Other way to make qq plot based on Franscesco's lecture
rstandard(obj2_bb_ri_role)
qqPlot(rstandard(obj2_bb_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_role))

# Homoscedasticity
#create the residuals plot 
plot(obj2_bb_ri_role, 1)
# If this plot shows no clear increase or decrease in dispersion of residuals 
# (y-direction), homoscedasticity can be assumed

# Linearity
# check this for continuous variables, in this case BB
# partial residual plot
crPlots(obj2_bb_ri_role)
# Partial plot should show no (clear) deviation from linearity. 
# IS THIS THE RIGHT WAY?

# Influential Outliers 
# Cook's distance > 1?
cooks.distance(obj2_bb_ri_role)
max(cooks.distance(obj2_bb_ri_role))
# if max cooks distance < 1, there are no influential outliers

# Multicollinearity
vif_obj2_bb_ri_role <- vif(obj2_bb_ri_role)
print(vif_obj2_bb_ri_role)
# VIF values below 10 indicate no multicollinearity problem



# Create the model to see if there are differences in nb_ri per role,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 role + β2 country + ε
obj2_nb_ri_role <- lm(nb_ri ~  as.factor(role_total) + as.factor(country), 
                      data = ri_os_survey)
summary(obj2_nb_ri_role)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_role)
qqPlot(rstandard(obj2_nb_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_role))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_role, 1)
# Linearity
car::crPlots(obj2_nb_ri_role)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_role)
max(cooks.distance(obj2_nb_ri_role))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_role <- vif(obj2_nb_ri_role)
print(vif_obj2_nb_ri_role)



# Create the model to see if there are differences in cb_ri per role,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 role + β2 country + ε
obj2_cb_ri_role <- lm(cb_ri ~  as.factor(role_total) + as.factor(country), 
                      data = ri_os_survey)
summary(obj2_cb_ri_role)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_role)
qqPlot(rstandard(obj2_cb_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_role))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_role, 1)
# Linearity
car::crPlots(obj2_cb_ri_role)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_role)
max(cooks.distance(obj2_cb_ri_role))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_role <- vif(obj2_cb_ri_role)
print(vif_obj2_cb_ri_role)



# Create the model to see if there are differences in attitude_ri per role,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 role + β2 country + ε
obj2_attitude_ri_role <- lm(attitude_ri ~  as.factor(role_total) + as.factor(country), 
                            data = ri_os_survey)
summary(obj2_attitude_ri_role)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_role)
qqPlot(rstandard(obj2_attitude_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_role))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_role, 1)
# Linearity
car::crPlots(obj2_attitude_ri_role)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_role)
max(cooks.distance(obj2_attitude_ri_role))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_role <- vif(obj2_attitude_ri_role)
print(vif_obj2_attitude_ri_role)




# Create the model to see if there are differences in pn_ri per role,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 role + β2 country + ε
obj2_pn_ri_role <- lm(pn_ri ~  as.factor(role_total) + as.factor(country), 
                      data = ri_os_survey)
summary(obj2_pn_ri_role)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_role)
qqPlot(rstandard(obj2_pn_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_role))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_role, 1)
# Linearity
car::crPlots(obj2_pn_ri_role)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_role)
max(cooks.distance(obj2_pn_ri_role))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_role <- vif(obj2_pn_ri_role)
print(vif_obj2_pn_ri_role)



# Create the model to see if there are differences in pbc_ri per role,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 role + β2 country + ε
obj2_pbc_ri_role <- lm(pbc_ri ~  as.factor(role_total) + as.factor(country), 
                       data = ri_os_survey)
summary(obj2_pbc_ri_role)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_role)
qqPlot(rstandard(obj2_pbc_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_role))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_role, 1)
# Linearity
car::crPlots(obj2_pbc_ri_role)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_role)
max(cooks.distance(obj2_pbc_ri_role))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_role <- vif(obj2_pbc_ri_role)
print(vif_obj2_pbc_ri_role)



# Create the model to see if there are differences in ef_ri per role,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 role + β2 country + ε
obj2_ef_ri_role <- lm(ef_ri ~  as.factor(role_total) + as.factor(country), 
                      data = ri_os_survey)
summary(obj2_ef_ri_role)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_role)
qqPlot(rstandard(obj2_ef_ri_role), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_role))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_role, 1)
# Linearity
car::crPlots(obj2_ef_ri_role)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_role)
max(cooks.distance(obj2_ef_ri_role))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_role <- vif(obj2_ef_ri_role)
print(vif_obj2_ef_ri_role)






# Create the model to see if there are differences in bb_ri per expertise,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 expertise + β2 country + ε
obj2_bb_ri_expertise <- lm(bb_ri ~ as.factor(expertise) + as.factor(country), 
                           data = ri_os_survey)
summary(obj2_bb_ri_expertise)
#Normality (qq-plot and histogram)
rstandard(obj2_bb_ri_expertise)
qqPlot(rstandard(obj2_bb_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_expertise))
# Homoscedasticity (residuals plot)
plot(obj2_bb_ri_expertise, 1)
# Linearity (partial residual plot)
crPlots(obj2_bb_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_bb_ri_expertise)
max(cooks.distance(obj2_bb_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_bb_ri_expertise <- vif(obj2_bb_ri_expertise)
print(vif_obj2_bb_ri_expertise)



# Create the model to see if there are differences in nb_ri per expertise,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 expertise + β2 country + ε
obj2_nb_ri_expertise <- lm(nb_ri ~  as.factor(expertise) + as.factor(country), 
                           data = ri_os_survey)
summary(obj2_nb_ri_expertise)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_expertise)
qqPlot(rstandard(obj2_nb_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_expertise))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_expertise, 1)
# Linearity
car::crPlots(obj2_nb_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_expertise)
max(cooks.distance(obj2_nb_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_expertise <- vif(obj2_nb_ri_expertise)
print(vif_obj2_nb_ri_expertise)



# Create the model to see if there are differences in cb_ri per expertise,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 expertise + β2 country + ε
obj2_cb_ri_expertise <- lm(cb_ri ~  as.factor(expertise) + as.factor(country), 
                           data = ri_os_survey)
summary(obj2_cb_ri_expertise)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_expertise)
qqPlot(rstandard(obj2_cb_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_expertise))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_expertise, 1)
# Linearity
car::crPlots(obj2_cb_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_expertise)
max(cooks.distance(obj2_cb_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_expertise <- vif(obj2_cb_ri_expertise)
print(vif_obj2_cb_ri_expertise)



# Create the model to see if there are differences in attitude_ri per expertise,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 expertise + β2 country + ε
obj2_attitude_ri_expertise <- lm(attitude_ri ~  as.factor(expertise) + as.factor(country), 
                                 data = ri_os_survey)
summary(obj2_attitude_ri_expertise)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_expertise)
qqPlot(rstandard(obj2_attitude_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_expertise))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_expertise, 1)
# Linearity
car::crPlots(obj2_attitude_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_expertise)
max(cooks.distance(obj2_attitude_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_expertise <- vif(obj2_attitude_ri_expertise)
print(vif_obj2_attitude_ri_expertise)




# Create the model to see if there are differences in pn_ri per expertise,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 expertise + β2 country + ε
obj2_pn_ri_expertise <- lm(pn_ri ~  as.factor(expertise) + as.factor(country), 
                           data = ri_os_survey)
summary(obj2_pn_ri_expertise)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_expertise)
qqPlot(rstandard(obj2_pn_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_expertise))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_expertise, 1)
# Linearity
car::crPlots(obj2_pn_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_expertise)
max(cooks.distance(obj2_pn_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_expertise <- vif(obj2_pn_ri_expertise)
print(vif_obj2_pn_ri_expertise)



# Create the model to see if there are differences in pbc_ri per expertise,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 expertise + β2 country + ε
obj2_pbc_ri_expertise <- lm(pbc_ri ~  as.factor(expertise) + as.factor(country), 
                            data = ri_os_survey)
summary(obj2_pbc_ri_expertise)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_expertise)
qqPlot(rstandard(obj2_pbc_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_expertise))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_expertise, 1)
# Linearity
car::crPlots(obj2_pbc_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_expertise)
max(cooks.distance(obj2_pbc_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_expertise <- vif(obj2_pbc_ri_expertise)
print(vif_obj2_pbc_ri_expertise)



# Create the model to see if there are differences in ef_ri per expertise,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 expertise + β2 country + ε
obj2_ef_ri_expertise <- lm(ef_ri ~  as.factor(expertise) + as.factor(country), 
                           data = ri_os_survey)
summary(obj2_ef_ri_expertise)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_expertise)
qqPlot(rstandard(obj2_ef_ri_expertise), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_expertise))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_expertise, 1)
# Linearity
car::crPlots(obj2_ef_ri_expertise)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_expertise)
max(cooks.distance(obj2_ef_ri_expertise))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_expertise <- vif(obj2_ef_ri_expertise)
print(vif_obj2_ef_ri_expertise)






# Create the model to see if there are differences in bb_ri per academic_qual,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 academic_qual + β2 country + ε
obj2_bb_ri_academic_qual <- lm(bb_ri ~ as.factor(academic_qual) + as.factor(country), 
                               data = ri_os_survey)
summary(obj2_bb_ri_academic_qual)
#Normality (qq-plot and histogram)
rstandard(obj2_bb_ri_academic_qual)
qqPlot(rstandard(obj2_bb_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_academic_qual))
# Homoscedasticity (residuals plot)
plot(obj2_bb_ri_academic_qual, 1)
# Linearity (partial residual plot)
crPlots(obj2_bb_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_bb_ri_academic_qual)
max(cooks.distance(obj2_bb_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_bb_ri_academic_qual <- vif(obj2_bb_ri_academic_qual)
print(vif_obj2_bb_ri_academic_qual)



# Create the model to see if there are differences in nb_ri per academic_qual,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 academic_qual + β2 country + ε
obj2_nb_ri_academic_qual <- lm(nb_ri ~  as.factor(academic_qual) + as.factor(country), 
                               data = ri_os_survey)
summary(obj2_nb_ri_academic_qual)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_academic_qual)
qqPlot(rstandard(obj2_nb_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_academic_qual))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_academic_qual, 1)
# Linearity
car::crPlots(obj2_nb_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_academic_qual)
max(cooks.distance(obj2_nb_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_academic_qual <- vif(obj2_nb_ri_academic_qual)
print(vif_obj2_nb_ri_academic_qual)



# Create the model to see if there are differences in cb_ri per academic_qual,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 academic_qual + β2 country + ε
obj2_cb_ri_academic_qual <- lm(cb_ri ~  as.factor(academic_qual) + as.factor(country), 
                               data = ri_os_survey)
summary(obj2_cb_ri_academic_qual)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_academic_qual)
qqPlot(rstandard(obj2_cb_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_academic_qual))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_academic_qual, 1)
# Linearity
car::crPlots(obj2_cb_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_academic_qual)
max(cooks.distance(obj2_cb_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_academic_qual <- vif(obj2_cb_ri_academic_qual)
print(vif_obj2_cb_ri_academic_qual)



# Create the model to see if there are differences in attitude_ri per academic_qual,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 academic_qual + β2 country + ε
obj2_attitude_ri_academic_qual <- lm(attitude_ri ~  as.factor(academic_qual) + as.factor(country), 
                                     data = ri_os_survey)
summary(obj2_attitude_ri_academic_qual)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_academic_qual)
qqPlot(rstandard(obj2_attitude_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_academic_qual))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_academic_qual, 1)
# Linearity
car::crPlots(obj2_attitude_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_academic_qual)
max(cooks.distance(obj2_attitude_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_academic_qual <- vif(obj2_attitude_ri_academic_qual)
print(vif_obj2_attitude_ri_academic_qual)




# Create the model to see if there are differences in pn_ri per academic_qual,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 academic_qual + β2 country + ε
obj2_pn_ri_academic_qual <- lm(pn_ri ~  as.factor(academic_qual) + as.factor(country), 
                               data = ri_os_survey)
summary(obj2_pn_ri_academic_qual)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_academic_qual)
qqPlot(rstandard(obj2_pn_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_academic_qual))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_academic_qual, 1)
# Linearity
car::crPlots(obj2_pn_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_academic_qual)
max(cooks.distance(obj2_pn_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_academic_qual <- vif(obj2_pn_ri_academic_qual)
print(vif_obj2_pn_ri_academic_qual)



# Create the model to see if there are differences in pbc_ri per academic_qual,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 academic_qual + β2 country + ε
obj2_pbc_ri_academic_qual <- lm(pbc_ri ~  as.factor(academic_qual) + as.factor(country), 
                                data = ri_os_survey)
summary(obj2_pbc_ri_academic_qual)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_academic_qual)
qqPlot(rstandard(obj2_pbc_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_academic_qual))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_academic_qual, 1)
# Linearity
car::crPlots(obj2_pbc_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_academic_qual)
max(cooks.distance(obj2_pbc_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_academic_qual <- vif(obj2_pbc_ri_academic_qual)
print(vif_obj2_pbc_ri_academic_qual)



# Create the model to see if there are differences in ef_ri per academic_qual,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 academic_qual + β2 country + ε
obj2_ef_ri_academic_qual <- lm(ef_ri ~  as.factor(academic_qual) + as.factor(country), 
                               data = ri_os_survey)
summary(obj2_ef_ri_academic_qual)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_academic_qual)
qqPlot(rstandard(obj2_ef_ri_academic_qual), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_academic_qual))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_academic_qual, 1)
# Linearity
car::crPlots(obj2_ef_ri_academic_qual)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_academic_qual)
max(cooks.distance(obj2_ef_ri_academic_qual))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_academic_qual <- vif(obj2_ef_ri_academic_qual)
print(vif_obj2_ef_ri_academic_qual)





# Create the model to see if there are differences in bb_ri per gender,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 gender + β2 country + ε
obj2_bb_ri_gender <- lm(bb_ri ~ as.factor(gender) + as.factor(country), 
                        data = ri_os_survey)
summary(obj2_bb_ri_gender)
#Normality (qq-plot and histogram)
rstandard(obj2_bb_ri_gender)
qqPlot(rstandard(obj2_bb_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_gender))
# Homoscedasticity (residuals plot)
plot(obj2_bb_ri_gender, 1)
# Linearity (partial residual plot)
crPlots(obj2_bb_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_bb_ri_gender)
max(cooks.distance(obj2_bb_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_bb_ri_gender <- vif(obj2_bb_ri_gender)
print(vif_obj2_bb_ri_gender)



# Create the model to see if there are differences in nb_ri per gender,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 gender + β2 country + ε
obj2_nb_ri_gender <- lm(nb_ri ~  as.factor(gender) + as.factor(country), 
                        data = ri_os_survey)
summary(obj2_nb_ri_gender)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_gender)
qqPlot(rstandard(obj2_nb_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_gender))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_gender, 1)
# Linearity
car::crPlots(obj2_nb_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_gender)
max(cooks.distance(obj2_nb_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_gender <- vif(obj2_nb_ri_gender)
print(vif_obj2_nb_ri_gender)



# Create the model to see if there are differences in cb_ri per gender,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 gender + β2 country + ε
obj2_cb_ri_gender <- lm(cb_ri ~  as.factor(gender) + as.factor(country), 
                        data = ri_os_survey)
summary(obj2_cb_ri_gender)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_gender)
qqPlot(rstandard(obj2_cb_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_gender))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_gender, 1)
# Linearity
car::crPlots(obj2_cb_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_gender)
max(cooks.distance(obj2_cb_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_gender <- vif(obj2_cb_ri_gender)
print(vif_obj2_cb_ri_gender)



# Create the model to see if there are differences in attitude_ri per gender,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 gender + β2 country + ε
obj2_attitude_ri_gender <- lm(attitude_ri ~  as.factor(gender) + as.factor(country), 
                              data = ri_os_survey)
summary(obj2_attitude_ri_gender)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_gender)
qqPlot(rstandard(obj2_attitude_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_gender))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_gender, 1)
# Linearity
car::crPlots(obj2_attitude_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_gender)
max(cooks.distance(obj2_attitude_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_gender <- vif(obj2_attitude_ri_gender)
print(vif_obj2_attitude_ri_gender)




# Create the model to see if there are differences in pn_ri per gender,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 gender + β2 country + ε
obj2_pn_ri_gender <- lm(pn_ri ~  as.factor(gender) + as.factor(country), 
                        data = ri_os_survey)
summary(obj2_pn_ri_gender)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_gender)
qqPlot(rstandard(obj2_pn_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_gender))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_gender, 1)
# Linearity
car::crPlots(obj2_pn_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_gender)
max(cooks.distance(obj2_pn_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_gender <- vif(obj2_pn_ri_gender)
print(vif_obj2_pn_ri_gender)



# Create the model to see if there are differences in pbc_ri per gender,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 gender + β2 country + ε
obj2_pbc_ri_gender <- lm(pbc_ri ~  as.factor(gender) + as.factor(country), 
                         data = ri_os_survey)
summary(obj2_pbc_ri_gender)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_gender)
qqPlot(rstandard(obj2_pbc_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_gender))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_gender, 1)
# Linearity
car::crPlots(obj2_pbc_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_gender)
max(cooks.distance(obj2_pbc_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_gender <- vif(obj2_pbc_ri_gender)
print(vif_obj2_pbc_ri_gender)



# Create the model to see if there are differences in ef_ri per gender,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 gender + β2 country + ε
obj2_ef_ri_gender <- lm(ef_ri ~  as.factor(gender) + as.factor(country), 
                        data = ri_os_survey)
summary(obj2_ef_ri_gender)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_gender)
qqPlot(rstandard(obj2_ef_ri_gender), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_gender))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_gender, 1)
# Linearity
car::crPlots(obj2_ef_ri_gender)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_gender)
max(cooks.distance(obj2_ef_ri_gender))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_gender <- vif(obj2_ef_ri_gender)
print(vif_obj2_ef_ri_gender)





# Create the model to see if there are differences in bb_ri per involvement_ri_total,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_bb_ri_involvement_ri_total <- lm(bb_ri ~ as.factor(involvement_ri_total) + as.factor(country), 
                                      data = ri_os_survey)
summary(obj2_bb_ri_involvement_ri_total)
#Normality (qq-plot and histogram)
rstandard(obj2_bb_ri_involvement_ri_total)
qqPlot(rstandard(obj2_bb_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_involvement_ri_total))
# Homoscedasticity (residuals plot)
plot(obj2_bb_ri_involvement_ri_total, 1)
# Linearity (partial residual plot)
crPlots(obj2_bb_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_bb_ri_involvement_ri_total)
max(cooks.distance(obj2_bb_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_bb_ri_involvement_ri_total <- vif(obj2_bb_ri_involvement_ri_total)
print(vif_obj2_bb_ri_involvement_ri_total)



# Create the model to see if there are differences in nb_ri per involvement_ri_total,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_nb_ri_involvement_ri_total <- lm(nb_ri ~  as.factor(involvement_ri_total) + as.factor(country), 
                                      data = ri_os_survey)
summary(obj2_nb_ri_involvement_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_involvement_ri_total)
qqPlot(rstandard(obj2_nb_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_involvement_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_involvement_ri_total, 1)
# Linearity
car::crPlots(obj2_nb_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_involvement_ri_total)
max(cooks.distance(obj2_nb_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_involvement_ri_total <- vif(obj2_nb_ri_involvement_ri_total)
print(vif_obj2_nb_ri_involvement_ri_total)



# Create the model to see if there are differences in cb_ri per involvement_ri_total,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_cb_ri_involvement_ri_total <- lm(cb_ri ~  as.factor(involvement_ri_total) + as.factor(country), 
                                      data = ri_os_survey)
summary(obj2_cb_ri_involvement_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_involvement_ri_total)
qqPlot(rstandard(obj2_cb_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_involvement_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_involvement_ri_total, 1)
# Linearity
car::crPlots(obj2_cb_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_involvement_ri_total)
max(cooks.distance(obj2_cb_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_involvement_ri_total <- vif(obj2_cb_ri_involvement_ri_total)
print(vif_obj2_cb_ri_involvement_ri_total)



# Create the model to see if there are differences in attitude_ri per involvement_ri_total,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_attitude_ri_involvement_ri_total <- lm(attitude_ri ~  as.factor(involvement_ri_total) + as.factor(country), 
                                            data = ri_os_survey)
summary(obj2_attitude_ri_involvement_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_involvement_ri_total)
qqPlot(rstandard(obj2_attitude_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_involvement_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_involvement_ri_total, 1)
# Linearity
car::crPlots(obj2_attitude_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_involvement_ri_total)
max(cooks.distance(obj2_attitude_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_involvement_ri_total <- vif(obj2_attitude_ri_involvement_ri_total)
print(vif_obj2_attitude_ri_involvement_ri_total)




# Create the model to see if there are differences in pn_ri per involvement_ri_total,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_pn_ri_involvement_ri_total <- lm(pn_ri ~  as.factor(involvement_ri_total) + as.factor(country), 
                                      data = ri_os_survey)
summary(obj2_pn_ri_involvement_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_involvement_ri_total)
qqPlot(rstandard(obj2_pn_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_involvement_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_involvement_ri_total, 1)
# Linearity
car::crPlots(obj2_pn_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_involvement_ri_total)
max(cooks.distance(obj2_pn_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_involvement_ri_total <- vif(obj2_pn_ri_involvement_ri_total)
print(vif_obj2_pn_ri_involvement_ri_total)



# Create the model to see if there are differences in pbc_ri per involvement_ri_total,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_pbc_ri_involvement_ri_total <- lm(pbc_ri ~  as.factor(involvement_ri_total) + as.factor(country), 
                                       data = ri_os_survey)
summary(obj2_pbc_ri_involvement_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_involvement_ri_total)
qqPlot(rstandard(obj2_pbc_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_involvement_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_involvement_ri_total, 1)
# Linearity
car::crPlots(obj2_pbc_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_involvement_ri_total)
max(cooks.distance(obj2_pbc_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_involvement_ri_total <- vif(obj2_pbc_ri_involvement_ri_total)
print(vif_obj2_pbc_ri_involvement_ri_total)



# Create the model to see if there are differences in ef_ri per involvement_ri_total,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 involvement_ri_total + β2 country + ε
obj2_ef_ri_involvement_ri_total <- lm(ef_ri ~  as.factor(involvement_ri_total) + as.factor(country), 
                                      data = ri_os_survey)
summary(obj2_ef_ri_involvement_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_involvement_ri_total)
qqPlot(rstandard(obj2_ef_ri_involvement_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_involvement_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_involvement_ri_total, 1)
# Linearity
car::crPlots(obj2_ef_ri_involvement_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_involvement_ri_total)
max(cooks.distance(obj2_ef_ri_involvement_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_involvement_ri_total <- vif(obj2_ef_ri_involvement_ri_total)
print(vif_obj2_ef_ri_involvement_ri_total)





# Create the model to see if there are differences in bb_ri per knowledge_ri,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_bb_ri_knowledge_ri <- lm(bb_ri ~ as.factor(knowledge_ri) + as.factor(country), 
                              data = ri_os_survey)
summary(obj2_bb_ri_knowledge_ri)
#Normality (qq-plot and histogram)
rstandard(obj2_bb_ri_knowledge_ri)
qqPlot(rstandard(obj2_bb_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_knowledge_ri))
# Homoscedasticity (residuals plot)
plot(obj2_bb_ri_knowledge_ri, 1)
# Linearity (partial residual plot)
crPlots(obj2_bb_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_bb_ri_knowledge_ri)
max(cooks.distance(obj2_bb_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_bb_ri_knowledge_ri <- vif(obj2_bb_ri_knowledge_ri)
print(vif_obj2_bb_ri_knowledge_ri)



# Create the model to see if there are differences in nb_ri per knowledge_ri,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_nb_ri_knowledge_ri <- lm(nb_ri ~  as.factor(knowledge_ri) + as.factor(country), 
                              data = ri_os_survey)
summary(obj2_nb_ri_knowledge_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_knowledge_ri)
qqPlot(rstandard(obj2_nb_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_knowledge_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_knowledge_ri, 1)
# Linearity
car::crPlots(obj2_nb_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_knowledge_ri)
max(cooks.distance(obj2_nb_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_knowledge_ri <- vif(obj2_nb_ri_knowledge_ri)
print(vif_obj2_nb_ri_knowledge_ri)



# Create the model to see if there are differences in cb_ri per knowledge_ri,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_cb_ri_knowledge_ri <- lm(cb_ri ~  as.factor(knowledge_ri) + as.factor(country), 
                              data = ri_os_survey)
summary(obj2_cb_ri_knowledge_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_knowledge_ri)
qqPlot(rstandard(obj2_cb_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_knowledge_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_knowledge_ri, 1)
# Linearity
car::crPlots(obj2_cb_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_knowledge_ri)
max(cooks.distance(obj2_cb_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_knowledge_ri <- vif(obj2_cb_ri_knowledge_ri)
print(vif_obj2_cb_ri_knowledge_ri)



# Create the model to see if there are differences in attitude_ri per knowledge_ri,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_attitude_ri_knowledge_ri <- lm(attitude_ri ~  as.factor(knowledge_ri) + as.factor(country), 
                                    data = ri_os_survey)
summary(obj2_attitude_ri_knowledge_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_knowledge_ri)
qqPlot(rstandard(obj2_attitude_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_knowledge_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_knowledge_ri, 1)
# Linearity
car::crPlots(obj2_attitude_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_knowledge_ri)
max(cooks.distance(obj2_attitude_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_knowledge_ri <- vif(obj2_attitude_ri_knowledge_ri)
print(vif_obj2_attitude_ri_knowledge_ri)




# Create the model to see if there are differences in pn_ri per knowledge_ri,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_pn_ri_knowledge_ri <- lm(pn_ri ~  as.factor(knowledge_ri) + as.factor(country), 
                              data = ri_os_survey)
summary(obj2_pn_ri_knowledge_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_knowledge_ri)
qqPlot(rstandard(obj2_pn_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_knowledge_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_knowledge_ri, 1)
# Linearity
car::crPlots(obj2_pn_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_knowledge_ri)
max(cooks.distance(obj2_pn_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_knowledge_ri <- vif(obj2_pn_ri_knowledge_ri)
print(vif_obj2_pn_ri_knowledge_ri)



# Create the model to see if there are differences in pbc_ri per knowledge_ri,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_pbc_ri_knowledge_ri <- lm(pbc_ri ~  as.factor(knowledge_ri) + as.factor(country), 
                               data = ri_os_survey)
summary(obj2_pbc_ri_knowledge_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_knowledge_ri)
qqPlot(rstandard(obj2_pbc_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_knowledge_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_knowledge_ri, 1)
# Linearity
car::crPlots(obj2_pbc_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_knowledge_ri)
max(cooks.distance(obj2_pbc_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_knowledge_ri <- vif(obj2_pbc_ri_knowledge_ri)
print(vif_obj2_pbc_ri_knowledge_ri)



# Create the model to see if there are differences in ef_ri per knowledge_ri,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 knowledge_ri + β2 country + ε
obj2_ef_ri_knowledge_ri <- lm(ef_ri ~  as.factor(knowledge_ri) + as.factor(country), 
                              data = ri_os_survey)
summary(obj2_ef_ri_knowledge_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_knowledge_ri)
qqPlot(rstandard(obj2_ef_ri_knowledge_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_knowledge_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_knowledge_ri, 1)
# Linearity
car::crPlots(obj2_ef_ri_knowledge_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_knowledge_ri)
max(cooks.distance(obj2_ef_ri_knowledge_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_knowledge_ri <- vif(obj2_ef_ri_knowledge_ri)
print(vif_obj2_ef_ri_knowledge_ri)





# Create the model to see if there are differences in bb_ri per awareness_ri_total,
# with a fixed effect for country of work 
# bb_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_bb_ri_awareness_ri_total <- lm(bb_ri ~ as.factor(awareness_ri_total) + as.factor(country), 
                                    data = ri_os_survey)
summary(obj2_bb_ri_awareness_ri_total)
#Normality (qq-plot and histogram)
rstandard(obj2_bb_ri_awareness_ri_total)
qqPlot(rstandard(obj2_bb_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_bb_ri_awareness_ri_total))
# Homoscedasticity (residuals plot)
plot(obj2_bb_ri_awareness_ri_total, 1)
# Linearity (partial residual plot)
crPlots(obj2_bb_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_bb_ri_awareness_ri_total)
max(cooks.distance(obj2_bb_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_bb_ri_awareness_ri_total <- vif(obj2_bb_ri_awareness_ri_total)
print(vif_obj2_bb_ri_awareness_ri_total)



# Create the model to see if there are differences in nb_ri per awareness_ri_total,
# with a fixed effect for country of work 
# nb_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_nb_ri_awareness_ri_total <- lm(nb_ri ~  as.factor(awareness_ri_total) + as.factor(country), 
                                    data = ri_os_survey)
summary(obj2_nb_ri_awareness_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_nb_ri_awareness_ri_total)
qqPlot(rstandard(obj2_nb_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_nb_ri_awareness_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_nb_ri_awareness_ri_total, 1)
# Linearity
car::crPlots(obj2_nb_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_nb_ri_awareness_ri_total)
max(cooks.distance(obj2_nb_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_nb_ri_awareness_ri_total <- vif(obj2_nb_ri_awareness_ri_total)
print(vif_obj2_nb_ri_awareness_ri_total)



# Create the model to see if there are differences in cb_ri per awareness_ri_total,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_cb_ri_awareness_ri_total <- lm(cb_ri ~  as.factor(awareness_ri_total) + as.factor(country), 
                                    data = ri_os_survey)
summary(obj2_cb_ri_awareness_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_awareness_ri_total)
qqPlot(rstandard(obj2_cb_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_awareness_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_awareness_ri_total, 1)
# Linearity
car::crPlots(obj2_cb_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_awareness_ri_total)
max(cooks.distance(obj2_cb_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_awareness_ri_total <- vif(obj2_cb_ri_awareness_ri_total)
print(vif_obj2_cb_ri_awareness_ri_total)



# Create the model to see if there are differences in attitude_ri per awareness_ri_total,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_attitude_ri_awareness_ri_total <- lm(attitude_ri ~  as.factor(awareness_ri_total) + as.factor(country), 
                                          data = ri_os_survey)
summary(obj2_attitude_ri_awareness_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_awareness_ri_total)
qqPlot(rstandard(obj2_attitude_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_awareness_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_awareness_ri_total, 1)
# Linearity
car::crPlots(obj2_attitude_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_awareness_ri_total)
max(cooks.distance(obj2_attitude_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_awareness_ri_total <- vif(obj2_attitude_ri_awareness_ri_total)
print(vif_obj2_attitude_ri_awareness_ri_total)




# Create the model to see if there are differences in pn_ri per awareness_ri_total,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_pn_ri_awareness_ri_total <- lm(pn_ri ~  as.factor(awareness_ri_total) + as.factor(country), 
                                    data = ri_os_survey)
summary(obj2_pn_ri_awareness_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_awareness_ri_total)
qqPlot(rstandard(obj2_pn_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_awareness_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_awareness_ri_total, 1)
# Linearity
car::crPlots(obj2_pn_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_awareness_ri_total)
max(cooks.distance(obj2_pn_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_awareness_ri_total <- vif(obj2_pn_ri_awareness_ri_total)
print(vif_obj2_pn_ri_awareness_ri_total)



# Create the model to see if there are differences in pbc_ri per awareness_ri_total,
# with a fixed effect for country of work 
# pbc_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_pbc_ri_awareness_ri_total <- lm(pbc_ri ~  as.factor(awareness_ri_total) + as.factor(country), 
                                     data = ri_os_survey)
summary(obj2_pbc_ri_awareness_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_pbc_ri_awareness_ri_total)
qqPlot(rstandard(obj2_pbc_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pbc_ri_awareness_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_pbc_ri_awareness_ri_total, 1)
# Linearity
car::crPlots(obj2_pbc_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pbc_ri_awareness_ri_total)
max(cooks.distance(obj2_pbc_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_pbc_ri_awareness_ri_total <- vif(obj2_pbc_ri_awareness_ri_total)
print(vif_obj2_pbc_ri_awareness_ri_total)



# Create the model to see if there are differences in ef_ri per awareness_ri_total,
# with a fixed effect for country of work 
# ef_ri = β0 + β1 awareness_ri_total + β2 country + ε
obj2_ef_ri_awareness_ri_total <- lm(ef_ri ~  as.factor(awareness_ri_total) + as.factor(country), 
                                    data = ri_os_survey)
summary(obj2_ef_ri_awareness_ri_total)
# Normality (qq-plot and histogram)
rstandard(obj2_ef_ri_awareness_ri_total)
qqPlot(rstandard(obj2_ef_ri_awareness_ri_total), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_ef_ri_awareness_ri_total))
# Homoscedasticity (residuals plot) 
plot(obj2_ef_ri_awareness_ri_total, 1)
# Linearity
car::crPlots(obj2_ef_ri_awareness_ri_total)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_ef_ri_awareness_ri_total)
max(cooks.distance(obj2_ef_ri_awareness_ri_total))
# Multicollinearity (VIF values below 10)
vif_obj2_ef_ri_awareness_ri_total <- vif(obj2_ef_ri_awareness_ri_total)
print(vif_obj2_ef_ri_awareness_ri_total)






# Ordinal Logistic Regression -------------------------------------------------

# for the variable attitude_os_crucial_quality, which is only based on 1 likert scale score


# Convert ordinal to factor for the outcome variable
ri_os_survey$attitude_os_crucial_quality <- factor(ri_os_survey$attitude_os_crucial_quality,
                                                   levels = c("1", "2", "3", "4", "5"), ordered = TRUE)
# Convert categorical variables to as.factor, as this is not possible inside the polr function
ri_os_survey$role_total <- as.factor(ri_os_survey$role_total)
ri_os_survey$expertise <- as.factor(ri_os_survey$expertise)
ri_os_survey$academic_qual <- as.factor(ri_os_survey$academic_qual)
ri_os_survey$gender <- as.factor(ri_os_survey$gender)
ri_os_survey$involvement_ri_total <- as.factor(ri_os_survey$involvement_ri_total)
ri_os_survey$knowledge_ri <- as.factor(ri_os_survey$knowledge_ri)
ri_os_survey$awareness_ri_total <- as.factor(ri_os_survey$awareness_ri_total)

# Create the model to see if there are differences in attitude_os per category of role,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 role + β2 country + ε

obj2_attitude_os_role <- polr(attitude_os_crucial_quality ~ role_total + country, 
                              data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_role)


# Assumptions: Independence, Proportional Odds, Linearity with Log-Odds, 
#  Multicollinearity, Sufficient Sample Size


# Independence is checked through study design. Every subject is included once 
# and the subjects are assumed to be independent, therefore the assumption of 
# independence is not violated. 

# Proportional Odds
# GIVES ERROR Error in eval(predvars, data, env) : object 'role_total' not found
brant(obj2_attitude_os_role)
# If the test indicates a significant result for any predictor p-value, 
# this suggests that the proportional odds assumption may be violated for that variable.

# Linearity with Log-Odds
# Does not need to be checked for ordinal logistic regression
# Partial plot should show no (clear) deviation from linearity. 
# IS THIS THE RIGHT WAY?
log_odds_attitude_os_role <- coef(obj2_attitude_os_role)
log_odds_attitude_os_role

# Multicollinearity
vif_obj2_attitude_os_role <- vif(obj2_attitude_os_role)
print(vif_obj2_attitude_os_role)
# VIF values below 10 indicate no multicollinearity problem

# Sufficient Sample Size
# Minimum of 15 events per variable
# Calculate EPV




# Create the model to see if there are differences in attitude_os per category of expertise,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 expertise + β2 country + ε

obj2_attitude_os_expertise <- polr(attitude_os_crucial_quality ~ expertise + country, 
                                   data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_expertise)
# Proportional Odds
brant(obj2_attitude_os_expertise)
# Multicollinearity
vif_obj2_attitude_os_expertise <- vif(obj2_attitude_os_expertise)
print(vif_obj2_attitude_os_expertise)
# VIF values below 10 indicate no multicollinearity problem



# Create the model to see if there are differences in attitude_os per category of academic_qual,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 academic_qual + β2 country + ε

obj2_attitude_os_academic_qual <- polr(attitude_os_crucial_quality ~ academic_qual + country, 
                                       data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_academic_qual)
# Proportional Odds
brant(obj2_attitude_os_academic_qual)
# Multicollinearity
vif_obj2_attitude_os_academic_qual <- vif(obj2_attitude_os_academic_qual)
print(vif_obj2_attitude_os_academic_qual)
# VIF values below 10 indicate no multicollinearity problem



# Create the model to see if there are differences in attitude_os per category of gender,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 gender + β2 country + ε

obj2_attitude_os_gender <- polr(attitude_os_crucial_quality ~ gender + country, 
                                data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_gender)
# Proportional Odds
brant(obj2_attitude_os_gender)
# Multicollinearity
vif_obj2_attitude_os_gender <- vif(obj2_attitude_os_gender)
print(vif_obj2_attitude_os_gender)
# VIF values below 10 indicate no multicollinearity problem



# Create the model to see if there are differences in attitude_os per category of involvement_ri_total,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 involvement_ri_total + β2 country + ε

obj2_attitude_os_involvement_ri_total <- polr(attitude_os_crucial_quality ~ involvement_ri_total + country, 
                                              data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_involvement_ri_total)
# Proportional Odds
brant(obj2_attitude_os_involvement_ri_total)
# Multicollinearity
vif_obj2_attitude_os_involvement_ri_total <- vif(obj2_attitude_os_involvement_ri_total)
print(vif_obj2_attitude_os_involvement_ri_total)
# VIF values below 10 indicate no multicollinearity problem



# Create the model to see if there are differences in attitude_os per category of knowledge_ri,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 knowledge_ri + β2 country + ε

obj2_attitude_os_knowledge_ri <- polr(attitude_os_crucial_quality ~ knowledge_ri + country, 
                                      data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_knowledge_ri)
# Proportional Odds
brant(obj2_attitude_os_knowledge_ri)
# Multicollinearity
vif_obj2_attitude_os_knowledge_ri <- vif(obj2_attitude_os_knowledge_ri)
print(vif_obj2_attitude_os_knowledge_ri)
# VIF values below 10 indicate no multicollinearity problem



# Create the model to see if there are differences in attitude_os per category of awareness_ri_total,
# with a fixed effect for country of work 
# attitude_os_crucial_quality = β0 + β1 awareness_ri_total + β2 country + ε

obj2_attitude_os_awareness_ri_total <- polr(attitude_os_crucial_quality ~ awareness_ri_total + country, 
                                            data = ri_os_survey, method = "logistic")
summary(obj2_attitude_os_awareness_ri_total)
# Proportional Odds
brant(obj2_attitude_os_awareness_ri_total)
# Multicollinearity
vif_obj2_attitude_os_awareness_ri_total <- vif(obj2_attitude_os_awareness_ri_total)
print(vif_obj2_attitude_os_awareness_ri_total)
# VIF values below 10 indicate no multicollinearity problem



# Multinominal Logistic Regression -------------------------------------------------


ri_os_survey$intention_ri <- factor(ri_os_survey$intention_ri)

# Create the model to see if there are differences in intention to comply per country
# intention = β0 + β1 country + ε
obj2_ri_intention_country <- multinom(intention_ri ~ as.factor(country), 
                                      data = ri_os_survey)
summary(obj2_ri_intention_country)
tidy(obj2_ri_intention_country, conf.int = TRUE)
# interpreting model: https://bookdown.org/sarahwerth2024/CategoricalBook/multinomial-logit-regression-r.html
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_country)$coefficients / summary(obj2_ri_intention_country)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_country <- vif(obj2_ri_intention_country)
print(vif_obj2_ri_intention_country)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem


# Create the model to see if there are differences in intention to comply per role,
# with a fixed effect for country of work 
# intention = β0 + β1 role + β2 country + ε
obj2_ri_intention_role <- multinom(intention_ri ~ as.factor(role_total) + as.factor(country), 
                                   data = ri_os_survey)
summary(obj2_ri_intention_role)

# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_role)$coefficients / summary(obj2_ri_intention_role)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values


# Assumptions: Independence, Linearity with Log-Odds, 
#  Multicollinearity, Sufficient Sample Size


# Independence is checked through study design. Every subject is included once 
# and the subjects are assumed to be independent, therefor the assumption of 
# independence is not violated. 

# Linearity with Log-Odds
# car::crPlots(obj2_ri_intention_role) does not work, no continuous variables
# Partial plot should show no (clear) deviation from linearity. 


# Multicollinearity
vif_obj2_ri_intention_role <- vif(obj2_ri_intention_role)
print(vif_obj2_ri_intention_role)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem

# Sufficient Sample Size
# Minimum of 15 events per variable
# Calculate EPV



# Create the model to see if there are differences in intention to comply per expertise,
# with a fixed effect for country of work 
# intention = β0 + β1 expertise + β2 country + ε
obj2_ri_intention_expertise <- multinom(intention_ri ~ as.factor(expertise) + as.factor(country), 
                                        data = ri_os_survey)
summary(obj2_ri_intention_expertise)
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_expertise)$coefficients / summary(obj2_ri_intention_expertise)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_expertise <- vif(obj2_ri_intention_expertise)
print(vif_obj2_ri_intention_expertise)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per academic_qual,
# with a fixed effect for country of work 
# intention = β0 + β1 academic_qual + β2 country + ε
obj2_ri_intention_academic_qual <- multinom(intention_ri ~ as.factor(academic_qual) + as.factor(country), 
                                            data = ri_os_survey)
summary(obj2_ri_intention_academic_qual)
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_academic_qual)$coefficients / summary(obj2_ri_intention_academic_qual)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_academic_qual <- vif(obj2_ri_intention_academic_qual)
print(vif_obj2_ri_intention_academic_qual)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per gender,
# with a fixed effect for country of work 
# intention = β0 + β1 gender + β2 country + ε
obj2_ri_intention_gender <- multinom(intention_ri ~ as.factor(gender) + as.factor(country), 
                                     data = ri_os_survey)
summary(obj2_ri_intention_gender)
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_gender)$coefficients / summary(obj2_ri_intention_gender)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_gender <- vif(obj2_ri_intention_gender)
print(vif_obj2_ri_intention_gender)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per involvement_ri_total,
# with a fixed effect for country of work 
# intention = β0 + β1 involvement_ri_total + β2 country + ε
obj2_ri_intention_involvement_ri_total <- multinom(intention_ri ~ as.factor(involvement_ri_total) + as.factor(country), 
                                                   data = ri_os_survey)
summary(obj2_ri_intention_involvement_ri_total)
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_involvement_ri_total)$coefficients / summary(obj2_ri_intention_involvement_ri_total)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_involvement_ri_total <- vif(obj2_ri_intention_involvement_ri_total)
print(vif_obj2_ri_intention_involvement_ri_total)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per knowledge_ri,
# with a fixed effect for country of work 
# intention = β0 + β1 knowledge_ri + β2 country + ε
obj2_ri_intention_knowledge_ri <- multinom(intention_ri ~ as.factor(knowledge_ri) + as.factor(country), 
                                           data = ri_os_survey)
summary(obj2_ri_intention_knowledge_ri)
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_knowledge_ri)$coefficients / summary(obj2_ri_intention_knowledge_ri)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_knowledge_ri <- vif(obj2_ri_intention_knowledge_ri)
print(vif_obj2_ri_intention_knowledge_ri)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per awareness_ri_total,
# with a fixed effect for country of work 
# intention = β0 + β1 awareness_ri_total + β2 country + ε
obj2_ri_intention_awareness_ri_total <- multinom(intention_ri ~ as.factor(awareness_ri_total) + as.factor(country), 
                                                 data = ri_os_survey)
summary(obj2_ri_intention_awareness_ri_total)
# Calculate z-values and p-values
z_values <- summary(obj2_ri_intention_awareness_ri_total)$coefficients / summary(obj2_ri_intention_awareness_ri_total)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_ri_intention_awareness_ri_total <- vif(obj2_ri_intention_awareness_ri_total)
print(vif_obj2_ri_intention_awareness_ri_total)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem





# Create the model to see if there are differences in intention to comply per role,
# with a fixed effect for country of work 
# intention = β0 + β1 role + β2 country + ε
obj2_os_intention_role <- multinom(intention_os ~ as.factor(role_total) + as.factor(country), 
                                   data = ri_os_survey)
summary(obj2_os_intention_role)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_role)$coefficients / summary(obj2_os_intention_role)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_os_intention_role <- vif(obj2_os_intention_role)
print(vif_obj2_os_intention_role)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per expertise,
# with a fixed effect for country of work 
# intention = β0 + β1 expertise + β2 country + ε
obj2_os_intention_expertise <- multinom(intention_os ~ as.factor(expertise) + as.factor(country), 
                                        data = ri_os_survey)
summary(obj2_os_intention_expertise)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_expertise)$coefficients / summary(obj2_os_intention_expertise)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_os_intention_expertise <- vif(obj2_os_intention_expertise)
print(vif_obj2_os_intention_expertise)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per academic_qual,
# with a fixed effect for country of work 
# intention = β0 + β1 academic_qual + β2 country + ε
obj2_os_intention_academic_qual <- multinom(intention_os ~ as.factor(academic_qual) + as.factor(country), 
                                            data = ri_os_survey)
summary(obj2_os_intention_academic_qual)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_academic_qual)$coefficients / summary(obj2_os_intention_academic_qual)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollineristy
vif_obj2_os_intention_academic_qual <- vif(obj2_os_intention_academic_qual)
print(vif_obj2_os_intention_academic_qual)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per gender,
# with a fixed effect for country of work 
# intention = β0 + β1 gender + β2 country + ε
obj2_os_intention_gender <- multinom(intention_os ~ as.factor(gender) + as.factor(country), 
                                     data = ri_os_survey)
summary(obj2_os_intention_gender)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_gender)$coefficients / summary(obj2_os_intention_gender)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_os_intention_gender <- vif(obj2_os_intention_gender)
print(vif_obj2_os_intention_gender)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per involvement_os_total,
# with a fixed effect for country of work 
# intention = β0 + β1 involvement_os_total + β2 country + ε
obj2_os_intention_involvement_ri_total <- multinom(intention_os ~ as.factor(involvement_ri_total) + as.factor(country), 
                                                   data = ri_os_survey)
summary(obj2_os_intention_involvement_ri_total)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_involvement_ri_total)$coefficients / summary(obj2_os_intention_involvement_ri_total)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_os_intention_involvement_ri_total <- vif(obj2_os_intention_involvement_ri_total)
print(vif_obj2_os_intention_involvement_ri_total)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per knowledge_os,
# with a fixed effect for country of work 
# intention = β0 + β1 knowledge_os + β2 country + ε
obj2_os_intention_knowledge_os <- multinom(intention_os ~ as.factor(knowledge_os) + as.factor(country), 
                                           data = ri_os_survey)
summary(obj2_os_intention_knowledge_os)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_knowledge_os)$coefficients / summary(obj2_os_intention_knowledge_os)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_os_intention_knowledge_os <- vif(obj2_os_intention_knowledge_os)
print(vif_obj2_os_intention_knowledge_os)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem



# Create the model to see if there are differences in intention to comply per awareness_os_total,
# with a fixed effect for country of work 
# intention = β0 + β1 awareness_os_total + β2 country + ε
obj2_os_intention_awareness_os_total <- multinom(intention_os ~ as.factor(awareness_os_total) + as.factor(country), 
                                                 data = ri_os_survey)
summary(obj2_os_intention_awareness_os_total)
# Calculate z-values and p-values
z_values <- summary(obj2_os_intention_awareness_os_total)$coefficients / summary(obj2_os_intention_awareness_os_total)$standard.errors
p_values <- (1 - pnorm(abs(z_values), 0, 1)) * 2
p_values
# Multicollinearity
vif_obj2_os_intention_awareness_os_total <- vif(obj2_os_intention_awareness_os_total)
print(vif_obj2_os_intention_awareness_os_total)
# GVIF^(1/2*DF) values below 5 indicate no multicollinearity problem






# Objective 3 ------------------------------------------------------------------



# starting model attitude_ri and bb_ri (unadjusted)
# Create the model to see if there are differences in attitude_ri per bb_ri,
# with a fixed effect for country of work 
# attitude_ri = β0 + β1 bb_ri + β2 country + ε
obj2_attitude_ri_bb_ri <- lm(attitude_ri ~  bb_ri + as.factor(country), 
                             data = ri_os_survey)
summary(obj2_attitude_ri_bb_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_attitude_ri_bb_ri)
qqPlot(rstandard(obj2_attitude_ri_bb_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_attitude_ri_bb_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_attitude_ri_bb_ri, 1)
# Linearity
car::crPlots(obj2_attitude_ri_bb_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_attitude_ri_bb_ri)
max(cooks.distance(obj2_attitude_ri_bb_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_bb_ri <- vif(obj2_attitude_ri_bb_ri)
print(obj2_attitude_ri_bb_ri)


# full model attitude_ri and bb_ri 
obj2_full_attitude_ri_bb_ri <- lm(attitude_ri ~  bb_ri + as.factor(country) 
                                  + as.factor(role_total)
                                  + as.factor(years_role_total)
                                  + as.factor(expertise)
                                  + as.factor(academic_qual)
                                  + as.factor(gender)
                                  + as.factor(involvement_ri_total)
                                  + as.factor(knowledge_ri)
                                  + as.factor(awareness_ri_total)
                                  + ef_ri_qualified_person
                                  + ef_ri_priority
                                  + ef_ri_misconduct_sanctioned
                                  + ef_ri_adequate_training
                                  , 
                                  data = ri_os_survey)
summary(obj2_full_attitude_ri_bb_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_full_attitude_ri_bb_ri)
qqPlot(rstandard(obj2_full_attitude_ri_bb_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_full_attitude_ri_bb_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_full_attitude_ri_bb_ri, 1)
# Linearity
car::crPlots(obj2_full_attitude_ri_bb_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_full_attitude_ri_bb_ri)
max(cooks.distance(obj2_full_attitude_ri_bb_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_attitude_ri_bb_ri <- vif(obj2_full_attitude_ri_bb_ri)
print(obj2_full_attitude_ri_bb_ri)


lrtest(obj2_attitude_ri_bb_ri, obj2_full_attitude_ri_bb_ri)


# Create the model to see if there are differences in pn_ri per nb_ri,
# with a fixed effect for country of work 
# pn_ri = β0 + β1 nb_ri + β2 country + ε
obj2_pn_ri_nb_ri <- lm(pn_ri ~  nb_ri + as.factor(country), 
                       data = ri_os_survey)
summary(obj2_pn_ri_nb_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_pn_ri_nb_ri)
qqPlot(rstandard(obj2_pn_ri_nb_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_pn_ri_nb_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_pn_ri_nb_ri, 1)
# Linearity
car::crPlots(obj2_pn_ri_nb_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_pn_ri_nb_ri)
max(cooks.distance(obj2_pn_ri_nb_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_pn_ri_nb_ri <- vif(obj2_pn_ri_nb_ri)
print(obj2_pn_ri_nb_ri)



# Create the model to see if there are differences in cb_ri per pbc_ri,
# with a fixed effect for country of work 
# cb_ri = β0 + β1 pbc_ri + β2 country + ε
obj2_cb_ri_pbc_ri <- lm(cb_ri ~  pbc_ri + as.factor(country), 
                        data = ri_os_survey)
summary(obj2_cb_ri_pbc_ri)
# Normality (qq-plot and histogram)
rstandard(obj2_cb_ri_pbc_ri)
qqPlot(rstandard(obj2_cb_ri_pbc_ri), dist="norm", ylab="standardized redisuals")
hist(rstandard(obj2_cb_ri_pbc_ri))
# Homoscedasticity (residuals plot) 
plot(obj2_cb_ri_pbc_ri, 1)
# Linearity
car::crPlots(obj2_cb_ri_pbc_ri)
# Influential Outliers (Cook's distance > 1?)
cooks.distance(obj2_cb_ri_pbc_ri)
max(cooks.distance(obj2_cb_ri_pbc_ri))
# Multicollinearity (VIF values below 10)
vif_obj2_cb_ri_pbc_ri <- vif(obj2_cb_ri_pbc_ri)
print(obj2_cb_ri_pbc_ri)