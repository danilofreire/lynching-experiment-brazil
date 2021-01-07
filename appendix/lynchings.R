##############################################################
## "Casting the First Stone: Lynching Attitudes in Brazil"  ##
## Danilo Freire and David Skarbek                          ##
## January 2021                                             ##
##############################################################

# Install and load required packages
packages <- c("cjoint", "cregg", "kableExtra",
              "janitor", "stargazer", "tidyverse")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

# Load the dataset, remove unused rows and columns,
# and convert variable names to snake case
df <- read_csv("../data/data.csv") %>%
  clean_names() %>%
  mutate(response_id            = as.character(response_id),
         consent                = as.factor(q1),
         progress               = as.numeric(progress),
         finished               = as.factor(finished),
         age                    = as.numeric(q2),
         gender                 = as.factor(q3),
         race                   = as.factor(q4),
         education              = as.factor(q5),
         region                 = as.factor(q6),
         household_income       = as.factor(q7),
         ideology               = as.factor(q8),
         death_penalty          = as.factor(q9),
         previous_victim        = as.character(q10),
         previous_victim_text   = as.character(q10_text),
         views_police           = as.factor(q11),
         views_justice          = as.factor(q12),
         exp02_control          = as.numeric(q18),
         exp02_police           = as.numeric(q19),
         exp02_slow_justice     = as.numeric(q20),
         exp02_small_punishment = as.numeric(q21),
         exp03_control          = as.numeric(q22),
         exp03_constitution     = as.numeric(q23),
         exp03_rights           = as.numeric(q24),
         exp03_vendetta         = as.numeric(q25)) %>%
  slice(-1L) %>%
  select(-c(q1:q12, q18:q25)) %>%
  relocate(response_id, consent, progress, finished,
           location_latitude, location_longitude) %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(across(where(is.factor), tolower))

# write_csv(df, "../data/data-portuguese.csv")

# Translate factor values from Portuguese to English
df <- df %>%
  mutate(consent = recode(consent,
                          "concordo"     = "Agree",
                          "não concordo" = "Disagree"),
         gender = recode(gender,
                         "feminino"              = "Female",
                         "masculino"             = "Male",
                         "outro"                 = "Other",
                         "prefiro não responder" = "Rather Not Say"),
         race = recode(race,
                       "amarela"               = "Asian",
                       "branca"                = "White",
                       "indígena"              = "Indigenous",
                       "outra"                 = "Other",
                       "parda"                 = "Mixed Race",
                       "prefiro não responder" = "Rather Not Say",
                       "preta"                 = "Black"),
         education = recode(education,
                            "da 1ª à 4ª série do ensino fundamental (antigo primário)" = "Primary School",
                            "da 5ª à 8ª série do ensino fundamental (antigo ginásio)"  = "Secondary School",
                            "ensino médio (antigo 2º grau)"                            = "High School",
                            "ensino superior"                                          = "College",
                            "mestrado ou doutorado"                                    = "Graduate School",
                            "não sei"                                                  = "Don't Know"),
         region = recode(region,
                         "centro-oeste" = "Center-West",
                         "nordeste"     = "Northeast",
                         "norte"        = "North",
                         "sudeste"      = "Southeast",
                         "sul"          = "South"),
         household_income = recode(household_income,
                                   "acima de r$ 20.000"       = "Above R$20,000",
                                   "até r$ 1.000"             = "Up to R$1,000",
                                   "de r$ 1.001 a r$ 2.000"   = "From R$1,001 to R$2,000",
                                   "de r$ 10.000 a r$ 20.000" = "From R$10,001 to R$20,000",
                                   "de r$ 2.001 a r$ 3.000"   = "From R$2,001 to R$3,000",
                                   "de r$ 3.001 a r$ 5.000"   = "From R$3,001 to R$5,000",
                                   "de r$ 5.001 a r$ 10.000"  = "From R$5,001 to R$10,000"),
         ideology = recode(ideology,
                           "centro"                = "Center",
                           "centro-direita"        = "Center-Right",
                           "centro-esquerda"       = "Center-Left",
                           "direita"               = "Right",
                           "esquerda"              = "Left",
                           "não sei"               = "Don't Know",
                           "prefiro não responder" = "Rather Not Say"),
         death_penalty = recode(death_penalty,
                                "não"                   = "No",
                                "não sei"               = "Don't Know",
                                "prefiro não responder" = "Rather Not Say",
                                "sim"                   = "Yes"),
         views_police = recode(views_police,
                               "boa"                   = "Good",
                               "muito boa"             = "Very Good",
                               "muito ruim"            = "Very Bad",
                               "não sei"               = "Don't Know",
                               "prefiro não responder" = "Rather Not Say",
                               "regular"               = "Regular",
                               "ruim"                  = "Bad"),
         views_justice = recode(views_justice,
                                "boa"                   = "Good",
                                "muito boa"             = "Very Good",
                                "muito ruim"            = "Very Bad",
                                "não sei"               = "Don't Know",
                                "prefiro não responder" = "Rather Not Say",
                                "regular"               = "Regular",
                                "ruim"                  = "Bad"),
         previous_victim_dummy = recode(previous_victim,
                                        "nenhum" = "No",
                                        .missing = NA_character_,
                                        .default = "Yes")) %>%
relocate(response_id:previous_victim, previous_victim_dummy,
         previous_victim_text:f_5_2_8)

# Check for duplicated values
count(get_dupes(df))

# Count people who agreed with consentment term
df %>%
  group_by(consent) %>%
  summarise(n = n()) %>%
  mutate(frequency = round(n / sum(n), 2))

######################

## Descriptive statistics

# Remove subjects who did not agree with consent form
df1 <- df %>% filter(consent == "Agree")

# Age
summary(df1$age)

ggplot(subset(df1, !is.na(age)), aes(age)) +
  geom_bar(fill = "#152238") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE)),
            color = "darkred", linetype = 5, size = 0.5) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Gender
print(df2 <- df1 %>%
      group_by(gender) %>%
      filter(!is.na(gender)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(gender, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Gender", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Race
print(df2 <- df1 %>%
      group_by(race) %>%
      filter(!is.na(race)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(race, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Race", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Education
print(df2 <- df1 %>%
      group_by(education) %>%
      filter(!is.na(education)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(education, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Education", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Household income
print(df2 <- df1 %>%
      group_by(household_income) %>%
      filter(!is.na(household_income)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(household_income, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Household Income", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Ideology
print(df2 <- df1 %>%
      group_by(ideology) %>%
      filter(!is.na(ideology)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(ideology, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Political Ideology", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Support for Death Penalty
print(df2 <- df1 %>%
      group_by(death_penalty) %>%
      filter(!is.na(death_penalty)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(death_penalty, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Support for Death Penalty", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Previous victimization
print(df2 <- df1 %>%
      group_by(previous_victim_dummy) %>%
      filter(!is.na(previous_victim_dummy)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(previous_victim_dummy, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Previous Victimization (12 Months)", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Views on policing
print(df2 <- df1 %>%
      group_by(views_police) %>%
      filter(!is.na(views_police)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(views_police, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Opinion about the Police Forces", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

# Views on the judiciary
print(df2 <- df1 %>%
      group_by(views_justice) %>%
      filter(!is.na(views_justice)) %>%
      summarise(n = n()) %>%
      mutate(frequency = round(n / sum(n), 3)))

ggplot(df2, aes(x = reorder(views_justice, -n), y = n)) +
  geom_bar(stat = "identity", fill = "#152238") +
  labs(title = "Opinion about the Judiciary", y = "", x = "") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y      = element_blank(),
        plot.title       = element_text(hjust = 0.5)) +
  geom_text(aes(label = n), position = position_dodge(width = 1),
            vjust = -0.75)

######################

## Experiment 01
conjoint_data <- read.qualtrics("../data/data-conjoint.csv",
                                responses = c("Q13", "Q14", "Q15",
                                              "Q16", "Q17"),
                                covariates = c("ResponseId",
                                               "Q1", "Q2", "Q3", "Q4",
                                               "Q5", "Q6", "Q7",
                                               "Q8", "Q9", "Q10",
                                               "Q11", "Q12"),
                                new.format = FALSE, respondentID = NULL)

conjoint_data <- conjoint_data %>%
  rename(response_id                      = ResponseId,
         Age                              = Q2,
         Gender                           = Q3,
         Race                             = Q4,
         Education                        = Q5,
         Region                           = Q6,
         "Household Income"               = Q7,
         Ideology                         = Q8,
         "Support death penalty"          = Q9,
         "Previous Victimization"         = Q10,
         "Offense"                        = Crime,
         "Opinion on Policing"            = Q11,
         "Opinion on Judiciary"           = Q12,
         "Gender of crime victim"         = "Gênero.da.vítima",
         "Gender of crime perpetrator"    = "Gênero.do(a).criminoso(a)",
         "Age of crime victim"            = "Idade.da.vítima",
         "Age of crime perpetrator"       = "Idade.do(a).criminoso(a)",
         "Lynching perpetrators"          = "Linchadores",
         "Race of crime perpetrator"      = "Raça.do(a).criminoso(a)",
         "Residency of crime perpetrator" = "Residência.do.criminoso") %>%
  mutate(`Gender of crime perpetrator` = fct_recode(`Gender of crime perpetrator`,
                                                    "Male"   = "Masculino",
                                                    "Female" = "Feminino"),
         `Age of crime perpetrator` = fct_recode(`Age of crime perpetrator`,
                                                 "Teenager" = "Adolescente",
                                                 "Adult"    = "Adulto(a)",
                                                 "Elderly"  = "Idoso(a)"),
         `Race of crime perpetrator` = fct_recode(`Race of crime perpetrator`,
                                                  "Asian"      = "Asiático(a)",
                                                  "White"      = "Branco(a)",
                                                  "Indigenous" = "Indígena",
                                                  "Black"      = "Negro(a)"),
         `Residency of crime perpetrator` = fct_recode(`Residency of crime perpetrator`,
                                                       "Another neighborhood" = "Mora em outro bairro",
                                                       "In the neighborhood"  = "Mora na vizinhança"),
         `Offense` = fct_recode(`Offense`,
                                "Murder"         = "Assassinou",
                                "Pick-pocketing" = "Bateu a carteira",
                                "Rape"           = "Estuprou",
                                "Molestation"    = "Molestou",
                                "Car theft"      = "Roubou o carro"),
         `Gender of crime victim` = fct_recode(`Gender of crime victim`,
                                               " Male"   = "Masculino",
                                               " Female" = "Feminino"),
         `Age of crime victim` = fct_recode(`Age of crime victim`,
                                            " Teenager" = "Adolescente",
                                            " Child"    = "Criança",
                                            " Adult"    = "Adulto(a)",
                                            " Elderly"  = "Idoso(a)"),
         `Lynching perpetrators` = fct_recode(`Lynching perpetrators`,
                                              "Family of the victim" = "Família da vítima",
                                              "Gangs" = "Gangues",
                                              "Bystanders" = "Pedestres",
                                              "Police" = "Polícia",
                                              "Neighbors" = "Vizinhos")) %>%
  select(-c(16, 18, 20, 22, 24, 26, 28, 30)) %>%
  mutate(response_id = tolower(response_id))

fm <- selected ~ `Gender of crime perpetrator` +
  `Age of crime perpetrator` + `Race of crime perpetrator` +
  `Residency of crime perpetrator` + `Offense` +
  `Gender of crime victim` + `Age of crime victim` +
  `Lynching perpetrators`

faces <- c(rep("plain", 5), "bold", # from bottom to top!
           rep("plain", 4), "bold",
           rep("plain", 2), "bold",
           rep("plain", 5), "bold",
           rep("plain", 2), "bold",
           rep("plain", 4), "bold",
           rep("plain", 3), "bold",
           rep("plain", 2), "bold")

plot(mm(conjoint_data, fm, id = ~response_id),
     vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "none",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)

mm(conjoint_data, fm, id = ~response_id)

# Gender
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  drop_na(gender) %>%
  filter(gender == c("Male", "Female"))
cjdt$Gender <- factor(cjdt$gender)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~Gender)
plot(mm_by, group = "Gender", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)

# Education
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  drop_na(education) %>%
  filter(education == c("College", "Graduate School",
                        "Primary School", "Secondary School",
                        "High School")) %>%
  mutate(education2 = case_when(education == "Primary School" ~ "Primary or Secondary School",
                                education == "Secondary School" ~ "Primary or Secondary School",
                                TRUE ~ education))
cjdt$Education <- factor(cjdt$education2)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~Education)
plot(mm_by, group = "Education", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8, begin = 0.25)

# Race
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  drop_na(race) %>%
  filter(race == c("Asian", "Black", "Mixed Race", "White"))
cjdt$Race <- factor(cjdt$race)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~Race)
plot(mm_by, group = "Race", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)

# Income
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  drop_na(household_income) %>%
  mutate(household_income2 = case_when(household_income == "Up to R$1,000" ~ "Up to R$3,000",
                                       household_income == "From R$1,001 to R$2,000" ~ "Up to R$3,000",
                                       household_income == "From R$2,001 to R$3,000" ~ "Up to R$3,000",
                                       household_income == "From R$3,001 to R$5,000" ~ "From R$3,001 to R$5,000",
                                       household_income == "From R$5,001 to R$10,000" ~ "Above R$5,000",
                                       household_income == "From R$10,001 to R$20,000" ~ "Above R$5,000",
                                       household_income == "Above R$20,000" ~ "Above R$5,000",
                                       TRUE ~ NA_character_))
cjdt$Income <- factor(cjdt$household_income2)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~Income)
plot(mm_by, group = "Income", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)

# Support for Death Penalty
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  drop_na(death_penalty)
cjdt$death_penalty <- factor(cjdt$death_penalty)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~death_penalty)
plot(mm_by, group = "death_penalty", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)

# Opinions on Policing
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  mutate(views_police2 = case_when(views_police == "Rather Not Say" ~ NA_character_,
                                   views_police == "Don't Know" ~ NA_character_,
                                   TRUE ~ views_police)) %>% drop_na(views_police2)
cjdt$Police <- factor(cjdt$views_police2)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~Police)
plot(mm_by, group = "Police", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)

# Opinions on the Judiciary
cjdt <- full_join(conjoint_data, df1, by = "response_id") %>%
  mutate(views_justice2 = case_when(views_justice == "Rather Not Say" ~ NA_character_,
                                   views_justice == "Don't Know" ~ NA_character_,
                                   TRUE ~ views_justice)) %>% drop_na(views_justice2)
cjdt$Judiciary <- factor(cjdt$views_justice2)
mm_by <- cj(cjdt, fm, id = ~response_id, estimate = "mm", by = ~Judiciary)
plot(mm_by, group = "Judiciary", vline = 0.5, header_fmt = "%s") +
  theme(legend.position = "bottom",
        axis.text.y = element_text(face = faces, size = 10)) +
  scale_colour_viridis_d(option = "inferno", end = 0.8)



## Experiment 02
df_exp02 <- df1 %>%
  mutate(exp02_outcomes = coalesce(exp02_control, exp02_police,
                                   exp02_slow_justice, exp02_small_punishment),
  exp02_any_treat = case_when(!is.na(exp02_control) ~ "0",
                              !is.na(exp02_police) ~ "1",
                              !is.na(exp02_slow_justice) ~ "1",
                              !is.na(exp02_small_punishment) ~ "1",
                              TRUE ~ NA_character_),
         exp02_police_treat = case_when(!is.na(exp02_control) ~ "0",
                                        !is.na(exp02_police) ~ "1"),
         exp02_slow_justice_treat = case_when(!is.na(exp02_control) ~ "0",
                                              !is.na(exp02_slow_justice) ~ "1"),
         exp02_small_punishment_treat = case_when(!is.na(exp02_control) ~ "0",
                                                  !is.na(exp02_small_punishment) ~ "1"))

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02)
m1cov <- coeftest(m1, vcov = vcovHC(m1, type = "HC2"))
m1se <- sqrt(diag(m1cov))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02)
m2cov <- coeftest(m2, vcov = vcovHC(m2, type = "HC2"))
m2se <- sqrt(diag(m2cov))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02)
m3cov <- coeftest(m3, vcov = vcovHC(m3, type = "HC2"))
m3se <- sqrt(diag(m3cov))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02)
m4cov <- coeftest(m4, vcov = vcovHC(m4, type = "HC2"))
m4se <- sqrt(diag(m4cov))

stargazer(m1, m2, m3, m4,
          se = list(m1se, m2se, m3se, m4se),
          align = TRUE)

## Subgroup analysis: gender = Female
table(df_exp02$gender)

df_exp02_group <- df_exp02 %>% filter(gender == "Female")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: gender = Male
df_exp02_group <- df_exp02 %>% filter(gender == "Male")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: death_penalty = Yes
df_exp02_group <- df_exp02 %>% filter(death_penalty == "Yes")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: death_penalty = No
df_exp02_group <- df_exp02 %>% filter(death_penalty == "No")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: education = College
df_exp02_group <- df_exp02 %>% filter(education == "College")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: education = High School
df_exp02_group <- df_exp02 %>% filter(education == "High School")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: education = Graduate School
df_exp02_group <- df_exp02 %>% filter(education == "Graduate School")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = White
df_exp02_group <- df_exp02 %>% filter(race == "White")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = Black
df_exp02_group <- df_exp02 %>% filter(race == "Black")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = Mixed Race
df_exp02_group <- df_exp02 %>% filter(race == "Mixed Race")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = Asian
df_exp02_group <- df_exp02 %>% filter(race == "Asian")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Right
table(df_exp02$ideology)

df_exp02_group <- df_exp02 %>% filter(ideology == "Right")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Left
df_exp02_group <- df_exp02 %>% filter(ideology == "Left")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Center
df_exp02_group <- df_exp02 %>% filter(ideology == "Center")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Center-Left
df_exp02_group <- df_exp02 %>% filter(ideology == "Center-Left")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Center-Right
df_exp02_group <- df_exp02 %>% filter(ideology == "Center-Right")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Don't Know
df_exp02_group <- df_exp02 %>% filter(ideology == "Don't Know")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Rather Not Say
df_exp02_group <- df_exp02 %>% filter(ideology == "Rather Not Say")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: previous_victim_dummy = Yes
df_exp02_group <- df_exp02 %>% filter(previous_victim_dummy == "Yes")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: previous_victim_dummy = No
df_exp02_group <- df_exp02 %>% filter(previous_victim_dummy == "No")

m1 <- lm(exp02_outcomes ~ exp02_any_treat, data = df_exp02_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp02_outcomes ~ exp02_police_treat, data = df_exp02_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp02_outcomes ~ exp02_slow_justice_treat, data = df_exp02_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp02_outcomes ~ exp02_small_punishment_treat, data = df_exp02_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Experiment 03
df_exp03 <- df1 %>%
  mutate(exp03_outcomes = coalesce(exp03_control, exp03_constitution,
                                   exp03_rights, exp03_vendetta),
         exp03_any_treat = case_when(!is.na(exp03_control) ~ "0",
                                     !is.na(exp03_constitution) ~ "1",
                                     !is.na(exp03_rights) ~ "1",
                                     !is.na(exp03_vendetta) ~ "1",
                                     TRUE ~ NA_character_),
         exp03_constitution_treat = case_when(!is.na(exp03_control) ~ "0",
                                              !is.na(exp03_constitution) ~ "1"),
         exp03_rights_treat = case_when(!is.na(exp03_control) ~ "0",
                                        !is.na(exp03_rights) ~ "1"),
         exp03_vendetta_treat = case_when(!is.na(exp03_control) ~ "0",
                                          !is.na(exp03_vendetta) ~ "1"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03)
m1cov <- coeftest(m1, vcov = vcovHC(m1, type = "HC2"))
m1se <- sqrt(diag(m1cov))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03)
m2cov <- coeftest(m2, vcov = vcovHC(m2, type = "HC2"))
m2se <- sqrt(diag(m2cov))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03)
m3cov <- coeftest(m3, vcov = vcovHC(m3, type = "HC2"))
m3se <- sqrt(diag(m3cov))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03)
m4cov <- coeftest(m4, vcov = vcovHC(m4, type = "HC2"))
m4se <- sqrt(diag(m4cov))

stargazer(m1, m2, m3, m4,
          se = list(m1se, m2se, m3se, m4se),
          align = TRUE)

## Subgroup analysis: gender = Female
df_exp03_group <- df_exp03 %>% filter(gender == "Female")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: gender = Male
df_exp03_group <- df_exp03 %>% filter(gender == "Male")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = White
df_exp03_group <- df_exp03 %>% filter(race == "White")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = Black
df_exp03_group <- df_exp03 %>% filter(race == "Black")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = Mixed Race
df_exp03_group <- df_exp03 %>% filter(race == "Mixed Race")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: race = Asian
df_exp03_group <- df_exp03 %>% filter(race == "Asian")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Right
table(df_exp03$ideology)

df_exp03_group <- df_exp03 %>% filter(ideology == "Right")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Left
df_exp03_group <- df_exp03 %>% filter(ideology == "Left")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Center
df_exp03_group <- df_exp03 %>% filter(ideology == "Center")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Center-Left
df_exp03_group <- df_exp03 %>% filter(ideology == "Center-Left")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Center-Right
df_exp03_group <- df_exp03 %>% filter(ideology == "Center-Right")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Don't Know
df_exp03_group <- df_exp03 %>% filter(ideology == "Don't Know")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: ideology = Rather Not Say
df_exp03_group <- df_exp03 %>% filter(ideology == "Rather Not Say")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: previous_victim_dummy = Yes
df_exp03_group <- df_exp03 %>% filter(previous_victim_dummy == "Yes")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: previous_victim_dummy = No
df_exp03_group <- df_exp03 %>% filter(previous_victim_dummy == "No")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: death_penalty = Yes
df_exp03_group <- df_exp03 %>% filter(death_penalty == "Yes")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: death_penalty = No
df_exp03_group <- df_exp03 %>% filter(death_penalty == "No")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: education = College
df_exp03_group <- df_exp03 %>% filter(education == "College")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: education = High School
df_exp03_group <- df_exp03 %>% filter(education == "High School")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: education = Graduate School
df_exp03_group <- df_exp03 %>% filter(education == "Graduate School")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: views_police = Good or Very Good
df_exp03_group <- df_exp03 %>% filter(views_police == c("Good", "Very Good"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: views_police = Regular
df_exp03_group <- df_exp03 %>% filter(views_police == "Regular")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: views_police = Bad or Very Bad
df_exp03_group <- df_exp03 %>% filter(views_police == c("Bad", "Very Bad"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: views_justice = Good or Very Good
df_exp03_group <- df_exp03 %>% filter(views_justice == c("Good", "Very Good"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: views_justice = Regular
df_exp03_group <- df_exp03 %>% filter(views_justice == "Regular")

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: views_justice = Bad or Very Bad
df_exp03_group <- df_exp03 %>% filter(views_justice == c("Bad", "Very Bad"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: household_income = Up to R$1,000 & From R$1,001 to R$2,000
df_exp03_group <- df_exp03 %>% filter(household_income == c("Up to R$1,000",
                                                            "From R$1,001 to R$2,000"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: household_income = From R$2,001 to R$3,000 & From R$3,001 to R$5,000
df_exp03_group <- df_exp03 %>% filter(household_income == c("From R$2,001 to R$3,000",
                                                            "From R$3,001 to R$5,000"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: household_income = From R$5,001 to R$10,000
df_exp03_group <- df_exp03 %>% filter(household_income == c("From R$5,001 to R$10,000"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))

## Subgroup analysis: household_income = From R$10,001 to R$20,000 & Above R$20,000
df_exp03_group <- df_exp03 %>% filter(household_income == c("From R$10,001 to R$20,000",
                                                            "Above R$20,000"))

m1 <- lm(exp03_outcomes ~ exp03_any_treat, data = df_exp03_group)
coeftest(m1, vcov = vcovHC(m1, type = "HC2"))

m2 <- lm(exp03_outcomes ~ exp03_constitution_treat, data = df_exp03_group)
coeftest(m2, vcov = vcovHC(m2, type = "HC2"))

m3 <- lm(exp03_outcomes ~ exp03_rights_treat, data = df_exp03_group)
coeftest(m3, vcov = vcovHC(m3, type = "HC2"))

m4 <- lm(exp03_outcomes ~ exp03_vendetta_treat, data = df_exp03_group)
coeftest(m4, vcov = vcovHC(m4, type = "HC2"))
