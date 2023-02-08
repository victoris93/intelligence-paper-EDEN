library(lavaan)
library(dplyr)
library(ggplot2)

load("participants_5.Rdata")
load("participants_11_CBCL.Rdata")
load("participants_11_SDQ.Rdata")
load("participants_11_MIA.Rdata")

######## ADHD

sem_vars_abs_gap_vpiq_ADHD_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "ADHD")

sem_vars_abs_gap_vpiq_ADHD_5y <- subset(sem_vars_abs_gap_vpiq_ADHD_5y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))

colnames(sem_vars_abs_gap_vpiq_ADHD_5y) <- c("ID","abs_gap_vpiq5", 
                                   "ADHD5")

sem_vars_abs_gap_vpiq_ADHD_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "ADHD, SDQ")

sem_vars_abs_gap_vpiq_ADHD_11y <- subset(sem_vars_abs_gap_vpiq_ADHD_11y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))
colnames(sem_vars_abs_gap_vpiq_ADHD_11y) <- c("ID", "abs_gap_vpiq11", 
                                     "ADHD11")

sem_ADHD_vars <- left_join(sem_vars_abs_gap_vpiq_ADHD_5y, sem_vars_abs_gap_vpiq_ADHD_11y)


common_ids <- intersect(na.omit(sem_ADHD_vars)$ID, sem_vars_abs_gap_vpiq_ADHD_11y$ID)
not_matching_ids <- sem_vars_abs_gap_vpiq_ADHD_5y$ID[!sem_vars_abs_gap_vpiq_ADHD_5y$ID %in% common_ids]


model_ADHD <- ' 
# regressions 
ADHD5 ~ abs_gap_vpiq5
abs_gap_vpiq11 ~ abs_gap_vpiq5
ADHD11 ~ abs_gap_vpiq5 + abs_gap_vpiq11 + ADHD5
'
fit_ADHD <- sem(model_ADHD, data = sem_ADHD_vars) 
summary(fit_ADHD, standardized = TRUE)
parameterEstimates(fit_ADHD)
modindices(fit_ADHD)

######## Internalizing Disorder

sem_vars_abs_gap_vpiq_dep_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "Internalizing Disorder")

sem_vars_abs_gap_vpiq_dep_5y <- subset(sem_vars_abs_gap_vpiq_dep_5y, select = c(`ID`, `Cognitive Score`,
                                                                `Psychopathology Score`))

colnames(sem_vars_abs_gap_vpiq_dep_5y) <- c("ID","abs_gap_vpiq5", 
                                    "dep5")

sem_vars_abs_gap_vpiq_dep_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "Internalizing Disorder, SDQ")

sem_vars_abs_gap_vpiq_dep_11y <- subset(sem_vars_abs_gap_vpiq_dep_11y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))
colnames(sem_vars_abs_gap_vpiq_dep_11y) <- c("ID", "abs_gap_vpiq11", 
                                     "dep11")

sem_dep_vars <- left_join(sem_vars_abs_gap_vpiq_dep_5y, sem_vars_abs_gap_vpiq_dep_11y)


common_ids <- intersect(na.omit(sem_dep_vars)$ID, sem_vars_abs_gap_vpiq_dep_11y$ID)
not_matching_ids <- sem_vars_abs_gap_vpiq_dep_5y$ID[!sem_vars_abs_gap_vpiq_dep_5y$ID %in% common_ids]

model_dep <- ' 
# regressions 
dep5 ~ abs_gap_vpiq5
abs_gap_vpiq11 ~ abs_gap_vpiq5
dep11 ~ abs_gap_vpiq5 + abs_gap_vpiq11 + dep5
'
fit_dep <- sem(model_dep, data = sem_dep_vars) 
summary(fit_dep, standardized = TRUE)
parameterEstimates(fit_dep)
modindices(fit_dep)

######## Social Problems

sem_vars_abs_gap_vpiq_social_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "Social Problems")

sem_vars_abs_gap_vpiq_social_5y <- subset(sem_vars_abs_gap_vpiq_social_5y, select = c(`ID`, `Cognitive Score`,
                                                                      `Psychopathology Score`))

colnames(sem_vars_abs_gap_vpiq_social_5y) <- c("ID","abs_gap_vpiq5", 
                                       "social5")

sem_vars_abs_gap_vpiq_social_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "Social Problems, SDQ")

sem_vars_abs_gap_vpiq_social_11y <- subset(sem_vars_abs_gap_vpiq_social_11y, select = c(`ID`, `Cognitive Score`,
                                                                        `Psychopathology Score`))
colnames(sem_vars_abs_gap_vpiq_social_11y) <- c("ID", "abs_gap_vpiq11", 
                                        "social11")

sem_social_vars <- left_join(sem_vars_abs_gap_vpiq_social_5y, sem_vars_abs_gap_vpiq_social_11y)


common_ids <- intersect(na.omit(sem_social_vars)$ID, sem_vars_abs_gap_vpiq_social_11y$ID)
not_matching_ids <- sem_vars_abs_gap_vpiq_social_5y$ID[!sem_vars_abs_gap_vpiq_social_5y$ID %in% common_ids]

model_social <- ' 
# regressions 
social5 ~ abs_gap_vpiq5
abs_gap_vpiq11 ~ abs_gap_vpiq5
social11 ~ abs_gap_vpiq5 + abs_gap_vpiq11 + social5
'
fit_social <- sem(model_social, data = sem_social_vars) 
summary(fit_social, standardized = TRUE)
parameterEstimates(fit_social)
modindices(fit_social)

######## Conduct Disorder

sem_vars_abs_gap_vpiq_conduct_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "Conduct Disorder")

sem_vars_abs_gap_vpiq_conduct_5y <- subset(sem_vars_abs_gap_vpiq_conduct_5y, select = c(`ID`, `Cognitive Score`,
                                                                        `Psychopathology Score`))

colnames(sem_vars_abs_gap_vpiq_conduct_5y) <- c("ID","abs_gap_vpiq5", 
                                        "conduct5")

sem_vars_abs_gap_vpiq_conduct_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "|VIQ-PIQ|",
         `Psychopathology Measure` == "Conduct Disorder, SDQ")

sem_vars_abs_gap_vpiq_conduct_11y <- subset(sem_vars_abs_gap_vpiq_conduct_11y, select = c(`ID`, `Cognitive Score`,
                                                                          `Psychopathology Score`))
colnames(sem_vars_abs_gap_vpiq_conduct_11y) <- c("ID", "abs_gap_vpiq11", 
                                         "conduct11")

sem_conduct_vars <- left_join(sem_vars_abs_gap_vpiq_conduct_5y, sem_vars_abs_gap_vpiq_conduct_11y)


common_ids <- intersect(na.omit(sem_conduct_vars)$ID, sem_vars_abs_gap_vpiq_conduct_11y$ID)
not_matching_ids <- sem_vars_abs_gap_vpiq_conduct_5y$ID[!sem_vars_abs_gap_vpiq_conduct_5y$ID %in% common_ids]

model_conduct <- ' 
# regressions 
conduct5 ~ abs_gap_vpiq5
abs_gap_vpiq11 ~ abs_gap_vpiq5
conduct11 ~ abs_gap_vpiq5 + abs_gap_vpiq11 + conduct5
'
fit_conduct <- sem(model_conduct, data = sem_conduct_vars) 
summary(fit_conduct, standardized = TRUE)
parameterEstimates(fit_conduct)
modindices(fit_conduct)
