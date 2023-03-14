library(lavaan)
library(dplyr)
library(ggplot2)

load("participants_5.Rdata")
load("participants_11_CBCL.Rdata")
load("participants_11_SDQ.Rdata")
load("participants_11_MIA.Rdata")

sem_vars_FSIQ_ADHD_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "ADHD")

sem_vars_FSIQ_ADHD_5y <- subset(sem_vars_FSIQ_ADHD_5y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))

colnames(sem_vars_FSIQ_ADHD_5y) <- c("ID","FSIQ5", 
                                   "ADHD5")

sem_vars_FSIQ_ADHD_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "ADHD, SDQ")

sem_vars_FSIQ_ADHD_11y <- subset(sem_vars_FSIQ_ADHD_11y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))
colnames(sem_vars_FSIQ_ADHD_11y) <- c("ID", "FSIQ11", 
                                     "ADHD11")

sem_ADHD_vars <- left_join(sem_vars_FSIQ_ADHD_5y, sem_vars_FSIQ_ADHD_11y)


common_ids <- intersect(na.omit(sem_ADHD_vars)$ID, sem_vars_FSIQ_ADHD_11y$ID)
not_matching_ids <- sem_vars_FSIQ_ADHD_5y$ID[!sem_vars_FSIQ_ADHD_5y$ID %in% common_ids]


model_ADHD <- ' 
# regressions 
ADHD5 ~ FSIQ5
FSIQ11 ~ FSIQ5
ADHD11 ~ FSIQ5 + FSIQ11 + ADHD5

# residual correlations
ADHD5 ~~ ADHD11
'
fit <- sem(model_ADHD, data = sem_ADHD_vars, missing = "ML") 
summary(fit, standardized = TRUE)
parameterEstimates(fit)
modindices(fit)



