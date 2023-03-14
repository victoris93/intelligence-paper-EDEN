library(dplyr)
library(ggplot2)
library(Hmisc)
library(polycor)
library(plyr)
library(xtable)
library(tidyverse)
library(reshape2)
library(gtools)
library(summarytools)
library(data.table)
library(lavaan)
library(knitr)
library(rstatix)

setwd("/Users/VictoriaShevchenko/Documents/STAGE\ M1")
load("new_envt_eden_full.RData") 
eden <- ndata
rm(ndata)

#### Variables of interest & Filtering ####
# Participants for 5.5 years___________________________________________________

participants_5 <- subset(eden, select = c("n_ident", 
                                          "nn_sexe2", 
                                          "agepsy5",
                                          "a5_qit", 
                                          "a5_qiv", 
                                          "a5_qip", 
                                          "cond5",
                                          "hyp5",
                                          "emo5", 
                                          "peer5")) 
participants_5$nn_sexe2 <- as.factor(participants_5$nn_sexe2)
participants_5$dom_gap_abs <- abs(participants_5$a5_qiv - participants_5$a5_qip)
participants_5$dom_gap <- participants_5$a5_qiv - participants_5$a5_qip 

participants_5 <- na.omit(participants_5)


colnames(participants_5) <- c("ID", 
                              "Sex",
                              "Age",
                              "Full-Scale IQ", 
                              "Verbal IQ", 
                              "Performance IQ",
                              "Conduct Disorder",
                              "ADHD",
                              "Internalizing Disorder", 
                              "Social Problems",
                              "|VIQ-PIQ|", 
                              "VIQ-PIQ")

# Variables of interest for 11.5 years________________________________________________________________

# Calculating Social Aggression and Conduct Disorder for MIA from scratch

MIA_AgS_index <- c("MIA1_38", "MIA1_42", "MIA1_48", "MIA2_31", "MIA2_33", "MIA2_47")
MIA_CON_index <- c("MIA1_34", "MIA2_02", "MIA2_07", "MIA2_19", "MIA2_24", "MIA2_36")
MIA_PHO_index  <- c("MIA1_02", "MIA1_08", "MIA1_14", "MIA1_19", "MIA1_26", "MIA1_32", "MIA1_37", "MIA1_43")
MIA_ANX_index <- c("MIA1_04", "MIA1_12", "MIA1_17", "MIA1_21", "MIA1_28", "MIA1_35", "MIA1_41", "MIA1_45", "MIA1_49")
MIA_DEP_index <- c("MIA1_05", "MIA1_29", "MIA2_03", "MIA2_13", "MIA2_20", "MIA2_26", "MIA2_37", "MIA2_42")
MIA_IMP_index <- c("MIA2_01", "MIA2_18", "MIA2_35", "MIA2_06", "MIA2_23", "MIA2_40")
MIA_HYP_index <- c("MIA1_01", "MIA2_11", "MIA1_13", "MIA2_29")
MIA_INA_index <- c("MIA1_20", "MIA2_45", "MIA1_25", "MIA2_15", "MIA1_33", "MIA2_32")

setDT(eden)[, AgS := rowMeans(.SD)*5, .SDcols=MIA_AgS_index]
eden[, CON := rowMeans(.SD)*5, .SDcols=MIA_CON_index]
eden[, PHO := rowMeans(.SD)*5, .SDcols=MIA_PHO_index]
eden[, ANX := rowMeans(.SD)*5, .SDcols=MIA_ANX_index]
eden[, DEP := rowMeans(.SD)*5, .SDcols=MIA_DEP_index]
eden[, IMP := rowMeans(.SD)*5, .SDcols=MIA_IMP_index]
eden[, HYP := rowMeans(.SD)*5, .SDcols=MIA_HYP_index]
eden[, INA := rowMeans(.SD)*5, .SDcols=MIA_INA_index]

# Selecting variables of interest for 11.5 y.o.

participants_11 <- subset(eden, select = c("n_ident", 
                                           "nn_sexe2", 
                                           "EVIP_age",
                                           "g", 
                                           "gc", 
                                           "gf", 
                                           "CBCLIII",
                                           "CBCLVI",
                                           "CBCLex", 
                                           "CBCLIV",
                                           "SDQEMO", 
                                           "SDQHYP",
                                           "SDQCOND", 
                                           "SDQPEER",
                                           "DEP",
                                           "IMP",
                                           "INA",
                                           "HYP",
                                           "ANX",
                                           "CON",
                                           "AgS",
                                           "PHO"))

participants_11$nn_sexe2 <- as.factor(participants_11$nn_sexe2)
participants_11 <- participants_11 %>% 
  mutate(a11_qiv = (100 + (participants_11$gc * 15 
                           / sd(participants_11$gc, na.rm = TRUE))),
         a11_qip = (100 + (participants_11$gf * 15 
                           / sd(participants_11$gf, na.rm = TRUE))),
         a11_qit = (100 + (participants_11$g * 15 
                           / sd(participants_11$g, na.rm = TRUE))))

participants_11 <- participants_11 %>% 
  mutate(dom_gap_abs = abs(participants_11$a11_qiv - participants_11$a11_qip),
         dom_gap = participants_11$a11_qiv - participants_11$a11_qip)

participants_11_CBCL <-subset(participants_11, select = c("n_ident",
                                                          "nn_sexe2", 
                                                          "EVIP_age", 
                                                          "a11_qit", 
                                                          "a11_qiv", 
                                                          "a11_qip", 
                                                          "dom_gap_abs", 
                                                          "dom_gap", 
                                                          "CBCLIII",
                                                          "CBCLVI",
                                                          "CBCLex", 
                                                          "CBCLIV"))

participants_11_CBCL <- participants_11_CBCL  %>% filter(!is.na(a11_qit) & !is.na(CBCLIII))

colnames(participants_11_CBCL) <- c("ID", 
                                    "Sex",
                                    "Age",
                                    "Full-Scale IQ", 
                                    "Verbal IQ", 
                                    "Performance IQ",
                                    "|VIQ-PIQ|",
                                    "VIQ-PIQ",
                                    "Internalizing Disorder, CBCL", 
                                    "ADHD, CBCL",
                                    "Conduct Disorder, CBCL",
                                    "Social Problems, CBCL")

participants_11_SDQ <- subset(participants_11, select = c("n_ident",
                                                         "nn_sexe2", 
                                                         "EVIP_age", 
                                                         "a11_qit", 
                                                         "a11_qiv", 
                                                         "a11_qip", 
                                                         "dom_gap_abs", 
                                                         "dom_gap", 
                                                         "SDQEMO",
                                                         "SDQHYP",
                                                         "SDQCOND", 
                                                         "SDQPEER"))

participants_11_SDQ <- participants_11_SDQ  %>% filter(!is.na(a11_qit) & !is.na(SDQEMO))                                                    
colnames(participants_11_SDQ) <- c("ID", 
                                   "Sex",
                                   "Age",
                                   "Full-Scale IQ", 
                                   "Verbal IQ", 
                                   "Performance IQ",
                                   "|VIQ-PIQ|",
                                   "VIQ-PIQ",
                                   "Internalizing Disorder, SDQ", 
                                   "ADHD, SDQ",
                                   "Conduct Disorder, SDQ",
                                   "Social Problems, SDQ")


participants_11_MIA <-subset(participants_11, select = c("n_ident",
                                                         "nn_sexe2", 
                                                         "EVIP_age", 
                                                         "a11_qit", 
                                                         "a11_qiv", 
                                                         "a11_qip", 
                                                         "dom_gap_abs", 
                                                         "dom_gap",
                                                         "DEP",
                                                         "IMP",
                                                         "INA",
                                                         "HYP",
                                                         "ANX",
                                                         "CON",
                                                         "AgS",
                                                         "PHO"))

participants_11_MIA$emo_mia <- (participants_11_MIA$DEP + participants_11_MIA$ANX) / 2
participants_11_MIA$peer_mia <- (participants_11_MIA$AgS + participants_11_MIA$PHO) / 2
participants_11_MIA$adhd_mia <- (participants_11_MIA$HYP + participants_11_MIA$INA + participants_11_MIA$IMP) / 3

participants_11_MIA <-  participants_11_MIA %>% filter(!is.na(a11_qit) & !is.na(peer_mia) & !is.na(emo_mia))  # Keeping participants with NAs for age

participants_11_MIA <- subset(participants_11_MIA, select = -c(DEP, ANX, PHO, AgS, HYP, INA, IMP))
colnames(participants_11_MIA) <- c("ID", 
                                   "Sex",
                                   "Age",
                                   "Full-Scale IQ", 
                                   "Verbal IQ", 
                                   "Performance IQ",
                                   "|VIQ-PIQ|",
                                   "VIQ-PIQ",
                                   "Conduct Disorder, MIA",
                                   "Internalizing Disorder, MIA",
                                   "Social Problems, MIA",
                                   "ADHD, MIA")

save(participants_5, file = "participants_5.Rdata")
save(participants_11_CBCL, file = "participants_11_CBCL.Rdata")
save(participants_11_SDQ, file = "participants_11_SDQ.Rdata")
save(participants_11_MIA, file = "participants_11_MIA.Rdata")
#### Descriptive statistics tables ####
summary_5 <- dfSummary(participants_5)
summary_11_CBCL <- dfSummary(participants_11_CBCL)
summary_11_SDQ <- dfSummary(participants_11_SDQ)
summary_11_MIA <- dfSummary(participants_11_MIA)

#### Correlations ####

# A separate function per age group due to different significance levels for each
cortable_5 <-function(x, removeTriangle=c("upper", "lower"),
                      result=c("none", "html", "latex")) { 
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type = "spearman")
  R <- correlation_matrix$r 
  p <- correlation_matrix$P  
  
  mystars <- ifelse(p < .003, "***", ifelse(p < .017, "** ", ifelse(p < .05, "*  ", ifelse(p < .05, "*   ", "    "))))
  
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  Rnew <- cbind(Rnew[6:ncol(x),1:5])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
}

# For 11 years
cortable_11 <-function(x, removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")) { 
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type = "spearman")
  R <- correlation_matrix$r 
  p <- correlation_matrix$P  
  
  mystars <- ifelse(p < .0011, "***", ifelse(p < .006, "** ", ifelse(p < .05, "*  ", ifelse(p < .05, "*   ", "    "))))
  
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  Rnew <- cbind(Rnew[6:ncol(x),1:5])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
}

correlation_vars_5 <- subset(participants_5, select = c("Full-Scale IQ", 
                                                        "Verbal IQ", 
                                                        "Performance IQ",
                                                        "|VIQ-PIQ|", 
                                                        "VIQ-PIQ",
                                                        "Internalizing Disorder",
                                                        "Conduct Disorder", 
                                                        "Social Problems",
                                                        "ADHD"))

correlation_vars_11_CBCL <- subset(participants_11_CBCL, select = c("Full-Scale IQ", 
                                                                    "Verbal IQ", 
                                                                    "Performance IQ",
                                                                    "|VIQ-PIQ|", 
                                                                    "VIQ-PIQ",
                                                                    "Internalizing Disorder, CBCL",
                                                                    "Conduct Disorder, CBCL", 
                                                                    "Social Problems, CBCL",
                                                                    "ADHD, CBCL"))

correlation_vars_11_SDQ <- subset(participants_11_SDQ, select = c("Full-Scale IQ", 
                                                                  "Verbal IQ", 
                                                                  "Performance IQ",
                                                                  "|VIQ-PIQ|", 
                                                                  "VIQ-PIQ",
                                                                  "Internalizing Disorder, SDQ",
                                                                  "Conduct Disorder, SDQ", 
                                                                  "Social Problems, SDQ",
                                                                  "ADHD, SDQ"))

correlation_vars_11_MIA <- subset(participants_11_MIA, select = c("Full-Scale IQ", 
                                                                  "Verbal IQ", 
                                                                  "Performance IQ",
                                                                  "|VIQ-PIQ|", 
                                                                  "VIQ-PIQ",
                                                                  "Internalizing Disorder, MIA",
                                                                  "Conduct Disorder, MIA", 
                                                                  "Social Problems, MIA",
                                                                  "ADHD, MIA"))

# Sig levels: * p < 0.05, ** p < 0.017  (0.05 / 3), *** p < 0.003 (0.05 / [3 * 5])
correlation_table_5 <- cortable_5(correlation_vars_5) 

# Sig levels: * p < 0.05, ** p < 0.006 (0.05 / 9), *** p < 0.0011 (0.05 / [9 * 5])
correlation_table_11_CBCL <- cortable_11(correlation_vars_11_CBCL)
correlation_table_11_SDQ <- cortable_11(correlation_vars_11_SDQ)
correlation_table_11_MIA <- cortable_11(correlation_vars_11_MIA)

#### Wide --> Long formatting ####
# 5 YEARS
long_participants_5 <- reshape(participants_5,
                               direction = "long",
                               idvar = c("ID", "Sex", "Age"),
                               varying = c("Full-Scale IQ", 
                                           "Verbal IQ",
                                           "Performance IQ",
                                           "|VIQ-PIQ|", 
                                           "VIQ-PIQ"),
                               v.names = "Cognitive Score",
                               timevar = "Cognitive Measure",
                               times = c("Full-Scale IQ", 
                                         "Verbal IQ",
                                         "Performance IQ",
                                         "|VIQ-PIQ|", 
                                         "VIQ-PIQ"))

long_participants_5 <- reshape(long_participants_5,
                               direction = "long",
                               idvar = c("ID", 
                                         "Sex",
                                         "Age", 
                                         "Cognitive Measure", 
                                         "Cognitive Score"),
                               varying = c("Conduct Disorder", 
                                           "Internalizing Disorder", 
                                           "Social Problems",
                                           "ADHD"),
                               v.names = "Psychopathology Score",
                               timevar = "Psychopathology Measure",
                               times = c("Conduct Disorder", 
                                         "Internalizing Disorder", 
                                         "Social Problems",
                                         "ADHD"))

long_participants_5$`Cognitive Measure` <- as.factor(long_participants_5$`Cognitive Measure`)
long_participants_5$`Psychopathology Measure` <- as.factor(long_participants_5$`Psychopathology Measure`)
long_participants_5$`Cognitive Measure` <- factor(long_participants_5$`Cognitive Measure`, 
                                                  levels = c("Full-Scale IQ", 
                                                            "Verbal IQ",
                                                            "Performance IQ",
                                                            "|VIQ-PIQ|", 
                                                            "VIQ-PIQ"))
long_participants_5$`Psychopathology Measure` <- factor(long_participants_5$`Psychopathology Measure`,
                                                        levels = c("Internalizing Disorder", 
                                                                   "Conduct Disorder",
                                                                   "Social Problems",
                                                                   "ADHD"))
# 11 YEARS

long_participants_11_CBCL <- reshape(participants_11_CBCL,
                                     direction = "long",
                                     idvar = c("ID", "Sex", "Age"),
                                     varying = c("Full-Scale IQ", 
                                                 "Verbal IQ",
                                                 "Performance IQ",
                                                 "|VIQ-PIQ|", 
                                                 "VIQ-PIQ"),
                                     v.names = "Cognitive Score",
                                     timevar = "Cognitive Measure",
                                     times = c("Full-Scale IQ", 
                                               "Verbal IQ",
                                               "Performance IQ",
                                               "|VIQ-PIQ|", 
                                               "VIQ-PIQ"))

long_participants_11_CBCL <- reshape(long_participants_11_CBCL,
                                     direction = "long",
                                     idvar = c("ID", "Sex", "Age", "Cognitive Score", "Cognitive Measure"),
                                     varying = c("Internalizing Disorder, CBCL", 
                                                 "Conduct Disorder, CBCL",
                                                 "Social Problems, CBCL",
                                                 "ADHD, CBCL"),
                                     v.names = "Psychopathology Score",
                                     timevar = "Psychopathology Measure",
                                     times = c("Internalizing Disorder, CBCL", 
                                               "Conduct Disorder, CBCL",
                                               "Social Problems, CBCL",
                                               "ADHD, CBCL")) 

long_participants_11_CBCL$`Cognitive Measure` <- as.factor(long_participants_11_CBCL$`Cognitive Measure`)
long_participants_11_CBCL$`Cognitive Measure` <- factor(long_participants_11_CBCL$`Cognitive Measure`, levels = c("Full-Scale IQ", 
                                                                                                                "Verbal IQ",
                                                                                                                "Performance IQ",
                                                                                                                "|VIQ-PIQ|", 
                                                                                                                "VIQ-PIQ"))
long_participants_11_CBCL$`Psychopathology Measure` <- as.factor(long_participants_11_CBCL$`Psychopathology Measure`)
long_participants_11_CBCL$`Psychopathology Measure` <- factor(long_participants_11_CBCL$`Psychopathology Measure`, levels = c("Internalizing Disorder, CBCL", 
                                                                                                                  "Conduct Disorder, CBCL",
                                                                                                                  "Social Problems, CBCL",
                                                                                                                  "ADHD, CBCL"))


# SDQ
long_participants_11_SDQ <- reshape(participants_11_SDQ,
                                    direction = "long",
                                    idvar = c("ID", "Sex", "Age"),
                                    varying = c("Full-Scale IQ", 
                                                "Verbal IQ",
                                                "Performance IQ",
                                                "|VIQ-PIQ|", 
                                                "VIQ-PIQ"),
                                    v.names = "Cognitive Score",
                                    timevar = "Cognitive Measure",
                                    times = c("Full-Scale IQ", 
                                              "Verbal IQ",
                                              "Performance IQ",
                                              "|VIQ-PIQ|", 
                                              "VIQ-PIQ"))

long_participants_11_SDQ <- reshape(long_participants_11_SDQ,
                                    direction = "long",
                                    idvar = c("ID", "Sex", "Age", "Cognitive Score", "Cognitive Measure"),
                                    varying = c("Internalizing Disorder, SDQ", 
                                                "Conduct Disorder, SDQ",
                                                "Social Problems, SDQ",
                                                "ADHD, SDQ"),
                                    v.names = "Psychopathology Score",
                                    timevar = "Psychopathology Measure",
                                    times = c("Internalizing Disorder, SDQ", 
                                              "Conduct Disorder, SDQ",
                                              "Social Problems, SDQ",
                                              "ADHD, SDQ")) 

long_participants_11_SDQ$`Psychopathology Measure` <- as.factor(long_participants_11_SDQ$`Psychopathology Measure`)
long_participants_11_SDQ$`Cognitive Measure` <- as.factor(long_participants_11_SDQ$`Cognitive Measure`)
long_participants_11_SDQ$`Cognitive Measure` <- factor(long_participants_11_SDQ$`Cognitive Measure`, levels = c("Full-Scale IQ", 
                                                                "Verbal IQ",
                                                                "Performance IQ",
                                                                "|VIQ-PIQ|", 
                                                                "VIQ-PIQ"))
long_participants_11_SDQ$`Psychopathology Measure` <- factor(long_participants_11_SDQ$`Psychopathology Measure`,
                                                        levels = c("Internalizing Disorder, SDQ", 
                                                                   "Conduct Disorder, SDQ",
                                                                   "Social Problems, SDQ",
                                                                   "ADHD, SDQ"))


# MIA
long_participants_11_MIA <- reshape(participants_11_MIA,
                                    direction = "long",
                                    idvar = c("ID", "Sex", "Age"),
                                    varying = c("Full-Scale IQ", 
                                                "Verbal IQ",
                                                "Performance IQ",
                                                "|VIQ-PIQ|", 
                                                "VIQ-PIQ"),
                                    v.names = "Cognitive Score",
                                    timevar = "Cognitive Measure",
                                    times = c("Full-Scale IQ", 
                                              "Verbal IQ",
                                              "Performance IQ",
                                              "|VIQ-PIQ|", 
                                              "VIQ-PIQ"))

long_participants_11_MIA <- reshape(long_participants_11_MIA,
                                    direction = "long",
                                    idvar = c("ID", "Sex", "Age", "Cognitive Score", "Cognitive Measure"),
                                    varying = c("Internalizing Disorder, MIA", 
                                                "Conduct Disorder, MIA",
                                                "Social Problems, MIA",
                                                "ADHD, MIA"),
                                    v.names = "Psychopathology Score",
                                    timevar = "Psychopathology Measure",
                                    times = c("Internalizing Disorder, MIA", 
                                              "Conduct Disorder, MIA",
                                              "Social Problems, MIA",
                                              "ADHD, MIA")) 

long_participants_11_MIA$`Cognitive Measure` <- as.factor(long_participants_11_MIA$`Cognitive Measure`)
long_participants_11_MIA$`Psychopathology Measure` <- as.factor(long_participants_11_MIA$`Psychopathology Measure`)
long_participants_11_MIA$`Cognitive Measure` <- factor(long_participants_11_MIA$`Cognitive Measure`, levels = c("Full-Scale IQ", 
                                                                                                                "Verbal IQ",
                                                                                                                "Performance IQ",
                                                                                                                "|VIQ-PIQ|", 
                                                                                                                "VIQ-PIQ"))
long_participants_11_MIA$`Psychopathology Measure` <- factor(long_participants_11_MIA$`Psychopathology Measure`, levels = c("Internalizing Disorder, MIA", 
                                                                                                                "Conduct Disorder, MIA",
                                                                                                                "Social Problems, MIA",
                                                                                                                "ADHD, MIA"))
save(long_participants_11_CBCL, file = "long_participants_11_CBCL.Rdata")
save(long_participants_11_SDQ, file = "long_participants_11_SDQ.Rdata")
save(long_participants_11_MIA, file = "long_participants_11_MIA.Rdata")
save(long_participants_5, file = "long_participants_5.Rdata")

#### Exploratory analyses: IQ vs Psychopathology ####
# 5 YEARS
plots_5_IQ_vs_psy = list()

for (Cognitive_Measure in levels(long_participants_5$`Cognitive Measure`)) {
  plots_5_IQ_vs_psy[[Cognitive_Measure]] = ggplot(long_participants_5 %>% filter(`Cognitive Measure` == Cognitive_Measure),
                                                  aes(x = `Cognitive Score`, y = `Psychopathology Score`, group = `Psychopathology Measure`)) +
    geom_point() + 
    geom_smooth(method='lm', color = "red", size = 1) +
    geom_count() +
    scale_size_area() +
    theme_bw() +
    labs(x = Cognitive_Measure) +
    facet_wrap(~ `Psychopathology Measure`)
  print(plots_5_IQ_vs_psy[[Cognitive_Measure]])
}

# 11 YEARS

plots_11_IQ_vs_psy_CBCL  = list()

for (Cognitive_Measure in levels(long_participants_11_CBCL$`Cognitive Measure`)) {
  plots_11_IQ_vs_psy_CBCL[[Cognitive_Measure]] = ggplot(long_participants_11_CBCL %>% 
                                                          filter(`Cognitive Measure` == Cognitive_Measure),
                                                        aes(x = `Cognitive Score`, y = `Psychopathology Score`, group = `Psychopathology Measure`)) +
    geom_point() + 
    geom_smooth(method='lm', color = "red", size = 1) +
    theme_bw() +
    labs(x = Cognitive_Measure) +
    facet_wrap(~ `Psychopathology Measure`)
  print(plots_11_IQ_vs_psy_CBCL[[Cognitive_Measure]])
}

plots_11_IQ_vs_psy_SDQ  = list()

for (Cognitive_Measure in levels(long_participants_11_SDQ$`Cognitive Measure`)) {
  plots_11_IQ_vs_psy_SDQ[[Cognitive_Measure]] = ggplot(long_participants_11_SDQ %>% 
                                                         filter(`Cognitive Measure` == Cognitive_Measure),
                                                       aes(x = `Cognitive Score`, y = `Psychopathology Score`, group = `Psychopathology Measure`)) +
    geom_point() + 
    geom_smooth(method='lm', color = "red", size = 1) +
    theme_bw() +
    labs(x = Cognitive_Measure) +
    facet_wrap(~ `Psychopathology Measure`)
  print(plots_11_IQ_vs_psy_SDQ[[Cognitive_Measure]])
}

plots_11_IQ_vs_psy_MIA  = list()

for (Cognitive_Measure in levels(long_participants_11_MIA$`Cognitive Measure`)) {
  plots_11_IQ_vs_psy_MIA[[Cognitive_Measure]] = ggplot(long_participants_11_MIA %>% 
                                                         filter(`Cognitive Measure` == Cognitive_Measure),
                                                       aes(x = `Cognitive Score`, y = `Psychopathology Score`, group = `Psychopathology Measure`)) +
    geom_point() + 
    geom_smooth(method='lm', color = "red", size = 1) +
    theme_bw() +
    labs(x = Cognitive_Measure) +
    facet_wrap(~ `Psychopathology Measure`)
  print(plots_11_IQ_vs_psy_MIA[[Cognitive_Measure]])
}

#### Graphs IQ Quintiles vs Psychopathology ####

# Data division in IQ quintiles
setDT(long_participants_5)[ , `Quintile` := quantcut(`Cognitive Score`, q = 5), by = `Cognitive Measure`] # Since levels are defined by unique values, quantcut() doesn't consider level (103,111] as q unique level for Performance and Verbal IQ, but as one single level. 
# I have to greate a separate data frame for Performance IQ and generate a separate graph out of it
long_participants_5_performance_IQ <-  long_participants_5 %>%  
  filter(`Cognitive Measure` == 'Performance IQ') %>%  
  mutate(Quintile = factor(Quintile, levels = c('[48,90]', '(90,96]', '(96,103]', '(103,111]', '(111,144]')))

long_participants_5 %>%  
  filter(`Cognitive Measure` == 'Performance IQ') %>%  
  mutate(Quintile = factor(Quintile, levels = c('[48,90]', '(90,96]', '(96,103]', '(103,111]', '(111,144]')))

long_participants_11_CBCL[ , `Cognitive Score` := floor(`Cognitive Score`)] # Rounding down the IQ scores to remove decimals in the x-axis
long_participants_11_CBCL[ , Quintile := quantcut(`Cognitive Score`, q = 5), by = `Cognitive Measure`] # Adding quintiles

long_participants_11_SDQ[ , `Cognitive Score` := floor(`Cognitive Score`)] # Rounding down the IQ scores to remove decimals in the x-axis
long_participants_11_SDQ[ , Quintile := quantcut(`Cognitive Score`, q = 5), by = `Cognitive Measure`] # Adding quintiles

long_participants_11_MIA[ , `Cognitive Score` := floor(`Cognitive Score`)] # Rounding down the IQ scores to remove decimals in the x-axis
long_participants_11_MIA[ , Quintile := quantcut(`Cognitive Score`, q = 5), by = `Cognitive Measure`] # Adding quintiles


# I know, I better replace all this with a for-loop, but I'll leave polishing for later

long_participants_5 %>% # In Performance IQ facet two X labels are permuted because of the deprecation of the duplicate (103,111] factor level
  ggplot(aes(x= `Quintile`, y =`Psychopathology Score`,  group = `Psychopathology Measure`, color=`Psychopathology Measure`)) + 
  geom_pointrange(stat="summary", fun.data=mean_se, fun.args = list(mult=2), position=position_dodge(0.05)) +
  geom_line(stat="summary", fun=mean) +
  facet_wrap(~`Cognitive Measure`, scales='free_x') +
  labs(x = "Quintiles", y ="Mean Score ± 2SE") +
  theme_bw()

long_participants_5_performance_IQ %>% # a distinct graph for Performance IQ which will be embedded in the previous grid in an image editor
  ggplot(aes(x= `Quintile`, y =`Psychopathology Score`,  group = `Psychopathology Measure`, color=`Psychopathology Measure`)) + 
  geom_pointrange(stat="summary", fun.data=mean_se, fun.args = list(mult=2), position=position_dodge(0.05)) +
  geom_line(stat="summary", fun=mean) +
  labs(x = "Quintiles", y ="Mean Score ± 2SE") +
  theme_bw() +
  theme(legend.position = "none")

long_participants_11_CBCL %>% 
  ggplot(aes(x= `Quintile`, y =`Psychopathology Score`,  group = `Psychopathology Measure`, color=`Psychopathology Measure`)) + 
  geom_pointrange(stat="summary", fun.data=mean_se, fun.args = list(mult=2), position=position_dodge(0.05)) +
  geom_line(stat="summary", fun=mean) +
  facet_wrap(~`Cognitive Measure`, scales='free_x') +
  labs(x = "IQ Quintiles", y ="Mean Score ± 2SE") +
  theme_bw()

long_participants_11_SDQ %>% 
  ggplot(aes(x= `Quintile`, y =`Psychopathology Score`,  group = `Psychopathology Measure`, color=`Psychopathology Measure`)) + 
  geom_pointrange(stat="summary", fun.data=mean_se, fun.args = list(mult=2), position=position_dodge(0.05)) +
  geom_line(stat="summary", fun=mean) +
  facet_wrap(~`Cognitive Measure`, scales='free_x') +
  labs(x = "IQ Quintiles", y ="Mean Score ± 2SE") +
  theme_bw()

long_participants_11_MIA %>% 
  ggplot(aes(x= `Quintile`, y =`Psychopathology Score`,  group = `Psychopathology Measure`, color=`Psychopathology Measure`)) + 
  geom_pointrange(stat="summary", fun.data=mean_se, fun.args = list(mult=2), position=position_dodge(0.05)) +
  geom_line(stat="summary", fun=mean) +
  facet_wrap(~`Cognitive Measure`, scales='free_x') +
  labs(x = "IQ Quintiles", y ="Mean Score ± 2SE") +
  theme_bw()


#### 11 YEARS: g Factor Analysis ####

# Covariance matrix

g_tests <- cbind(eden$EVIP_score_adj, eden$READ_COMP_score_adj, eden$MAT_score_adj, eden$IDC_score_adj)
colnames(g_tests) <- c("EVIP", "Reading Comprehension", "Mathematics", "Concept Identification")
cor_matrix_g_tests <- cor(g_tests, y = NULL, use = "complete.obs",
                          method = "pearson")
cov_matrix_g_tests <- cov(g_tests, y = NULL, use = "complete.obs",
                          method = "pearson")

#Maximum likelihood: rotation applicable
fa(cor_matrix_g_tests, n.factors = 2, rotate = "oblimin", fm="lm")##VS## This imposes a single factor for some reason

#Confirmatory FA
g_eden <- subset(eden, select=-c(gc, gf, g)) 
cor(g_eden$EVIP_score_adj, g_eden$COMP_score_adj, use = "complete.obs", method = "spearman")
gFactor.model <- '
gc=~EVIP_score_adj + COMP_score_adj
gf=~MAT_score_adj + IDC_score_adj'

gFactor.fit <- cfa(gFactor.model,
                     g_eden) 

inspect(gFactor.fit)
summary_gFactor <- summary(gFactor.fit, fit.measures = TRUE, standardized = TRUE)
factor_cov_table <- residuals(gFactor.fit, type = "cor")
factor_cov_table

parameterEstimates(gFactor.fit, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  mutate(stars = ifelse(pvalue < .001, "***", 
                        ifelse(pvalue < .01, "**", 
                               ifelse(pvalue < .05, "*", "")))) %>%
  select('Latent Factor'=lhs, 
         Indicator=rhs, 
         B=est, 
         SE=se, Z=z, 
         Beta=std.all, 
         sig=stars) %>% 
  kable(digits = 3, format="pandoc", caption="Factor Loadings") ##VS## NA valiues for some indicators. A saturated model?


g_predict_ML <- as.data.frame(lavPredict(gFactor.fit, newdata = g_eden, method = "ML")) # the distribution looks fishy
g_predict <- as.data.frame(lavPredict(gFactor.fit, newdata = g_eden))
summary_g <- dfSummary(g_predict)

# Comparing distributions

# gc
new_gc_histogram <- hist(g_predict$gc)
ava_gc_histogram <- hist(eden$gc)

plot(new_gc_histogram, col=rgb(0,0,1,1/4))
plot(ava_gc_histogram, col=rgb(1,0,0,1/4), add = TRUE)
legend("topleft", inset=.02,
       c("New gc","Ava's gc"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), horiz=TRUE, cex=0.8)

#gf


new_gf_histogram <- hist(g_predict$gf)
ava_gf_histogram <- hist(eden$gf)

plot(new_gf_histogram, col=rgb(0,0,1,1/4), xlab="gf", main = "Comparing Distributions")
plot(ava_gf_histogram, col=rgb(1,0,0,1/4), add = TRUE)
legend("topleft", inset=.02,
       c("New gf","Ava's gf"), fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)), horiz=TRUE, cex=0.8)

g_eden[, c("gc", "gf") := lavPredict(gFactor.fit, newdata = g_eden), by = "n_ident"] # Tried inserting new gc and gf into the original dataframe, throws an error. Is it the argument of by= ?

g_vars <- subset(eden, select = c(classe_gr, EVIP_total_score, COMP_total_score, MAT_total_score, IDC_total_score, EVIP_score_adj, COMP_score_adj, MAT_score_adj, IDC_score_adj))

g_vars_no_NA_classe <- g_vars %>% filter(!is.na(classe_gr))
g_vars_no_NA_total_score <- eden %>% filter(!is.na(EVIP_total_score), !is.na(COMP_total_score), !is.na(MAT_total_score), !is.na(IDC_total_score))
g_vars_no_NA_adj <- g_vars %>% filter(!is.na(EVIP_score_adj), !is.na(COMP_score_adj), !is.na(MAT_score_adj), !is.na(IDC_score_adj))
summary(g_vars)

g_vars_no_NA_adj <- g_vars_no_NA_adj %>% filter(!is.na(classe_gr))

library(cNORM)
#Creating separate data frames to avoid participant exclusion due to NAs
MAT_norm_data <- na.omit(subset(eden, select = c(n_ident, MAT_total_score, classe_gr)))
EVIP_norm_data <- na.omit(subset(eden, select = c(n_ident, EVIP_total_score, classe_gr)))
COMP_norm_data <- na.omit(subset(eden, select = c(n_ident, COMP_total_score, classe_gr)))
IDC_norm_data <- na.omit(subset(eden, select = c(n_ident, IDC_total_score, classe_gr)))

#MAT
cnorm.MAT <- cnorm(raw = MAT_norm_data$MAT_total_score, group = MAT_norm_data$classe_gr)
#plot(cnorm.MAT, "series", start = 2, end = 5)
plot(cnorm.MAT, "subset")
plotRaw(cnorm.MAT)
#cnorm.cv(cnorm.MAT$data, max = 5, repetitions = 2)
MAT_norm_data$MAT_norm_score <- predictNorm(MAT_norm_data$MAT_total_score, MAT_norm_data$classe_gr, cnorm.MAT) # Normalized score for MAT
hist(MAT_norm_data$MAT_norm_score)

# EVIP
cnorm.EVIP <- cnorm(raw = EVIP_norm_data$EVIP_total_score, group = EVIP_norm_data$classe_gr) # 5 regressors.
plot(cnorm.EVIP, "subset")
plotRaw(cnorm.EVIP)
normTable(c(1, 2, 3), cnorm.EVIP, CI = .9, reliability = .94)
EVIP_norm_data$EVIP_norm_score <- predictNorm(EVIP_norm_data$EVIP_total_score, EVIP_norm_data$classe_gr, cnorm.EVIP) # Normalized score for EVIP
hist(EVIP_norm_data$EVIP_norm_score)

# COMP
cnorm.COMP <- cnorm(raw = COMP_norm_data$COMP_total_score, group = COMP_norm_data$classe_gr) # 11 regressors. WHAT? OMG
plot(cnorm.COMP, "series", start = 2, end = 10)
plot(cnorm.COMP, "subset")
plotRaw(cnorm.COMP)
normTable(c(1, 2, 3), cnorm.COMP, CI = .9, reliability = .94)
COMP_norm_data$COMP_norm_score <- predictNorm(COMP_norm_data$COMP_total_score, COMP_norm_data$classe_gr, cnorm.COMP) # Normalized score for COMP
hist(COMP_norm_data$COMP_norm_score)

# IDC
cnorm.IDC <- cnorm(raw = IDC_norm_data$IDC_total_score, group = IDC_norm_data$classe_gr) # 3 regressors.
plot(cnorm.IDC, "series", start = 2, end = 10)
plot(cnorm.IDC, "subset")
plotRaw(cnorm.IDC)
normTable(c(1, 2, 3), cnorm.IDC, CI = .9, reliability = .94)
IDC_norm_data$IDC_norm_score <- predictNorm(IDC_norm_data$IDC_total_score, IDC_norm_data$classe_gr, cnorm.IDC) # Normalized score for IDC
hist(IDC_norm_data$IDC_norm_score)

#Comparing means of internalizing disorder of the 4th and 5th quintiles of TIQ and VIQ
# Full-Scale IQ
total_test_data_5 <- participants_5 %>%
  mutate(Quintile = quantcut(`Full-Scale IQ`, q = 5))
total_test_data_5 <- total_cohend_data_5 %>%
  filter(Quintile == '(107,114]' | Quintile == '(114,142]')
total_test_data_5$Quintile <- factor(total_test_data_5$Quintile)

# Testing the assumption of normal distribution
names(total_test_data_5)[names(total_test_data_5) == "Internalizing Disorder"] <- "IntDis"

total_test_data_5 %>%
  group_by(Quintile) %>%
  shapiro_test(IntDis) # the data are not normally distributed

total_test_data_5 %>%
  group_by(Quintile) %>%
  get_summary_stats()

names(total_test_data_5)[names(total_test_data_5) == "IntDis"] <- "Internalizing Disorder"

#testing the assumption of equal variance
#total_test_data_5 %>% levene_test(`Internalizing Disorder` ~ Quintile, center = mean) #not significant
#cohens_d(total_cohend_data_5, `Internalizing Disorder` ~ Quintile, var.equal = TRUE, paired = FALSE)

#Wilcoxon sum-rank test (two-sample)
total_stat_test <- total_test_data_5 %>%
  wilcox_test(`Internalizing Disorder` ~ Quintile, paired = FALSE, alternative = "greater", exact = TRUE) %>%
  add_significance()
total_test_data_5 %>% wilcox_effsize(`Internalizing Disorder` ~ Quintile, paired = FALSE, alternative = "greater")

total_test_data_5 %>%
  group_by(Quintile) %>%
  median(`Internalizing Disorder`)
# Verbal IQ
verbal_test_data_5 <- participants_5 %>%
  mutate(Quintile = quantcut(`Verbal IQ`, q = 5))
verbal_test_data_5 <- verbal_test_data_5 %>%
  filter(Quintile == '(110,118]' | Quintile == '(118,147]')
verbal_test_data_5$Quintile <- factor(verbal_test_data_5$Quintile)

# Testing the assumption of normal distribution
names(verbal_test_data_5)[names(verbal_test_data_5) == "Internalizing Disorder"] <- "IntDis"

verbal_test_data_5 %>%
  group_by(Quintile) %>%
  shapiro_test(IntDis) # the data are not normally distributed

verbal_test_data_5 %>%
  group_by(Quintile) %>%
  get_summary_stats()

names(verbal_test_data_5)[names(verbal_test_data_5) == "IntDis"] <- "Internalizing Disorder"

#Wilcoxon sum-rank test (two-sample)
wilcox.test(`Internalizing Disorder` ~ Quintile, verbal_test_data_5, paired = FALSE, alternative = "greater")
verbal_stat_test <- verbal_test_data_5 %>%
  wilcox_test(`Internalizing Disorder` ~ Quintile, paired = FALSE, alternative = "greater", exact = TRUE) %>%
  add_significance()

verbal_test_data_5 %>% wilcox_effsize(`Internalizing Disorder` ~ Quintile, paired = FALSE, alternative = "greater")
# -----------------------SEM----------------------------
library(lavaan)

long_participants_11_MIA %>% 
  filter(`Cognitive Measure` == Cognitive_Measure)

model5y_ADHD <- ' 
# measurement model 
SDQ5_ADHD =~ 
ADHD5 =~ CBCL5 + SDQ5 + MIA5
IQ11 =~ VIQ11 + PIQ11
ADHD11 =~ CBCL11 + SDQ11 + MIA11
# regressions 
ADHD5 ~ IQ5
IQ11 ~ IQ5
ADHD11 ~ IQ5 + ADHD5 + IQ11
# residual correlations (because the same measurements have been used at both time points).
VIQ5 ~~ VIQ11
PIQ5 ~~ PIQ11
CBCL5 ~~ CBCL 11
SDQ5 ~~ SDQ11
MIA5 ~~ MIA11
'
fit <- sem(model, data = EDEN) 
summary(fit, standardized = TRUE)
modindices(fit, sort = TRUE, maximum.number = 5)


