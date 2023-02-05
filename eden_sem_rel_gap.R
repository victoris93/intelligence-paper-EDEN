library(lavaan)
library(dplyr)
library(ggplot2)
library(tidySEM)

add_sig_levels_to_graph_data <- function(graph_data, sig_levels) {
  graph_data$edges$label <- graph_data$edges$est
  
  pvals <- graph_data$edges$pval
  
  for (i in 1:length(pvals)) {
    if (is.na(pvals[i]) == TRUE) {
      pvals[i] = 0.000
    }
    if (pvals[i] <= sig_levels[3]) {
      graph_data$edges$label[i] <- paste0(graph_data$edges$label[i], "***")
    } else if (pvals[i] <= sig_levels[2]) {
      graph_data$edges$label[i] <- paste0(graph_data$edges$label[i], "**")
    } else if (pvals[i] <= sig_levels[1]) {
      graph_data$edges$label[i] <- paste0(graph_data$edges$label[i], "*")
    }
  }
  
  return(graph_data)
}

load("participants_5.Rdata")
load("participants_11_CBCL.Rdata")
load("participants_11_SDQ.Rdata")
load("participants_11_MIA.Rdata")

######## ADHD

sem_vars_rel_gap_vpiq_ADHD_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "ADHD")

sem_vars_rel_gap_vpiq_ADHD_5y <- subset(sem_vars_rel_gap_vpiq_ADHD_5y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))

colnames(sem_vars_rel_gap_vpiq_ADHD_5y) <- c("ID","rel_gap_vpiq5", 
                                   "ADHD5")

sem_vars_rel_gap_vpiq_ADHD_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "ADHD, SDQ")

sem_vars_rel_gap_vpiq_ADHD_11y <- subset(sem_vars_rel_gap_vpiq_ADHD_11y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))
colnames(sem_vars_rel_gap_vpiq_ADHD_11y) <- c("ID", "rel_gap_vpiq11", 
                                     "ADHD11")

sem_ADHD_vars <- left_join(sem_vars_rel_gap_vpiq_ADHD_5y, sem_vars_rel_gap_vpiq_ADHD_11y)


common_ids <- intersect(na.omit(sem_ADHD_vars)$ID, sem_vars_rel_gap_vpiq_ADHD_11y$ID)
not_matching_ids <- sem_vars_rel_gap_vpiq_ADHD_5y$ID[!sem_vars_rel_gap_vpiq_ADHD_5y$ID %in% common_ids]


model_ADHD <- ' 
# regressions 
ADHD5 ~ rel_gap_vpiq5
rel_gap_vpiq11 ~ rel_gap_vpiq5
ADHD11 ~ rel_gap_vpiq5 + rel_gap_vpiq11 + ADHD5
'
fit_ADHD <- sem(model_ADHD, data = sem_ADHD_vars) 
summary(fit_ADHD, standardized = TRUE)
parameterEstimates(fit_ADHD)
modindices(fit_ADHD)

######## Visualization

layout <- get_layout(fit_ADHD, layout_algorithm = "layout_on_grid")
graph_data_rel_gap_vpiq_ADHD <- prepare_graph(model = fit_ADHD, layout = layout)
plot(graph_data_rel_gap_vpiq_ADHD)
graph_data_rel_gap_vpiq_ADHD <- add_sig_levels_to_graph_data(graph_data_rel_gap_vpiq_ADHD, c(0.05, 0.0125, 0.004))

graph_data_rel_gap_vpiq_ADHD %>%
  edit_graph({ label = c("ADHD, 11y", "ADHD, 5y", "VIQ-PIQ, 11y", "VIQ-PIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "right", "top", "top", "right", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ connect_to= c("bottom", "left", "bottom", "bottom", "left", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ show = TRUE}, element = "edges") %>%
  plot()

######## Internalizing Disorder

sem_vars_rel_gap_vpiq_dep_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "Internalizing Disorder")

sem_vars_rel_gap_vpiq_dep_5y <- subset(sem_vars_rel_gap_vpiq_dep_5y, select = c(`ID`, `Cognitive Score`,
                                                                `Psychopathology Score`))

colnames(sem_vars_rel_gap_vpiq_dep_5y) <- c("ID","rel_gap_vpiq5", 
                                    "dep5")

sem_vars_rel_gap_vpiq_dep_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "Internalizing Disorder, SDQ")

sem_vars_rel_gap_vpiq_dep_11y <- subset(sem_vars_rel_gap_vpiq_dep_11y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))
colnames(sem_vars_rel_gap_vpiq_dep_11y) <- c("ID", "rel_gap_vpiq11", 
                                     "dep11")

sem_dep_vars <- left_join(sem_vars_rel_gap_vpiq_dep_5y, sem_vars_rel_gap_vpiq_dep_11y)


common_ids <- intersect(na.omit(sem_dep_vars)$ID, sem_vars_rel_gap_vpiq_dep_11y$ID)
not_matching_ids <- sem_vars_rel_gap_vpiq_dep_5y$ID[!sem_vars_rel_gap_vpiq_dep_5y$ID %in% common_ids]

model_dep <- ' 
# regressions 
dep5 ~ rel_gap_vpiq5
rel_gap_vpiq11 ~ rel_gap_vpiq5
dep11 ~ rel_gap_vpiq5 + rel_gap_vpiq11 + dep5
'

fit_dep <- sem(model_dep, data = sem_dep_vars) 
summary(fit_dep, standardized = TRUE)
parameterEstimates(fit_dep)
modindices(fit_dep)

######## Visualization

layout <- get_layout(fit_dep, layout_algorithm = "layout_on_grid")
graph_data_rel_gap_vpiq_dep <- prepare_graph(model = fit_dep, layout = layout)
graph_sem(fit_dep, layout = layout)
graph_data_rel_gap_vpiq_dep <- add_sig_levels_to_graph_data(graph_data_rel_gap_vpiq_dep, c(0.05, 0.0125, 0.004))

graph_data_rel_gap_vpiq_dep %>%
  edit_graph({ label = c("Int. Dis., 11y", "Int. Dis., 5y", "VIQ-PIQ, 11y", "VIQ-PIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "right", "top", "top", "right", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ connect_to= c("bottom", "left", "bottom", "bottom", "left", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  plot()

graph_sem(fit_dep, layout = layout)
      
######## Social Problems

sem_vars_rel_gap_vpiq_social_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "Social Problems")

sem_vars_rel_gap_vpiq_social_5y <- subset(sem_vars_rel_gap_vpiq_social_5y, select = c(`ID`, `Cognitive Score`,
                                                                      `Psychopathology Score`))

colnames(sem_vars_rel_gap_vpiq_social_5y) <- c("ID","rel_gap_vpiq5", 
                                       "social5")

sem_vars_rel_gap_vpiq_social_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "Social Problems, SDQ")

sem_vars_rel_gap_vpiq_social_11y <- subset(sem_vars_rel_gap_vpiq_social_11y, select = c(`ID`, `Cognitive Score`,
                                                                        `Psychopathology Score`))
colnames(sem_vars_rel_gap_vpiq_social_11y) <- c("ID", "rel_gap_vpiq11", 
                                        "social11")

sem_social_vars <- left_join(sem_vars_rel_gap_vpiq_social_5y, sem_vars_rel_gap_vpiq_social_11y)


common_ids <- intersect(na.omit(sem_social_vars)$ID, sem_vars_rel_gap_vpiq_social_11y$ID)
not_matching_ids <- sem_vars_rel_gap_vpiq_social_5y$ID[!sem_vars_rel_gap_vpiq_social_5y$ID %in% common_ids]

model_social <- ' 
# regressions 
social5 ~ rel_gap_vpiq5
rel_gap_vpiq11 ~ rel_gap_vpiq5
social11 ~ rel_gap_vpiq5 + rel_gap_vpiq11 + social5
'
fit_social <- sem(model_social, data = sem_social_vars, stars = c(0.05, 0.0125, 0.004)) 
summary(fit_social, standardized = TRUE)
parameterEstimates(fit_social)
modindices(fit_social)

######## Visualization

layout <- get_layout(fit_social, layout_algorithm = "layout_on_grid")
graph_data_rel_gap_vpiq_social <- prepare_graph(model = fit_social, layout = layout, stars = c(0.05, 0.0125, 0.004))

graph_data_rel_gap_vpiq_social %>%
  edit_graph({ label = c("VIQ-PIQ, 11y", "VIQ-PIQ, 5y", "Social Pr., 11y", "Social Pr., 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  plot()

graph_sem(fit_social, layout = layout)

######## Conduct Disorder

sem_vars_rel_gap_vpiq_conduct_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "Conduct Disorder")

sem_vars_rel_gap_vpiq_conduct_5y <- subset(sem_vars_rel_gap_vpiq_conduct_5y, select = c(`ID`, `Cognitive Score`,
                                                                        `Psychopathology Score`))

colnames(sem_vars_rel_gap_vpiq_conduct_5y) <- c("ID","rel_gap_vpiq5", 
                                        "conduct5")

sem_vars_rel_gap_vpiq_conduct_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "VIQ-PIQ",
         `Psychopathology Measure` == "Conduct Disorder, SDQ")

sem_vars_rel_gap_vpiq_conduct_11y <- subset(sem_vars_rel_gap_vpiq_conduct_11y, select = c(`ID`, `Cognitive Score`,
                                                                          `Psychopathology Score`))
colnames(sem_vars_rel_gap_vpiq_conduct_11y) <- c("ID", "rel_gap_vpiq11", 
                                         "conduct11")

sem_conduct_vars <- left_join(sem_vars_rel_gap_vpiq_conduct_5y, sem_vars_rel_gap_vpiq_conduct_11y)


common_ids <- intersect(na.omit(sem_conduct_vars)$ID, sem_vars_rel_gap_vpiq_conduct_11y$ID)
not_matching_ids <- sem_vars_rel_gap_vpiq_conduct_5y$ID[!sem_vars_rel_gap_vpiq_conduct_5y$ID %in% common_ids]

model_conduct <- ' 
# regressions 
conduct5 ~ rel_gap_vpiq5
rel_gap_vpiq11 ~ rel_gap_vpiq5
conduct11 ~ rel_gap_vpiq5 + rel_gap_vpiq11 + conduct5
'
fit_conduct <- sem(model_conduct, data = sem_conduct_vars) 
summary(fit_conduct, standardized = TRUE)
parameterEstimates(fit_conduct)
modindices(fit_conduct)

######## Visualization

layout <- get_layout(fit_conduct, layout_algorithm = "layout_on_grid")
graph_sem(fit_conduct, layout = layout)
graph_data_rel_gap_vpiq_conduct <- prepare_graph(model = fit_conduct, layout = layout)

graph_data_rel_gap_vpiq_conduct <- add_sig_levels_to_graph_data(graph_data_rel_gap_vpiq_conduct, c(0.05, 0.0125, 0.004))

graph_data_rel_gap_vpiq_conduct %>%
  edit_graph({ label = c("Cond. Dis., 11y", "Cond. Dis., 5y", "VIQ-PIQ, 11y", "VIQ-PIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  plot()

graph_data_rel_gap_vpiq_conduct <- add_sig_levels_to_graph_data(graph_data_rel_gap_vpiq_conduct, c(0.05, 0.0125, 0.004))
plot(graph_data_rel_gap_vpiq_conduct)
graph_sem(fit_conduct, layout = layout)
