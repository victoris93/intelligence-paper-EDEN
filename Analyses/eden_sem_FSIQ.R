library(lavaan)
library(dplyr)
library(ggplot2)

add_sig_levels_to_graph_data <- function(graph_data, sig_levels) {
  graph_data$edges$label <- graph_data$edges$est
  
  pvals <- graph_data$edges$pval
  
  for (i in 1:length(pvals)) {
    if (is.na(pvals[i]) == TRUE) {
      pvals[i] = 0.000
    }
    if (pvals[i] <= sig_levels[2]) {
      graph_data$edges$label[i] <- paste0(graph_data$edges$label[i], "**")
    } else if (pvals[i] < sig_levels[1]) {
      graph_data$edges$label[i] <- paste0(graph_data$edges$label[i], "*")
    }
  }
  return(graph_data)
}

load("long_participants_5.Rdata")
load("long_participants_11_CBCL.Rdata")
load("long_participants_11_SDQ.Rdata")
load("long_participants_11_MIA.Rdata")

######## ADHD

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

# standardize variables
sem_ADHD_vars$ADHD5_std = (sem_ADHD_vars$ADHD5 - mean(sem_ADHD_vars$ADHD5)) / sd(sem_ADHD_vars$ADHD5)
sem_ADHD_vars$FSIQ5_std = (sem_ADHD_vars$FSIQ5 - mean(sem_ADHD_vars$FSIQ5)) / sd(sem_ADHD_vars$FSIQ5)
sem_ADHD_vars$ADHD11_std = (sem_ADHD_vars$ADHD11 - mean(sem_ADHD_vars$ADHD11, na.rm = TRUE)) / sd(sem_ADHD_vars$ADHD11, na.rm = TRUE)
sem_ADHD_vars$FSIQ11_std = (sem_ADHD_vars$FSIQ11 - mean(sem_ADHD_vars$FSIQ11, na.rm = TRUE)) / sd(sem_ADHD_vars$FSIQ11, na.rm = TRUE)

model_ADHD <-  '
# correlations
ADHD5_std ~~ FSIQ5_std
ADHD11_std ~~ FSIQ11_std

# auto-regressions
FSIQ11_std ~ FSIQ5_std
ADHD11_std ~ ADHD5_std

# cross-lagged regressions
FSIQ11_std ~ ADHD5_std
ADHD11_std ~ FSIQ5_std
'

#' 
## regressions 
#ADHD5 ~ FSIQ5
#FSIQ11 ~ FSIQ5
#ADHD11 ~ FSIQ5 + FSIQ11 + ADHD5
#'
fit_ADHD <- sem(model_ADHD, data = sem_ADHD_vars) 
summary(fit_ADHD, standardized = TRUE)
parameterEstimates(fit_ADHD)
modindices(fit_ADHD)

######## Visualization

layout <- get_layout(fit_ADHD, layout_algorithm = "layout_on_grid")
graph_sem(fit_ADHD, layout = layout)
graph_data_fsiq_ADHD <- prepare_graph(model = fit_ADHD, layout = layout)
graph_data_fsiq_ADHD <- add_sig_levels_to_graph_data(graph_data_fsiq_ADHD, c(0.05, 0.004))

graph_data_fsiq_ADHD %>%
  edit_graph({ label = c("ADHD, 11y", "ADHD, 5y", "FSIQ, 11y", "FSIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "red", "red", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "top", "right", "right", "bottom", "top", "right", "top", "bottom", "top") }, element = "edges") %>% 
  edit_graph({ connect_to= c("bottom", "bottom", "left", "left", "top", "bottom", "right", "top", "bottom", "top") }, element = "edges") %>%
  edit_graph({ curvature= c(NA, NA, NA, NA, 60, -60, NA, NA, NA, NA) }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>%
  linetype_sig_edges(linetype = 1) %>% 
  plot()

######## Internalizing Disorder

sem_vars_FSIQ_dep_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "Internalizing Disorder")

sem_vars_FSIQ_dep_5y <- subset(sem_vars_FSIQ_dep_5y, select = c(`ID`, `Cognitive Score`,
                                                                `Psychopathology Score`))

colnames(sem_vars_FSIQ_dep_5y) <- c("ID","FSIQ5", 
                                    "dep5")

sem_vars_FSIQ_dep_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "Internalizing Disorder, SDQ")

sem_vars_FSIQ_dep_11y <- subset(sem_vars_FSIQ_dep_11y, select = c(`ID`, `Cognitive Score`,
                                                                  `Psychopathology Score`))
colnames(sem_vars_FSIQ_dep_11y) <- c("ID", "FSIQ11", 
                                     "dep11")

sem_dep_vars <- left_join(sem_vars_FSIQ_dep_5y, sem_vars_FSIQ_dep_11y)

# standardize variables
sem_dep_vars$dep5_std = (sem_dep_vars$dep5 - mean(sem_dep_vars$dep5)) / sd(sem_dep_vars$dep5)
sem_dep_vars$FSIQ5_std = (sem_dep_vars$FSIQ5 - mean(sem_dep_vars$FSIQ5)) / sd(sem_dep_vars$FSIQ5)
sem_dep_vars$dep11_std = (sem_dep_vars$dep11 - mean(sem_dep_vars$dep11, na.rm = TRUE)) / sd(sem_dep_vars$dep11, na.rm = TRUE)
sem_dep_vars$FSIQ11_std = (sem_dep_vars$FSIQ11 - mean(sem_dep_vars$FSIQ11, na.rm = TRUE)) / sd(sem_dep_vars$FSIQ11, na.rm = TRUE)

model_dep <- ' 
# correlations
dep5_std ~~ FSIQ5_std
dep11_std ~~ FSIQ11_std

# auto-regressions
FSIQ11_std ~ FSIQ5_std
dep11_std ~ dep5_std

# cross-lagged regressions
FSIQ11_std ~ dep5_std
dep11_std ~ FSIQ5_std
'

fit_dep <- sem(model_dep, data = sem_dep_vars)
summary(fit_dep, standardized = TRUE)
parameterEstimates(fit_dep)
modindices(fit_dep)

######## Visualization

layout <- get_layout(fit_dep, layout_algorithm = "layout_on_grid")
graph_sem(fit_dep, layout = layout)
graph_data_fsiq_dep <- prepare_graph(model = fit_dep, layout = layout)
graph_data_fsiq_dep <- add_sig_levels_to_graph_data(graph_data_fsiq_dep, c(0.05, 0.004))

graph_data_fsiq_dep %>%
  edit_graph({ label = c("Int.Dis., 11y", "Int.Dis., 5y", "FSIQ, 11y", "FSIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "red", "red", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "top", "right", "right", "bottom", "top", "right", "top", "bottom", "top") }, element = "edges") %>% 
  edit_graph({ connect_to= c("bottom", "bottom", "left", "left", "top", "bottom", "right", "top", "bottom", "top") }, element = "edges") %>%
  edit_graph({ curvature= c(NA, NA, NA, NA, 60, -60, NA, NA, NA, NA) }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>%
  linetype_sig_edges(linetype = 1) %>% 
  plot()

######## Social Problems

sem_vars_FSIQ_social_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "Social Problems")

sem_vars_FSIQ_social_5y <- subset(sem_vars_FSIQ_social_5y, select = c(`ID`, `Cognitive Score`,
                                                                      `Psychopathology Score`))

colnames(sem_vars_FSIQ_social_5y) <- c("ID","FSIQ5", 
                                       "social5")

sem_vars_FSIQ_social_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "Social Problems, SDQ")

sem_vars_FSIQ_social_11y <- subset(sem_vars_FSIQ_social_11y, select = c(`ID`, `Cognitive Score`,
                                                                        `Psychopathology Score`))
colnames(sem_vars_FSIQ_social_11y) <- c("ID", "FSIQ11", 
                                        "social11")

sem_social_vars <- left_join(sem_vars_FSIQ_social_5y, sem_vars_FSIQ_social_11y)

# standardize variables
sem_social_vars$social5_std = (sem_social_vars$social5 - mean(sem_social_vars$social5)) / sd(sem_social_vars$social5)
sem_social_vars$FSIQ5_std = (sem_social_vars$FSIQ5 - mean(sem_social_vars$FSIQ5)) / sd(sem_social_vars$FSIQ5)
sem_social_vars$social11_std = (sem_social_vars$social11 - mean(sem_social_vars$social11, na.rm = TRUE)) / sd(sem_social_vars$social11, na.rm = TRUE)
sem_social_vars$FSIQ11_std = (sem_social_vars$FSIQ11 - mean(sem_social_vars$FSIQ11, na.rm = TRUE)) / sd(sem_social_vars$FSIQ11, na.rm = TRUE)

model_social <- ' 
# correlations
social5_std ~~ FSIQ5_std
social11_std ~~ FSIQ11_std

# auto-regressions
FSIQ11_std ~ FSIQ5_std
social11_std ~ social5_std

# cross-lagged regressions
FSIQ11_std ~ social5_std
social11_std ~ FSIQ5_std
'

fit_social <- sem(model_social, data = sem_social_vars) 
summary(fit_social, standardized = TRUE)
parameterEstimates(fit_social)
modindices(fit_social)

######## Visualization

layout <- get_layout(fit_social, layout_algorithm = "layout_on_grid")
graph_sem(fit_social, layout = layout)
graph_data_fsiq_social <- prepare_graph(model = fit_social, layout = layout)
graph_data_fsiq_social <- add_sig_levels_to_graph_data(graph_data_fsiq_social, c(0.05, 0.004))

graph_data_fsiq_social %>%
  edit_graph({ label = c("FSIQ, 11y", "FSIQ, 5y", "Social Pr., 11y", "Social Pr., 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "red", "red", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "top", "right", "right", "bottom", "top", "right", "top", "bottom", "top") }, element = "edges") %>% 
  edit_graph({ connect_to= c("bottom", "bottom", "left", "left", "top", "bottom", "right", "top", "bottom", "top") }, element = "edges") %>%
  edit_graph({ curvature= c(NA, NA, NA, NA, 60, -60, NA, NA, NA, NA) }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>%
  linetype_sig_edges(linetype = 1) %>% 
  plot()

######## Conduct Disorder

sem_vars_FSIQ_conduct_5y <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "Conduct Disorder")

sem_vars_FSIQ_conduct_5y <- subset(sem_vars_FSIQ_conduct_5y, select = c(`ID`, `Cognitive Score`,
                                                                        `Psychopathology Score`))

colnames(sem_vars_FSIQ_conduct_5y) <- c("ID","FSIQ5", 
                                        "conduct5")

sem_vars_FSIQ_conduct_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "Conduct Disorder, SDQ")

sem_vars_FSIQ_conduct_11y <- subset(sem_vars_FSIQ_conduct_11y, select = c(`ID`, `Cognitive Score`,
                                                                          `Psychopathology Score`))
colnames(sem_vars_FSIQ_conduct_11y) <- c("ID", "FSIQ11", 
                                         "conduct11")

sem_conduct_vars <- left_join(sem_vars_FSIQ_conduct_5y, sem_vars_FSIQ_conduct_11y)

# standardize variables
sem_conduct_vars$conduct5_std = (sem_conduct_vars$conduct5 - mean(sem_conduct_vars$conduct5)) / sd(sem_conduct_vars$conduct5)
sem_conduct_vars$FSIQ5_std = (sem_conduct_vars$FSIQ5 - mean(sem_conduct_vars$FSIQ5)) / sd(sem_conduct_vars$FSIQ5)
sem_conduct_vars$conduct11_std = (sem_conduct_vars$conduct11 - mean(sem_conduct_vars$conduct11, na.rm = TRUE)) / sd(sem_conduct_vars$conduct11, na.rm = TRUE)
sem_conduct_vars$FSIQ11_std = (sem_conduct_vars$FSIQ11 - mean(sem_conduct_vars$FSIQ11, na.rm = TRUE)) / sd(sem_conduct_vars$FSIQ11, na.rm = TRUE)

model_conduct <- ' 
# correlations
conduct5_std ~~ FSIQ5_std
conduct11_std ~~ FSIQ11_std

# auto-regressions
FSIQ11_std ~ FSIQ5_std
conduct11_std ~ conduct5_std

# cross-lagged regressions
FSIQ11_std ~ conduct5_std
conduct11_std ~ FSIQ5_std
'
fit_conduct <- sem(model_conduct, data = sem_conduct_vars) 
summary(fit_conduct, standardized = TRUE)
parameterEstimates(fit_conduct)
modindices(fit_conduct)

######## Visualization

layout <- get_layout(fit_conduct, layout_algorithm = "layout_on_grid")
graph_sem(fit_conduct, layout = layout)
graph_data_fsiq_conduct <- prepare_graph(model = fit_conduct, layout = layout)
graph_data_fsiq_conduct <- add_sig_levels_to_graph_data(graph_data_fsiq_conduct, c(0.05, 0.004))

graph_data_fsiq_conduct %>%
  edit_graph({ label = c("Conduct Dis., 11y", "Conduct Dis., 5y", "FSIQ, 11y", "FSIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "red", "red", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "top", "right", "right", "bottom", "top", "right", "top", "bottom", "top") }, element = "edges") %>% 
  edit_graph({ connect_to= c("bottom", "bottom", "left", "left", "top", "bottom", "right", "top", "bottom", "top") }, element = "edges") %>%
  edit_graph({ curvature= c(NA, NA, NA, NA, 60, -60, NA, NA, NA, NA) }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>%
  linetype_sig_edges(linetype = 1) %>% 
  plot()

