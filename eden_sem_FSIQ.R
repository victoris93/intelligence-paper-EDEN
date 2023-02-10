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

##################################################################

sem_vars_fsiq <- long_participants_5 %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "ADHD")

sem_vars_fsiq <- subset(sem_vars_fsiq, select = c(`ID`, `Cognitive Score`,
                                                  `Psychopathology Score`))
colnames(sem_vars_fsiq) <- c("ID","FSIQ5", 
                             "ADHD5")
sem_vars_fsiq$dep5 <- filter(long_participants_5, `Cognitive Measure` == "Total IQ", `Psychopathology Measure` == "Internalizing Disorder")$`Psychopathology Score`
sem_vars_fsiq$social5 <- filter(long_participants_5, `Cognitive Measure` == "Total IQ", `Psychopathology Measure` == "Social Problems")$`Psychopathology Score`
sem_vars_fsiq$conduct5 <- filter(long_participants_5, `Cognitive Measure` == "Total IQ", `Psychopathology Measure` == "Social Problems")$`Psychopathology Score`

# 11Y

sem_vars_fsiq_11y <- long_participants_11_SDQ %>% 
  filter(`Cognitive Measure` == "Total IQ",
         `Psychopathology Measure` == "ADHD, SDQ")

sem_vars_fsiq_11y <- subset(sem_vars_fsiq_11y, select = c(`ID`, `Cognitive Score`,
                                                          `Psychopathology Score`))
colnames(sem_vars_fsiq_11y) <- c("ID","FSIQ11", 
                                 "ADHD11")

sem_vars_fsiq_11y$dep11 <- filter(long_participants_11_SDQ, `Cognitive Measure` == "Total IQ", `Psychopathology Measure` == "Internalizing Disorder, SDQ")$`Psychopathology Score`
sem_vars_fsiq_11y$social11 <- filter(long_participants_11_SDQ, `Cognitive Measure` == "Total IQ", `Psychopathology Measure` == "Social Problems, SDQ")$`Psychopathology Score`
sem_vars_fsiq_11y$conduct11 <- filter(long_participants_11_SDQ, `Cognitive Measure` == "Total IQ", `Psychopathology Measure` == "Conduct Disorder, SDQ")$`Psychopathology Score`

sem_vars_fsiq <- left_join(sem_vars_fsiq, sem_vars_fsiq_11y)

model_pFactor <-  '
#latent psy variables
p5 =~ ADHD5 + dep5 + social5 + conduct5
p11 =~ ADHD11 + dep11 + social11 + conduct11

# correlations
p5 ~~ FSIQ5
p11 ~~ FSIQ11

# auto-regressions
FSIQ11 ~ FSIQ5
p11 ~ p5

# cross-lagged regressions
FSIQ11 ~ p5
p11 ~ FSIQ5
'

fit_pFactor <- sem(model_pFactor, data = sem_vars_fsiq) 
summary(fit_ADHD, standardized = TRUE)
parameterEstimates(fit_ADHD)
modindices(fit_ADHD)

######## Visualization

layout <- get_layout(fit_ADHD, layout_algorithm = "layout_on_grid")
graph_sem(fit_ADHD, layout = layout)
graph_data_fsiq_ADHD <- prepare_graph(model = fit_ADHD, layout = layout)
graph_data_fsiq_ADHD <- add_sig_levels_to_graph_data(graph_data_fsiq_ADHD, c(0.05, 0.004))
##################################################################

model_ADHD <-  '

# correlations
ADHD5 ~~ FSIQ5
ADHD11 ~~ FSIQ11

# auto-regressions
FSIQ11 ~ FSIQ5
ADHD11 ~ ADHD5

# cross-lagged regressions
FSIQ11 ~ ADHD5
ADHD11 ~ FSIQ5
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
  #edit_graph({ label = c("ADHD, 11y", "ADHD, 5y", "FSIQ, 11y", "FSIQ, 5y") }, element = "nodes") %>%
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

model_dep <- ' 
# regressions 
dep5 ~ FSIQ5
FSIQ11 ~ FSIQ5
dep11 ~ FSIQ5 + FSIQ11 + dep5
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
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "right", "top", "top", "right", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ connect_to= c("bottom", "left", "bottom", "bottom", "left", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>% 
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


model_social <- ' 
# regressions 
social5 ~ FSIQ5
FSIQ11 ~ FSIQ5
social11 ~ FSIQ5 + FSIQ11 + social5
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

graph_data_rel_gap_vpiq_social %>%
  edit_graph({ label = c("FSIQ, 11y", "FSIQ, 5y", "Social Pr., 11y", "Social Pr., 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "right", "top", "top", "right", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ connect_to= c("bottom", "left", "bottom", "bottom", "left", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>% 
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

model_conduct <- ' 
# regressions 
conduct5 ~ FSIQ5
FSIQ11 ~ FSIQ5
conduct11 ~ FSIQ5 + FSIQ11 + conduct5
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

graph_data_rel_gap_vpiq_conduct %>%
  edit_graph({ label = c("Conduct Dis., 11y", "Conduct Dis., 5y", "FSIQ, 11y", "FSIQ, 5y") }, element = "nodes") %>%
  edit_graph({ color = c("black", "black", "red", "red", "black", "black", "black", "black", "black") }, element = "edges") %>%
  edit_graph({ connect_from = c("top", "right", "top", "top", "right", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ connect_to= c("bottom", "left", "bottom", "bottom", "left", "top", "bottom", "top", "bottom") }, element = "edges") %>%
  edit_graph({ show = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)}, element = "edges") %>%
  linetype_nonsig_edges(linetype = 2) %>% 
  plot()


