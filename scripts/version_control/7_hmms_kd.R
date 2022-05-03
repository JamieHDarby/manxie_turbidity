
# HMM analysis ------------------------------------------------------------

manxie_ls_restricted <- pos_dive_df %>%
  # Restrict data to sections with > 100 points
  split(., . $ID) %>%
  .[sapply(
    ., function(x) dim(x)[1]) > 100] %>%
  bind_rows(.) %>%
  filter(!is.na(KD490))

# Extract these variables
HMM_data <- manxie_ls_restricted[c("x", "y", "section",
                                   "KD490", "zsd", "dives")] %>%
  
  # Rename trip_id to ID for the HMM function
  rename(ID = section) %>%
  
  # MomentuHMM's data prep funtion
  momentuHMM::prepData(., type="UTM") %>%
  
  # Sort out the 0 steos and the NAs
  mutate(step = ifelse(step == 0, 1, step),
         step = ifelse(is.na(step), 1, step),
         angle = ifelse(is.na(angle), 0, angle),
         step = (ifelse(step > 5000, 5000, step)),
         KD490 = log(KD490),
         dive_bool = ifelse(dives > 0, 2, NA))

# Run HMM on the prepared data
system.time(
  StateModel <-
    momentuHMM::fitHMM(
      data = HMM_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(80, 500, 2700,
                           40, 600, 1000),
                  angle = c(20, 1, 35)),
      formula = ~1))

# Run HMM on the prepared data, using dives
system.time(
  StateModel_dives <-
    momentuHMM::fitHMM(
      data = HMM_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(80, 500, 2700,
                           40, 600, 1000),
                  angle = c(20, 1, 35)),
      knownStates = HMM_data$dive_bool,
      formula = ~1))

# Run HMM on the prepared data, with KD490 as a covariate
system.time(
  StateModel_KD490 <-
    momentuHMM::fitHMM(
      data = HMM_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(80, 500, 2700,
                           40, 600, 1000),
                  angle = c(20, 1, 35)),
      formula = ~KD490))

# Run HMM on the prepared data, with KD490 as a covariate as well as dives
system.time(
  StateModel_KD490_dives <-
    momentuHMM::fitHMM(
      data = HMM_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(80, 500, 2700,
                           40, 600, 1000),
                  angle = c(20, 1, 35)),
      knownStates = HMM_data$dive_bool,
      formula = ~KD490))

momentuHMM::AICweights(StateModel, StateModel_dives,
                       StateModel_KD490,
                       StateModel_KD490_dives)

momentuHMM::AICweights(StateModel,
                       StateModel_KD490)

AIC(StateModel)
AIC(StateModel_KD490)

StateModel$mle[1:2]
StateModel_KD490$mle[1:2]

momentuHMM::plotStationary(StateModel_KD490, plotCI = T)
momentuHMM::plotStationary(StateModel_KD490_dives, plotCI = T)

# Recombine hmm state into the overall dataframe
manxie_ls_restricted$hmm_state <- momentuHMM::viterbi(StateModel)
manxie_ls_restricted$hmm_state_KD <- momentuHMM::viterbi(StateModel_KD490)

manxie_ls_restricted %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>%
  filter(dive_bool > 0) %>%
ggplot() +
  geom_violin(aes(y = max_depth, x = as.factor(hmm_state), fill = as.factor(hmm_state)))

manxie_ls_restricted %>% filter(dives > 0) %>% select(hmm_state) %>% table()
manxie_ls_restricted %>% filter(dives == 0) %>% select(hmm_state) %>% table()
manxie_ls_restricted %>% filter(dives > 0) %>% select(hmm_state_KD) %>% table()
manxie_ls_restricted %>% filter(dives == 0) %>% select(hmm_state_KD) %>% table()
manxie_ls_restricted %>% filter(hmm_state != 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()
manxie_ls_restricted %>% filter(hmm_state == 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()


sum(manxie_ls_restricted$hmm_state == manxie_ls_restricted$hmm_state_KD) /
  nrow(manxie_ls_restricted)
