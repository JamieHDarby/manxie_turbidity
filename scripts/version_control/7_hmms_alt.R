
# HMM analysis ------------------------------------------------------------

manxie_ls_restricted <- manxie_ls %>%
  # Restrict data to sections with > 100 points
  split(., . $ID) %>%
  .[sapply(
    ., function(x) dim(x)[1]) > 100] %>%
  bind_rows(.) %>%
  filter(!is.na(KD490))

# Extract these variables
HMM_data <- manxie_ls_restricted[c("x", "y", "section",
                                   "KD490")] %>%
  
  # Rename trip_id to ID for the HMM function
  rename(ID = section) %>%
  
  # MomentuHMM's data prep funtion
  momentuHMM::prepData(., type="UTM") %>%
  
  # Sort out the 0 steos and the NAs
  mutate(step = ifelse(step == 0, 1, step),
         step = ifelse(is.na(step), 1, step),
         angle = ifelse(is.na(angle), 0, angle),
         step = (ifelse(step > 5000, 5000, step)),
         KD490 = log(KD490))

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
                  angle = c(35, 1, 20)),
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
                  angle = c(35, 1, 20)),
      formula = ~KD490))

momentuHMM::AICweights(StateModel,
                       StateModel_KD490, k = 2)

AIC(StateModel)
AIC(StateModel_KD490)

StateModel$mle[1:2]
StateModel_KD490$mle[1:2]

momentuHMM::plotStationary(StateModel_KD490, plotCI = T)

# Recombine hmm state into the overall dataframe
manxie_ls_restricted$hmm_state <- momentuHMM::viterbi(StateModel)
manxie_ls_restricted$hmm_state_KD <- momentuHMM::viterbi(StateModel_KD490)
sum(manxie_ls_restricted$hmm_state == manxie_ls_restricted$hmm_state_KD) /
  nrow(manxie_ls_restricted)
