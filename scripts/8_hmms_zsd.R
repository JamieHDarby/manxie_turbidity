
# HMM analysis ------------------------------------------------------------


save(manxie_ls_restricted, file = "data/cleaned/manxie_ls_int_env2_res.RData")

hmm_plot_df <- momentuHMM::plotStationary(StateModel_zsd, plotCI = T, return = T)$zsd

state_probs <- ggplot() +
  geom_ribbon(data = hmm_plot_df$Transit,
              aes(x = cov, ymin = lci, ymax = uci, fill = "Transit"), alpha = 0.3) +
  geom_line(data = hmm_plot_df$Transit,
            aes(x = cov, y = est, colour = "Transit"), size = 1.2) +
  geom_ribbon(data = hmm_plot_df$ARS,
              aes(x = cov, ymin = lci, ymax = uci, fill = "ARS"), alpha = 0.3) +
  geom_line(data = hmm_plot_df$ARS,
            aes(x = cov, y = est, colour = "ARS"), size = 1.2) +
  geom_ribbon(data = hmm_plot_df$Rest,
              aes(x = cov, ymin = lci, ymax = uci, fill = "Rest"), alpha = 0.3) +
  geom_line(data = hmm_plot_df$Rest,
            aes(x = cov, y = est, colour = "Rest"), size = 1.2) +
  labs(x = "Secchi disk depth (m)",
       y = "Stationary state probabilities",
       fill = "State") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_y_continuous(limits = c(0, 1)) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2", guide = "none")

ggsave(state_probs, filename = "plots/state_probs.png", dpi = 500, width = 5, height = 5)

momentuHMM::plotStationary(StateModel_zsd_dives, plotCI = T)

manxie_ls_restricted %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>%
  filter(dive_bool > 0) %>%
ggplot() +
  geom_violin(aes(y = max_depth, x = as.factor(hmm_state), fill = as.factor(hmm_state)))

manxie_ls_restricted %>% filter(dives > 0) %>% select(hmm_state) %>% table()
manxie_ls_restricted %>% select(hmm_state) %>% table()

manxie_ls_restricted %>% filter(dives > 0) %>% select(hmm_state_zsd) %>% table()
manxie_ls_restricted %>% select(hmm_state_zsd) %>% table()

manxie_ls_restricted %>% filter(hmm_state != 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()
manxie_ls_restricted %>% filter(hmm_state == 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()
manxie_ls_restricted %>% filter(hmm_state_zsd != 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()
manxie_ls_restricted %>% filter(hmm_state_zsd == 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()

sum(manxie_ls_restricted$hmm_state == manxie_ls_restricted$hmm_state_zsd) /
  nrow(manxie_ls_restricted)

manxie_ls_restricted %>% filter(hmm_state_zsd == 1) %>% select(dives) %>% sum()
manxie_ls_restricted %>% filter(hmm_state_zsd == 2) %>% select(dives) %>% sum()
manxie_ls_restricted %>% filter(hmm_state_zsd == 3) %>% select(dives) %>% sum()

sum(manxie_ls_restricted$dives)

ggplot(manxie_ls_restricted) +
  coord_cartesian(xlim = c(200000, 225000),
                  ylim = c(5750000, 5800000)) +
  geom_path(aes(x = x, y = y, group = trip_id)) +
  geom_point(aes(x = x, y = y, colour = as.factor(hmm_state_zsd)), size = 2) +
  geom_point(aes(x = x, y = y, size = dives), colour = "red") +
  scale_size_continuous(range = c(0.5, 3), limits = c(0.1, 20))

# install.packages("qdapRegex")
require(splines)
# Run HMM on the prepared data, with KD490 as a covariate
system.time(
  StateModel_zsd_sun <-
    momentuHMM::fitHMM(
      data = HMM_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(80, 500, 2700,
                           40, 600, 1000),
                  angle = c(20, 1, 35)),
      formula = ~zsd + bs(sunangle, degree = 3)))

momentuHMM::plotStationary(StateModel_zsd_sun, plotCI = T)

system.time(
  StateModel_sun <-
    momentuHMM::fitHMM(
      data = HMM_data,
      nbStates = 3,
      dist = list(step = "gamma",
                  angle = "vm"),
      Par0 = list(step = c(80, 500, 2700,
                           40, 600, 1000),
                  angle = c(20, 1, 35)),
      formula = ~bs(sunangle, degree = 3)))

momentuHMM::AICweights(StateModel,
                       StateModel_zsd,
                       # StateModel_sun,
                       StateModel_zsd_sun)

anova_dives <- manxie_ls_restricted %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0),
         hmm_state = as.factor(hmm_state)) %>%
  filter(dive_bool > 0) %>%
  aov(data = ., formula = time_under ~ hmm_state)

summary(anova_dives)  
plot(anova_dives)
plot(TukeyHSD(anova_dives))

manxie_ls_restricted %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0),
         hmm_state = as.factor(hmm_state)) %>%
  filter(dive_bool > 0) %>%
  ggplot() +
  
  geom_boxplot(aes(x = hmm_state, y = time_under, fill = hmm_state))


manxie_ls_restricted$hmm_state_zsd_sun <- momentuHMM::viterbi(StateModel_zsd_sun)

manxie_ls_restricted %>% filter(hmm_state_zsd_sun != 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()
manxie_ls_restricted %>% filter(hmm_state_zsd_sun == 2) %>%
  mutate(dive_bool = ifelse(dives > 0, 1, 0)) %>% select(dive_bool) %>% table()

StateModel_zsd_sun$mle
