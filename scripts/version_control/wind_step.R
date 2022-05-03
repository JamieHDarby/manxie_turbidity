
mod_df <- manxie_df %>% 
  filter(embc_state == 3 &
           !is.na(speed) &
           speed > 5) %>%
  mutate(WindOffsetAbs = abs(WindOffset)) %>%
  mutate(response = ifelse(embc_state == 4, 1, 0))

mod_df$ARStart <- rep(F, nrow(mod_df))
for(i in 2:nrow(mod_df)){
  if(mod_df$trip_id[i] != mod_df$trip_id[i - 1]){
    mod_df$ARStart[i] <- T}}

ggplot() + geom_point(data = mod_df,
                      aes(x = WindOffsetAbs,
                          y = speed,
                          size = WindSpeed,
                          colour = ID),
                      alpha = 0.4)

mod1 <- gam(speed ~ te(WindOffset, WindSpeed, bs = "ts"),
             correlation = corCAR1(0.95, form = ~ date_time|trip_id),
             family = gaussian(link = "log"),
             method = "REML",
             niterPQL = 20,
            data = mod_df)

summary(mod1)
plot(mod1)
require(mgcViz); plot(mgcViz::getViz(mod1))

mod1 <- bam(data = mod_df,
            formula = response ~ 
              te(WindOffsetAbs, WindSpeed, bs = "ts") +
              s(CloudTotal, bs = "ts") +
              s(ChlA, bs = "ts") +
              s(Wave, bs = "ts") +
              s(Charnock, bs = "ts") +
              s(SST, bs = "ts") +
              s(sunangle, bs = "ts") +
              s(Vapour, bs = "ts") +
              te(MoonFrac, MoonPos, bs = "ts") +
              s(x, y, bs = "tp"),
            discrete = T,
            gamma = 1.4,
            method = 'fREML',
            AR.start = ARStart,
            family = binomial(link = "logit"),
            rho = 0.7)

summary(mod1)
plot(mod1)
