
# Set up the environment --------------------------------------------------

source("scripts/1_prep.R")
load(file = "data/cleaned/manxie_ls_int_env2.RData")
load(file = "data/cleaned/manxie_dive_df.RData")

# Split the pos data into a list to append positions ----------------------

# Split dive dataframe into list by ID
pos_ls <- split(manxie_ls, manxie_ls$ID)

# Run this function to append locations and other variables to dives
pos_ls <- lapply(pos_ls, FUN = pos_attach, dive_df = manx_dive_df)

# Bring to dataframe format
pos_dive_df <- do.call(rbind, pos_ls) %>%
  # Get rid of trips with no dive data
  mutate(trip_id = as.character(trip_id)) %>%
  split(., .$trip_id) %>%
  lapply(., function(x){
    if(anyNA(x$dives)){x <- NULL}else{
      if(sum(x$dives) < 1){x <- NULL}
    }
    x
  }) %>%
  do.call(rbind, .)

# Take a look at distributions of count data
pos_dive_df %>%
  filter(dives > 0) %>%
  ggplot() + 
  geom_density(aes(x = dives, fill = ID), position = "stack")

# Build dataframe to run model on
mod_df <- pos_dive_df %>%
  
  mutate(ID = as.factor(ID),
         trip_id = as.factor(trip_id),
         KD490 = log(KD490),
         chla = log(chla),
         dive_bool = ifelse(dives > 0, 1, 0)) %>%
  
  filter(!is.na(KD490))

# Create AR.Start variable for mgcv::bam modelling
mod_df$arstart <- F
for(i in 2:nrow(mod_df)){
  if(mod_df$section[i] != mod_df$section[i - 1]){
    mod_df$arstart[i] <- T
  }
}

system.time(
  kd_mod <- bam(data = mod_df,
             family = "gaussian",
             formula = zsd ~
               s(sunangle, bs = "ts", k = 5) +
               s(WindSpeed, bs = "ts", k = 5) +
               s(cloud_total, bs = "ts", k = 5) +
               s(precip, bs = "ts", k = 5) +
               te(Depth, Currents, bs = "ts", k = 5) +
               s(SST, bs = "ts", k = 5) +
               s(Coast, bs = "ts", k = 5) +
               s(wave, bs = "ts", k = 5),   
             gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.985))

summary(kd_mod)
# plot(getViz(kd_mod))

acf(residuals(kd_mod))[1]

manxie_ls$turbidity_mod <- predict.bam(kd_mod, manxie_ls)
pos_dive_df$turbidity_mod <- predict.bam(kd_mod, pos_dive_df)

# Run mgcv::bam model
system.time(
mod <- bam(data = mod_df,
           family = "nb",
           formula = dives ~
             # s(sunangle, bs = "ts", k = 5) +
             s(cloud_total, bs = "ts", k = 5) +
             s(KD490, bs = "ts", k = 5) +
             s(MOD, bs = "cc", k = 5) +
             s(ID, bs = "re"),
           na.action = "na.omit",
           gamma = 1.2,
           method = "fREML",
           discrete = T,
           AR.start = arstart,
           select = T,
           rho = 0.5))

# Summarise this initial model
summary(mod)

# Test the autoregressive structure and retroapply this to above
acf(residuals(mod))[1]

plot(getViz(mod))

ggplot(mod_df, aes(x = (KD490), y = turbidity_mod)) +
  geom_point() + geom_smooth(method = "lm", show.legend = T)

summary(lm(formula = mod_df$turbidity_mod ~ mod_df$KD490))


# Run mgcv::bam model
system.time(
  mod <- bam(data = mod_df,
             family = "nb",
             formula = dives ~
               te(sunangle,cloud_total,zsd, bs = "ts", k = 5) +
               s(ID, bs = "re"),
             na.action = "na.omit",
             gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.5))

summary(mod)

mgcViz::plotRGL(sm(getViz(mod), 1), fix = c("sunangle"=50), residuals = F)
rgl::clear3d()

# Run mgcv::bam model
system.time(
  mod_bin <- bam(data = mod_df,
             family = "binomial",
             formula = dive_bool ~
               s(sunangle, bs = "ts", k = 5) +
               s(cloud_total, bs = "ts", k = 5) +
               s(zsd, bs = "ts", k = 5) +
               s(ID, bs = "re"),
             na.action = "na.omit",
             gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.5))

# Summarise this initial model
summary(mod_bin)

# Test the autoregressive structure and retroapply this to above
acf(residuals(mod_bin))[1]

plot(getViz(mod_bin))
