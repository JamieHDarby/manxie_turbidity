
# Set up the environment --------------------------------------------------

source("scripts/1_prep.R")
load(file = "data/cleaned/manxie_ls_int_env2.RData")
load(file = "data/cleaned/manx_dives_labelled.RData")

manx_dives$ID <- manx_dives$id

# Split the pos data into a list to append positions ----------------------

# Split dive dataframe into list by ID
pos_ls <- split(manxie_ls, manxie_ls$ID)

# Run this function to append locations and other variables to dives
pos_ls <- lapply(pos_ls, FUN = pos_attach, dive_df = manx_dives)

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
         dive_bool = ifelse(dives > 0, 1, 0),
         rise_fall = as.factor(ifelse(HOD < 13, "R", "F")),
         date = as.factor(as.Date(date_time))) %>%
  
  filter(!is.na(zsd))

# Create AR.Start variable for mgcv::bam modelling
mod_df$arstart <- F
for(i in 2:nrow(mod_df)){
  if(mod_df$section[i] != mod_df$section[i - 1]){
    mod_df$arstart[i] <- T
  }
}

# Run mgcv::bam model
system.time(
mod <- bam(data = mod_df,
           family = "nb",
           formula = dives ~
             te(sunangle, zsd, bs = "ts", k = 5) +
             # s(sunangle, bs = "ts", k = 5) +
             # s(zsd, bs = "ts", k = 5) +
             s(cloud_total, bs = "ts", k = 5) +
             s(MOD, bs = "cc", k = 4) +
             hmm_state_zsd +
             s(ID, bs = "re"),
           na.action = "na.omit",
           gamma = 1.2,
           method = "fREML",
           discrete = T,
           AR.start = arstart,
           select = T,
           rho = 0.5))

AIC(mod)

# Summarise this initial model
summary(mod)

# Test the autoregressive structure and retroapply this to above
acf(residuals(mod))[1]

plot(getViz(mod))

concurvity(mod)

# Run mgcv::bam model
system.time(
  mod_bin <- bam(data = mod_df,
             family = "binomial",
             formula = hmm_state ~
               te(sunangle, zsd, bs = "ts", k = 5) +
               # s(sunangle, bs = "ts", k = 5) +
               # s(zsd, bs = "ts", k = 5) +
               s(cloud_total, bs = "ts", k = 5) +
               s(MOD, bs = "cc", k = 4) +
               s(ID, bs = "re"),
             na.action = "na.omit",
             gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.69))

# Summarise this initial model
summary(mod_bin)

# Test the autoregressive structure and retroapply this to above
acf(residuals(mod_bin))[1]

plot(getViz(mod_bin))

concurvity(mod_bin)
