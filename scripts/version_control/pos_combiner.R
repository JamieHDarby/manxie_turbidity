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
         dive_bool = ifelse(dives > 0, 1, 0)) %>%
  
  filter(!is.na(KD490))

# Create AR.Start variable for mgcv::bam modelling
mod_df$arstart <- F
for(i in 2:nrow(mod_df)){
  if(mod_df$trip_id[i] != mod_df$trip_id[i - 1]){
    mod_df$arstart[i] <- T
  }
}

# Run mgcv::bam model
system.time(
mod <- bam(data = mod_df,
           family = "nb",
           formula = dives ~
             te(sunangle, bs = "ts", k = 5) +
             te(sunangle, cloud_total, bs = "ts", k = 5) +
             s(KD490, bs = "ts", k = 5) +
             s(ID, bs = "re"),              
           gamma = 1.4,
           na.action = "na.fail",
           method = "fREML",
           discrete = T,
           AR.start = arstart,
           select = T,
           rho = 0.47))

summary(mod)
plot(getViz(mod))

acf(residuals(mod))[1]

system.time(
  mod <- bam(data = mod_df,
             family = "gaussian",
             formula = time_under ~
               s(sunangle, bs = "ts", k = 5) +
               s(KD490, bs = "ts", k = 5) +
               s(cloud_total, bs = "ts", k = 5) +
               s(WindSpeed, bs = "ts") +
               s(trip_id, bs = "re"),             
             gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.37))

summary(mod)
plot(getViz(mod))

acf(residuals(mod))[1]
