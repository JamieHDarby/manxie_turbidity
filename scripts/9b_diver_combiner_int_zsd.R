
# Split the dive data into a list to append positions ---------------------
load(file = "data/cleaned/manxie_ls_int_env2_res.RData")
manx_dives$bout <- manx_dive_df$bout

# Split dive dataframe into list by ID
dive_ls <- split(manx_dives, manx_dives$ID)

# Run this function to append locations and other variables to dives
dive_ls <- lapply(dive_ls, FUN = dive_attach, pos_df = manxie_ls_restricted,
                  vars = c("Longitude", "Latitude", "wave",
                           "WindSpeed", "sunangle", "precip",
                           "cloud_total", "cloud_high", "cloud_low",
                           "KD490", "Slope", "Depth", "Coast",
                           "Currents", "Stratification", "PointDist",
                           "chla", "SST", "trip_id", "zsd", "HOD", "MOD",
                           "hmm_state", "hmm_state_zsd"))

# Get it back into a dataframe, create a combined ID dive variable
dive_pos_df <- bind_rows(dive_ls) %>%
  mutate(id_dive = paste(ID, dive, sep = "_"),
         id_bout = paste(ID, bout, sep = "_"))

# Create a meta df of dives
dive_pos_meta <- dive_pos_df %>%

  filter(depth > 10) %>%
  
  split(., .$id_dive) %>%
  
  lapply(., function(x){
    
    depth <- max(x$depth, na.rm = T)
    
    duration <- abs(as.numeric(difftime(x$date_time[1], x$date_time[nrow(x)], units = "secs")))
    
    shape <- abs(depth / duration)
    
    df <- data.frame(depth, duration, shape)
    
    vars <- c("ID", "trip_id", "date_time", "Longitude", "Latitude",
              "id_dive", "bout", "tag_type",
              "wave", "WindSpeed", "sunangle", "precip",
              "cloud_total", "cloud_high", "cloud_low",
              "KD490", "Slope", "Depth", "Coast",
              "Currents", "Stratification", "PointDist",
              "chla", "SST", "zsd", "HOD", "MOD", "hmm_state_zsd",
              "ascent_speed", "descent_speed", "bottom_time")
    
    df[, vars] <- x[(ceiling(nrow(x) / 2)), vars]
    
    df$shape_2 <- (df$bottom_time / df$duration)
    
    df
  }) %>%
  
  do.call(rbind, .) %>%
  
  filter(depth > 0,
         duration > 0,
         shape < 2.5) %>%
  
  mutate(ID = as.factor(ID),
         WindSpeed = WindSpeed ^ (1/1.5),
         KD490 = log(KD490),
         wave = log(wave),
         precip = precip ^ (1/5),
         cloud = cloud_total * 100,
         PointDist_trans = PointDist ^ (1/2),
         depth = depth ^ (1/3),
         hmm = as.factor(hmm_state_zsd))

# Create AR.Start variable for mgcv::bam modelling
dive_pos_meta$arstart <- F
for(i in 2:nrow(dive_pos_meta)){
  if(dive_pos_meta$bout[i] != dive_pos_meta$bout[i - 1]){
    dive_pos_meta$arstart[i] <- T
  }
}

system.time(
  mod1 <- bam(formula = depth ~ 
                s(sunangle, bs = "ts", k = 5) +
                s(cloud, bs = "ts", k = 5) +
                s(Depth, bs = "ts", k = 5) +
                # s(depth, bs = "ts", k = 5) +
                s(MOD, bs = "cc", k = 4) +
                s(zsd, bs = "ts", k = 5) +
               s(ID, bs = "re") + hmm,
              gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.18,
              data = dive_pos_meta))

system.time(
  mod2 <- bam(formula = depth ~ 
                te(sunangle, zsd, bs = "ts", k = 4) +
                s(cloud, bs = "ts", k = 5) +
                s(Depth, bs = "ts", k = 5) +
                # s(depth, bs = "ts", k = 5) +
                s(MOD, bs = "cc", k = 4) +
                s(ID, bs = "re") + hmm,
              gamma = 1.2,
              method = "fREML",
              discrete = T,
              AR.start = arstart,
              select = T,
              rho = 0.18,
              data = dive_pos_meta))

AIC(mod1, mod2, k = 2)

acf(residuals(mod2))[1]

summary(mod1)
summary(mod2)
plot(getViz(mod1))
plot(getViz(mod2))

concurvity(mod2)

qq(getViz(mod2))

# Create a meta df of dives
dive_pos_meta <- dive_pos_df %>%
  split(., .$id_bout) %>%

  lapply(., function(x){
    ind <- which(x$depth == max(x$depth, na.rm = T))[1]
    x <- x[which(x$id_dive == x$id_dive[ind]), ]
    x
  }) %>%

    do.call(rbind, .) %>%
  
  filter(depth > 1) %>%
  
  split(., .$id_dive) %>%
  
  lapply(., function(x){
    
    depth <- max(x$depth, na.rm = T)
    
    duration <- abs(as.numeric(difftime(x$date_time[1], x$date_time[nrow(x)], units = "secs")))
    
    shape <- abs(depth / duration)
    
    df <- data.frame(depth, duration, shape)
    
    vars <- c("ID", "trip_id", "date_time", "Longitude", "Latitude",
              "id_dive", "bout", "tag_type",
              "wave", "WindSpeed", "sunangle", "precip",
              "cloud_total", "cloud_high", "cloud_low",
              "KD490", "Slope", "Depth", "Coast",
              "Currents", "Stratification", "PointDist",
              "chla", "SST", "zsd", "HOD", "MOD", "hmm_state_zsd",
              "ascent_speed", "descent_speed", "bottom_time")
    
    df[, vars] <- x[(ceiling(nrow(x) / 2)), vars]
    
    df$shape_2 <- (df$bottom_time / df$duration)
    
    df
  }) %>%
  
  do.call(rbind, .) %>%
  
  filter(depth > 0,
         duration > 0,
         shape < 2.5) %>%
  
  mutate(ID = as.factor(ID),
         trip_id = as.factor(trip_id),
         # depth = -depth,
         WindSpeed = WindSpeed ^ (1/1.5),
         KD490 = log(KD490),
         wave = log(wave),
         depth = (depth),
         precip = precip ^ (1/5),
         cloud = cloud_total * 100,
         PointDist_trans = PointDist ^ (1/2),
         hmm = as.factor(hmm_state_zsd))

system.time(
  mod1 <- bam(formula = depth ~ 
                s(sunangle, bs = "ts", k = 5) +  
                s(cloud, bs = "ts", k = 5) +
                s(zsd, bs = "ts", k = 5) +
                # s(Depth, bs = "ts", k = 5) +
                s(MOD, bs = "cc", k = 4) +
                s(ID, bs = "re") + hmm,
              gamma = 1.2,
              method = "REML",
              family = gaussian(),
              data = dive_pos_meta))

system.time(
  mod2 <- bam(formula = depth ~ 
                te(sunangle, zsd, bs = "ts", k = 5) +
                s(cloud, bs = "ts", k = 5) +
                s(MOD, bs = "cc", k = 4) +
                # s(Depth, bs = "ts", k = 5) +
                s(ID, bs = "re") + hmm,
              gamma = 1.2,
              family = gaussian(),
              method = "REML",
              data = dive_pos_meta))

(acf(residuals(mod1))[1])

AIC(mod1, mod2, k = 2)

summary(mod1)
summary(mod2)
plot(getViz(mod1))
plot(getViz(mod2))

qq(getViz(mod2), rep = 50,
   showReps = T, CI = "none",
   a.qqpoi = list("shape" = 19),
   a.replin = list("alpha" = 0.2))

system.time(
  mod_tag <- glm(formula = depth ~ tag_type,
                na.action = "na.fail",
              data = dive_pos_meta))

summary(mod_tag)
plot(mod_tag)
dredge(mod_tag)

dive_pos_meta2 <- dive_pos_meta %>%
  mutate(tag_weight = ifelse(tag_type != "g5",
                             ifelse(tag_type == "pt", 3.5, 10.5), 13))

system.time(
  mod_tag <- glm(formula = depth ~ tag_weight,
                 na.action = "na.fail",
                 data = dive_pos_meta2))

summary(mod_tag)
plot(mod_tag)
dredge(mod_tag)

install.packages("DHARMa")

require(DHARMa)

sim.output <- simulateResiduals(mod2, n = 200)
plot(sim.output)
testDispersion(sim.output)

