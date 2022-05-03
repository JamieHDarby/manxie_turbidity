
# Split the dive data into a list to append positions ---------------------

# Split dive dataframe into list by ID
dive_ls <- split(manx_dive_df, manx_dive_df$ID)

# Run this function to append locations and other variables to dives
dive_ls <- lapply(dive_ls, FUN = dive_attach, pos_df = manxie_ls)

# Get it back into a dataframe, create a combined ID dive variable
dive_pos_df <- bind_rows(dive_ls) %>%
  mutate(id_dive = paste(ID, dive, sep = "_"),
         id_bout = paste(ID, bout, sep = "_"))

# Create a meta df of dives
dive_pos_meta <- dive_pos_df %>%
  split(., .$id_bout) %>%

  lapply(., function(x){
    ind <- which(x$depth == min(x$depth, na.rm = T))[1]
    x <- x[which(x$id_dive == x$id_dive[ind]), ]
    x
  }) %>%

  do.call(rbind, .) %>%
  
  filter(depth < -1) %>%
  
  split(., .$id_dive) %>%
  
  lapply(., function(x){
    
    depth <- min(x$depth, na.rm = T)
    
    duration <- abs(as.numeric(difftime(x$date_time[1], x$date_time[nrow(x)], units = "secs")))
    
    shape <- abs(depth / duration)
    
    df <- data.frame(depth, duration, shape)
    
    vars <- c("ID", "date_time", "Longitude", "Latitude",
              "id_dive", "bout",
              "wave", "WindSpeed", "sunangle", "precip",
              "cloud_total", "cloud_high", "cloud_low",
              "KD490", "Slope", "Depth", "Coast",
              "Currents", "Stratification", "PointDist")
    
    df[, vars] <- x[(ceiling(nrow(x) / 2)), vars]
    
    df
  }) %>%
  
  do.call(rbind, .) %>%
  
  filter(depth < 0,
         duration > 0,
         shape < 2.5) %>%
  
  mutate(ID = as.factor(ID),
         depth = -depth,
         WindSpeed_trans = WindSpeed ^ (1/1.5),
         KD490_trans = log(KD490),
         Wave_trans = wave ^ (1/4),
         Precip_trans = precip ^ (1/5),
         cloud_trans = cloud_total ^ (2),
         PointDist_trans = PointDist ^ (1/2))

# Create AR.Start variable for mgcv::bam modelling
dive_pos_meta$arstart <- F
for(i in 2:nrow(dive_pos_meta)){
  if(dive_pos_meta$ID[i] != dive_pos_meta$ID[i - 1]){
    dive_pos_meta$arstart[i] <- T
  }
}

system.time(
  mod1 <- bam(formula = depth ~ 
               s(sunangle, bs = "ts", k = 5) +
                s(KD490, bs = "ts", k = 5) +
                s(SST, bs = "ts", k = 5) +
               s(ID, bs = "re"),
              gamma = 1.2,
             method = "fREML",
             # discrete = T,
             # AR.start = arstart,
             select = T,
             # rho = 0.1,
              data = dive_pos_meta))

summary(mod1)
plot(getViz(mod1))

gam.check(mod1)

acf(residuals(mod1))[1]

concurvity(mod1)

system.time(
  mod2 <- bam(formula = depth ~ 
                s(sunangle, bs = "ts", k = 5) +
                s(cloud_total, bs = "ts", k = 5) +
                s(ID, bs = "re"),
              gamma = 1.2,
              method = "fREML",
              discrete = T,
              AR.start = arstart,
              select = T,
              rho = 0.28,
              data = dive_pos_meta))

AIC(mod1)
AIC(mod2)

dive_pos_meta2 <- dive_pos_meta %>% filter(!is.na(KD490), !is.na(depth))

mod3 <- bam(formula = depth ~ 
              te(sunangle, cloud_total, bs = "ts", k = 5) +
              s(KD490_trans, bs = "ts", k = 5) +
              s(ID, bs = "re"),
            gamma = 1.2,
            na.action = "na.fail",
            method = "fREML",
            discrete = T,
            AR.start = arstart,
            select = T,
            rho = 0.28,
            data = dive_pos_meta2)

dredge(mod3)

summary(mod3)
plot(getViz(mod3))
