
# Split the dive data into a list to append positions ---------------------

# Split dive dataframe into list by ID
dive_ls <- split(manx_dive_df, manx_dive_df$ID)

# Run this function to append locations and other variables to dives
dive_ls <- lapply(dive_ls, FUN = dive_attach, pos_df = manxie_ls,
                  vars = c("Longitude", "Latitude", "wave",
                           "WindSpeed", "sunangle", "precip",
                           "cloud_total", "cloud_high", "cloud_low",
                           "KD490", "Slope", "Depth", "Coast",
                           "Currents", "Stratification", "PointDist",
                           "chla", "SST", "trip_id", "zsd", "HOD", "MOD"))

# Get it back into a dataframe, create a combined ID dive variable
dive_pos_df <- bind_rows(dive_ls) %>%
  mutate(id_dive = paste(ID, dive, sep = "_"),
         id_bout = paste(ID, bout, sep = "_"))

# Create a meta df of dives
dive_pos_meta <- dive_pos_df %>%
  # split(., .$id_bout) %>%
  # 
  # lapply(., function(x){
  #   ind <- which(x$depth == min(x$depth, na.rm = T))[1]
  #   x <- x[which(x$id_dive == x$id_dive[ind]), ]
  #   x
  # }) %>%
  # 
  #   do.call(rbind, .) %>%

  filter(depth < -1) %>%
  
  split(., .$id_dive) %>%
  
  lapply(., function(x){
    
    depth <- min(x$depth, na.rm = T)
    
    duration <- abs(as.numeric(difftime(x$date_time[1], x$date_time[nrow(x)], units = "secs")))
    
    shape <- abs(depth / duration)
    
    df <- data.frame(depth, duration, shape)
    
    vars <- c("ID", "trip_id", "date_time", "Longitude", "Latitude",
              "id_dive", "bout", "tag_type",
              "wave", "WindSpeed", "sunangle", "precip",
              "cloud_total", "cloud_high", "cloud_low",
              "KD490", "Slope", "Depth", "Coast",
              "Currents", "Stratification", "PointDist",
              "chla", "SST", "zsd", "HOD", "MOD")
    
    df[, vars] <- x[(ceiling(nrow(x) / 2)), vars]
    
    df
  }) %>%
  
  do.call(rbind, .) %>%
  
  filter(depth < 0,
         duration > 0,
         shape < 2.5) %>%
  
  mutate(ID = as.factor(ID),
         depth = -depth,
         WindSpeed = WindSpeed ^ (1/1.5),
         KD490 = log(KD490),
         wave = log(wave),
         precip = precip ^ (1/5),
         cloud = cloud_total * 100,
         PointDist_trans = PointDist ^ (1/2))

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
                s(MOD, bs = "ts", k = 5) +
                s(zsd, bs = "ts", k = 5) +
               s(ID, bs = "re"),
              gamma = 1.2,
             method = "fREML",
             discrete = T,
             AR.start = arstart,
             select = T,
             rho = 0.3,
              data = dive_pos_meta))

system.time(
  mod2 <- bam(formula = depth ~ 
                te(sunangle, zsd, cloud, bs = "ts", k = 3) +
                # s(KD490, bs = "ts", k = 5) +
                s(Depth, bs = "ts", k = 5) +
                s(MOD, bs = "ts", k = 5) +
                s(ID, bs = "re"),
              gamma = 1.2,
              method = "fREML",
              discrete = T,
              AR.start = arstart,
              select = T,
              rho = 0.3,
              data = dive_pos_meta))

AIC(mod1, mod2, k = 2)

ggplot(dive_pos_meta, aes(x = MOD)) + geom_density(position = "stack") +
  scale_x_continuous(limits = c(0, 24))

summary(mod1)
summary(mod2)
plot(getViz(mod1))
plot(getViz(mod2))

mgcViz::plot.mgcv.smooth.MD(sm(getViz(mod2), 1), fix = c("sunangle"=30))

library(rgl)

mgcViz::plotRGL(sm(getViz(mod2), 1), fix = c("sunangle"=30), residuals = T)
rgl::clear3d()

gam.check(mod1)

acf(residuals(mod1))[1]

concurvity(mod1)

system.time(
  mod1 <- gamm(formula = depth ~ 
                s(sunangle, cloud, bs = "ts", k = 5) +
                s(KD490, bs = "ts", k = 5) +
                 s(ID, bs = "re"),
                random = list(ID = ~1),
              correlation = corAR1(value = 0.3, form = ~ date_time | bout),
              select = T,
              data = dive_pos_meta))

summary(mod1$gam)
summary(mod1$lme)
plot(getViz(mod1$gam))



dive_pos_meta2 <- dive_pos_meta %>%
  mutate(date_time = as.factor(as.numeric(date_time)))
  
system.time(
  mod1 <- glmmTMB(formula = depth ~ 
                 sunangle:cloud +
                 KD490 + ar1(date_time + 0 | bout)
               + (1|ID),
               family = "gaussian",
               data = dive_pos_meta2))

summary(mod1)
mod1$modelInfo

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

dive_pos_meta2 <- dive_pos_meta %>% filter(!is.na(KD490), !is.na(depth)) %>%
  mutate(tag_type = as.factor(tag_type))

mod3 <- bam(formula = depth ~
              tag_type +
              s(sunangle, bs = "ts", k = 5),
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

system.time(
  mod_tag <- glm(formula = depth ~ sunangle + tag_type,
                 na.action = "na.omit",
                 data = dive_pos_meta))

summary(mod_tag)
plot(mod_tag)
dredge(mod_tag)

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
    
    vars <- c("ID", "trip_id", "date_time", "Longitude", "Latitude",
              "id_dive", "bout", "tag_type",
              "wave", "WindSpeed", "sunangle", "precip",
              "cloud_total", "cloud_high", "cloud_low",
              "KD490", "Slope", "Depth", "Coast",
              "Currents", "Stratification", "PointDist",
              "chla", "SST", "zsd", "HOD", "MOD")
    
    df[, vars] <- x[(ceiling(nrow(x) / 2)), vars]
    
    df
  }) %>%
  
  do.call(rbind, .) %>%
  
  filter(depth < 0,
         duration > 0,
         shape < 2.5) %>%
  
  mutate(ID = as.factor(ID),
         depth = -depth,
         WindSpeed = WindSpeed ^ (1/1.5),
         KD490 = log(KD490),
         wave = log(wave),
         precip = precip ^ (1/5),
         cloud = cloud_total * 100,
         PointDist_trans = PointDist ^ (1/2))

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
                s(zsd, bs = "ts", k = 5) +
                s(Depth, bs = "ts", k = 5) +
                s(ID, bs = "re"),
              gamma = 1.2,
              method = "fREML",
              discrete = T,
              AR.start = arstart,
              select = T,
              # rho = 0.3,
              data = dive_pos_meta))

system.time(
  mod2 <- bam(formula = depth ~ 
                te(sunangle, cloud, zsd, bs = "ts", k = 5) +
                # s(zsd, bs = "ts", k = 5) +
                s(Depth, bs = "ts", k = 5) +
                s(ID, bs = "re"),
              gamma = 1.2,
              method = "fREML",
              discrete = T,
              AR.start = arstart,
              select = T,
              rho = 0.3,
              data = dive_pos_meta))

AIC(mod1, mod2, k = 2)

summary(mod1)
summary(mod2)
plot(getViz(mod1))
plot(getViz(mod2))

system.time(
  mod_tag <- lm(formula = depth ~ tag_type,
                na.action = "na.omit",
              data = dive_pos_meta))

summary(mod_tag)
plot(mod_tag)
dredge(mod_tag)

dive_pos_meta2 <- dive_pos_meta %>%
  mutate(tag_weight = ifelse(tag_type == "g5", ifelse(tag_type == "pt", 3.5, 10.5), 3.5))

system.time(
  mod_tag <- glm(formula = depth ~ tag_weight,
                 na.action = "na.omit",
                 data = dive_pos_meta2))

