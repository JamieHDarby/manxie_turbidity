# Read in track csv
mnx_all <-
  read.csv(
    file =
      "data/tracks/gb_2014-15.csv")

# Colony coords
col_lon = -10.5213
col_lat = 52.1036

# Read in environmental data
mnx_env <- read.csv(
  file =
    "data/env/gb_movebank.csv")

# POSIXct for tracks
mnx_all <- timecombo(mnx_all)

# Sort the correct IDs from the env df
mnx_env <- mnx_env[which(mnx_env$ID %in% mnx_all$ID),]

# POSIXct of th time variable
mnx_env <- timecombo(mnx_env)

# Sort tracks by ID and time
sort.df.1 <- with(mnx_all, mnx_all[order(ID, date_time),])

# Sort env dfby ID and time
sort.df.2 <- with(mnx_env, mnx_env[order(ID, date_time),])

# Extract the correct variables
mnx_all <- cbind(sort.df.1,
                 sort.df.2[c("CloudLow",
                             "CloudMedium",
                             "CloudHigh",
                             "CloudTotal",
                             "WindU",
                             "WindV", 
                             "ChlA",
                             "SST",
                             "Vapour",
                             "Wave",
                             "Charnock")])

# Remove these
rm(sort.df.1, sort.df.2, mnx_env)

# Strip out track frames for sorting trips from non-trip points
track_frames <- mnx_all %>%
  select(Longitude, Latitude, ID, date_time) %>%
  rename(TrackTime = date_time)

# List by ID
track_frames <- split(track_frames,
                      track_frames$ID,
                      drop = TRUE)

# Function to assign trip names
tripID <- mclapply(track_frames,
                   Birdtrip,
                   x = col_lon,
                   y = col_lat,
                   InBuff = 3,
                   RetBuff = 2,
                   Dur = 1,
                   mc.cores = numCores)

# Distance between each point
mnx_all$dist <-
  do.call(rbind, track_frames)$dist

# Distance to colony
mnx_all$PointDist <-
  PointDist(mnx_all,
            x = col_lon,
            y = col_lat)

# Blow these away
rm(track_frames, col_lon, col_lat)

# Append trip_id back onto overall df
mnx_all$trip_id <- as.factor((Reduce(
  rbind, tripID))$trip_id)

# Returns variable to tell you whether the track comes back
mnx_all$returns <- as.factor((Reduce(
  rbind, tripID))$Returns)  

# This df lacks colony positions
manxie_gb <- 
  mnx_all[
    which(mnx_all$trip_id != "-1"),]

# This can go
rm(mnx_all, tripID)
