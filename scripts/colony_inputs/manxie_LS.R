
# Read in and combine manx tracks -----------------------------------------

# Read in the raw CatLog data
M18_210623_trimmed <- read.csv("data/Manx_2021/GPS/M18_210623_trimmed.csv") %>%
  mutate(ID = "EF44340")

M2_210623_trimmed <- read.csv("data/Manx_2021/GPS/M2_210623_trimmed.csv") %>%
  mutate(ID = "EG44116")

M3_210622_trimmed <- read.csv("data/Manx_2021/GPS/M3_210622_trimmed.csv") %>%
  mutate(ID = "EG44123")

M17113_210627_trimmed <- read.csv("data/Manx_2021/GPS/17113_210627_trimmed.csv") %>%
  mutate(ID = "EG44341")

M17134_210628_trimmed <- read.csv("data/Manx_2021/GPS/17134_210628_trimmed.csv") %>%
  mutate(ID = "EG44137")

M17126_210628_trimmed <- read.csv("data/Manx_2021/GPS/17126_210629_trimmed.csv") %>%
  mutate(ID = "EY53459")

M11_210629_trimmed <- read.csv("data/Manx_2021/GPS/M11_210629_trimmed.csv") %>%
  mutate(ID = "EY53457")

M2_210630_trimmed <- read.csv("data/Manx_2021/GPS/M2_210630_trimmed.csv") %>%
  mutate(ID = "EF44334")

M20_210630_trimmed <- read.csv("data/Manx_2021/GPS/M20_210630_trimmed.csv") %>%
  mutate(ID = "EG44124")

M15_210630_trimmed <- read.csv("data/Manx_2021/GPS/M15_210630_trimmed.csv") %>%
  mutate(ID = "EF44338")

M17110_210630_trimmed <- read.csv("data/Manx_2021/GPS/17110_210630_trimmed.csv") %>%
  mutate(ID = "EF44350")

M17137_210630_trimmed <- read.csv("data/Manx_2021/GPS/17137_210630_trimmed.csv") %>%
  mutate(ID = "EG44102")

M17126_210701_trimmed <- read.csv("data/Manx_2021/GPS/17126_210701_trimmed.csv") %>%
  mutate(ID = "EG44139")

M17119_210703_trimmed <- read.csv("data/Manx_2021/GPS/17119_210703_trimmed.csv") %>%
  mutate(ID = "EX02516")

M11_210703_trimmed <- read.csv("data/Manx_2021/GPS/M11_210703_trimmed.csv") %>%
  mutate(ID = "EG44120")

M14_210702_trimmed <- read.csv("data/Manx_2021/GPS/M14_210702_trimmed.csv") %>%
  mutate(ID = "EF44334")

M3_210702_trimmed <- read.csv("data/Manx_2021/GPS/M3_210702_trimmed.csv") %>%
  mutate(ID = "EG44141")

M8_210701_trimmed <- read.csv("data/Manx_2021/GPS/M8_210701_trimmed.csv") %>%
  mutate(ID = "EG44120")

M5_210701_trimmed <- read.csv("data/Manx_2021/GPS/M5_210701_trimmed.csv") %>%
  mutate(ID = "EG44140")

M17110_210704_trimmed <- read.csv("data/Manx_2021/GPS/17110_210704_trimmed.csv") %>%
  mutate(ID = "EG44102")

M17112_210717_trimmed <- read.csv("data/Manx_2021/GPS/17112_210717_trimmed.csv") %>%
  mutate(ID = "EG44131")

M20_210722_trimmed <- read.csv("data/Manx_2021/GPS/M20_210722_trimmed.csv") %>%
  mutate(ID = "EX02533")

M17119_210723_trimmed <- read.csv("data/Manx_2021/GPS/17119_210723_trimmed.csv") %>%
  mutate(ID = "EX02532")

M14_210725_trimmed <- read.csv("data/Manx_2021/GPS/M14_210725_trimmed.csv") %>%
  mutate(ID = "EX02547")

M15_210726_trimmed <- read.csv("data/Manx_2021/GPS/M15_210726_trimmed.csv") %>%
  mutate(ID = "EX02540")

M3_210731_trimmed <- read.csv("data/Manx_2021/GPS/M3_210731_trimmed.csv") %>%
  mutate(ID = "EG44137")

M17126_210726_trimmed <- read.csv("data/Manx_2021/GPS/17126_210731_trimmed.csv") %>%
  mutate(ID = "EX02529")

M17137_210731_trimmed <- read.csv("data/Manx_2021/GPS/17137_210731_trimmed.csv") %>%
  mutate(ID = "EX02528")

M17138_210802_trimmed <- read.csv("data/Manx_2021/GPS/17138_210802_trimmed.csv") %>%
  mutate(ID = "EX02533")

M15_210804_trimmed <- read.csv("data/Manx_2021/GPS/M15_210804_trimmed.csv") %>%
  mutate(ID = "EA15031")

M17135_210805_trimmed <- read.csv("data/Manx_2021/GPS/17135_210805_trimmed.csv") %>%
  mutate(ID = "EX02522")

M5_210805_trimmed <- read.csv("data/Manx_2021/GPS/M5_210805_trimmed.csv") %>%
  mutate(ID = "EX02546")

# Combine the CatLog data
manx_df <- rbind(M18_210623_trimmed, M2_210623_trimmed, M3_210622_trimmed,
                 M17113_210627_trimmed, M17126_210628_trimmed, M17134_210628_trimmed,
                 M11_210629_trimmed, M2_210630_trimmed, M15_210630_trimmed,
                 M20_210630_trimmed, M17137_210630_trimmed, M17110_210630_trimmed,
                 M17126_210701_trimmed, M17119_210703_trimmed, M11_210703_trimmed,
                 M14_210702_trimmed, M3_210702_trimmed, M8_210701_trimmed,
                 M5_210701_trimmed, M17110_210704_trimmed, M17112_210717_trimmed,
                 M20_210722_trimmed, M17119_210723_trimmed, M14_210725_trimmed,
                 M15_210726_trimmed, M3_210731_trimmed, M17126_210726_trimmed,
                 M17137_210731_trimmed, M17138_210802_trimmed, M15_210804_trimmed,
                 M17135_210805_trimmed, M5_210805_trimmed) %>%
  # Filter out missing locations
  filter(Latitude > 0) %>%
  # Convert data to UTM30
  WGS2UTM30(.)

# Read in all the PathTrack data
tag_31276 <- read.csv("data/Manx_2021/GPS/Obs190721_022236_Tag31276_cleaned.txt") %>%
  mutate(ID = "EG44130")

tag_31285 <- read.csv("data/Manx_2021/GPS/Obs190721_170556_Tag31285_cleaned.txt") %>%
  mutate(ID = "EG44137")

tag_31334 <- read.csv("data/Manx_2021/GPS/Obs220721_031602_Tag31334_cleaned.txt") %>%
  mutate(ID = "EF44339")

tag_31301 <- read.csv("data/Manx_2021/GPS/Obs230721_234348_Tag31301_cleaned.txt") %>%
  mutate(ID = "EY53457")

tag_31346 <- read.csv("data/Manx_2021/GPS/Obs260721_004021_Tag31346_cleaned.txt") %>%
  mutate(ID = "EX02541")

tag_31389 <- read.csv("data/Manx_2021/GPS/Obs270721_232254_Tag31389_cleaned.txt") %>%
  mutate(ID = "EN40369")

tag_31436 <- read.csv("data/Manx_2021/GPS/Obs280721_025409_Tag31436_cleaned.txt") %>%
  mutate(ID = "ET03928")

tag_31388 <- read.csv("data/Manx_2021/GPS/Obs290721_001045_Tag31388_cleaned.txt") %>%
  mutate(ID = "EF44339")

tag_31325 <- read.csv("data/Manx_2021/GPS/Obs290721_001359_Tag31325_cleaned.txt") %>%
  mutate(ID = "EX02548")

tag_31308 <- read.csv("data/Manx_2021/GPS/Obs310721_010055_Tag31308_cleaned.txt") %>%
  mutate(ID = "EX02530")

tag_31324 <- read.csv("data/Manx_2021/GPS/Obs020821_011111_Tag31324_cleaned.txt") %>%
  mutate(ID = "EX02542")

tag_31388b <- read.csv("data/Manx_2021/GPS/Obs030821_001434_Tag31388_cleaned.txt") %>%
  mutate(ID = "EX02532")

tag_31436b <- read.csv("data/Manx_2021/GPS/Obs040821_023650_Tag31436_cleaned.txt") %>%
  mutate(ID = "EA15032")

tag_31308b <- read.csv("data/Manx_2021/GPS/Obs070821_234715_Tag31308_cleaned.txt") %>%
  mutate(ID = "EX02529")

# Combine the PathTrack data
manx_df_pt <- rbind(tag_31276, tag_31285, tag_31301, tag_31308,
                    tag_31325, tag_31334, tag_31346, tag_31388,
                    tag_31389, tag_31436, tag_31324, tag_31388b,
                    tag_31436b, tag_31308b) %>%
  # Filter out the missing locations
  filter(Latitude > 0) %>%
  # Convert to UTM
  WGS2UTM30(.)

# POSIXct for tracks
manx_df$date_time <- lubridate::mdy_hms(paste(manx_df$Date, manx_df$Time, sep = ""))

# Jiggery pokery to make a POSIXct date_time from the PathTrack inputs
manx_df_pt$date_time <-
  lubridate::dmy_hms(
    paste(
      paste(
        manx_df_pt$day,
        manx_df_pt$month,
        manx_df_pt$year,
        sep = "/"),
      paste(manx_df_pt$hour,
            manx_df_pt$minute,
            manx_df_pt$second,
            sep = ":"),
      sep = " "))

# Combine the dataframes with columns in "var"
var <- c("ID", "date_time", "Latitude", "Longitude", "ID", "x", "y")
manxie_df <- rbind(manx_df[var], manx_df_pt[var])

# Sort tracks by ID and time
manxie_df <- with(manxie_df, manxie_df[order(ID, date_time),])

# Remove the raw dataframes
rm(tag_31276, tag_31285, tag_31301, tag_31308,
   tag_31325, tag_31334, tag_31346, tag_31388,
   tag_31389, tag_31436, tag_31324, tag_31388b,
   tag_31436b, tag_31308b, M18_210623_trimmed,
   M2_210623_trimmed, M3_210622_trimmed,
   M17113_210627_trimmed, M17126_210628_trimmed, M17134_210628_trimmed,
   M11_210629_trimmed, M2_210630_trimmed, M15_210630_trimmed,
   M20_210630_trimmed, M17137_210630_trimmed, M17110_210630_trimmed,
   M17126_210701_trimmed, M17119_210703_trimmed, M11_210703_trimmed,
   M14_210702_trimmed, M3_210702_trimmed, M8_210701_trimmed,
   M5_210701_trimmed, M17110_210704_trimmed, M17112_210717_trimmed,
   M20_210722_trimmed, M17119_210723_trimmed, M14_210725_trimmed,
   M15_210726_trimmed, M3_210731_trimmed, M17126_210726_trimmed,
   M17137_210731_trimmed, M17138_210802_trimmed, M15_210804_trimmed,
   M17135_210805_trimmed, M5_210805_trimmed, manx_df, manx_df_pt)

# Set colony location
col_lon <- -6.586853
col_lat <- 52.13686

# Strip out track frames for sorting trips from non-trip points
track_frames <- manxie_df %>%
  dplyr::select(Longitude, Latitude, ID, date_time) %>%
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
                   InBuff = 1,
                   RetBuff = 5,
                   Dur = 6,
                   mc.cores = 1)

# Distance between each point
manx_df$dist <-
  do.call(rbind, track_frames)$dist

# Distance to colony
manx_df$PointDist <-
  PointDist(manx_df,
            x = col_lon,
            y = col_lat)

# Blow these away
rm(track_frames)

# Append trip_id back onto overall df
manxie_df$trip_id <- as.factor((Reduce(
  rbind, tripID))$trip_id)

# Returns variable to tell you whether the track comes back
manxie_df$returns <- as.factor((Reduce(
  rbind, tripID))$Returns)

# Write out .csv of manxie data to get envirmental variables
write.csv(x = manxie_df, file = "data/cleaned/manxie_ls_df.csv")

manx_env <- read.csv("data/env/ls_env.csv") %>%
  mutate(date_time = dmy_hms(date_time)) %>%
  with(., .[order(ID, date_time), ]) %>%
  select(-ID, -Latitude, -Longitude, -date_time)

manxie_df <- cbind(manxie_df, manx_env)

# This df lacks colony positions
manxie_ls <- 
  manxie_df[
    which(manxie_df$trip_id != "-1"),] %>%
  EnvAppend()

rm(manxie_df)

# Distance to colony
manxie_ls$PointDist <-
  PointDist(manxie_ls,
            x = col_lon,
            y = col_lat)

# This can go
rm(tripID)

# Plot out the raw data
ggplot() +
  coord_cartesian(xlim = c(-100000, 500000),
                  ylim = c(5600000, 6200000)) +
  theme(aspect.ratio = (1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom") +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  geom_path(data = manxie_ls,
            aes(x = x,
                y = y,
                colour = ID),
            alpha = 0.6,
            size = 0.8)

# Check how many trips and individuals we have tracks for
length(unique(manxie_ls$trip_id))
length(unique(manxie_ls$ID))

# Save it for later
save(manxie_ls, file = "data/cleaned/manxie_ls.RData")
load(file = "data/cleaned/manxie_ls.RData")
