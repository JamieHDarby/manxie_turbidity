G1 <- read.csv("data/Gannet_2021/20210726-162330_GPS55_trimmed.csv") %>%
  mutate(ID = "55") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G2 <- read.csv("data/Gannet_2021/20210726-163642_GPS52_trimmed.csv") %>%
  mutate(ID = "52") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G3 <- read.csv("data/Gannet_2021/20210727-155711_GPS8_trimmed.csv") %>%
  mutate(ID = "8") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G4 <- read.csv("data/Gannet_2021/20210727-160021_GPS12_trimmed.csv") %>%
  mutate(ID = "12") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G5 <- read.csv("data/Gannet_2021/20210727-160125_GPS21R_trimmed.csv") %>%
  mutate(ID = "21R") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G6 <- read.csv("data/Gannet_2021/20210727-160318_GPS21_trimmed.csv") %>%
  mutate(ID = "21") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G7 <- read.csv("data/Gannet_2021/20210729-163338_GPS1_trimmed.csv") %>%
  mutate(ID = "1") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G8 <- read.csv("data/Gannet_2021/20210729-163554_GPS3_trimmed.csv") %>%
  mutate(ID = "3") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G9 <- read.csv("data/Gannet_2021/20210802-155235_GPS26_trimmed.csv") %>%
  mutate(ID = "26") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

G10 <- read.csv("data/Gannet_2021/20210802-155708_GPS20_trimmed.csv") %>%
  mutate(ID = "20") %>%
  mutate(date_time = lubridate::dmy_hms(paste(Date, Time, sep = "")))

gannet_df <- rbind(G1, G2, G3, G4, G5, G6, G7, G8, G9, G10) %>%
  filter(Latitude > 0) %>%
  WGS2UTM30(.)

# Plot out the raw data from Celtic Sea colonies
gannet_tracks <- 
  ggplot() +
  coord_cartesian(xlim = c(-11, -6),
                  ylim = c(49.5, 52.5)) +
  theme(aspect.ratio = (4/5),
        # axis.text = element_blank(),
        # axis.title = element_blank(),
        # axis.ticks = element_blank(),
        panel.background = element_blank()) +
  geom_polygon(data = land_df_wgs,
               aes(x = long,
                   y = lat,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  labs(x = "Longitude", y = "Latitude") +
  geom_path(data = gannet_df,
            aes(x = Longitude,
                y = Latitude,
                colour = ID),
            alpha = 0.6,
            size = 0.8)

ggsave(gannet_tracks, filename = "plots/gannet_tracks.png",
       width = 8, height = 6, dpi = 500)

# Sort tracks by ID and time
gannet_df <- with(gannet_df, gannet_df[order(ID, date_time),])

col_lon <- -6.6217
col_lat <- 52.1096

# Strip out track frames for sorting trips from non-trip points
track_frames <- gannet_df %>%
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
                   RetBuff = 10,
                   Dur = 6,
                   mc.cores = numCores)

# Distance between each point
gannet_df$dist <-
  do.call(rbind, track_frames)$dist

# Distance to colony
gannet_df$PointDist <-
  PointDist(gannet_df,
            x = col_lon,
            y = col_lat)

# Blow these away
rm(track_frames, col_lon, col_lat)

# Append trip_id back onto overall df
gannet_df$trip_id <- as.factor((Reduce(
  rbind, tripID))$trip_id)

# Returns variable to tell you whether the track comes back
gannet_df$returns <- as.factor((Reduce(
  rbind, tripID))$Returns)  

# This df lacks colony positions
gannet_ls <- 
  gannet_df[
    which(gannet_df$trip_id != "-1"),]

# This can go
rm(tripID)

# Plot out the raw data from Celtic Sea colonies
ggplot() +
  coord_cartesian(xlim = c(-200000, 400000),
                  ylim = c(5500000, 6100000)) +
  theme(aspect.ratio = (1),
        # axis.text = element_blank(),
        # axis.title = element_blank(),
        # axis.ticks = element_blank(),
        panel.background = element_blank()) +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  geom_path(data = gannet_ls,
            aes(x = x,
                y = y,
                colour = trip_id),
            alpha = 0.6,
            size = 0.8) +
  geom_path(data = manxie_ls,
            aes(x = x,
                y = y,
                colour = trip_id),
            alpha = 0.6,
            size = 0.8)

length(unique(gannet_ls$trip_id))
length(unique(manxie_ls$trip_id))

length(unique(gannet_ls$ID))
length(unique(manxie_ls$ID))
