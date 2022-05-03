
# PathTrack TDR input -----------------------------------------------------

# Read in tag data
tdr_31276 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs190721_022236_Tag31276Press_cleaned.txt") %>%
  mutate(ID = "EG44130")

tdr_31285 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs190721_170556_Tag31285Press_cleaned.txt") %>%
  mutate(ID = "EG44137")

tdr_31334 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs220721_031602_Tag31334Press_cleaned.txt") %>%
  mutate(ID = "EF44339")

  tdr_31301 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs230721_234348_Tag31301Press_cleaned.txt") %>%
  mutate(ID = "EY53457")

tdr_31346 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs260721_004021_Tag31346Press_cleaned.txt") %>%
  mutate(ID = "EX02541")

tdr_31389 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs270721_232254_Tag31389Press_cleaned.txt") %>%
  mutate(ID = "EN40369")

tdr_31436 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs280721_025409_Tag31436Press_cleaned.txt") %>%
  mutate(ID = "ET03928")

tdr_31388 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs290721_001045_Tag31388Press_cleaned.txt") %>%
  mutate(ID = "EF44339")

tdr_31325 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs290721_001359_Tag31325Press_cleaned.txt") %>%
  mutate(ID = "EX02548")

tdr_31308 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs310721_010055_Tag31308Press_cleaned.txt") %>%
  mutate(ID = "EX02530")

tdr_31324 <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs020821_011111_Tag31324Press_cleaned.txt") %>%
  mutate(ID = "EX02542")

tdr_31388b <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs030821_001434_Tag31388Press_cleaned.txt") %>%
  mutate(ID = "EX02532")

tdr_31436b <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs040821_023650_Tag31436Press_cleaned.txt") %>%
  mutate(ID = "EA15032")

tdr_31308b <- read.csv("data/Manx_2021/TDR/PT_TDR/Obs070821_234715_Tag31308Press_cleaned.txt") %>%
  mutate(ID = "EX02529")

# Combine all of the PathTrack TDR data
manx_dive_pt <- rbind(tdr_31276, tdr_31285, tdr_31301, tdr_31308,
                    tdr_31325, tdr_31334, tdr_31346, tdr_31388,
                    tdr_31389, tdr_31436, tdr_31324, tdr_31388b,
                    tdr_31436b, tdr_31308b) %>%
  mutate(depth = ifelse(depth < -200, NA, depth))

# Do a bit of jiggy jiggy to make a POSIXct date_time variable
manx_dive_pt$date_time <-
  lubridate::dmy_hms(
    paste(
      paste(
        manx_dive_pt$day,
        manx_dive_pt$month,
        manx_dive_pt$year,
        sep = "/"),
      paste(manx_dive_pt$hour,
            manx_dive_pt$minute,
            manx_dive_pt$second,
            sep = ":"),
      sep = " "))

# Example plot
manx_dive_pt %>%
  ggplot() +
  geom_point(aes(x = date_time, y = depth, colour = depth)) +
  facet_wrap(facets = ~ID, ncol = 1)

# Cefas TDR input ---------------------------------------------------------

# Read in all the tag data
tdr_A18669 <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18669_31-07-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EG44137")

tdr_A18673 <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18673_27-07-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02549")

tdr_A18673b <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18673_03-08-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02533")

tdr_A18678 <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18678_06-08-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02527")

tdr_A18679 <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18679_26-07-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02547")

tdr_A18679b <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18679_31-07-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02529")

tdr_A18680 <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18680_24-07-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02525")

tdr_A18681 <- cefas_tdr("data/Manx_2021/TDR/Cefas_TDR/A18681_04-08-2021.csv") %>%
  mutate(depth = -(pressure - median(pressure)),
         ID = "EX02531")

# Combine all of the processed Cefas recordings
manx_dive_cfs <- rbind(tdr_A18669, tdr_A18673, tdr_A18673b,
                       tdr_A18678, tdr_A18679, tdr_A18679b,
                       tdr_A18680, tdr_A18681) %>%
  # Filter to only include dives
  filter(depth < -1)
  
# Plot out dives recorded by Cefas TDRs
manx_dive_cfs %>%
  ggplot() +
  geom_point(aes(x = date_time, y = depth, colour = depth)) +
  facet_wrap(facets = ~ID, ncol = 1)

# Combine the data from the 2 TDR types
manx_dive_df <- rbind(manx_dive_cfs[,c("ID", "date_time", "depth")],
                      manx_dive_pt[,c("ID", "date_time", "depth")]) %>%
  # This identifies individual dives and dive bouts. Bouts defined by 30 minute gaps
  dive_split(., bout_time = 30)

# Get rid of raw dive data
rm(tdr_31276, tdr_31285, tdr_31301, tdr_31308,
   tdr_31325, tdr_31334, tdr_31346, tdr_31388,
   tdr_31389, tdr_31436, tdr_31324, tdr_31388b,
   tdr_31436b, tdr_31308b, tdr_A18669, tdr_A18673, 
   tdr_A18673b, tdr_A18678, tdr_A18679, tdr_A18679b,
   tdr_A18680, tdr_A18681)

# Look at all the dives together (needs a tall screen)
ggplot(manx_dive_df) +
  geom_point(aes(x = date_time, y = depth, colour = depth)) +
  facet_wrap(facets = ~ID, ncol = 1)

# Pull out one dive trace to look at
ggplot(manx_dive_df %>% filter(ID == "EG44137")) +
  geom_point(aes(x = date_time, y = depth, colour = as.factor(dive))) +
  geom_path(aes(x = date_time, y = depth, colour = as.factor(dive))) +
  facet_wrap(facets = ~ID, ncol = 1)# +
  # theme(legend.position = "none")

# Save off the output to avoid having to run all of the above code again
save(manx_dive_df, file = "data/cleaned/manxie_dive_df.RData")
load(file = "data/cleaned/manxie_dive_df.RData")
