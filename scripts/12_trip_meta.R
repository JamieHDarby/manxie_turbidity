
trip_meta <- function(x){
  duration <- as.numeric(difftime(x$date_time[nrow(x)],
                                  x$date_time[1],
                                  units = "mins"))
  
  x$step <- 0
  for(i in 2:nrow(x)){
    x$step[i] <- pointDistance(p1 = c(x[(i), "Longitude"],
                                    x[(i), "Latitude"]),
                             p2 = c(x[(i - 1), "Longitude"],
                                    x[(i - 1), "Latitude"]),
                             lonlat = T)
  }
  
  dist_travelled <- sum(x$step)
  
  max_dist <- max(x$PointDist)
  
  travel_rate <- dist_travelled / duration
  
  mean_depth <- mean(x$max_depth[which(x$dives > 0)])
  
  dive_sum <- sum(x$dives, na.rm = T)
  
  dive_rate <- dive_sum / duration
  
  zsd_mean <- mean(x$zsd, na.rm = T)
  
  wave_mean <- mean(x$wave, na.rm = T)
  
  ID <- x$ID[1]
  
  df <- data.frame(duration, dist_travelled, max_dist, travel_rate,
                   mean_depth, dive_sum, dive_rate,
                   zsd_mean, wave_mean)
}

trip_meta_df <- 
  split(pos_dive_df, pos_dive_df$trip_id) %>%
  
  lapply(., trip_meta) %>%
  
  bind_rows()

ggplot(trip_meta_df, aes(x = mean_depth, y = dive_rate, colour = dist_travelled, size = wave_mean)) +
  geom_point() +
  geom_smooth(method = "lm")

lm(data = trip_meta_df, formula = mean_depth ~ dive_rate) %>%
  summary()

length(unique(dive_pos_df$id_dive[which(dive_pos_df$sunangle < 106)]))
