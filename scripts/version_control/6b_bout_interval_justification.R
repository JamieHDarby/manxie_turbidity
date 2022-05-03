dive_meta_ls <- split(dive_pos_meta, as.character(dive_pos_meta$trip_id))

interval_ls <- lapply(dive_meta_ls, FUN = function(x){
  
  dive_interval <- rep(NA, (nrow(x) - 1))
  
  for(i in 2:nrow(x)){
    dive_interval[i - 1] <- as.numeric(
      abs(
        difftime(x$date_time[i],
                 x$date_time[i - 1],
                 units = "secs")))
  }
  
  data.frame(dive_interval, x$id[1])
}) %>%
  bind_rows()# %>%
  # mutate(dive_interval = ifelse(dive_interval > 3600, 3600, dive_interval))

ggplot(interval_ls) +
  geom_density(aes(x = dive_interval), fill = "dark blue", alpha = 0.4) +
  geom_vline(xintercept = 300, colour = "dark red", size = 2) +
  scale_x_continuous(trans = "log")

test <- rep(0, 100)
for(i in 1:100){
test[i] <- quantile(interval_ls$dive_interval, i/100)}

ggplot()+ geom_path(aes(y = test, x = 1:100)) +
  geom_vline(xintercept = 80) +
  geom_hline(yintercept = 1800)

test <- arrange(interval_ls, dive_interval)

summary(test)

test2 <- abs(diff(diff(test$dive_interval)))

ggplot() + geom_path(aes(y = test2, x = 1:3705)) +
  geom_smooth(aes(y = test2, x = 1:3705), method = "loess") +
  geom_vline(xintercept = 2400, colour = "dark red", size = 2) +
  scale_y_continuous(trans = "log")

test$dive_interval[2400]
