
# Create a meta df of dives
dive_meta <- dive_pos_df %>%
  
  filter(depth < -1) %>%
  
  split(., .$id_dive) %>%
  
  lapply(., function(x){x <- x[which.min(x$depth), ]; x}) %>%
  
  bind_rows(.)
    
# Create a list of intervals, bind back into a dataframe, exclude any intervals
# greater than 1 hour
interval_ls <- split(dive_meta, as.character(dive_meta$trip_id)) %>%
  
  lapply(., FUN = function(x){
  
  dive_interval <- rep(NA, (nrow(x) - 1))
  
  for(i in 2:nrow(x)){
    dive_interval[i - 1] <- as.numeric(
      abs(
        difftime(x$date_time[i],
                 x$date_time[i - 1],
                 units = "secs")))
  }
  
  data.frame(dive_interval, x$ID[1])
}) %>%
  bind_rows() %>% filter(dive_interval < 3600)

# Plot out the log distribution of intervals
ggplot(interval_ls) +
  geom_density(aes(x = dive_interval)) +
  geom_vline(xintercept = 300, colour = "dark red", size = 2) +
  scale_x_continuous(trans = "log")

# Apply bout frequencies function
bout_frequencies <- boutfreqs(x = interval_ls$dive_interval,
                              bw = 30,
                              method = c("seq.diff"),
                              plot = TRUE)


# Apply broken stick methods to refine the expected bout ending criterion
bout_init <- boutinit(bout_frequencies,
                      x.break = 300,
                      plot = T)

# Non-linear least squares of frequencies using starting parameters from above
bout_fit <- fitNLSbouts(bout_frequencies,
                        start = bout_init,
                        maxiter = 2000)

# Get bout ending criterion
bec(bout_fit)
