
load(file = "data/cleaned/manxie_ls_int_env.RData")
zsd <- raster::stack("data/env/dataset-oc-glo-opt-multi-l4-zsd_interpolated_4km_daily-rt_1643023109817.nc")

plot(zsd)
NAvalue(zsd) <- 32767

zsd[[1]]

date_seq <- seq.Date(from = ymd("2021-07-01"), to = ymd("2021-08-09"), by = 1)

manxie_ls$zsd <- NA

for(i in 1:length(date_seq)){
  index <- which(as.Date(manxie_ls$date_time) == date_seq[i])
  
  if(length(index) > 0){
  # Transfrom Fulmar_state_df to spatial points
  sp <- SpatialPoints(manxie_ls[index, c("Longitude", "Latitude")],
                      proj4string = CRS("+proj=longlat +datum=WGS84"))
# Append each raster layer to manxie_ls using spatial points
manxie_ls$zsd[index] <- raster::extract(zsd[[i]], sp)}
}

manxie_ls$HOD <- as.numeric(format(manxie_ls$date_time, format = "%H"))
manxie_ls$MOD <- (as.numeric(format(manxie_ls$date_time, format = "%M")) / 60) + manxie_ls$HOD

save(manxie_ls, file = "data/cleaned/manxie_ls_int_env2.RData")
