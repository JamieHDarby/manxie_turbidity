
# Read in James Waggitt's env rasters
bathy_depth <- raster::raster(x = "data/env/EMOD_UK_Bathymetry_1km_UTM30N.asc")
bathy_feat <- raster::raster(x = "data/env/EMOD_UK_BathymetryFeature_1km_UTM30N.asc")
strat_ind <- raster::raster(x = "data/env/MEMS_UK_StratificationIndex_1km_UTM30N.asc")
tidal_curs <- raster::raster(x = "data/env/MEMS_UK_TidalCurrentsMaximum_1km_UTM30N.asc")

# Give them UTM30
proj4string(bathy_depth) <- CRS("+init=epsg:32630")
proj4string(bathy_feat) <- CRS("+init=epsg:32630")
proj4string(strat_ind) <- CRS("+init=epsg:32630")
proj4string(tidal_curs) <- CRS("+init=epsg:32630")

# Read in the NASA MODIS variables
KD490_1 <- raster::raster("data/env/A20211932021200.L3m_8D_KD490_Kd_490_4km.nc")
KD490_2 <- raster::raster("data/env/A20212012021208.L3m_8D_KD490_Kd_490_4km.nc")
KD490_3 <- raster::raster("data/env/A20212092021216.L3m_8D_KD490_Kd_490_4km.nc")
KD490_4 <- raster::raster("data/env/A20212172021224.L3m_8D_KD490_Kd_490_4km.nc")

# Read in the coast distance variable
Coast <- raster::raster(x = "data/env/Coast_dist.tif")

# Original data is signed, unsign it
values(Coast)[which(values(Coast) < 0)] <- 0

# Change these values to NAs
NAvalue(Coast) <- -32767

# Get a big auld bathy object on the go
NEABathy <- marmap::getNOAA.bathy(lon1 = -41, lon2 = 15,
                                  lat1 = 40, lat2 = 68, resolution = 2)

# Pull out slope data to the shape of the bathy_feat raster
bathySlope <- 
        raster::terrain(marmap::as.raster(NEABathy),
                        opt = 'slope', neighbors = 8)

# Align all rasters -------------------------------------------------------

# Project to UTM 30 of same extent as JW's env raster layers
Coast_UTM <- raster::projectRaster(Coast, bathy_feat, method = "bilinear")
Slope_UTM <- raster::projectRaster(bathySlope, bathy_feat, method = "bilinear")
KD490_1_UTM <- raster::projectRaster(KD490_1, bathy_feat, method = "bilinear")
KD490_2_UTM <- raster::projectRaster(KD490_2, bathy_feat, method = "bilinear")
KD490_3_UTM <- raster::projectRaster(KD490_3, bathy_feat, method = "bilinear")
KD490_4_UTM <- raster::projectRaster(KD490_4, bathy_feat, method = "bilinear")

# Append all raster data to fulmar dataframe ------------------------------

# Transfrom Fulmar_state_df to spatial points
sp <- SpatialPoints(manxie_ls[c("Longitude", "Latitude")],
                    proj4string = CRS("+proj=longlat +datum=WGS84"))

# Give this UTM30
xysp <- spTransform(sp, CRSobj = CRS("+proj=utm +zone=30 ellps=WGS84"))

# Append each raster layer to Fulmar_state_df using spatial points
manxie_ls$Depth <- raster::extract(bathy_depth, xysp)
manxie_ls$Roughness <- raster::extract(bathy_feat, xysp)
manxie_ls$Stratification <- raster::extract(strat_ind, xysp)
manxie_ls$Currents <- raster::extract(tidal_curs, xysp)
manxie_ls$Slope <- raster::extract(Slope_UTM, xysp)
manxie_ls$Coast <- raster::extract(Coast_UTM, xysp)

# Write a blank into KD490 for manx dataframe
manxie_ls$KD490 <- NA

# Day indicator to append correct KD490 value
julian <- format(manxie_ls$date_time, format = "%j") %>%
  as.numeric(.)

# Append KD490 from any of 4 rasters depending on date
ind <- which(julian > 192 & julian < 201)
manxie_ls$KD490[ind] <- raster::extract(KD490_1_UTM, xysp[ind])
ind <- which(julian > 200 & julian < 208)
manxie_ls$KD490[ind] <- raster::extract(KD490_2_UTM, xysp[ind])
ind <- which(julian > 207 & julian < 216)
manxie_ls$KD490[ind] <- raster::extract(KD490_3_UTM, xysp[ind])
ind <- which(julian > 215 & julian < 224)
manxie_ls$KD490[ind] <- raster::extract(KD490_4_UTM, xysp[ind])

# Get rid of these
rm(sp, xysp, KD490_1, KD490_1_UTM, KD490_2,
   KD490_2_UTM, KD490_3, KD490_3_UTM, KD490_4, KD490_4_UTM,
   Coast_UTM, Coast, Slope_UTM, julian, strat_ind, NEABathy,
   tidal_curs, bathy_depth, bathy_feat, bathySlope, ind)

# Save it off to be loaded next time without running all of the above
save(manxie_ls, file = "data/cleaned/manxie_ls_env.RData")
load(file = "data/cleaned/manxie_ls_env.RData")
