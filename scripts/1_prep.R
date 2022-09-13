
# Set number of cores -----------------------------------------------------

{
  if (Sys.info()[['sysname']] == "Windows")            
  {
    numCores <- 1
  }
  
  if (Sys.info()[['sysname']] != "Windows")
  {
    numCores <- parallel::detectCores()
  }
}

# Library read-in ---------------------------------------------------------

{
  if(!require(ggplot2)) install.packages("ggplot2")
  if(!require(lme4)) install.packages("lme4")
  if(!require(nlme)) install.packages("nlme")
  if(!require(stats)) install.packages("stats")
  if(!require(maptools)) install.packages("maptools")
  if(!require(move)) install.packages("move")
  if(!require(adehabitatLT)) install.packages("adehabitatLT")
  if(!require(spatstat)) install.packages("spatstat")
  if(!require(rgdal)) install.packages("rgdal")
  if(!require(tidyr)) install.packages("tidyr")
  if(!require(dplyr)) install.packages("dplyr")
  if(!require(sp)) install.packages("sp")
  if(!require(viridis)) install.packages("viridis")
  if(!require(mapdata)) install.packages("mapdata")
  if(!require(maps)) install.packages("maps")
  if(!require(ggmap)) install.packages("ggmap")
  if(!require(momentuHMM)) install.packages("momentuHMM")
  if(!require(tidyverse)) install.packages("tidyverse")
  if(!require(EMbC)) install.packages("EMbC")
  if(!require(ggpubr)) install.packages("ggpubr")
  if(!require(cowplot)) install.packages("cowplot")
  if(!require(oce)) install.packages("oce")
  if(!require(caret)) install.packages("caret")
  if(!require(mgcv)) install.packages("mgcv")
  if(!require(MuMIn)) install.packages("MuMIn")
  if(!require(DescTools)) install.packages("DescTools")
  if(!require(ResourceSelection)) install.packages("ResourceSelection")
  if(!require(suncalc)) install.packages("suncalc")
  if(!require(marmap)) install.packages("marmap")
  if(!require(PresenceAbsence)) install.packages("PresenceAbsence")
  if(!require(ROCR)) install.packages("ROCR")
  if(!require(PathInterpolatR)) install.packages("PathInterpolatR")
  if(!require(lubridate)) install.packages("lubridate")
  if(!require(mgcViz)) install.packages("mgcViz")
  if(!require(birk)) install.packages("birk")
  if(!require(diveMove)) install.packages("diveMove")
  if(!require(DHARMa)) install.packages("DHARMa")
}

# UTM302WGS ---------------------------------------------------------------

UTM302WGS<-function(dataframe)
{ 
  data.xy =dataframe[c("x","y")]
  
  xysp<-SpatialPoints(data.xy)
  
  proj4string(xysp) <-
    CRS("+init=epsg:32630")
  
  xysp<-
    spTransform(xysp,
                CRS("+init=epsg:4326"))
  
  dataframe$Longitude<-xysp$x
  
  dataframe$Latitude<-xysp$y
  
  return(dataframe)
}

# WGS2UTM30 ---------------------------------------------------------------

WGS2UTM30 <- function(dataframe)
{ 
  data.xy = dataframe[c("Longitude","Latitude")]
  
  xysp<-SpatialPoints(data.xy)
  
  proj4string(xysp) <-
    CRS("+init=epsg:4326")
  
  xysp<-
    spTransform(
      xysp,
      CRS("+init=epsg:32630"))
  
  dataframe$x<-xysp$Longitude
  
  dataframe$y<-xysp$Latitude
  
  return(dataframe)
}

# PointDist ---------------------------------------------------------------

PointDist<-function(dataframe, x, y)
{
  PointDist<-rep(NA,nrow(dataframe))
  
  long<- x
  
  lat<- y
  
  longrep<-rep(long,nrow(dataframe))
  
  latrep<-rep(lat,nrow(dataframe))
  
  PointDist[1:nrow(dataframe)] = pointDistance(
    matrix(c(dataframe$Longitude,
             dataframe$Latitude),
           ncol = 2),
    matrix(c(longrep,
             latrep),
           ncol = 2),
    longlat=TRUE,
    allpairs=FALSE)
  PointDist
}

# TripSplit ---------------------------------------------------------------

tripSplit <- function(Track,
                      Colony,
                      InnerBuff = 15,
                      ReturnBuff = 45,
                      Duration = 12,
                      plotit = F,
                      MidPoint = F)
  
{
  if(!"Latitude" %in% names(Track))
    stop("Latitude field does not exist")
  
  if(!"Longitude" %in% names(Track))
    stop("Longitude field does not exist")
  
  if(!"ID" %in% names(Track))
    stop("ID field does not exist")
  
  if(!"TrackTime" %in% names(Track))
    stop ("TrackTime field does not exist")
  
  if(!"Latitude" %in% names(Colony))
    stop("Colony missing Latitude field")
  
  if(!"Longitude" %in% names(Colony))
    stop("Colony missing Longitude field")
  
  if(!(is.double(InnerBuff) &
       is.double(ReturnBuff)))
    stop (
      "InnerBuff and ReturnBuff should be numbers")
  
  require(sp)
  
  require(maps)
  
  require(mapdata)
  
  require(rgdal)
  
  require(geosphere)
  
  if(class(Track) != "SpatialPointsDataFrame")
    
  {
    Track.Wgs <- SpatialPoints(
      data.frame(
        Track$Longitude, Track$Latitude),
      proj4string=CRS(
        "+init=epsg:4326"))
    
    Track.Projected <- spTransform(
      Track.Wgs,
      CRS=CRS(paste(
        "+proj=laea +lon_0=",
        Colony$Longitude, " +lat_0=",
        Colony$Latitude, sep="")))
    
    Track <- SpatialPointsDataFrame(
      Track.Projected,
      data = Track,
      match.ID = F)
  }
  rbind
  {
    if(MidPoint == FALSE)
    {
      
      Colony.Wgs <- SpatialPoints(
        data.frame(Colony$Longitude,
                   Colony$Latitude),
        proj4string=CRS(
          "+init=epsg:4326"))
      
      Colony.Projected <- spTransform(
        Colony.Wgs,
        CRS=CRS(paste("+proj=laea +lon_0=", 
                      Colony$Longitude,
                      " +lat_0=",
                      Colony$Latitude, sep="")))
    } 
    else
    {
      mid_point<-data.frame(
        centroid(cbind(Track$Longitude,
                       Track$Latitude)))
      
      Colony.Wgs <- SpatialPoints(
        data.frame(Colony$Longitude,
                   Colony$Latitude),
        proj4string=CRS(
          "+init=epsg:4326"))
      
      Colony.Projected <- spTransform(
        Colony.Wgs,
        CRS=CRS(paste("+proj=laea +lon_0=",
                      mid_point$lon,
                      " +lat_0=",
                      mid_point$lat,
                      sep="")))
      
      Track <- spTransform(
        Track,
        CRS=CRS(paste(
          "+proj=laea +lon_0=",
          mid_point$lon,
          " +lat_0=",
          mid_point$lat,
          sep="")))
    }
  } 
  
  Track$X <- Track@coords[,1]
  
  Track$Y <- Track@coords[,2]
  
  Track$Returns <- "Y"
  
  Track$trip_id <- 0
  
  Track$ColDist <- spDists(Track,
                           Colony.Projected)
  
  Trip.Sequence <- 0
  
  Time.Diff <- 0
  
  Max.Dist <- 0
  
  ReturnBuff <- ReturnBuff * 1000
  
  InnerBuff <- InnerBuff * 1000
  
  if(
    plotit == TRUE)
  {
    plot(Track, pch=1, cex=0.5)
    
    legend("topleft", paste(Track$ID[1]))
    
    points(Colony.Projected,
           pch=18, cex=1.5,
           col=2)
  }
  
  i <- 0
  
  while(i < nrow(Track))
  {
    i <- i + 1
    if(Track$ColDist[i] < InnerBuff)
    {Track$trip_id[i] <- -1}
    
    else
    {
      k <- i
      
      if(i == nrow(Track))
      {Track$trip_id[i] <- -1;
      break}
      
      Dist <- Track$ColDist[i]
      
      while(Dist >= InnerBuff)
      {
        if(k == nrow(Track) &
           Dist < ReturnBuff) {break}
        else
        {
          if(k == nrow(Track))
          {
            print(
              paste(
                "track ",
                Track$ID[1],
                Trip.Sequence + 1,
                " does not return to the colony",
                sep=""))
            
            Track$Returns[i:k] <- "N" ; break
          }
        }
        
        k <- k + 1
        
        if(plotit == TRUE)
        {
          points(Track[k,],
                 col=2,
                 pch=16,
                 cex=0.5)
        }
        
        Dist <- Track$ColDist[k]
      }
      
      Time.Diff <- 
        difftime(Track$TrackTime[k],
                 Track$TrackTime[i], units = "hours")
      
      Max.Dist <- max(Track$ColDist[i:k])
      
      if(Time.Diff < Duration |
         Max.Dist < InnerBuff)
        
      {
        Track$trip_id[i:k] <- -1;
        
        i <- k;
        
        print(paste("trip ",
                    Track$ID[1],
                    Trip.Sequence + 1,
                    " is too small a trip"))
        
        next
      }
      Trip.Sequence <- Trip.Sequence + 1
      
      Track$trip_id[i:k] <- paste(Track$ID[1],
                                  Trip.Sequence,
                                  sep="_")
      
      i <- k
      
      print(paste(Track$ID[1],
                  Trip.Sequence, sep="_"))
    }
  }
  if(plotit == TRUE)
  {
    points(Track, pch=16,
           cex=0.75,
           col=as.factor(Track$trip_id))
  }
  
  return(Track)
}

# trip_cleaner ------------------------------------------------------------

trip_cleaner <- function(x, t = 60)
{
  ind <- 1
  
  x$section <- paste(x$trip_id, ind, sep = "_")
  
  for(i in 2:(nrow(x)))
  {
    diff <- abs(as.numeric(difftime(time1 = x$date_time[i],
                                    time2 = x$date_time[i - 1],
                                    units = "mins")))
    
    if(diff > t){ind <- ind + 1}
    
    x$section[i] <- paste(x$trip_id[i], ind, sep = "_")
  }
  x
}

# BirdTrip ----------------------------------------------------------------

Birdtrip <- function(df, x, y,
                   InBuff = 1,
                   RetBuff = 1,
                   Dur = 0)
{
  Longitude <- x
  
  Latitude <- y
  
  Colony <- data.frame(Longitude,Latitude)
  
  Track <- df
  
  as.data.frame(tripSplit(Track, Colony,
                          InnerBuff = InBuff,
                          ReturnBuff = RetBuff,
                          Duration = Dur,
                          plotit = F,
                          MidPoint = F))
}

# TripFunc ----------------------------------------------------------------

TripFunc <- function(x)
{
  trip_stats <- split(x,
                      x$trip_id)
  
  trip_dists <- lapply(trip_stats, function(x)
  {
    dist <- sum(x$dist)
    
    id <- x$id[1]
    
    df <- data.frame(dist,id)
  })
  
  trip_travel <- Reduce(rbind,trip_dists)
  
  avg_trip<-split(trip_travel,trip_travel$id)
}

# tracksplit --------------------------------------------------------------

tracksplit <- function(
  x, leasttime = 20, maxdist = 3)
{
  i <- 0
  
  k <- 1
  
  num <- nrow(x)
  
  diff <- rep(0, num)
  
  dist <- rep(0, num)
  
  split <- rep(0, num)
  
  split[1] <- paste(
    x$ID[1], 1, sep = '_')
  
  for (i in 1:(num - 1))
  {
    if (x$ID[i] == x$ID[i + 1])
    {
      diff[i] <- abs(as.numeric(difftime(
        x$date_time[i + 1], x$date_time[i],
        units = "mins")))
      
      dist[i] <- pointDistance(
        c(x$Longitude[i],
          x$Latitude[i]),
        c(x$Longitude[i + 1],
          x$Latitude[i + 1]),
        longlat = TRUE,
        allpairs = FALSE)
      
      if ((diff[i] > leasttime) |
          (dist[i] > (maxdist*1000)))
      {
        k <- k + 1}
    }
    else
    {k <- 1}
    split[i + 1] <- paste(x$ID[i + 1], k, sep = '_')
  }
  split
}

# EnvAppend ---------------------------------------------------------------

EnvAppend <- function(x)
{
  x$WindSpeed <- rep(0, nrow(x))
  x$WindDirection <- rep(0, nrow(x))
  x$Trajectory <- rep(0, nrow(x))
  x$WindOffset <- rep(0, nrow(x))
  
  if(!require(fossil)) install.packages("fossil")
  if(!require(oce)) install.packages("oce")
  
  if(nrow(x) > 0)
  {
    for(i in 1:nrow(x)){
      x$WindSpeed[i] <- sqrt((x$WindV[i] ^ 2) + (x$WindU[i] ^ 2))
      
      x$WindDirection[i] <-
        (((atan2((x$WindU[i]/x$WindSpeed[i]),
                 (x$WindV[i]/x$WindSpeed[i]))) * (180 / pi)) + 180)
      
      if(x$WindDirection[i] > 180)
      {x$WindDirection[i] <- - 360 + abs(x$WindDirection[i])}
      
      if(i < nrow(x))
      {
        x$Trajectory[i] <- earth.bear(x$Longitude[i],
                                      x$Latitude[i],
                                      x$Longitude[i + 1],
                                      x$Latitude[i + 1])
        
        if(x$Trajectory[i] > 180)
        {x$Trajectory[i] <- (360 - abs(x$Trajectory[i])) * -1}
        
        temp1 <- (x$Trajectory[i])
        
        temp2 <- (x$WindDirection[i])
        
        temp3 <- temp1 - temp2
        
        if(temp3 > 180)
        {temp3 <- (360 - temp3) * -1}
        if(temp3 < -180)
        {temp3 <- (360 - abs(temp3))}
        
        x$WindOffset[i] <- temp3
      }
    }
    
    
    x$sunangle <- (sunAngle(t = x$date_time,
                            lat = x$Latitude,
                            lon = x$Longitude))$altitude
    
    x$DayNight <- rep("Day", nrow(x))
    
    x$DayNight[which(as.numeric(x$sunangle) < -6)] <- "Night"
    
  }
  
  x
}

# LinStepR ----------------------------------------------------------------

LinStepR <- function(df, t = 60, extras = NULL)
{
  time_range <- seq(from = (df$date_time[1]),
                    to = df$date_time[nrow(df)],
                    by = t,)
  
  out <- data.frame(date_time = time_range,
                    x = rep(df$x[1], length(time_range)),
                    y = rep(df$y[1], length(time_range)))
  
  out[extras] <- df[1, extras]
  
  for(i in 2:nrow(out))
  {
    index <- birk::which.closest(vec = df$date_time,
                                 x = out$date_time[i])
    
    if(df$date_time[index] > out$date_time[i]){index <- index - 1}
    
    indices <- (index):(index + 1)
    
    if(indices[1] < 1){indices[1] <- 1}
    
    if(indices[2] > nrow(df)){indices[2] <- nrow(df)}
    
    xyt <- df[indices, c("x", "y", "date_time")] %>%
      mutate(date_time = as.numeric(date_time))
    
    t.slice <- as.numeric(out$date_time[i])
    
    out[i, c("x", "y")] <-
      (PathInterpolatR::linear(xyt = xyt,
                               t.slice = t.slice))[,(1:2)]
    
    if(anyNA(out[i, c("x", "y")])){
      out[i, c("x", "y")] <- df[index, c("x", "y")]
    }
  }
  out
}

# cefas_tdr ---------------------------------------------------------------

cefas_tdr <- function(x){
  # Read in the data with a space as a delimiter
  read.csv(x,
           header = F, sep = " ") %>%
    
    # Filter the first column to a string length of 10, and the second between 20 and 24
    filter(str_length(V1) == 10, str_length(V2) > 20, str_length(V2) <24) %>%
    
    # Get rid of other columns
    select(c("V1", "V2")) %>%
    
    # Combine these two columns
    mutate(data = paste(V1, V2, sep = " ")) %>%
    
    # Split by comma into 3 target variables, gives plenty warnings, ignore
    separate(col = data, into = c("date", "pressure", "temp"), sep = ",") %>%
    
    # Filtering out temperature NA values discards non-target rows kept until now
    filter(!is.na(temp)) %>%
    
    # Mutate data into correct format
    mutate(temp = as.numeric(temp),
           pressure = as.numeric(pressure),
           date_time = dmy_hms(date)) %>%
    
    # Ditch the non-target variables
    select(-V1, -V2, -date)
}

# Dive split --------------------------------------------------------------

dive_split <- function(x, bout_time = 30){
  
  # Split dataframe by ID
  ls <- split(x, x$ID)
  
  # Loop through this list
  for(i in 1:length(ls)){
    # Create the new variabes
    ls[[i]]$dive <- 1
    ls[[i]]$bout <- 1
    
    # Create indicators
    dive <- 1
    bout <- 1
    
    # Loop through each element of this list
    for(j in 2:nrow(ls[[i]])){
      # Check interval between dive records
      diff <- abs(
        as.numeric(
          difftime(ls[[i]]$date_time[j],
                   ls[[i]]$date_time[j - 1],
                   units = "sec")))
      
      # Check the intervals and shift indicators
      if(diff > 5){dive <- dive + 1}
      if(diff > bout_time*60){bout <- bout + 1}
      
      # Write the appropriate indicators into the current row
      ls[[i]]$dive[j] <- dive
      ls[[i]]$bout[j] <- bout
    }}
  
  # Recombine this list
  x <- do.call(rbind, ls)
  
  # Output the recombined dataframe
  x
}

# dive_attach -------------------------------------------------------------

# Create a function to append data of interest to dives
dive_attach <- function(x,
                        pos_df,
                        vars = c("Longitude", "Latitude", "Wave",
                                 "WindSpeed", "sunangle", "Precip",
                                 "cloud_total", "cloud_high", "cloud_low",
                                 "KD490", "Slope", "Depth", "Coast",
                                 "Currents", "Stratification", "PointDist")){
  
  if(!x$ID[1] %in% pos_df$ID){x[, vars] <- NA}
  else{
    for(i in 1:nrow(x)){
      y <- pos_df[which(pos_df$ID == x$ID[i]), ]
      
      ind <- which.closest(y$date_time, x$date_time[i])
      
      if(abs(as.numeric(difftime(y$date_time[ind], x$date_time[i], units = "mins"))) < 20){
        x[i, vars] <- y[ind, vars]
      }
      else{
        x[i, vars] <- NA
      }
    }
  }
  return(x)
}

# pos_attach --------------------------------------------------------------

# Create a function to append data of interest to dives
pos_attach <- function(x, dive_df){
  
  vars <- c("dives", "max_depth", "time_under", "time_depth")
  
  x[, vars] <- 0
  
  if(!x$ID[1] %in% dive_df$ID){x[, vars] <- NA}
  
  else{
    for(i in 1:nrow(x)){
      y <- dive_df[which(dive_df$ID == x$ID[i]), ]
      
      start <- x$date_time[i] - 150
      end <- x$date_time[i] + 150
      
      y <- y[which(y$date_time > start &y$date_time < end), ]
      
      if(nrow(y) > 0){
      x$dives[i] <- length(unique(y$dive))
      
      x$max_depth[i] <- max(y$depth, na.rm = T)
      
      x$time_under[i] <- nrow(y) * 2
      
      x$time_depth[i] <- sum(sqrt(abs(y$depth))) * 2
      }
    }
  }
  return(x)
}

# Land shapes -------------------------------------------------------------

if(!exists("land_df_wgs")){
  load("data/shapes/land_df_wgs.RData")              
}

if(!exists("land_df_utm30")){
  load("data/shapes/land_df_utm30.RData")              
}


