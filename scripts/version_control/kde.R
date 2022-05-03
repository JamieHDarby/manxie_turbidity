manxie_ls$sun.angle <-
  oce::sunAngle(
    t = manxie_ls$date_time,
    latitude = manxie_ls$Latitude,
    longitude = manxie_ls$Longitude
  )$altitude

# KDE for Irish colonies
kde.manxie <- manxie_ls %>%
  # Keep useful variables
  dplyr::select(ID, x, y, PointDist, sun.angle) %>%
  dplyr::filter(sun.angle > 0) %>%
  mutate(inv.log.dist = log(1/PointDist),
         sqrt.dist = sqrt(PointDist),
         log.dist = log(PointDist)) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("x", "y")],
    proj4string = CRS("+proj=utm +zone=30N ellps=WGS84"),
    data = .) %>%
  # KDE estimation
  spatialEco::sp.kde(., y = .$sqrt.dist, bw = 5000, nr = 80, nc = 120,
         newdata = c(50000, 350000, 5650000, 5850000))

plot(kde.manxie)

manx.aerial <- read.csv("data/aerial/Density_mx_summer2021.csv") %>%
  filter(!is.na(Latitude) & !is.na(densitykm2)) %>%
  WGS2UTM30(.)

manx.aerial.rstr <- raster::rasterize(x = manx.aerial[,c("x", "y")],
                                      y = kde.manxie,
                                      field = manx.aerial$densitykm2) %>%
  raster::aggregate(fact = 2, fun = "mean")

plot(manx.aerial.rstr)

kde.manxie2 <- raster::aggregate(kde.manxie, fact = 2, fun = "mean")


values(kde.manxie2)[which(is.na(values(manx.aerial.rstr)))] <- NA

manx.aerial.rstr.trans <- log(manx.aerial.rstr + 0.1)

stack <- raster::stack(kde.manxie2, manx.aerial.rstr.trans)

raster::layerStats(stack, stat = "Pearson", na.rm = T)

stack.df <- as.data.frame(rasterToPoints(stack))

ttest.res <- SpatialPack::modified.ttest(x = stack.df$kde,
                                         y = (stack.df$layer),
                                         coords = stack.df[,c("x", "y")])

ttest.res

kde.manxie.df <- as.data.frame(rasterToPoints(kde.manxie2))

aerial.manxie.df <- as.data.frame(rasterToPoints(manx.aerial.rstr.trans))

tracking.kde.plot <- 
  ggplot() +
  coord_cartesian(xlim = c(100000, 300000),
                  ylim = c(5650000, 5850000)) +
  theme(aspect.ratio = (1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  scale_fill_viridis_c(option = "A", 
                       breaks = c(1e-10, 3.5e-10, 6e-10),
                       labels = c("Low", "Medium", "High")) +
  geom_raster(data = kde.manxie.df,
            aes(x = x,
                y = y,
                fill = kde)) +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = '#dddce3',
               fill = '#454545',
               size = 0.1) +
  ggrepel::geom_label_repel(aes(x = 254551,
                                y = 5782329,
                                label = "Little Saltee"),
                            nudge_x = -20000, nudge_y = 30000, colour = "dark red") +
  geom_point(aes(x = 254551,
                 y = 5782329),
             colour = "red", size = 3) +
  ggsn::scalebar(location = "bottomleft",
           y.min = 5660000,
           y.max = 5850000,
           x.min = 110000,
           x.max = 300000,
           dist_unit = "km",
           dist = 20,
           st.size = 4,
           st.bottom = F,
           transform = F,
           st.dist = .02,
           st.color = "black") +
labs(fill = "Density", x = "Eastings (m) UTM30", y = "Northings (m)")

aerial.plot <- 
  ggplot() +
  coord_cartesian(xlim = c(100000, 300000),
                  ylim = c(5650000, 5850000)) +
  theme(aspect.ratio = (1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank()) +
  scale_fill_viridis_c(option = "A",
                       breaks = c(-2, 1, 4),
                       labels = c("Low", "Medium", "High")) +
  geom_raster(data = aerial.manxie.df,
              aes(x = x,
                  y = y,
                  fill = layer)) +
  ggsn::scalebar(location = "bottomleft",
                 y.min = 5660000,
                 y.max = 5850000,
                 x.min = 110000,
                 x.max = 300000,
                 dist_unit = "km",
                 dist = 20,
                 st.size = 4,
                 st.bottom = F,
                 transform = F,
                 st.dist = .02,
                 st.color = "black") +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = '#dddce3',
               fill = '#454545',
               size = 0.1) +
  labs(fill = "Density", x = "Eastings (m) UTM30", y = "Northings (m)")

ggsave(tracking.kde.plot, filename = "plots/tracking_kde.png", width = 8, height = 6.5)
ggsave(aerial.plot, filename = "plots/aerial_plot.png", width = 8, height = 6.5)


# KDE for Irish colonies
kde.manxie.aerial <- manx.aerial %>%
  # Keep useful variables
  dplyr::select(x, y, mxobserved, gridsizekm2, area_surveyedkm2) %>%
  mutate(weight = mxobserved * (gridsizekm2 / area_surveyedkm2)) %>%
  # Transform to spatial points data frame
  SpatialPointsDataFrame(
    coords = .[c("x", "y")],
    proj4string = CRS("+proj=utm +zone=30N ellps=WGS84"),
    data = .) %>%
  # KDE estimation
  spatialEco::sp.kde(., y = .$weight, bw = 5000, nr = 80, nc = 120,
                     newdata = c(50000, 350000, 5650000, 5850000))


kde.manxie.aerial2 <- raster::aggregate(kde.manxie.aerial, fact = 2, fun = "mean")

plot(kde.manxie.aerial2)

values(kde.manxie.aerial2)[which(is.na(values(kde.manxie2)))] <- NA

stack <- raster::stack(kde.manxie2, kde.manxie.aerial2)

raster::layerStats(stack, stat = "Pearson", na.rm = T)

stack.df <- as.data.frame(rasterToPoints(stack))

ttest.res <- SpatialPack::modified.ttest(x = stack.df$kde.1,
                                         y = (stack.df$kde.2),
                                         coords = stack.df[,c("x", "y")])

ttest.res


