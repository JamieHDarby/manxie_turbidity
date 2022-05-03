
zsd <- raster::stack(
  "data/env/dataset-oc-glo-opt-multi-l4-zsd_interpolated_4km_daily-rt_1643023109817.nc")

NAvalue(zsd) <- 32767

zsd_old <- raster::stack(
  "data/env/dataset-oc-glo-opt-multi-l4-zsd_interpolated_4km_daily-rep_1649333044855.nc")

NAvalue(zsd_old) <- 32767

zsd_mean <- raster::mean(zsd)
zsd_old_mean <- raster::mean(zsd_old)

total_extext <- raster::intersect(zsd_mean, zsd_old_mean)

plot(total_extext)

zsd_mean <- raster::crop(zsd_mean, total_extext)
zsd_old_mean <- raster::crop(zsd_old_mean, total_extext)

zsd_all <- raster::mean(zsd_mean, zsd_old_mean)

plot(zsd_all)

zsd_all_utm <- projectRaster(zsd_all, crs = CRS("+init=epsg:32630"))

zsd_all_df <- rasterToPoints(zsd_all_utm) %>%
  as.data.frame()
  
track_plot <-
  ggplot(manxie_ls) +
  coord_cartesian(xlim = c(-50000, 500000),
                  ylim = c(5650000, 6200000)) +
  geom_raster(data = zsd_all_df,
              aes(x = x, y = y, fill = layer)) +
    scale_fill_viridis_c(trans = "reverse",
                         option = "D",
                         limits = c(30, 0),
                         values = c(0, 1.6)) +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = 'dark grey',
               fill = '#262624',
               size = 0.1) +
  ggsn::north(location = "topleft",
              symbol = 4,
              scale = 0.1,
              x.min = -50000,
              x.max = 500000,
              y.min = 5650000,
              y.max = 6200000) +
  ggsn::scalebar(location = "bottomleft",
                 y.min = 5650000,
                 y.max = 6200000,
                 x.min = -50000,
                 x.max = 500000,
                 dist_unit = "km",
                 dist = 100,
                 st.size = 4,
                 st.bottom = F,
                 transform = F,
                 st.dist = .03,
                 st.color = "black") +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey"),
        panel.grid = element_blank(),
        legend.position = "none") +
  geom_path(aes(x = x, y = y, group = trip_id), colour = "white",
            alpha = 0.6, size = 0.8) + 
  geom_point(aes(x = 254695, y = 5782789),
             fill = "blue", size = 4, shape = 23)

dive_plot <-
  ggplot(pos_dive_df) +
  coord_cartesian(xlim = c(-50000, 500000),
                  ylim = c(5650000, 6200000)) +
  geom_raster(data = zsd_all_df,
              aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(trans = "reverse",
                       option = "D",
                       limits = c(30, 0),
                       values = c(0, 1.6)) +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = 'dark grey',
               fill = '#262624',
               size = 0.1) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank()) +
  geom_path(aes(x = x, y = y, group = trip_id), 
            colour = "white", size = 0.8, alpha = 0.6) +
  geom_point(aes(x = x, y = y, size = dives),
             alpha = 0.2, colour = "dark red") +
  scale_size_continuous(range = c(0, 4),
                        breaks = c(0, 3, 6, 9, 12), limits = c(1, 12)) +
  labs(size = "Dives /\nTrack\nPoint", fill = "Mean\nZsd (m)")

track_dive_plot <- 
  cowplot::plot_grid(track_plot, dive_plot, rel_widths = c(1, 1.2))

track_dive_plot

ggsave(track_dive_plot, dpi = 500, height = 4, width = 9,
       filename = "plots/track_dive_plot.png")
