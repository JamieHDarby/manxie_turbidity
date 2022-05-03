

track_plot <- 
  ggplot(manxie_ls) +
  coord_cartesian(xlim = c(-50000, 500000),
                  ylim = c(5650000, 6200000)) +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = '#262624',
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
        panel.background = element_rect(fill = "#007371"),
        panel.grid = element_blank()) +
  geom_path(aes(x = x, y = y, group = trip_id), colour = "white",
            alpha = 0.6, size = 0.8) + 
  geom_point(aes(x = 254695, y = 5782789),
             fill = "blue", size = 4, shape = 23)

dive_plot <- 
  ggplot(pos_dive_df) +
  coord_cartesian(xlim = c(-50000, 500000),
                  ylim = c(5650000, 6200000)) +
  geom_polygon(data = land_df_utm30,
               aes(x = x,
                   y = y,
                   group = group),
               color = '#dddce3',
               fill = '#262624',
               size = 0.1) +
  theme_minimal() +
  theme(aspect.ratio = 1,
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  geom_path(aes(x = x, y = y, group = trip_id), 
            colour = "dark grey", size = 0.6) +
  geom_point(aes(x = x, y = y, size = dives),
             alpha = 0.2, colour = "dark red") +
  scale_size_continuous(range = c(0, 4),
                        breaks = c(0, 3, 6, 9, 12), limits = c(1, 12)) +
  labs(size = "Dives /\nTrack\nPoint")

track_dive_plot <- 
  cowplot::plot_grid(track_plot, dive_plot, rel_widths = c(0.8, 1))

ggsave(track_dive_plot, dpi = 500, height = 4, width = 9,
       filename = "plots/track_dive_plot.png")



viz_ob <- getViz(mod)
mgcViz::l_fitRaster()
p1 <- plot(sm(viz_ob, select = 1)) +
  l_fitRaster() +
  l_fitContour(binwidth = 0.5) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "Effect on dive rate (per location)") +
  theme(legend.title = element_blank())

p2 <- plot(sm(viz_ob, select = 3)) +
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Time of day (hours from midnight)",
       y = "Effect on dive rate\n(per location)") +
  scale_x_continuous(limits = c(0, 24),
                     breaks = c(0, 6, 12, 18, 24))

p3 <- plot(sm(viz_ob, select = 4)) +
  l_fitLine(linetype = 1)  +
  l_points() +
  l_ciLine(linetype = 3) +
  labs(y = "Effect of individual on dive rate\n(per location)", title = NULL)

mod_effects <- gridPrint(p1, gridPrint(p2, p3, nrow = 1), nrow = 2)

ggsave(mod_effects, filename = "plots/effects_plot_rate.png",
       dpi = 500, width = 8, height = 8)

ggplot(mod_df) + geom_violin(aes(y = zsd, fill = as.factor(dive_bool),
                                 x = as.factor(dive_bool)))
