viz_ob <- getViz(mod2)

p1 <- plot(sm(viz_ob, select = 1)) +
  l_fitRaster() +
  l_fitContour(binwidth = 0.5) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "Effect on dive depth (m)") +
  theme(legend.title = element_blank())

p2 <- plot(sm(viz_ob, select = 2)) +
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Cloud cover (%)",
       y = "Effect on dive depth (m)")
  

p3 <- plot(sm(viz_ob, select = 3)) + 
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Time of day (hour)", y = "Effect on dive depth (m)") +
  scale_x_continuous(limits = c(5, 24),
                     breaks = c(6, 12, 18, 24))

p4 <- plot(sm(viz_ob, select = 5)) +
  l_fitLine(linetype = 1)  +
  l_points() +
  l_ciLine(linetype = 3) +
  labs(y = "Effect of individual on dive depth (m)", title = NULL)

mod_effects <- gridPrint(p1, p2, p3, p4, nrow = 2)

ggsave(mod_effects, filename = "plots/effects_plot_depth.png",
       dpi = 500, width = 8, height = 6)

ggplot(dive_pos_meta) + geom_density(aes(x = zsd))

qq(viz_ob, rep = 50,
   showReps = T, CI = "none",
   a.qqpoi = list("shape" = 19),
   a.replin = list("alpha" = 0.2))


state_dive_plot <- ggplot() + 
  geom_errorbar(aes(x = c("Rest", "ARS", "Transit"),
                    y = c(13.07, 12.7, 9.83),
                    ymin = c(11.9812, 11.7451, 8.5749),
                    ymax = c(14.1534, 13.6565, 11.0895)),
                size = 1.2, colour = "Black") +
  geom_point(aes(x = c("Rest", "ARS", "Transit"),
                 y = c(13.07, 12.7, 9.83)),
             size = 5, colour = "Black", shape = 15) +
  labs(x = NULL, y = "Dive depth (m)", colour = NULL) +
  theme_minimal()

ggsave(state_dive_plot, filename = "plots/state_dive_plot.png",
       width = 5, height = 5, dpi = 500)
