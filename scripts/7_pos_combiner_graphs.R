
# Get mgcViz to extract smooths to plot from the model --------------------

viz_ob <- getViz(mod)

p1 <- plot(sm(viz_ob, select = 1)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 0.5) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "a) Dive rate (per location)") +
  theme(legend.title = element_blank())

p2 <- plot(sm(viz_ob, select = 3)) +
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Time of day (hours from midnight)",
       y = "Dive rate\n(per location)",
       title = "b)") +
  scale_x_continuous(limits = c(0, 24),
                     breaks = c(0, 6, 12, 18, 24))

p3 <- plot(sm(viz_ob, select = 4)) +
  l_fitLine(linetype = 1)  +
  l_points() +
  l_ciLine(linetype = 3) +
  labs(y = "Effect of individual on dive rate\n(per location)", title = "c)")

mod_effects <- gridPrint(p1, gridPrint(p2, p3, nrow = 1), nrow = 2)

ggsave(mod_effects, filename = "plots/effects_plot_rate.png",
       dpi = 500, width = 8, height = 6)

ggplot(mod_df) + geom_violin(aes(y = zsd, fill = as.factor(dive_bool),
                                 x = as.factor(dive_bool)))

dive_timing_plot <- 
  ggplot(mod_df) +
  geom_histogram(aes(x = MOD, weight = dives),
                 fill = "#007371",
                 alpha = 0.5, binwidth = 0.5) +
  geom_smooth(aes(x = MOD, y = (sunangle*3.5)), colour = "black") +
  labs(x = "Hour", y = "Dives") + 
  scale_y_continuous(
    sec.axis = sec_axis(trans =~./3.5, name="Solar angle (degrees)")) +
  scale_x_continuous(limits = c(0, 24),
                     breaks = c(0, 6, 12, 18, 24)) +
  geom_hline(aes(yintercept = 0))

ggsave(dive_timing_plot, filename = "plots/dive_time_plot.png",
       width = 5, height = 3, dpi = 500)

