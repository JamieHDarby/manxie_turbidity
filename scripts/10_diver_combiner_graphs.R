
# Get mgcViz to extract smooths to plot from the model --------------------

viz_ob <- getViz(mod2)

p1 <- plot(sm(viz_ob, select = 1)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 0.5) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "a) Dive depth (m)") +
  theme(legend.title = element_blank())

p2 <- plot(sm(viz_ob, select = 2)) +
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Cloud cover (%)",
       y = "Dive depth (m)",
       title = "b)")
  

p3 <- plot(sm(viz_ob, select = 3)) + 
  l_fitLine(linetype = 1)  +
  l_ciPoly(fill = "#162c4d", alpha = 0.7) +
  l_ciLine(linetype = 3) +
  l_rug() +
  labs(x = "Time of day (hour)", y = "Dive depth (m)",
       title = "c)") +
  scale_x_continuous(limits = c(0, 24),
                     breaks = c(0, 6, 12, 18, 24))

p4 <- plot(sm(viz_ob, select = 4)) +
  l_fitLine(linetype = 1)  +
  l_points() +
  l_ciLine(linetype = 3) +
  labs(y = "Effect of individual on dive depth (m)", title = "d)")

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
                    y = c(12.96, 12.77, 10.09),
                    ymin = c(12.03, 11.99, 9.07),
                    ymax = c(13.89, 13.55, 11.11)),
                size = 1.2, colour = "Black") +
  geom_point(aes(x = c("Rest", "ARS", "Transit"),
                 y = c(13.07, 12.7, 9.83)),
             size = 5, colour = "Black", shape = 15) +
  labs(x = NULL, y = "Dive depth (m)", colour = NULL) +
  theme_minimal()

ggsave(state_dive_plot, filename = "plots/state_dive_plot.png",
       width = 5, height = 5, dpi = 500)

viz_ob <- getViz(mod3)

p1 <- plot(sm(viz_ob, select = 1)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EA15032") +
  theme(legend.title = element_blank())

p2 <- plot(sm(viz_ob, select = 2)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EF44339") +
  theme(legend.title = element_blank())

p3 <- plot(sm(viz_ob, select = 3)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EG44130") +
  theme(legend.title = element_blank())

p4 <- plot(sm(viz_ob, select = 4)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EG44137") +
  theme(legend.title = element_blank())

p5 <- plot(sm(viz_ob, select = 5)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EN40369") +
  theme(legend.title = element_blank())

p6 <- plot(sm(viz_ob, select = 6)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "ET03928") +
  theme(legend.title = element_blank())

p7 <- plot(sm(viz_ob, select = 7)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02529") +
  theme(legend.title = element_blank())

p8 <- plot(sm(viz_ob, select = 8)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02530") +
  theme(legend.title = element_blank())

p9 <- plot(sm(viz_ob, select = 9)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02532") +
  theme(legend.title = element_blank())

p10 <- plot(sm(viz_ob, select = 10)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02533") +
  theme(legend.title = element_blank())

p11 <- plot(sm(viz_ob, select = 11)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02541") +
  theme(legend.title = element_blank())

p12 <- plot(sm(viz_ob, select = 12)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02542") +
  theme(legend.title = element_blank())

p13 <- plot(sm(viz_ob, select = 13)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02547") +
  theme(legend.title = element_blank())

p14 <- plot(sm(viz_ob, select = 14)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EX02548") +
  theme(legend.title = element_blank())

p15 <- plot(sm(viz_ob, select = 15)) +
  l_fitRaster() +
  scale_fill_viridis(option = "B") +
  l_fitContour(binwidth = 1) +
  labs(x = "Solar angle (degrees)", y = "Secchi disk depth (Zsd, m)",
       title = "EY53457") +
  theme(legend.title = element_blank())

gridPrint(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)

id_effects <- gridPrint(p1, p2, p8, p9, p11, p13, p14, p15, ncol = 2)

ggsave(id_effects, filename = "plots/id_effects_plot_depth.png",
       dpi = 500, width = 6, height = 10)
