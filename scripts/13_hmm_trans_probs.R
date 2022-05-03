
require(momentuHMM)

cov.df <- data.frame(zsd=seq(from=min(StateModel_zsd$data$zsd), max(StateModel_zsd$data$zsd),by=0.2),
                     zsd_dummy=seq(from=min(StateModel_zsd$data$zsd), max(StateModel_zsd$data$zsd),by=0.2)
)

head(cov.df)


ci.list <- lapply(1:nrow(cov.df), function(x) {
  print(x)
  cov.sub.df <- cov.df[x,]
  return(CIreal(StateModel_zsd,covs=cov.sub.df)$gamma)
})

cov.means <- lapply(ci.list, '[[', 1) 
cov.lb <- lapply(ci.list,'[[',3)
cov.ub <- lapply(ci.list,'[[',4)


mod.means <- unlist(lapply(cov.means,'[[',1)) 
mod.lb <- unlist(lapply(cov.lb,'[[',1))
mod.ub <- unlist(lapply(cov.ub,'[[',1))
mod.1_1_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "a. Rest -> Rest",
                         state_1 = "1",
                         state_2 = "1")

mod.means <- unlist(lapply(cov.means,'[[',4)) 
mod.lb <- unlist(lapply(cov.lb,'[[',4))
mod.ub <- unlist(lapply(cov.ub,'[[',4))
mod.1_2_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "b. Rest -> ARS",
                         state_1 = "1",
                         state_2 = "2")

mod.means <- unlist(lapply(cov.means,'[[',7)) 
mod.lb <- unlist(lapply(cov.lb,'[[',7))
mod.ub <- unlist(lapply(cov.ub,'[[',7))
mod.1_3_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "c. Rest -> Transit",
                         state_1 = "1",
                         state_2 = "3")

mod.means <- unlist(lapply(cov.means,'[[',2)) 
mod.lb <- unlist(lapply(cov.lb,'[[',2))
mod.ub <- unlist(lapply(cov.ub,'[[',2))
mod.2_1_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "d. ARS -> Rest",
                         state_1 = "2",
                         state_2 = "1")

mod.means <- unlist(lapply(cov.means,'[[',5)) 
mod.lb <- unlist(lapply(cov.lb,'[[',5))
mod.ub <- unlist(lapply(cov.ub,'[[',5))
mod.2_2_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "e. ARS -> ARS",
                         state_1 = "2",
                         state_2 = "2")

mod.means <- unlist(lapply(cov.means,'[[',8)) 
mod.lb <- unlist(lapply(cov.lb,'[[',8))
mod.ub <- unlist(lapply(cov.ub,'[[',8))
mod.2_3_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "f. ARS -> Transit",
                         state_1 = "2",
                         state_2 = "3")

mod.means <- unlist(lapply(cov.means,'[[',3)) 
mod.lb <- unlist(lapply(cov.lb,'[[',3))
mod.ub <- unlist(lapply(cov.ub,'[[',3))
mod.3_1_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "g. Transit -> Rest",
                         state_1 = "3",
                         state_2 = "1")

mod.means <- unlist(lapply(cov.means,'[[',6)) 
mod.lb <- unlist(lapply(cov.lb,'[[',6))
mod.ub <- unlist(lapply(cov.ub,'[[',6))
mod.3_2_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "h. Transit -> ARS",
                         state_1 = "3",
                         state_2 = "2")

mod.means <- unlist(lapply(cov.means,'[[',9)) 
mod.lb <- unlist(lapply(cov.lb,'[[',9))
mod.ub <- unlist(lapply(cov.ub,'[[',9))
mod.3_3_df <- data.frame(zsd=cov.df$zsd,
                         mean=mod.means,
                         lower_bound=mod.lb,
                         upper_bound=mod.ub,
                         trans = "i. Transit -> Transit",
                         state_1 = "3",
                         state_2 = "3")

mod.trans.df <- rbind(mod.1_1_df, mod.1_2_df, mod.1_3_df,
                      mod.2_1_df, mod.2_2_df, mod.2_3_df,
                      mod.3_1_df, mod.3_2_df, mod.3_3_df)

require(wesanderson)
pal = wes_palette(name = "GrandBudapest1")

# all together
state_trans_plot <- ggplot(mod.trans.df, aes(x = zsd, fill = state_1, y = mean)) +
  #ylim(0.5, 1)+ # can adjust depending on range of values
  theme_minimal() + ylab("Transition probability") +
  xlab("Secchi disk depth (m)") +
  scale_x_continuous(limits=c(0, 17)) + #xlab("Wind speed (ms-1)")+
  theme(legend.position = "none",
        axis.line = element_line(),
        axis.ticks = element_line(),
        strip.background = element_rect(size = 0, fill = "light grey")) +
  facet_wrap(facets = ~trans) +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
    geom_ribbon(size = 1.3, aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.3) + 
    geom_line(size = 1, aes(colour = state_1))
  
ggsave(state_trans_plot, filename = "plots/state_trans_plot.png", width = 6, height = 5, dpi = 500)



mod.2_2_df$trans <- "c. ARS -> ARS"
mod.3_2_df$trans <- "b. Transit -> ARS"
mod.3_3_df$trans <- "a. Transit -> Transit"

mod.trans.df <- rbind(mod.2_2_df, mod.3_2_df, mod.3_3_df)

# all together
state_trans_plot_res <- ggplot(mod.trans.df, aes(x = zsd, fill = trans, y = mean)) +
  #ylim(0.5, 1)+ # can adjust depending on range of values
  theme_minimal() + ylab("Transition probability") +
  xlab("Secchi disk depth (m)") +
  scale_x_continuous(limits=c(0, 17)) + #xlab("Wind speed (ms-1)")+
  theme(legend.position = "none",
        axis.line = element_line(),
        axis.ticks = element_line(),
        strip.background = element_rect(size = 0, fill = "light grey")) +
  facet_wrap(facets = ~trans, nrow = 1, scales = "free_y") +
  scale_fill_brewer(palette = "Dark2") +
  scale_colour_brewer(palette = "Dark2") +
  geom_ribbon(size = 1.3, aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.3) + 
  geom_line(size = 1, aes(colour = trans))

ggsave(state_trans_plot_res, filename = "plots/state_trans_plot_restricted.png", width = 6, height = 3, dpi = 500)
