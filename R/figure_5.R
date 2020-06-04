#Figure 5

type = c(
  rep("Kiti et al. (2014)", length(adjusted_r0_kiti))
)
values <- c(
  adjusted_r0_kiti
)
adj_r0_df<- data.frame(r_values, type, values)
#Plotting change in R
density_kiti<-ggplot(adj_r0_df) +
  geom_density((aes(x=values, fill=type)),alpha = 0.8, position = "identity") +
  theme_bw() +
  labs(
    x = expression("Estimate of" ~ R[0]),
    y = "Density",
    title = "A - Physical contacts") +
  scale_fill_manual(
    name="Comparison",
    values=c("#996666","#6d979a", "#7ca1a4")
  ) +
  scale_x_continuous(
    limits = c(0.0, 1.25),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25)
  ) +
  geom_vline(xintercept = 1, col = "black") +
  theme(
    legend.position = c(0.19, 0.7),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )

#bottom panel
#Figure 5

type = c(
  rep("Prem et al. (2020)", length(adjusted_r0_prem_new)),
  rep("Prem et al. (2017)", length(adjusted_r0_prem_old))
)
values <- c(
  adjusted_r0_prem_new, 
  adjusted_r0_prem_old
)
adj_r0_df<- data.frame(r_values, type, values)
#Plotting change in R
density_prem<-ggplot(adj_r0_df) +
  geom_density((aes(x=values, fill=type)),alpha = 0.8, position = "identity") +
  theme_bw() +
  labs(
    x = expression("Estimate of" ~ R[0]),
    y = "Density",
    title = "B - All contacts") +
  scale_fill_manual(
    name="Comparison",
    values=c("#a47ba1", "#7ca1a4")
  ) +
  scale_x_continuous(
    limits = c(0.0, 1.25),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25)
  ) +
  geom_vline(xintercept = 1, col = "black") +
  theme(
    legend.position = c(0.19, 0.7),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  ) 

#name part of the plotting grid
p1 <- density_kiti
p2 <- density_prem


#arrange as one big and four little
r0_estimates<-grid.arrange(
  grobs = list(p1, p2),
  # widths = c(1, 1, 1),
  layout_matrix = rbind(c(1),
                        c(2))
)

ggsave(filename = "C:/Users/matt_/Google Drive/Research/COVID/Pop council/kenyamix/outputs/Fig_5_r0_estimates.png", r0_estimates, width = 5, height = 9, dpi = 1000)
r0_estimates
