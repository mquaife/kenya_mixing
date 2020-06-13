#First need to run code for figs 1-5  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#left - Prem adjusted matrix (new colours)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_imputed_prem_summary<-ggplot(plot_allages_prem, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  theme_bw() +
  ggtitle("A - Contact matrix")+
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Contact age") + ylab("Respondent age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 2, 6, 10, 24)/24,
    limits = c(0, 25))
plot_imputed_prem_summary

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#middle - SES 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ses_fig_summary<-ggplot(wide_data, aes(quintile_s1,n_contacts))+
  theme_bw() +
  ggtitle("A - Contacts by socioeconomic quintile")+
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_ses, aes(x = quintile_s1, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Socioeconomic quintile (1=poorest, 5=richest)")+
  scale_y_continuous(name="Number of contacts")+
  coord_cartesian(ylim = c(0, 50))
ses_fig_summary



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#right - R0 plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    title = "C - Estimated" ~ R[0]*" - Physical contacts") +
  scale_fill_manual(
    name="Pre-COVID-19 comparison",
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
    title = "B - Estimated" ~ R[0]*" - All contacts") +
  scale_fill_manual(
    name="Pre-COVID-19 comparison",
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
r0_estimates_summary<-grid.arrange(
  grobs = list(p2, p1),
  # widths = c(1, 1, 1),
  layout_matrix = rbind(c(1),
                        c(2))
)

r0_estimates_summary

l<-plot_imputed_prem_summary
m<-ses_fig_summary
r<-r0_estimates_summary

#summary_fig<-grid.arrange(
#  grobs = list(l,m,r),
  # widths = c(1, 1, 1),
#  layout_matrix = rbind(c(1, 2, 3))
#)
#  summary_fig
  
  summary_fig_2<-cowplot::plot_grid(m,r, align = "h", ncol = 2)
  summary_fig_2
  
  ggsave(filename = "outputs/kenya_mixing_summary_fig.png", summary_fig_2, width = 12, height = 6, dpi = 1000)
  
