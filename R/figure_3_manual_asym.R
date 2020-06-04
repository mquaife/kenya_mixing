#Figure 3------------------------------------------------------------------------------------------------------------------------------------------------------------------

#mage age cat lits
age_labels<-c("0-17" = "0-17", "18-59" = "18-59","60-120" = "60+")
#set titles in centre of all plots
theme_update(plot.title = element_text(hjust = 0.5))


manual_matrix[1,1]<-NA
manual_matrix[2,1]<-NA
manual_matrix[3,1]<-NA
manual_plot <- melt(manual_matrix, varnames = c("age1", "age2"), value.name = "contacts")
manual_matrix_hh[1,1]<-NA
manual_matrix_hh[2,1]<-NA
manual_matrix_hh[3,1]<-NA
manual_plot_hh <- melt(manual_matrix_hh, varnames = c("age1", "age2"), value.name = "contacts")
#manual_plot_hh_S1<- kenya_plot_hh_S1[-c(10,11,12,13,14,15), ]
manual_matrix_nonhh[1,1]<-NA
manual_matrix_nonhh[2,1]<-NA
manual_matrix_nonhh[3,1]<-NA
manual_plot_nonhh <- melt(manual_matrix_nonhh, varnames = c("age1", "age2"), value.name = "contacts")
#manual_plot_nonhh_S1<- kenya_plot_nonhh_S1[-c(10,11,12,13,14,15), ]
manual_matrix_phys[1,1]<-NA
manual_matrix_phys[2,1]<-NA
manual_matrix_phys[3,1]<-NA
manual_plot_phys <- melt(manual_matrix_phys, varnames = c("age1", "age2"), value.name = "contacts")
#manual_plot_phys_S1<- kenya_plot_phys_S1[-c(10,11,12,13,14,15), ]
manual_matrix_nonphys[1,1]<-NA
manual_matrix_nonphys[2,1]<-NA
manual_matrix_nonphys[3,1]<-NA
manual_plot_nonphys <- melt(manual_matrix_nonphys, varnames = c("age1", "age2"), value.name = "contacts")
#manual_plot_nonphys_S1<- kenya_plot_nonphys_S1[-c(10,11,12,13,14,15), ]

manual_plot_fig<-ggplot(manual_plot, aes(x = age2, y = age1, fill = contacts)) + 
  ggtitle("A - Aggregate mixing matrix")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
     na.value = "gray",
    values = c(0, 2, 4, 10, 24)/24,
    limits = c(0, 20))
manual_plot_fig

hh<-ggplot(manual_plot_hh, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
ggtitle("B - Household contacts")+
  theme_bw() +
theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
     na.value = "gray",
    values = c(0, 2, 4, 10, 24)/24,
    limits = c(0, 20))
hh

nonhh<-ggplot(manual_plot_nonhh, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  ggtitle("C - Non-household contacts")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
     na.value = "gray",
    values = c(0, 2, 4, 10, 24)/24,
    limits = c(0, 20))
nonhh


phys<-ggplot(manual_plot_phys, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  ggtitle("D - Physical contacts")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
     na.value = "gray",
    values = c(0, 2, 4, 10, 24)/24,
    limits = c(0, 20))
phys


nonphys<-ggplot(manual_plot_nonphys, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  ggtitle("E - Non-physical contacts")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
     na.value = "gray",
    values = c(0, 2, 4, 10, 24)/24,
    limits = c(0, 20))
nonphys


p1<-manual_plot_fig
p2 <- hh
p3 <- nonhh
p4 <- phys
p5 <- nonphys


adjusted_matrices_fig<-grid.arrange(
  grobs = list(p1, p2, p3,p4,p5),
  # widths = c(1, 1, 1),
  layout_matrix = rbind(c(1, 2, 3),
                        c(1, 4, 5))
)



ggsave(filename = "outputs/fig_3_asym_.png", adjusted_matrices_fig, width = 13, height = 6.5, dpi = 1000)

