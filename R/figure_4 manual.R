#Figure 3------------------------------------------------------------------------------------------------------------------------------------------------------------------
#make age cat labs
#age_labels<-c("[0,18)" = "0-17", "[18,50)" = "18-50","50+" = "50+")
age_labels<-c("0-17" = "0-17", "18-49" = "18-49","50-120" = "50+")

#set titles in centre of all plots
theme_update(plot.title = element_text(hjust = 0.5))

##########################################################
#
#                 Importing data to make figs with
#
##########################################################

#assuming 1-3 run

##########################################################
#
#                 Coding for plots
#
##########################################################


#plot physical contacts only
manual_50plus_phys_mean[,1]<-NA
plot_physonly <- melt(manual_50plus_phys_mean, varnames = c("age1", "age2"), value.name = "contacts")
phys_plot<-ggplot(plot_physonly, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  ggtitle("A - Current study - physical contacts only")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+
  scale_y_discrete(labels=age_labels)+xlab("Contact age") + ylab("Respondent age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 1, 3, 5, 12)/12,
    limits = c(0, 14))
phys_plot

#plot kiti B
plot_kiti <- melt(adjusted_matrix_kiti, varnames = c("age1", "age2"), value.name = "contacts")
kiti_plot<-ggplot(plot_kiti, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  theme_bw() +
  ggtitle("B - Adjusted physical matrix from Kiti et al. (2014)")+
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+
 scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 1, 3, 5, 12)/12,
    limits = c(0, 14))
kiti_plot

#Plotting age-imputed matrix (Kiti) C
plot_allages_kiti <- melt(manual_matrix_adj_by_kiti, varnames = c("age1", "age2"), value.name = "contacts")
plot_imputed_kiti<-ggplot(plot_allages_kiti, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  theme_bw() +
  ggtitle("C - Imputed physical matrix using Kiti et al. (2014)")+
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 1, 3, 5, 12)/12,
    limits = c(0, 14))
plot_imputed_kiti

#plot all contacts D
manual_50plus_mean[,1]<-NA
plot_physonly <- melt(manual_50plus_mean, varnames = c("age1", "age2"), value.name = "contacts")
all_plot<-ggplot(plot_physonly, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  ggtitle("D - Current study - all contacts")+
  theme_bw() +
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+
  scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 4, 12, 20, 48)/48,
    limits = c(0, 50))
all_plot

#plot Prem E
plot_prem <- melt(adjusted_prem_matrix_new, varnames = c("age1", "age2"), value.name = "contacts")
prem_plot<-ggplot(plot_prem, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  theme_bw() +
  ggtitle("E - Adjusted matrix from Prem et al (2020)")+
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ 
  xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 4, 12, 20, 48)/48,
    limits = c(0, 50))
prem_plot

#Plotting age-imputed matrix (Prem) F
plot_allages_prem <- melt(manual_matrix_adj_by_prem_new, varnames = c("age1", "age2"), value.name = "contacts")
plot_imputed_prem<-ggplot(plot_allages_prem, aes(x = age2, y = age1, fill = contacts)) + theme(legend.position = "bottom") + 
  theme_bw() +
  ggtitle("F - Imputed matrix using Prem et al. (2020)")+
  theme(legend.position = "bottom") + 
  labs(fill="Contacts")+scale_x_discrete(labels=age_labels)+ scale_y_discrete(labels=age_labels)+xlab("Respondent age") + ylab("Contact age")+
  geom_tile()+
  geom_text(aes(label = round(contacts,digits=1))) +
  scale_fill_gradientn(
    colors=c("#0D5257","#00BF6F", "#FFB81C"),
    na.value = "gray",
    values = c(0, 4, 12, 20, 48)/48,
    limits = c(0, 50))
plot_imputed_prem

#name part of the plotting grid
p1 <- phys_plot
p2 <- kiti_plot
p3 <- plot_imputed_kiti
p4 <- all_plot
p5 <- prem_plot
p6 <- plot_imputed_prem


#arrange as one big and four little
adjusted_matrices_fig<-grid.arrange(
  grobs = list(p1, p2, p3,p4,p5,p6),
  # widths = c(1, 1, 1),
  layout_matrix = rbind(c(1, 2, 3),
                        c(4, 5, 6))
)

ggsave(filename = "outputs/Fig_4_adjusted_matrices.png", adjusted_matrices_fig, width = 15, height = 6.5, dpi = 1000)
