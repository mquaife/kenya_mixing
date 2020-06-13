#defaulting titles to centre
theme_update(plot.title = element_text(hjust = 0.5))

contacts<- read.csv("Data/imputed_justcategory_60plus.csv")
#only contacts that we have individual-level detail on
contacts_det<-subset(contacts, contacts$contact_detail==1)
contacts_hh<-subset(contacts, contacts$contact_kind=="Household Contacts")
contacts_nonhh<-subset(contacts, contacts$contact_kind=="Non-Household Contacts" & contacts$contact_detail==1)

#recoding a little to make interpretation easier
contacts_nonhh$q5_place_contact[contacts_nonhh$q5_place_contact=="At a place of leisure such as a restaurant, bar/koroga"]<-"Other indoor place"
contacts_nonhh$q5_place_contact[contacts_nonhh$q5_place_contact=="Community Building"]<-"Other indoor place"
contacts_nonhh$q5_place_contact[contacts_nonhh$q5_place_contact=="Medical setting (e.g. hospital)"]<-"Other indoor place"
contacts_nonhh$q5_place_contact[contacts_nonhh$q5_place_contact=="Bodaboda"]<-"Other outdoor place"
unique(contacts_nonhh$q5_place_contact)
contacts_nonhh$q5_place_contact[contacts_nonhh$q5_place_contact=="At home or someone elseâ???Ts house"]<-"A house"

contacts_nonhh$q7_masks[contacts_nonhh$q7_masks=="Respondent only"]<-"Me only"
contacts_nonhh$q7_masks[contacts_nonhh$q7_masks=="Contact only"]<-"Other person only"

#Household contacts
hh_sex<-ggplot(data = contacts_hh) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q3_sex, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="Percentage of contacts", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Contact gender")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))
  hh_sex

hh_phys<-ggplot(data = contacts_hh) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q4_contacttype, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Contact type")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))

#pulling together into one fig
#hh_figs <- ggarrange(hh_sex, hh_phys,
                   # labels = c("A", "B"),
                   # ncol = 2, nrow = 1)
#hh_figs

hh_figs<-cowplot::plot_grid(hh_sex, hh_phys, align = "h", ncol = 5)
hh_figs

#Non-household contacts
nonhh_sex<-ggplot(data = contacts_nonhh) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q3_sex, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="Percentage of contacts", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Contact gender")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))
nonhh_sex

nonhh_phys<-ggplot(data = contacts_nonhh) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q4_contacttype, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Contact type")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))

unique(contacts_nonhh$q5_place_contact)
contacts_nonhh$q5_place_contact<-factor(contacts_nonhh$q5_place_contact,levels = c("A house","Shop","Work (outdoor) ", "Work (indoor)","Market","Other outdoor place","Other indoor place"))
nonhh_place<-ggplot(data = contacts_nonhh) +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q5_place_contact, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Contact place")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))
nonhh_place

unique(contacts_nonhh$q6_duration)
contacts_nonhh$q6_duration<-factor(contacts_nonhh$q6_duration,levels = c("< 5 mins","5 - 14 mins","15 - 59 mins","1 - 4 hours","More than 4 hours"))
nonhh_duration<-ggplot(data = contacts_nonhh) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q6_duration, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Contact duration")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))
nonhh_duration

unique((contacts_nonhh$q6_duration
        ))

nonhh_masks<-ggplot(data = contacts_nonhh) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = q7_masks, y = ..prop.., group = 1), stat = "count")+
  scale_y_continuous(name="", labels = scales::percent_format(accuracy = 1))+  scale_x_discrete(name="")+  
  ggtitle("Mask wearing")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_cartesian(ylim = c(0, .85))
nonhh_masks

#pulling together into one fig
#figure_nonhh <- ggarrange(nonhh_sex, nonhh_phys,nonhh_masks, nonhh_place,nonhh_duration,
                       #labels = c("A", "B","C","D","E"),
                       #ncol = 5, nrow = 1)
#figure_nonhh

#cowplot makes a nicer horizontal axis alignment
nonhh_figs<-cowplot::plot_grid(nonhh_sex, nonhh_phys,nonhh_masks, nonhh_place,nonhh_duration, align = "h", ncol = 5)

figure_contact_chars <- ggarrange(hh_figs,nonhh_figs,
                          labels = c("A", "B"),  vjust=0.7, hjust=0,
                          ncol = 1, nrow = 2)
figure_contact_chars

ggsave(filename = "outputs/fig2_contact_chars.png", figure_contact_chars, width = 13, height = 6.5, dpi = 1000)

