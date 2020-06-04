#Figure 1------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Import wide data
wide_data<-data.frame(read.csv("Data/contacts_pseudowide.csv"))
wide_data_hh<-data.frame(read.csv("Data/contacts_pseudowide_hh.csv"))
wide_data_nonhh<-data.frame(read.csv("Data/contacts_pseudowide_nonhh.csv"))
mean(wide_data$n_contacts)
mean(wide_data_hh$n_contacts)

mean(wide_data_hh$n_contacts)
median(wide_data_hh$n_contacts)
quantile(wide_data_hh$n_contacts)
mean(wide_data_nonhh$n_contacts)
median(wide_data_nonhh$n_contacts)
quantile(wide_data_nonhh$n_contacts)

#making cat variable for hh_size
wide_data$hh_size_cat[wide_data$hh_size==1]<-"1"
wide_data$hh_size_cat[wide_data$hh_size==2]<-"2"
wide_data$hh_size_cat[wide_data$hh_size==3]<-"3"
wide_data$hh_size_cat[wide_data$hh_size==4]<-"4"
wide_data$hh_size_cat[wide_data$hh_size==5]<-"5"
wide_data$hh_size_cat[wide_data$hh_size==6]<-"6"
wide_data$hh_size_cat[wide_data$hh_size>=7]<-"7+"

wide_data_hh$hh_size_cat[wide_data_hh$hh_size==1]<-"1"
wide_data_hh$hh_size_cat[wide_data_hh$hh_size==2]<-"2"
wide_data_hh$hh_size_cat[wide_data_hh$hh_size==3]<-"3"
wide_data_hh$hh_size_cat[wide_data_hh$hh_size==4]<-"4"
wide_data_hh$hh_size_cat[wide_data_hh$hh_size==5]<-"5"
wide_data_hh$hh_size_cat[wide_data_hh$hh_size==6]<-"6"
wide_data_hh$hh_size_cat[wide_data_hh$hh_size>=7]<-"7+"

wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size==1]<-"1"
wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size==2]<-"2"
wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size==3]<-"3"
wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size==4]<-"4"
wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size==5]<-"5"
wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size==6]<-"6"
wide_data_nonhh$hh_size_cat[wide_data_nonhh$hh_size>=7]<-"7+"

#making cat variable for part age
wide_data$part_age_cat[wide_data$part_age>=18 & wide_data$part_age<=29]<-"18-29"
wide_data$part_age_cat[wide_data$part_age>=30 & wide_data$part_age<=39]<-"30-39"
wide_data$part_age_cat[wide_data$part_age>=40 & wide_data$part_age<=49]<-"40-49"
wide_data$part_age_cat[wide_data$part_age>=50 & wide_data$part_age<=59]<-"50-59"
wide_data$part_age_cat[wide_data$part_age>=60 ]<-"60+"

#binning none and primary education together
wide_data$education[wide_data$education=="None"]<-"Primary School"
wide_data$education[wide_data$education=="Primary School"]<-"None / Primary only"
wide_data$education[wide_data$education=="Higher education"]<-"Higher"

#calculates stats for the median text
summ_age <- wide_data %>% 
  group_by(part_age_cat) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))
summ_hhsize <- wide_data %>% 
  group_by(hh_size_cat) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))
summ_hhsize_hh <- wide_data_hh %>% 
  group_by(hh_size_cat) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))
summ_hhsize_nonhh <- wide_data_nonhh %>% 
  group_by(hh_size_cat) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))
summ_sex <- wide_data %>% 
  group_by(q100_sex) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))
summ_education <- wide_data %>% 
  group_by(education) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))
summ_ses <- wide_data %>% 
  group_by(quintile_s1) %>% 
  summarize(mean = mean(n_contacts), median = median(n_contacts), sd = sd(n_contacts))





#plots each group of interest
age_fig<-ggplot(wide_data, aes(part_age_cat,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  #stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="black") + #adds average dot
  geom_text(data = summ_age, aes(x = part_age_cat, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Participant Age")+
  scale_y_continuous(name="Number of contacts")+
  coord_cartesian(ylim = c(0, 60))
age_fig

hh_size_fig<-ggplot(wide_data, aes(hh_size_cat,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_hhsize, aes(x = hh_size_cat, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Household size")+
  scale_y_continuous(name="Number of contacts")+
  coord_cartesian(ylim = c(0, 60))
hh_size_fig

sex_fig<-ggplot(wide_data, aes(q100_sex,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_sex, aes(x = q100_sex, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Sex")+
  scale_y_continuous(name="Number of contacts")+
  coord_cartesian(ylim = c(0, 60))
sex_fig


wide_data$education<-factor(wide_data$education,levels = c("None / Primary only","Secondary","Higher"))
education_fig<-ggplot(wide_data, aes(education,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_education, aes(x = education, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Education level")+
  scale_y_continuous(name="Number of contacts")+
  coord_cartesian(ylim = c(0, 60))
education_fig

wide_data$quintile_s1<-factor(wide_data$quintile_s1,levels = c("1","2","3","4","5"))
ses_fig<-ggplot(wide_data, aes(quintile_s1,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_ses, aes(x = quintile_s1, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Socioeconomic quintile (1=poorest, 5=richest)")+
  scale_y_continuous(name="Number of contacts")+
  coord_cartesian(ylim = c(0, 60))
ses_fig

hist_plot<-ggplot(data = wide_data) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_bar(mapping = aes(x = n_contacts, y = ..count.., group = 1), stat = "count")+
  scale_y_continuous(name="Number of respondents")+  scale_x_continuous(name="Number of contacts")+  
  ggtitle("Distribution of contacts")+theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_vline(xintercept = 17.88,color="black")+
  geom_text(aes(x=19, label="mean: 18", y=12), colour="black", angle=90)
hist_plot

figure_sex_age_hhsize_education <- ggarrange(ses_fig,sex_fig, age_fig,education_fig,hh_size_fig,hist_plot,
                                             labels = c("A", "B", "C","D","E","F"),
                                             ncol = 2, nrow = 3)
ggsave(filename = "outputs/fig_1_contactsbychars.png", figure_sex_age_hhsize_education, width = 10, height = 15, dpi = 1000)







#SPlitting household size within and outside of household, but not super clear what interpretation of small N are
hh_size_fig_hh<-ggplot(wide_data_hh, aes(hh_size_cat,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_hhsize_hh, aes(x = hh_size_cat, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Household size")+
  scale_y_continuous(name="Number of household contacts")+
  coord_cartesian(ylim = c(0, 60))
hh_size_fig_hh

hh_size_fig_nonhh<-ggplot(wide_data_nonhh, aes(hh_size_cat,n_contacts))+
  theme_bw() +
  geom_boxplot(outlier.shape=NA)+
  geom_text(data = summ_hhsize_nonhh, aes(x = hh_size_cat, y = median, label = paste(median)),position =position_nudge(y=3) )+
  scale_x_discrete(name= "Household size")+
  scale_y_continuous(name="Number of non-household contacts")+
  coord_cartesian(ylim = c(0, 60))
hh_size_fig_nonhh

#places all six on one 2x2 figure
png('figure_sex_age_hhsize_education.png')
figure_sex_age_hhsize_education <- ggarrange(ses_fig,sex_fig, age_fig,education_fig,hh_size_fig,hh_size_fig_hh,hh_size_fig_nonhh,
                                             labels = c("A", "B", "C","D","E","F"),
                                             ncol = 2, nrow = 3)
figure_sex_age_hhsize_education
dev.off()