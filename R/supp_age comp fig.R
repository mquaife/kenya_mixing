f1<-ggplot(data = population_data_kenya) + 
  geom_bar(mapping = aes(x = age, y = total, group = 1),stat="identity")+
  labs(
    x = "Age",
    y = "Number of people",
    title = "A - Kenya country-level") 

f2<-ggplot(data = population_data_kilifi) + 
  geom_bar(mapping = aes(x = age, y = total, group = 1),stat="identity")+
  labs(
    x = "Age",
    y = "Number of people",
    title = "B - Kilifi") 

f3<-ggplot(data = population_data_informal) + 
  geom_bar(mapping = aes(x = age, y = total, group = 1),stat="identity")+
  labs(
    x = "Age",
    y = "Number of people",
    title = "C - Informal settlements") 

pop_comp<-ggarrange(f1, f2,f3,
                    labels = c("", "", ""),
                    ncol = 3, nrow = 1)
pop_comp

ggsave(filename = "C:/Users/matt_/Google Drive/Research/COVID/Pop council/kenyamix/outputs/supp_pop-comp.png", pop_comp, width = 10, height = 5, dpi = 1000)
