df_HL<-readRDS("df_effcos_HL.rds")
df_LL<-readRDS("df_effcos_LL.rds")
df_H<-readRDS("df_effcos_H.rds")

df<-data.frame(cost=c(df_HL$p,df_LL$p,df_H$p),
           eff=c(df_HL$eff,df_LL$eff,df_H$eff),
           wtp=c(df_HL$wtp,df_LL$wtp,df_H$wtp),
           group=c(rep("Low fatigue-High replacement",length(df_HL$p)),rep("Low fatigue-Low replacement",length(df_LL$p)),rep("High fatigue",length(df_H$p))))

ggplot(df, aes(cost, eff, z = wtp)) + 
  geom_contour_filled() + 
  facet_grid(.~ group )+
  xlab("cost (£)") +
  ylab("efficacy") + 
  labs(fill = "probability of cost effectiveness")+
  scale_y_continuous(breaks=c(0.1,0.3,0.5,0.7,0.9),labels = c(0.1,0.3,0.5,0.7,0.9),limits = c(0.1,0.9))+
  scale_x_continuous(labels = scales::comma_format()) +
  theme(axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        axis.text.y = element_text(size=18),
        axis.text.x = element_text(size=18),
        strip.text.x = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size=18))
