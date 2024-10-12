#Updated on Sep 3, 2024
#Fig S6 with shielding cost included

rm(list = ls())
library(scales)

para_input<-read.csv(file = "templates/LAAB_PrEP_template_s2_1.csv")
source("scripts/plot.R")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(LYG*k[i] > COST)/length(LYG)
}
df1<-data.frame(buget=k,prob=wtp,group=rep("Low fatigue-High replacement",length(k)))

para_input<-read.csv(file = "templates/LAAB_PrEP_template_s3_1.csv")
source("scripts/plot.R")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(LYG*k[i] > COST)/length(LYG)
}
df3<-data.frame(buget=k,prob=wtp,group=rep("High fatigue",length(k)))

para_input<-read.csv(file = "templates/LAAB_PrEP_template_s1_1.csv")
source("scripts/plot.R")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(LYG*k[i] > COST)/length(LYG)
}
df4<-data.frame(buget=k,prob=wtp,group=rep("Low fatigue-Low replacement",length(k)))


df<-rbind(df1,df3,df4)
p1<-ggplot(data=df,aes(x=buget,y=prob,group=group,colour=group,fill=group))+
  geom_line(lwd=1.5)+
  geom_hline(yintercept=0.9, linetype="dashed", color = "brown",size=1)+
  geom_vline(xintercept = 30000, color = "red", size=1,linetype="dashed")+
  geom_vline(xintercept = 20000, color = "purple", size=1,linetype="dashed")+
  geom_vline(xintercept = 100000, color = "blue", size=1,linetype="dashed")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels = c(0,0.2,0.4,0.6,0.8,1),limits = c(0,1))+
  scale_x_continuous(breaks=c(0,20000,40000,60000,80000,100000,120000),label=comma,limits = c(0,120000))+
  xlab("")+ylab("Probability of cost effectiveness")+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    axis.text.x=element_blank(),
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.3,0.6),
    legend.text=element_text(size=14),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  ) + labs(tag = "a")

#------Sensitivity analysis for time horizon-------#
df<-read.csv("outputs/LAAB_PrEP_outputs_th2.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_1.csv")

df<-read.csv("outputs/LAAB_PrEP_outputs_th5.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_2.csv")

df<-read.csv("outputs/LAAB_PrEP_outputs_th10.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_3.csv")

x1<-read.csv("wtp_1.csv")
x2<-read.csv("wtp_2.csv")
x3<-read.csv("wtp_3.csv")

df<-data.frame(buget=rep(x1$buget,3),
               wtp=c(x1$prob,x2$prob,x3$prob),
               group=rep(c("Time horizon 2 years","Time horizon 5 years","Time horizon 10 years"),each=length(x1$buget)))
df$group<- factor(df$group, levels = c("Time horizon 10 years","Time horizon 5 years","Time horizon 2 years"))

p2<-ggplot(data=df, aes(x=buget, y=wtp, group=group)) +
  geom_line(aes(color=group),lwd=1.5)+
  labs(tag = "b")+
  geom_hline(yintercept=0.9, linetype="dashed", color = "brown",size=1)+
  geom_vline(xintercept = 30000, color = "red", size=1,linetype="dashed")+
  geom_vline(xintercept = 20000, color = "purple", size=1,linetype="dashed")+
  geom_vline(xintercept = 100000, color = "blue", size=1,linetype="dashed")+
  theme_minimal() +
  xlab("")+
  ylab("")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels = c(0,0.2,0.4,0.6,0.8,1),limits = c(0,1))+
  scale_x_continuous(breaks=c(0,20000,40000,60000,80000,100000,120000),label=comma,limits = c(0,120000))+
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme(
    axis.text.x=element_blank(),
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.4,0.5),
    legend.text=element_text(size=14),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )  
#------Sensitivity analysis for incidence of hospitalisation-------#
wtp <- 30000 # willingness to pay threshold
np <- 4      # number of parameters on composite parameter

# Model output analysis
mop<-read.csv(file = "outputs/LAAB_PrEP_outputs.csv") 

# remove redundant parameters
aa<-subset(mop, select = -c(X,LNH, LNU, LNV, thetaH,deltaC))

ind1<-which(aa$y_hobs<0.01)
ind2<-which(aa$y_hobs>0.01 & aa$y_hobs < 0.02)
ind3<-which(aa$y_hobs>0.02 & aa$y_hobs < 0.03)
ind4<-which(aa$y_hobs>0.03 & aa$y_hobs < 0.04)
ind5<-which(aa$y_hobs>0.04 & aa$y_hobs < 0.05)

wtp1<-wtp2<-wtp3<-wtp4<-wtp5<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp1[i]<-sum(aa$LYG[ind1]*k[i] > aa$COST[ind1])/length(aa$LYG[ind1])
}

for (i in 1:length(k)) {
  wtp2[i]<-sum(aa$LYG[ind2]*k[i] > aa$COST[ind2])/length(aa$LYG[ind2])
}

for (i in 1:length(k)) {
  wtp3[i]<-sum(aa$LYG[ind3]*k[i] > aa$COST[ind3])/length(aa$LYG[ind3])
}

for (i in 1:length(k)) {
  wtp4[i]<-sum(aa$LYG[ind4]*k[i] > aa$COST[ind4])/length(aa$LYG[ind4])
}

for (i in 1:length(k)) {
  wtp5[i]<-sum(aa$LYG[ind5]*k[i] > aa$COST[ind5])/length(aa$LYG[ind5])
}

data_ggplot2<-data.frame(buget=rep(k,5),prob=c(wtp1,wtp2,wtp3,wtp4,wtp5),group=as.character(c(rep(c('Inc of hosp <0.01 per person per year','Inc of hosp (0.01,0.02) per person per year','Inc of hosp (0.02,0.03) per person per year','Inc of hosp (0.03,0.04) per person per year','Inc of hosp (0.04,0.05) per person per year'),each=length(k)))))
data_ggplot2$group<- factor(data_ggplot2$group, levels = c("Inc of hosp (0.04,0.05) per person per year","Inc of hosp (0.03,0.04) per person per year","Inc of hosp (0.02,0.03) per person per year","Inc of hosp (0.01,0.02) per person per year","Inc of hosp <0.01 per person per year"))

p3<-data_ggplot2 %>%
  ggplot( aes(x=buget, y=prob, group=group, colour=group)) +
  theme_minimal() +
  geom_line(lwd=1.5)+
  geom_hline(yintercept=0.9, linetype="dashed", color = "brown",size=1)+
  geom_vline(xintercept = 30000, color = "red", size=1,linetype="dashed")+
  geom_vline(xintercept = 20000, color = "purple", size=1,linetype="dashed")+
  geom_vline(xintercept = 100000, color = "blue", size=1,linetype="dashed")+
  xlab("Willingness to pay (GBP)")+ylab("Probability of cost effectiveness")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels = c(0,0.2,0.4,0.6,0.8,1),limits = c(0,1))+
  scale_x_continuous(breaks=c(0,20000,40000,60000,80000,100000,120000),label=comma,limits = c(0,120000))+
  labs(tag = "c")+
  theme_minimal() +
  theme(
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.3,0.4),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.ticks.length=unit(0.20,"cm"),
    axis.title.y = element_text(size=18),
    legend.text=element_text(size=14),
    axis.title.x = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1))

#individual risk group analysis 
df<-read.csv("CEA_OUTPUTS_advancedHIV.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_1.csv")

df<-read.csv("CEA_OUTPUTS_endstage.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_2.csv")

df<-read.csv("CEA_OUTPUTS_haematolog.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_3.csv")

df<-read.csv("CEA_OUTPUTS_highdose.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_4.csv")

df<-read.csv("CEA_OUTPUTS_organtrans.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_5.csv")

df<-read.csv("CEA_OUTPUTS_primaryimmu.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_6.csv")

df<-read.csv("CEA_OUTPUTS_secondaryimmo.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_7.csv")

df<-read.csv("CEA_OUTPUTS_solidtum.csv")
wtp<-k<-seq(from=0,to=120000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(df$LYG*k[i] > df$COST)/length(df$LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)
write.csv(data_ggplot2,"wtp_8.csv")

x1<-read.csv("wtp_1.csv")
x2<-read.csv("wtp_2.csv")
x3<-read.csv("wtp_3.csv")
x4<-read.csv("wtp_4.csv")
x5<-read.csv("wtp_5.csv")
x6<-read.csv("wtp_6.csv")
x7<-read.csv("wtp_7.csv")
x8<-read.csv("wtp_8.csv")

df<-data.frame(buget=rep(x1$buget,8),
               wtp=c(x1$prob,x2$prob,x3$prob,x4$prob,x5$prob,x6$prob,x7$prob,x8$prob),
               group=rep(c("AdvancedHIV","Endstagerenal","Malignancies","Corticosteroids", "Organtrans", "Primaryimmuno", "Secondaryimmuno", "Solidtumour"),each=length(x1$buget)))
df$group<- factor(df$group, levels = c("Malignancies","Organtrans","Endstagerenal","Primaryimmuno","Secondaryimmuno","AdvancedHIV","Solidtumour","Corticosteroids"))

p4<-ggplot(data=df, aes(x=buget, y=wtp, group=group)) +
  geom_line(aes(color=group),lwd=1.5)+scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  guides(color = guide_legend(ncol  = 2))+
  theme_minimal() +
  xlab("Willingness to pay (GBP)")+
  ylab("")+
  geom_hline(yintercept=0.9, linetype="dashed", color = "brown",size=1)+
  geom_vline(xintercept = 30000, color = "red", size=1,linetype="dashed")+
  geom_vline(xintercept = 20000, color = "purple", size=1,linetype="dashed")+
  geom_vline(xintercept = 100000, color = "blue", size=1,linetype="dashed")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels = c(0,0.2,0.4,0.6,0.8,1),limits = c(0,1))+
  scale_x_continuous(breaks=c(0,20000,40000,60000,80000,100000,120000),label=comma,limits = c(0,120000))+
  labs(tag = "d")+
  theme(
    text = element_text(size=18),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.3,0.4),
    legend.title = element_blank(),
    legend.text=element_text(size=14),
    axis.ticks = element_line(colour = "black", size = 1),
    axis.ticks.length=unit(0.20,"cm"),
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1))

ggarrange(p1,p2,p3,p4,ncol = 2, nrow = 2,
          align="v")
