########################################################################
# LAAB model output analysis #
# multiway sensitivity analysis Fig S5
########################################################################
rm(list = ls())
# load the library
library(forcats)
library("DescTools")
library('corrr')
library("ggcorrplot")
library("overlapping")
library(ggplot2)
library(dplyr)
library(ggpubr)

#time horizon is 2 years
mop<-read.csv("C:/Users/sc7685/OneDrive - Princeton University/projects/5_LAAB/LAAB_hop/UploadGit/outputs/LAAB_PrEP_outputs_th2.csv")
# remove redundant parameters
aa<-subset(mop, select = -c(X,LNH, LNU, LNV, thetaH,deltaC,lambda_obs))

# correlation with LYG
w <- subset(aa,select =-c(ICER,COST))
# calculate correlation coefficient threshold
b<-FisherZInv(1.96/(length(w[,1])-3)^0.5)
cc <- cor(w[,])
z <- cc[1:(length(cc[1,])-1),length(cc[1,])]
# show significant correlations
z[abs(z)>b]

data <- data.frame(
  name=names(tail(sort(abs(z)),5)),
  val=z[names(tail(sort(abs(z)),5))]
)

# Reverse side
p2<-data %>%
  mutate(name = fct_reorder(name, desc(-abs(val)))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  scale_x_discrete(labels = c('rho' = expression(rho),
                              'q0'   = expression("u"[0]),
                              'qs'   = expression("u"[s]),
                              'y_hobs'   = expression("y"[H]),
                              'eta'   = expression(eta)))+
  xlab("") +
  ylab(~ paste("Correlation with incremental QALYs, ",Delta,"Q"))+
  theme_bw()+
  ylim(-0.6,0.6)+
  labs(tag = "a")+
  theme(
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.1,0.9),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x = element_text(size=15),
    # panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )

#time horizon is 5 years
mop<-read.csv("C:/Users/sc7685/OneDrive - Princeton University/projects/5_LAAB/LAAB_hop/UploadGit/outputs/LAAB_PrEP_outputs_th5.csv")
# remove redundant parameters
aa<-subset(mop, select = -c(X,LNH, LNU, LNV, thetaH,deltaC,lambda_obs))

# correlation with LYG
w <- subset(aa,select =-c(ICER,COST))
# calculate correlation coefficient threshold
b<-FisherZInv(1.96/(length(w[,1])-3)^0.5)
cc <- cor(w[,])
z <- cc[1:(length(cc[1,])-1),length(cc[1,])]
# show significant correlations
z[abs(z)>b]

data <- data.frame(
  name=names(tail(sort(abs(z)),5)),
  val=z[names(tail(sort(abs(z)),5))]
)

# Reverse side
p3<-data %>%
  mutate(name = fct_reorder(name, desc(-abs(val)))) %>%
  ggplot( aes(x=name, y=val)) +
  scale_x_discrete(labels = c('rho' = expression(rho),
                              'epsilonC'   = expression(epsilon[C]),
                              'qs'   = expression("u"[0]),
                              'y_hobs'   = expression("y"[H]),
                              'eta'   = expression(eta)))+
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  ylim(-0.6,0.6)+
  ylab(~ paste("Correlation with incremental QALYs, ",Delta,"Q"))+
  theme_bw()+
  labs(tag = "b")+
  theme(
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.1,0.9),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x = element_text(size=15),
    # panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )

#time horizon is 10 years
mop<-read.csv("C:/Users/sc7685/OneDrive - Princeton University/projects/5_LAAB/LAAB_hop/UploadGit/outputs/LAAB_PrEP_outputs_th10.csv.csv")
# remove redundant parameters
aa<-subset(mop, select = -c(X,LNH, LNU, LNV, thetaH,deltaC,lambda_obs))

##########################
# LYG                    #
##########################

# correlation with LYG
w <- subset(aa,select =-c(ICER,COST))
# calculate correlation coefficient threshold
b<-FisherZInv(1.96/(length(w[,1])-3)^0.5)
cc <- cor(w[,])
z <- cc[1:(length(cc[1,])-1),length(cc[1,])]
# show significant correlations
z[abs(z)>b]

data <- data.frame(
  name=names(tail(sort(abs(z)),5)),
  val=z[names(tail(sort(abs(z)),5))]
)
# Reverse side
p4<-data %>%
  mutate(name = fct_reorder(name, desc(-abs(val)))) %>%
  ggplot( aes(x=name, y=val)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  labs(tag = "c")+
  ylab(~ paste("Correlation with incremental QALYs, ",Delta,"Q"))+
  ylim(-0.6,0.6)+
  theme_bw()+
  scale_x_discrete(labels = c('rho' = expression(rho),
                              'mu'   = expression(mu),
                              'qs'   = expression("u"[0]),
                              'y_hobs'   = expression("y"[H]),
                              'sigma0'   = expression(sigma)))+
  theme(
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0.1,0.9),
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.text.x = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.title.x = element_text(size=15),
    # panel.border = element_blank(),
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=1, b=0, l=0, "cm")
  )

ggarrange(p2,p3,p4,ncol = 3, nrow = 1)
ggsave("nc2.jpg", ggarrange(p2,p3,p4,ncol = 3, nrow = 1), height = 3.5, width = 14)
