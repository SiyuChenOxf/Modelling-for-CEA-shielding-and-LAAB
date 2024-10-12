# Script for plotting Fig 2

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyverse)
library(ggpubr)
library(scales)

#p1
plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Upper_Bound=df2$val,Lower_Bound=df1$val,UL_Difference=df1$val-df2$val)
  df<-df[-28,]
  # original value of output
  base.value <- df3[1,2]
  
  # get order of parameters according to size of intervals
  # (I use this to define the ordering of the factors which I then use to define the positions in the plot)
  order.parameters <- df %>% arrange(abs(UL_Difference)) %>%
    mutate(Parameter=factor(x=Parameter, levels=Parameter)) %>%
    select(Parameter) %>% unlist() %>% levels()
  
  # width of columns in plot (value between 0 and 1)
  width <- 0.95
  
  # get data frame in shape for ggplot and geom_rect
  df.2 <- df %>% 
    # gather columns Lower_Bound and Upper_Bound into a single column using gather
    gather(key='type', value='output.value', Lower_Bound:Upper_Bound) %>%
    # just reordering columns
    select(Parameter, type, output.value, UL_Difference) %>%
    # create the columns for geom_rect
    mutate(Parameter=factor(Parameter, levels=order.parameters),
           ymin=pmin(output.value, base.value),
           ymax=pmax(output.value, base.value),
           xmin=as.numeric(Parameter)-width/2,
           xmax=as.numeric(Parameter)+width/2) %>% 
    top_n(10) # only plot the top 5 influential parameters
  
  # create plot
  ggplot() + 
    geom_rect(data = df.2, 
              aes(ymin=ymin,ymax=ymax,  xmin=xmin,xmax=xmax,  fill=type)) +
    theme_bw() +   
    scale_y_continuous(label=comma)+
    theme(axis.title.y=element_blank(), 
          legend.position = '',
          legend.title = element_blank(),
          text = element_text(size=14),
          axis.text.y = element_text(size=18),
          axis.text.x = element_text(size=18)) + 
    geom_hline(yintercept = base.value) +
    scale_x_continuous(breaks = c(1:length(order.parameters)),
                       labels = order.parameters)+
    coord_flip() 
}
setwd("/Users/schen/Library/CloudStorage/OneDrive-Nexus365/Princeton_project_folder/NewGitCodes/outputs/FigS7")
p1<-plot_fig(readRDS("dataCOST_max2.rds"),readRDS("dataCOST_min2.rds"),readRDS("dataCOST_mean2.rds"))+ggtitle("Incremental costs,T = 2 years")+ labs(tag="(a)")
p2<-plot_fig(readRDS("dataLYG_max2.rds"),readRDS("dataLYG_min2.rds"),readRDS("dataLYG_mean2.rds"))+ggtitle("Incremental LYG,T = 2 years")+ labs(tag="(b)")
p3<-plot_fig(readRDS("dataICER_max2.rds"),readRDS("dataICER_min2.rds"),readRDS("dataICER_mean2.rds"))+ggtitle("ICER,T = 2 years")+ labs(tag="(c)")

p4<-plot_fig(readRDS("dataCOST_max5.rds"),readRDS("dataCOST_min5.rds"),readRDS("dataCOST_mean5.rds"))+ggtitle("Incremental costs,T = 5 years")+ labs(tag="(d)")
p5<-plot_fig(readRDS("dataLYG_max5.rds"),readRDS("dataLYG_min5.rds"),readRDS("dataLYG_mean5.rds"))+ggtitle("Incremental LYG,T = 5 years")+ labs(tag="(e)")
p6<-plot_fig(readRDS("dataICER_max5.rds"),readRDS("dataICER_min5.rds"),readRDS("dataICER_mean5.rds"))+ggtitle("ICER,T = 5 years")+ labs(tag="(d)")

p7<-plot_fig(readRDS("dataCOST_max10.rds"),readRDS("dataCOST_min10.rds"),readRDS("dataCOST_mean10.rds"))+ggtitle("Incremental costs,T = 10 years")+ labs(tag="(g)")
p8<-plot_fig(readRDS("dataLYG_max10.rds"),readRDS("dataLYG_min10.rds"),readRDS("dataLYG_mean10.rds"))+ggtitle("Incremental LYG,T = 10 years")+ labs(tag="(h)")
p9<-plot_fig(readRDS("dataICER_max10.rds"),readRDS("dataICER_min10.rds"),readRDS("dataICER_mean10.rds"))+ggtitle("ICER,T = 10 years")+ labs(tag="(i)")


ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,ncol = 3,nrow = 3)
