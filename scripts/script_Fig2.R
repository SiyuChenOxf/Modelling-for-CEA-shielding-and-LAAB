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
    scale_x_continuous(breaks = c(1:length(order.parameters)),
                       labels =  c(expression("q"[0]),
                                   expression("dur"),
                                   expression("y"[y]),
                                   expression("c"[s]),
                                   expression("q"[s]),
                                   expression("delta"[v]),
                                   expression("delta"[U]),
                                   expression("c"[v]),
                                   expression("LMV"),
                                   expression(nu[c]),
                                   expression("p"[v]),
                                   expression("p"[H]),
                                   expression("LMH"),
                                   expression("c"[U]),
                                   expression(rho),
                                   expression(delta[H]),
                                   expression("LMU"),
                                   expression("p"[U]),
                                   expression(epsilon[H]),
                                   expression("c"[H]),
                                   expression(eta),
                                   expression(epsilon[c]),
                                   expression("y"[H]),
                                   expression("k"[c]),
                                   expression(sigma[0]),
                                   expression(mu),
                                   expression("c"[d])))+
    coord_flip() 
}

p1<-plot_fig(readRDS("dataCOST_max2_nocosta.rds"),readRDS("dataCOST_min2_nocosta.rds"),readRDS("dataCOST_mean2_nocosta.rds"))+ggtitle("Incremental costs,T = 2 years")+ labs(tag="(a)")

#p2 & p3
plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
    scale_x_continuous(breaks = c(1:length(order.parameters)),
                       labels =  c(expression("q"[0]),
                                   expression("dur"),
                                   expression("y"[y]),
                                   expression("c"[s]),
                                   expression("q"[s]),
                                   expression("delta"[v]),
                                   expression(rho),
                                   expression("c"[v]),
                                   expression("LMV"),
                                   expression("delta"[U]),
                                   expression("p"[v]),
                                   expression(nu[c]),
                                   expression("p"[H]),
                                   expression(epsilon[H]),
                                   expression("LMH"),
                                   expression("c"[U]),
                                   expression(epsilon[c]),
                                   expression("LMU"),
                                   expression("p"[U]),
                                   expression("c"[H]),
                                   expression(delta[H]),
                                   expression(eta),
                                   expression("y"[H]),
                                   expression(sigma[0]),
                                   expression("k"[c]),
                                   expression(mu),
                                   expression("c"[d])))+
    coord_flip() 
}

p2<-plot_fig(readRDS("dataCOST_max5_nocosta.rds"),readRDS("dataCOST_min5_nocosta.rds"),readRDS("dataCOST_mean5_nocosta.rds"))+ggtitle("Incremental costs,T = 5 years")+ labs(tag="(b)")

p3<-plot_fig(readRDS("dataCOST_max10_nocosta.rds"),readRDS("dataCOST_min10_nocosta.rds"),readRDS("dataCOST_mean10_nocosta.rds"))+ggtitle("Incremental costs,T= 10 years")+ labs(tag="(c)")

plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
                       labels =  c(
                         expression("dur"),
                         expression("c"[d]),
                         expression("c"[s]),
                         expression("c"[v]),
                         expression("c"[U]),
                         expression("c"[H]),
                         expression("k"[c]),
                         expression("LMV"),
                         expression("p"[v]),
                         expression("LMU"),
                         expression("LMH"),
                         expression("p"[U]),
                         expression("delta"[v]),
                         expression(nu[c]),
                         expression("p"[H]),
                         expression(delta[U]),
                         expression("y"[y]),
                         expression(epsilon[H]),
                         expression(mu),
                         expression(epsilon[c]),
                         expression(delta[H]),
                         expression("y"[H]),
                         expression("q"[0]),
                         expression(sigma[0]),
                         expression(eta),
                         expression(rho),
                         expression("q"[s])))+
    coord_flip() 
}

p4<-plot_fig(readRDS("dataLYG_max2_nocosta.rds"),readRDS("dataLYG_min2_nocosta.rds"),readRDS("dataLYG_mean2_nocosta.rds"))+ggtitle("Incremental QALY,T = 2 years")+ labs(tag="(d)")

plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
                       labels =  c(
                         expression("dur"),
                         expression("c"[d]),
                         expression("c"[s]),
                         expression("c"[v]),
                         expression("c"[U]),
                         expression("c"[H]),
                         expression("k"[c]),
                         expression("LMV"),
                         expression("p"[v]),
                         expression("LMU"),
                         expression("LMH"),
                         expression("p"[U]),
                         expression("delta"[v]),
                         expression(nu[c]),
                         expression("p"[H]),
                         expression(delta[U]),
                         expression("y"[y]),
                         expression(epsilon[H]),
                         expression("q"[0]),
                         expression(delta[H]),
                         expression(epsilon[c]),
                         expression(mu),
                         expression("y"[H]),
                         expression(sigma[0]),
                         expression(eta),
                         expression(rho),
                         expression("q"[s])))+
    coord_flip() 
}

p5<-plot_fig(readRDS("dataLYG_max5_nocosta.rds"),readRDS("dataLYG_min5_nocosta.rds"),readRDS("dataLYG_mean5_nocosta.rds"))+ggtitle("Incremental QALY,T = 5 years")+ labs(tag="(e)")

plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
                       labels =  c(
                         expression("dur"),
                         expression("c"[d]),
                         expression("c"[s]),
                         expression("c"[v]),
                         expression("c"[U]),
                         expression("c"[H]),
                         expression("k"[c]),
                         expression("LMV"),
                         expression("p"[v]),
                         expression("LMU"),
                         expression("LMH"),
                         expression("p"[U]),
                         expression("delta"[v]),
                         expression(nu[c]),
                         expression("p"[H]),
                         expression(delta[U]),
                         expression(epsilon[H]),
                         expression("q"[0]),
                         expression("k"[y]),
                         expression(delta[H]),
                         expression(epsilon[c]),
                         expression("y"[H]),
                         expression(eta),
                         expression(sigma[0]),
                         expression(mu),
                         expression(rho),
                         expression("q"[s])))+
    coord_flip() 
}

p6<-plot_fig(readRDS("dataLYG_max10_nocosta.rds"),readRDS("dataLYG_min10_nocosta.rds"),readRDS("dataLYG_mean10_nocosta.rds"))+ggtitle("Incremental QALY,T = 10 years")+ labs(tag="(f)")

plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
                       labels =  c(
                         expression("dur"),
                         expression("c"[s]),
                         expression("delta"[v]),
                         expression("LMV"),
                         expression("c"[v]),
                         expression("p"[v]),
                         expression("LMH"),
                         expression("c"[U]),
                         expression("LMU"),
                         expression(nu[c]),
                         expression("p"[H]),
                         expression("p"[U]),
                         expression(delta[U]),
                         expression("c"[H]),
                         expression(mu),
                         expression("k"[c]),
                         expression("k"[y]),
                         expression(epsilon[H]),
                         expression(delta[H]),
                         expression(epsilon[c]),
                         expression("q"[0]),
                         expression("y"[H]),
                         expression(sigma[0]),
                         expression("c"[d]),
                         expression(eta),
                         expression(rho),
                         expression("q"[s])))+
    coord_flip() 
}

p7<-plot_fig(readRDS("dataICER_max2_nocosta.rds"),readRDS("dataICER_min2_nocosta.rds"),readRDS("dataICER_mean2_nocosta.rds"))+ggtitle("ICER,T = 2 years")+ labs(tag="(g)")

plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
                       labels =  c(
                         expression("dur"),
                         expression("c"[s]),
                         expression("LMV"),
                         expression("c"[v]),
                         expression("delta"[v]),
                         expression("p"[v]),
                         expression("LMH"),
                         expression("c"[U]),
                         expression("LMU"),
                         expression("p"[U]),
                         expression(nu[c]),
                         expression("p"[H]),
                         expression("c"[H]),
                         expression(delta[U]),
                         expression(mu),
                         expression("k"[c]),
                         expression("k"[y]),
                         expression(epsilon[H]),
                         expression("q"[0]),
                         expression(delta[H]),
                         expression(epsilon[c]),
                         expression(sigma[0]),
                         expression("y"[H]),
                         expression("c"[d]),
                         expression(eta),
                         expression(rho),
                         expression("q"[s])))+
    coord_flip() 
}

p8<-plot_fig(readRDS("dataICER_max5_nocosta.rds"),readRDS("dataICER_min5_nocosta.rds"),readRDS("dataICER_mean5_nocosta.rds"))+ggtitle("ICER,T = 5 years")+ labs(tag="(h)")
plot_fig<-function(df1,df2,df3){
  df<-data.frame(Parameter=gsub('_v','',df1$names),Lower_Bound=df1$val,Upper_Bound=df2$val,UL_Difference=df1$val-df2$val)
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
              aes(ymax=ymax, ymin=ymin, xmax=xmax, xmin=xmin, fill=type)) +
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
                       labels =  c(
                         expression("dur"),
                         expression("c"[s]),
                         expression("LMV"),
                         expression("c"[v]),
                         expression("p"[v]),
                         expression("delta"[v]),
                         expression("LMH"),
                         expression("c"[U]),
                         expression("LMU"),
                         expression("p"[U]),
                         expression("c"[H]),
                         expression(nu[c]),
                         expression("p"[H]),
                         expression(delta[U]),
                         expression(mu),
                         expression(epsilon[H]),
                         expression("k"[c]),
                         expression("k"[y]),
                         expression("q"[0]),
                         expression(epsilon[c]),
                         expression(delta[H]),
                         expression(sigma[0]),
                         expression(eta),
                         expression("y"[H]),
                         expression("c"[d]),
                         expression(rho),
                         expression("q"[s])))+
    coord_flip() 
}

p9<-plot_fig(readRDS("dataICER_max10_nocosta.rds"),readRDS("dataICER_min10_nocosta.rds"),readRDS("dataICER_mean10_nocosta.rds"))+ggtitle("ICER,T = 10 years")+ labs(tag="(i)")

ggarrange(p1,p4,p7,p2,p5,p8,p3,p6,p9,ncol = 3,nrow = 3)