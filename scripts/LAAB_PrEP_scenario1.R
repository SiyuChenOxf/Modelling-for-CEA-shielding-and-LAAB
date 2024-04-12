#######################################
### A model for LAAB prophylaxis ###
### Script for generating scenario low fatigue and low replacement  ###
#######################################
rm(list = ls())       #To clear all variables in the current environment
set.seed(100)         #To set random number for repeatability  

setwd(".../")                                       

#library packages
library(deSolve)
library(parallel) 
library(foreach)
library(doParallel)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(varhandle)  
library(dplyr)
library(gtools)

## common values for plotting
ymax_kft <- 0.012
ymax_exposure <- 25
colors_Dark<-brewer.pal(7,"Dark2")
colors_Spectral<-brewer.pal(7,"Spectral")
xlab_font_size=12
font_size = 16
font_size_title = 16
lwd = 0.5
pt_size = 0.4
right_margin=1

#parallel computing
parallel.cores <- detectCores()-1 
cl <- makeCluster(parallel.cores)
registerDoParallel(cl)
time1 <- Sys.time()

#Import parameters from csv files 
para_input<-read.csv(file = "LAAB_PrEP_template_s1.csv")
para_input<-unfactor(para_input)                                                             

#sample size of non-parametric bootstrap
samples<-100000 

time_start <- 0
time_stop <- as.numeric(para_input$parameter1[para_input$symbol=="T"]) #time horizon T
deltat<- 0.1                                                           #ODE output time step
tps <- seq(time_start , time_stop , by = deltat)                       #ODE output time point

# risk group characteristics parameters
Y_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="Y"]),as.numeric(para_input$parameter2[para_input$symbol=="Y"]),by=as.numeric(para_input$parameter3[para_input$symbol=="Y"]))
mu_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="mu"]),as.numeric(para_input$parameter2[para_input$symbol=="mu"]),by=as.numeric(para_input$parameter3[para_input$symbol=="mu"]))                 
pH_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="pH"]),as.numeric(para_input$parameter2[para_input$symbol=="pH"]),by=as.numeric(para_input$parameter3[para_input$symbol=="pH"])) 
pU_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="pU"]),as.numeric(para_input$parameter2[para_input$symbol=="pU"]),by=as.numeric(para_input$parameter3[para_input$symbol=="pU"])) 
pV_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="pV"]),as.numeric(para_input$parameter2[para_input$symbol=="pV"]),by=as.numeric(para_input$parameter3[para_input$symbol=="pV"])) 
deltaH_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="deltaH"]),as.numeric(para_input$parameter2[para_input$symbol=="deltaH"]),by=as.numeric(para_input$parameter3[para_input$symbol=="deltaH"])) 
deltaU_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="deltaU"]), as.numeric(para_input$parameter2[para_input$symbol=="deltaU"]),by=as.numeric(para_input$parameter3[para_input$symbol=="deltaU"])) 
deltaV_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="deltaV"]), as.numeric(para_input$parameter2[para_input$symbol=="deltaV"]),by=as.numeric(para_input$parameter3[para_input$symbol=="deltaV"])) 
q0_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="q0"]), as.numeric(para_input$parameter2[para_input$symbol=="q0"]),by=as.numeric(para_input$parameter3[para_input$symbol=="q0"])) 
y_hobs_v = seq(as.numeric(para_input$parameter1[para_input$symbol=="y_hobs"]), as.numeric(para_input$parameter2[para_input$symbol=="y_hobs"]),by=as.numeric(para_input$parameter3[para_input$symbol=="y_hobs"])) 

# LAAB parameters (epsilon: conditional probabilities)
epsilonC_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="epsilonC"]),as.numeric(para_input$parameter2[para_input$symbol=="epsilonC"]),by=as.numeric(para_input$parameter3[para_input$symbol=="epsilonC"]))     
epsilonH_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="epsilonH"]),as.numeric(para_input$parameter2[para_input$symbol=="epsilonH"]),by=as.numeric(para_input$parameter3[para_input$symbol=="epsilonH"]))     
dur_v <-seq(as.numeric(para_input$parameter1[para_input$symbol=="dur"]),as.numeric(para_input$parameter2[para_input$symbol=="dur"]),by=as.numeric(para_input$parameter3[para_input$symbol=="dur"]))     

# epidemiological parameters
nuC_v   <- rgamma(n = 1000,shape = as.numeric(para_input$parameter1[para_input$symbol=="nuC"]),rate = as.numeric(para_input$parameter2[para_input$symbol=="nuC"]))
LMH_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="LMH"]),as.numeric(para_input$parameter2[para_input$symbol=="LMH"]),by=as.numeric(para_input$parameter3[para_input$symbol=="LMH"]))     
LMU_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="LMU"]),as.numeric(para_input$parameter2[para_input$symbol=="LMU"]),by=as.numeric(para_input$parameter3[para_input$symbol=="LMU"]))     
LMV_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="LMV"]),as.numeric(para_input$parameter2[para_input$symbol=="LMV"]),by=as.numeric(para_input$parameter3[para_input$symbol=="LMV"]))     

# economics parameters
ky_v <-  seq(as.numeric(para_input$parameter1[para_input$symbol=="ky"]),as.numeric(para_input$parameter2[para_input$symbol=="ky"]),by=as.numeric(para_input$parameter3[para_input$symbol=="ky"]))          
kc_v <-  seq(as.numeric(para_input$parameter1[para_input$symbol=="kc"]),as.numeric(para_input$parameter2[para_input$symbol=="kc"]),by=as.numeric(para_input$parameter3[para_input$symbol=="kc"]))          

cH_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="cH"]),as.numeric(para_input$parameter2[para_input$symbol=="cH"]),by=as.numeric(para_input$parameter3[para_input$symbol=="cH"]))     
cU_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="cU"]),as.numeric(para_input$parameter2[para_input$symbol=="cU"]),by=as.numeric(para_input$parameter3[para_input$symbol=="cU"]))     
cV_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="cV"]),as.numeric(para_input$parameter2[para_input$symbol=="cV"]),by=as.numeric(para_input$parameter3[para_input$symbol=="cV"]))     
cd_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="cd"]),as.numeric(para_input$parameter2[para_input$symbol=="cd"]),by=as.numeric(para_input$parameter3[para_input$symbol=="cd"]))     

# shielding parameters
sigma0_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="sigma0"]),as.numeric(para_input$parameter2[para_input$symbol=="sigma0"]),by=as.numeric(para_input$parameter3[para_input$symbol=="sigma0"]))      
rho_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="rho"]),as.numeric(para_input$parameter2[para_input$symbol=="rho"]),by=as.numeric(para_input$parameter3[para_input$symbol=="rho"]))                 
cs_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="cs"]),as.numeric(para_input$parameter2[para_input$symbol=="cs"]),by=as.numeric(para_input$parameter3[para_input$symbol=="cs"]))                     
eta_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="eta"]),as.numeric(para_input$parameter2[para_input$symbol=="eta"]),by=as.numeric(para_input$parameter3[para_input$symbol=="eta"]))                 
qs_v <- seq(as.numeric(para_input$parameter1[para_input$symbol=="qs"]),as.numeric(para_input$parameter2[para_input$symbol=="qs"]),by=as.numeric(para_input$parameter3[para_input$symbol=="qs"]))                 


#prophylaxis ODE system
prophylaxis<-function(t, state, parameters) {
  with(as.list(c(state, parameters)),{
    
    nuH = 365.25*(((1-deltaH)/LNH)+(deltaH/LMH))
    nuU = 365.25*(((1-deltaU)/LNU)+(deltaU/LMU))
    nuV = 365.25*(((1-deltaV)/LNV)+(deltaV/LMV))
    
    IncC <- (1-(1-epsilonH)*pH)*(1-epsilonC)*lambda*S
    IncH <- (1-epsilonH)*pH*(1-pU)*(1-epsilonC)*lambda*S
    IncU <- (1-epsilonH)*pH*pU*(1-pV)*(1-epsilonC)*lambda*S
    IncV <- (1-epsilonH)*pH*pU*pV*(1-epsilonC)*lambda*S
    
    dS  <- -(1-epsilonC)*lambda*S-mu*S+(1-deltaC)*nuC*IC+(1-deltaH)*nuH*IH+(1-deltaU)*nuU*IU+(1-deltaV)*nuV*IV
    dIC <- IncC-(nuC+mu)*IC
    
    dIH <- IncH-(nuH+mu)*IH
    dIH1 <- IncH *deltaH 
    dIH2 <- IncH *(1-deltaH)  
    
    dIU <- IncU-(nuU+mu)*IU
    dIU1 <-IncU *deltaU
    dIU2 <-IncU * (1-deltaU)
    
    dIV <- IncV -(nuV+mu)*IV
    dIV1 <-IncV *deltaV
    dIV2 <-IncV * (1-deltaV)
    
    # cumulative compartments
    dM  <- deltaC*nuC*IC+deltaH*nuH*IH+deltaU*nuU*IU+deltaV*nuV*IV+mu*(S+IC+IH+IU+IV)
    
    dCs <- IncC
    dH <- IncH
    dU <- IncU
    dV <- IncV
    
    # discounted costs
    dC <- (cH*(LMH*deltaH+LNH*(1-deltaH))*IncH+cU*(LMU*deltaU+LNU*(1-deltaU))*IncU+cV*(LMV*deltaV+LNV*(1-deltaV))*IncV)*((1-kc)^t)
    
    list(c(dS, dIC, dIH, dIU, dIV,  dH, dU, dV, dM,  dC,dIH1,dIH2,dIU1,dIU2,dIV1, dIV2,dCs))}) 
}


#create parallel output format function
comb <- function(x, ...) {
  lapply(seq_along(x),
         function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
}

oper <- foreach(i=1:samples, .combine='comb', .multicombine=TRUE,
                .init=list(list(), list(), list(), list(),list(),list(),list(),list(),list(),list(),
                           list(), list(), list(), list(),list(),list(),list(),list(),list(),list(),
                           list(), list(), list(), list(),list(),list(),list(),list(),list(),list(),
                           list(), list(), list(), list(),list(),list(),list(),list(),list())) %dopar% {
                             
                             # PARAMETERS
                             parameters <- c(
                               
                               # risk group characteristics parameters
                               mu = mu_v[round(runif(1, 1, length(mu_v)))],                              
                               nuC=  365.25/(nuC_v[round(runif(1, 1, length(nuC_v)))]),                 
                               q0 = q0_v[round(runif(1, 1, length(q0_v)))],  
                               y_hobs = y_hobs_v[round(runif(1, 1, length(y_hobs_v)))],  
                               
                               deltaC = 0,                                           # probability of dying if IC
                               deltaH = deltaH_v[round(runif(1, 1, length(deltaH_v)))],  
                               deltaU = deltaU_v[round(runif(1, 1, length(deltaU_v)))],  
                               deltaV = deltaV_v[round(runif(1, 1, length(deltaV_v)))],  
                               
                               pH = pH_v[round(runif(1, 1, length(pH_v)))],  
                               pU = pU_v[round(runif(1, 1, length(pU_v)))],  
                               pV = pV_v[round(runif(1, 1, length(pV_v)))],  
                               
                               # LAAB efficacy parameters       
                               epsilonC = epsilonC_v[round(runif(1, 1, length(epsilonC_v)))],
                               epsilonH = epsilonH_v[round(runif(1, 1, length(epsilonH_v)))],
                               dur = dur_v[round(runif(1, 1, length(dur_v)))],                  
                               
                               # epidemiological parameters
                               LMH = LMH_v[round(runif(1, 1, length(LMH_v)))],            
                               LMU = LMU_v[round(runif(1, 1, length(LMU_v)))],            
                               LMV = LMV_v[round(runif(1, 1, length(LMV_v)))],            
                               
                               # economics parameters
                               ky = ky_v[round(runif(1, 1, length(ky_v)))],            
                               kc = kc_v[round(runif(1, 1, length(kc_v)))],            
                               
                               cH = cH_v[round(runif(1, 1, length(cH_v)))],               
                               cU = cU_v[round(runif(1, 1, length(cU_v)))],               
                               cV = cV_v[round(runif(1, 1, length(cV_v)))],               
                               cd = cd_v[round(runif(1, 1, length(cd_v)))],               
                               
                               # shielding parameters
                               sigma0 = sigma0_v[round(runif(1, 1, length(sigma0_v)))],
                               rho = rho_v[round(runif(1, 1, length(rho_v)))],
                               cs = cs_v[round(runif(1, 1, length(cs_v)))],
                               eta = eta_v[round(runif(1, 1, length(eta_v)))],
                               qs = qs_v[round(runif(1, 1, length(qs_v)))]
                               
                             )
                             
                             parameters["LNH"] <- parameters["LMH"]
                             parameters["LNU"] <- parameters["LMU"]
                             parameters["LNV"] <- parameters["LMV"]
                             
                             parameters["lambda_obs"] <- parameters["y_hobs"]/parameters["pH"]
                             parameters["lambda_gen"] <- parameters["lambda_obs"]/(1-parameters["sigma0"]) #transfer observed FOI in IC given shielding to FOI in general population without shielding (lambda_obs < or = lambda_gen)
                             
                             Y<-Y_v*1000000                                                               # population size
                             
                             # INITIAL CONDITIONS
                             istate <- c(S = Y,
                                         IC = 0,
                                         IH = 0,
                                         IU = 0,
                                         IV = 0,
                                         H = 0,
                                         U = 0,
                                         V = 0,
                                         M = 0,
                                         C = 0,
                                         IH1 = 0,
                                         IH2 = 0,
                                         IU1 = 0,
                                         IU2 = 0,
                                         IV1 = 0,
                                         IV2 = 0,
                                         Cs = 0)
                             
                             thetaH <- 1-(1-parameters["epsilonC"])*(1-parameters["epsilonH"])                                           
                             epsilonC<-parameters["epsilonC"]
                             epsilonH<-parameters["epsilonH"]
                             
                             # run intervention
                             parameters["lambda"] <-  parameters["lambda_gen"]*(1-(1-parameters["eta"])*parameters["sigma0"]*(1-parameters["rho"]))
                             
                             out1 <- deSolve::ode(y = istate, times = tps, func = prophylaxis, parms = parameters,method="bdf")
                             
                             # run baseline
                             parameters['epsilonC']<-0
                             parameters['epsilonH']<-0
                             
                             parameters["lambda"] <-  parameters["lambda_gen"]*(1-(1-parameters["eta"])*parameters["sigma0"])
                             
                             lambda_base<-parameters["lambda"]                                                                                         
                             
                             out0 <- deSolve::ode(y = istate, times = tps, func = prophylaxis, parms = parameters,method="bdf")
                             
                             check<-rowSums(out1[,c("S","IC","IH","IU","IV","M")])
                             
                             nt <- length(out0[,1])
                             
                             
                             # discounted QALY
                             LY0 <- (sum((Y-out0[,'M'])*((1-parameters['ky'])^out0[,1])))*deltat * (parameters["q0"]*(1-(1-parameters["qs"])*(1-parameters["eta"])))
                             LY1 <- (sum((Y-out1[,'M'])*((1-parameters['ky'])^out1[,1])))*deltat * (parameters["q0"]*(1-(1-parameters["qs"])*((1-parameters["eta"])*(1-parameters["rho"]))))
                             # LY1 <- (sum(out0[,'M']*((1-parameters['ky'])^out0[,1])))*deltat
                             # LY0 <- (sum(out1[,'M']*((1-parameters['ky'])^out1[,1])))*deltat
                             
                             #discounted life years gained
                             LYG <- LY1-LY0   #discounted life years gained of the cohort
                             
                             #discounted treatment costs
                             C0<-out0[nt,'C']
                             C1<-out1[nt,'C']
                             
                             #discounted dosing costs
                             dosep <- round(seq(0 , nt , by = parameters['dur']/deltat))+1
                             if (dosep[length(dosep)]>nt) {dosep<-dosep[-length(dosep)]}
                             dosep<-dosep[-3]  #exact to doses
                             Cd <- parameters['cd']*sum(out1[dosep,c('S','IC','IH','IU','IV')]*((1-parameters['kc'])^out1[dosep,1]))
                             
                             #discounted shielding costs
                             payp <- round(seq(0 , nt , by = (1/52)/deltat))+1
                             payp<-payp[-((length(payp)-sum(payp>nt)+1):length(payp))]
                             
                             Cs0 <- (parameters['cs'])*sum(out0[payp,c('S','IC','IH','IU','IV')]*((1-parameters['kc'])^out0[payp,1]))
                             Cs1 <- (parameters['cs'])*sum(out1[payp,c('S','IC','IH','IU','IV')]*((1-parameters['kc'])^out1[payp,1]))
                             
                             # incremental costs of intervention
                             COST_1<- C1 + Cd + (1-parameters["rho"])*(1-parameters["eta"])*Cs1
                             COST_0<- C0 + (1-parameters["eta"])*Cs0
                             COST<-COST_1 - COST_0
                             
                             # cost per life year gained
                             ICER <- COST/LYG                                  
                             
                             list(
                               parameters["lambda_gen"],
                               parameters["mu"],
                               parameters["nuC"],
                               parameters["pH"],
                               parameters["pU"],
                               parameters["pV"],
                               parameters["deltaC"],
                               parameters["deltaH"],
                               parameters["deltaU"],
                               parameters["deltaV"],
                               epsilonC,
                               epsilonH,
                               thetaH,
                               parameters["dur"],
                               parameters["LMH"],
                               parameters["LMU"],
                               parameters["LMV"],
                               parameters["LNH"],
                               parameters["LNU"],
                               parameters["LNV"],
                               parameters["ky"],
                               parameters["kc"],
                               parameters["cd"],
                               parameters["cH"],
                               parameters["cU"],
                               parameters["cV"],
                               COST,
                               LYG,
                               ICER,
                               Y,
                               parameters["sigma0"],
                               parameters["rho"],
                               parameters["lambda_obs"],
                               parameters["cs"],
                               parameters["kc"],
                               parameters["eta"],
                               parameters["q0"],
                               parameters["qs"],
                               parameters["y_hobs"])
                           }

stopImplicitCluster()
stopCluster(cl)
time2 <- Sys.time()
time2-time1

lambda_gen<-as.numeric(as.matrix(oper[[1]]))
mu<-as.numeric(as.matrix(oper[[2]]))
nuC<-as.numeric(as.matrix(oper[[3]]))
pH<-as.numeric(as.matrix(oper[[4]]))
pU<-as.numeric(as.matrix(oper[[5]]))
pV<-as.numeric(as.matrix(oper[[6]]))
deltaC<-as.numeric(as.matrix(oper[[7]]))
deltaH<-as.numeric(as.matrix(oper[[8]]))
deltaU<-as.numeric(as.matrix(oper[[9]]))
deltaV<-as.numeric(as.matrix(oper[[10]]))

epsilonC<-as.numeric(as.matrix(oper[[11]]))
epsilonH<-as.numeric(as.matrix(oper[[12]]))

thetaH<-as.numeric(as.matrix(oper[[13]]))

dur<-as.numeric(as.matrix(oper[[14]]))
LMH<-as.numeric(as.matrix(oper[[15]]))
LMU<-as.numeric(as.matrix(oper[[16]]))
LMV<-as.numeric(as.matrix(oper[[17]]))
LNH<-as.numeric(as.matrix(oper[[18]]))
LNU<-as.numeric(as.matrix(oper[[19]]))
LNV<-as.numeric(as.matrix(oper[[20]]))
ky<-as.numeric(as.matrix(oper[[21]]))
kc<-as.numeric(as.matrix(oper[[22]]))
cd<-as.numeric(as.matrix(oper[[23]]))
cH<-as.numeric(as.matrix(oper[[24]]))
cU<-as.numeric(as.matrix(oper[[25]]))
cV<-as.numeric(as.matrix(oper[[26]]))

COST<-as.numeric(as.matrix(oper[[27]]))
LYG<-as.numeric(as.matrix(oper[[28]]))

ICER<-as.numeric(as.matrix(oper[[29]]))
Y<-as.numeric(as.matrix(oper[[30]]))

sigma0<-as.numeric(as.matrix(oper[[31]]))
rho<-as.numeric(as.matrix(oper[[32]]))
lambda_obs<-as.numeric(as.matrix(oper[[33]]))
cs<-as.numeric(as.matrix(oper[[34]]))
kc<-as.numeric(as.matrix(oper[[35]]))

eta<-as.numeric(as.matrix(oper[[36]]))
q0<-as.numeric(as.matrix(oper[[37]]))
qs<-as.numeric(as.matrix(oper[[38]]))
y_hobs<-as.numeric(as.matrix(oper[[39]]))

data_output<-data.frame(
  #input parameters
  eta=eta,
  lambda_obs=lambda_obs,
  sigma0=sigma0,
  rho=rho,
  mu=mu,
  nuC=nuC,
  pH=pH,
  pU=pU,
  pV=pV,
  deltaC=deltaC,
  deltaH=deltaH,
  deltaU=deltaU,
  deltaV=deltaV,
  thetaH=thetaH,
  epsilonC=epsilonC,
  epsilonH=epsilonH,
  LMH=LMH,
  LMU=LMU,
  LMV=LMV,
  LNH=LNH,
  LNU=LNU,
  LNV=LNV,
  ky=ky,
  kc=kc,
  cs=cs,
  cd=cd,
  cH=cH,
  cU=cU,
  cV=cV,
  q0=q0,
  qs=qs,
  y_hobs=y_hobs,
  #outputs
  COST=COST,
  LYG=LYG,
  ICER=ICER            #incremental cost per life year gained
)

write.csv(x = data_output,file = "LAAB_PrEP_outputs_s1.csv")

###ggplot for COST-EFFECTIVENESS plane and WTP curve without replacement##
threshold_ICER<-30000
data_ggplot1<-data.frame(lyg=LYG,cost=COST)

wtp<-k<-seq(from=0,to=80000,by=1000)
for (i in 1:length(k)) {
  wtp[i]<-sum(LYG*k[i] > COST)/length(LYG)
}
data_ggplot2<-data.frame(buget=k,prob=wtp)

p1<-ggplot(data=data_ggplot1,aes(x=lyg,y=cost))+
  geom_point()+
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)+
  scale_y_continuous(label=comma)+
  # scale_y_continuous(breaks=c(-15000000,-10000000,-5000000,0,5000000,10000000,15000000),labels = comma,limit = c(-15000000, 15000000))+
  geom_line(data=data_ggplot1,aes(x=lyg,y=threshold_ICER*lyg),col="red",lwd=0.5)+
  xlab("Life years gained")+ylab("Costs (GBP)")


style1<- p1+scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    text = element_text(size=20),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=20),
    axis.title.x = element_text(size=20),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
# style1

p2<-ggplot(data=data_ggplot2,aes(x=buget,y=prob))+
  geom_line(size=1)+
  geom_vline(xintercept = threshold_ICER, color = "red", size=0.5)+
  xlab("Willingness to pay (GBP)")+ylab("Probability of cost effectiveness")+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels = c(0,0.2,0.4,0.6,0.8,1),limits = c(0,1))+
  scale_x_continuous(label=comma)


style2<- p2+scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_minimal() +
  theme(
    text = element_text(size=20),
    plot.title = element_text(face = "bold", size = 20,hjust = 0.5),
    legend.background = element_rect(fill = "white", size = 1.25, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = "none",
    legend.title = element_blank(),
    axis.ticks = element_line(colour = "grey50", size = 0.2),
    axis.title.y = element_text(size=20),
    axis.title.x = element_text(size=20),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black",size = 1),
    plot.margin = margin(t=0, r=right_margin, b=0, l=0, "cm")
  )
# style2

ggarrange(style1,style2,ncol = 2, nrow = 1)

