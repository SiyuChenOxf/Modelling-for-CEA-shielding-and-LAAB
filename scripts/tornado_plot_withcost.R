#######################################
### A model for LAAB-PrEP ###
### To generate RDS files for Fig S7  ###
#######################################
rm(list = ls())        
set.seed(100)          

# setwd(".../")                                       

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

#Import parameters from csv files 
para_input<-read.csv(file = "~/Library/CloudStorage/OneDrive-Nexus365/Princeton_project_folder/NewGitCodes/templates/LAAB_PrEP_template.csv")

para_input<-unfactor(para_input)                                                                              

#sample size of non-parametric bootstrap
samples<-5000 

time_start <- 0
time_stop <- 2#as.numeric(para_input$parameter1[para_input$symbol=="T"]) #time horizon T
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
ky_v <-  0.035#seq(as.numeric(para_input$parameter1[para_input$symbol=="ky"]),as.numeric(para_input$parameter2[para_input$symbol=="ky"]),by=as.numeric(para_input$parameter3[para_input$symbol=="ky"]))          
kc_v <-  0.035#seq(as.numeric(para_input$parameter1[para_input$symbol=="kc"]),as.numeric(para_input$parameter2[para_input$symbol=="kc"]),by=as.numeric(para_input$parameter3[para_input$symbol=="kc"]))          

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

par_matrix<-data.frame(names=c("mu_v","pH_v","pU_v","pV_v","deltaH_v","deltaU_v","deltaV_v","q0_v","y_hobs_v",
                               "epsilonC_v","epsilonH_v","dur_v","nuC_v","LMH_v","LMU_v","LMV_v","ky_v","kc_v",
                               "cH_v","cU_v","cV_v","cd_v","sigma0_v","rho_v","cs_v","eta_v","qs_v"),
                       max=c(max(mu_v),max(pH_v),max(pU_v),max(pV_v),max(deltaH_v),max(deltaU_v),max(deltaV_v),max(q0_v),max(y_hobs_v),
                             max(epsilonC_v),max(epsilonH_v),max(dur_v),max(nuC_v),max(LMH_v),max(LMU_v),max(LMV_v),max(ky_v),max(kc_v),
                             max(cH_v),max(cU_v),max(cV_v),max(cd_v),max(sigma0_v),max(rho_v),max(cs_v),max(eta_v),max(qs_v)),
                       mean=c(mean(mu_v),mean(pH_v),mean(pU_v),mean(pV_v),mean(deltaH_v),mean(deltaU_v),mean(deltaV_v),mean(q0_v),mean(y_hobs_v),
                              mean(epsilonC_v),mean(epsilonH_v),mean(dur_v),mean(nuC_v),mean(LMH_v),mean(LMU_v),mean(LMV_v),mean(ky_v),mean(kc_v),
                              mean(cH_v),mean(cU_v),mean(cV_v),mean(cd_v),mean(sigma0_v),mean(rho_v),mean(cs_v),mean(eta_v),mean(qs_v)),
                       min=c(min(mu_v),min(pH_v),min(pU_v),min(pV_v),min(deltaH_v),min(deltaU_v),min(deltaV_v),min(q0_v),min(y_hobs_v),
                             min(epsilonC_v),min(epsilonH_v),min(dur_v),min(nuC_v),min(LMH_v),min(LMU_v),min(LMV_v),min(ky_v),min(kc_v),
                             min(cH_v),min(cU_v),min(cV_v),min(cd_v),min(sigma0_v),min(rho_v),min(cs_v),min(eta_v),min(qs_v)))

df<-matrix(NA,nrow = 27,ncol = 27)
for (i in 1:27) {
  df_par<-par_matrix$mean
  df_par[i]<-par_matrix$max[i]
  df[i,]<-df_par
}
df<-as.data.frame(df)
rownames(df)<-par_matrix$names
colnames(df)<-par_matrix$names

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


df<-rbind(df,par_matrix$mean)
df_out<-rep(NA,28)

for (i in 1:28) {
  
  # PARAMETERS
  parameters <- c(
    
    # risk group characteristics parameters
    mu = df[i,"mu_v"],                              
    nuC=  365.25/df[i,"nuC_v"],                 
    q0 = df[i,"q0_v"],  
    y_hobs = df[i,"y_hobs_v"],  
    
    deltaC = 0,                                           
    deltaH = df[i,"deltaH_v"],  
    deltaU = df[i,"deltaU_v"],  
    deltaV = df[i,"deltaV_v"],  
    
    pH = df[i,"pH_v"],  
    pU = df[i,"pU_v"],  
    pV = df[i,"pV_v"],  
    
    # LAAB efficacy parameters       
    epsilonC = df[i,"epsilonC_v"],
    epsilonH = df[i,"epsilonH_v"],
    dur = df[i,"dur_v"],                  
    
    # epidemiological parameters
    LMH = df[i,"LMH_v"],            
    LMU = df[i,"LMU_v"],            
    LMV = df[i,"LMV_v"],            
    
    # economics parameters
    ky = df[i,"ky_v"],            
    kc = df[i,"kc_v"],            
    
    cH = df[i,"cH_v"],               
    cU = df[i,"cU_v"],               
    cV = df[i,"cV_v"],               
    cd = df[i,"cd_v"],               
    
    # shielding parameters
    sigma0 = df[i,"sigma0_v"],
    rho = df[i,"rho_v"],
    cs = df[i,"cs_v"],
    eta = df[i,"eta_v"],
    qs = df[i,"qs_v"]
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
  names(ICER)<-"ICER"
  names(thetaH)<-"thetaH"
  names(COST)<-"COST"
  names(LYG)<-"LYG"
  
  df_out[i]<-COST
  print(i)
}

data<-data.frame(names=c(par_matrix$names,"max"),
                 val=df_out
)
saveRDS(data,"/Users/schen/Library/CloudStorage/OneDrive-Nexus365/Princeton_project_folder/NewGitCodes/outputs/FigS7/dataCOST_max2.rds")

