library(mizer)
library(ggplot2)
library(cowplot)

#Defining functions 
# define Balanced Harvesting policy -we will investigate a=0,0.25,0.50,0.75,1
BalancedHarvest <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth * n^a, 2, params@w^((2*a)-1), "*")
  production_density * usual_f_mort
}

#Functions used to evaluate simulations
#define total yield
TotalYield <- function(sim) {
  y <- getYield(sim)
  y_total <- rowSums(y)
  return(y_total)
}

#define total biomass
TotalBiomass <- function(sim) {
  b <- getBiomass(sim)
  b_total <- rowSums(b)
  return(b_total)
}


#define total abundance 
TotalAbundance <- function(sim) { 
  n <- getN(sim)
  n_total <- rowSums(n) 
  return(n_total)
}

#Defining function used to calculate MSSY on TB model
MSSY <- function(efforts, Fmort) {
  total_yieldsFmort <- c() # initialize empty vector to store results
  for (effort in efforts) {
    Fmort <- setFishing(Fmort, initial_effort = effort)
    Fmortst <- projectToSteady(Fmort)
    Fmortpr <- project(Fmort, t_max = 100)
    total_yieldFmort <- TotalYield(Fmortpr)[101]
    total_yieldsFmort <- c(total_yieldsFmort, total_yieldFmort) # storing results 
  }
  plot(efforts, total_yieldsFmort, xlab='Inital Effort', ylab='Yield')
  total_yieldsFmort <- data.frame(total_yieldsFmort)
  efforts <- data.frame(efforts)
  Fmort <- cbind(efforts, total_yieldsFmort)
  return(Fmort)
}

#Defining function used to calculate MSSY on NS model
NSMSSY <- function(efforts, Fmort) {
  total_yieldsFmort <- c() # initialize empty vector to store results
  for (effort in efforts) {
    Fmortst <- projectToSteady(Fmort, effort=effort)
    Fmortpr <- project(Fmort,effort=effort, t_max = 100)
    total_yieldFmort <- TotalYield(Fmortpr)[101]
    total_yieldsFmort <- c(total_yieldsFmort, total_yieldFmort) # storing results 
  }
  plot(efforts, total_yieldsFmort, xlab='Inital Effort', ylab='Yield')
  total_yieldsFmort <- data.frame(total_yieldsFmort)
  efforts <- data.frame(efforts)
  Fmort <- cbind(efforts, total_yieldsFmort)
  return(Fmort)
}

#First we look for the MSSY of each policy on a steady TB model whose system is initially unfished system (X denotes initially unfished)
#setting up TB model with modified reproduction(0.8) and modified sigma(2*1.3=2.6)
TB <- newTraitParams(no_sp = 12, gear_names = "None", knife_edge_size = NA, min_w_max  = 30, max_w_inf = 40000, reproduction_level = 0.8, sigma = 2.6)
TBst <- projectToSteady(TB)
TBpr <- project(TB, t_max=100)

#checking everything is as it should be 
TBPARAMS <- getParams(TBpr)
getReproductionLevel(TB)

#find MSSY for size at entry (maturity cutoff) fishing on initally unfished system
TBXKE <- TBst
TBXKE@gear_params$knife_edge_size <- TBXKE@species_params$w_mat
TBXKE@species_params$knife_edge_size <- TBXKE@species_params$w_mat
gear_params(TBXKE)$gear <- "maturity"
species_params(TBXKE)$gear <- "maturity"

#Indentifying c for MSSY KE
efforts <- seq(from=20.1, to=20.5, by=0.1) 
Fmort <- TBXKE
TBXKEresults <- MSSY(efforts, Fmort) #c= 20.2, MSSY= 0.02179049

#projection for XKE MSSY:
TBXKEMSSY <- TBXKE
TBXKEMSSY<- setFishing(TBXKEMSSY, initial_effort = 20.2)
TBXKEMSSYst <- projectToSteady(TBXKEMSSY)
TBXKEMSSYpr <- project(TBXKEMSSY, t_max=100)
TotalYield((TBXKEMSSYpr))[101] #0.02179049
#for later plots
TBXMSSYTotalBiomassKE <- TotalBiomass((TBXKEMSSYpr))
TBXMSSYTotalAbundanceKE <-TotalAbundance(TBXKEMSSYpr)
TBXMSSYTotalYieldKE <- TotalYield((TBXKEMSSYpr))

#find MSSY for BH a=0 (productivity) on unfished system 
a=0
TBXBH_0<- TBst
TBXBH_0@gear_params$knife_edge_size <- TBXBH_0@species_params$w_mat
TBXBH_0@species_params$knife_edge_size <-   TBXBH_0@species_params$w_mat
gear_params(TBXBH_0)$gear <- "balanced"
species_params(TBXBH_0)$gear <- "balanced"
TBXBH_0 <- setRateFunction(TBXBH_0, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=26.2, to=26.6, by=0.1) 
Fmort <- TBXBH_0
TBXBH_0results <- MSSY(efforts, Fmort) #c=26.5, MSSY=0.01815824

#projection for MSSY a=0 on initally unfished system
TBXBH_0MSSY <- TBXBH_0
TBXBH_0MSSY <- setFishing(TBXBH_0MSSY, initial_effort =26.5 ) 
TBXBH_0MSSYpr <-project(TBXBH_0MSSY, t_max=100)
TotalYield((TBXBH_0MSSYpr))[101]#0.01815824 
#later MSSY plots 
TBXMSSYTotalBiomass0 <- TotalBiomass((TBXBH_0MSSYpr))
TBXMSSYTotalAbundance0 <-TotalAbundance(TBXBH_0MSSYpr)
TBXMSSYTotalYield0 <- TotalYield((TBXBH_0MSSYpr))

#find MSSY for BH a=0.25 on an intially unfished system 
a=0.25
TBXBH_025<- TBst
TBXBH_025@gear_params$knife_edge_size <- TBXBH_025@species_params$w_mat
TBXBH_025@species_params$knife_edge_size <-   TBXBH_025@species_params$w_mat
gear_params(TBXBH_025)$gear <- "balanced"
species_params(TBXBH_025)$gear <- "balanced"
TBXBH_025 <- setRateFunction(TBXBH_025, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=256, to=258, by=0.1) 
Fmort <- TBXBH_025
TBXBH_025results <- MSSY(efforts, Fmort) #c=257.2, MSSY=0.01886146

#projection for MSSY a=0.25 on initally unfished system
TBXBH_025MSSY <- TBXBH_025
TBXBH_025MSSY<- setFishing(TBXBH_025MSSY, initial_effort = 257.2) 
TBXBH_025MSSYpr <-project(TBXBH_025MSSY, t_max=100)
TotalYield((TBXBH_025MSSYpr))[101] #0.01886146 

#later MSSY plots
TBXMSSYTotalBiomass025 <- TotalBiomass((TBXBH_025MSSYpr))
TBXMSSYTotalAbundance025 <-TotalAbundance(TBXBH_025MSSYpr)
TBXMSSYTotalYield025<- TotalYield((TBXBH_025MSSYpr))

#find MSSY for BH a=0.5 on an intially unfished system
a=0.5
TBXBH_05<- TBst
TBXBH_05@gear_params$knife_edge_size <- TBXBH_05@species_params$w_mat
TBXBH_05@species_params$knife_edge_size <-   TBXBH_05@species_params$w_mat
gear_params(TBXBH_05)$gear <- "balanced"
species_params(TBXBH_05)$gear <- "balanced"
TBXBH_05 <- setRateFunction(TBXBH_05, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=2420, to=2440, by=5) 
Fmort <- TBXBH_05
TBXBH_05results <- MSSY(efforts, Fmort) #c=2430, MSSY=0.01920968

#projection for MSSY a=0.5 on initally unfished system
TBXBH_05MSSY <- TBXBH_05
TBXBH_05MSSY <- setFishing(TBXBH_05MSSY, initial_effort = 2430) 
TBXBH_05MSSYpr <-project(TBXBH_05MSSY, t_max=100)
TotalYield((TBXBH_05MSSYpr))[101] #0.01920968 

#later MSSY plots
TBXMSSYTotalBiomass05 <- TotalBiomass((TBXBH_05MSSYpr))
TBXMSSYTotalAbundance05 <-TotalAbundance(TBXBH_05MSSYpr)
TBXMSSYTotalYield05<- TotalYield((TBXBH_05MSSYpr))

#find MSSY for BH a=0.75 on initially unfished
a=0.75
TBXBH_075<- TBst
TBXBH_075@gear_params$knife_edge_size <- TBXBH_075@species_params$w_mat
TBXBH_075@species_params$knife_edge_size <-   TBXBH_075@species_params$w_mat
gear_params(TBXBH_075)$gear <- "balanced"
species_params(TBXBH_075)$gear <- "balanced"
TBXBH_075 <- setRateFunction(TBXBH_075, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=23400, to=23800, by=50) 
Fmort <- TBXBH_075
TBXBH_075results <- MSSY(efforts, Fmort) #c=23600, MSSY=0.01941574

#projection for a=0.5  MSSY
TBXBH_075MSSY <- TBXBH_075
TBXBH_075MSSY <- setFishing(TBXBH_075MSSY, initial_effort = 23600) 
TBXBH_075MSSYpr <-project(TBXBH_075MSSY, t_max=100)
TotalYield((TBXBH_075MSSYpr))[101]#0.01941574

#later plots
TBXMSSYTotalBiomass075 <- TotalBiomass((TBXBH_075MSSYpr))
TBXMSSYTotalAbundance075 <-TotalAbundance(TBXBH_075MSSYpr)
TBXMSSYTotalYield075<- TotalYield((TBXBH_075MSSYpr))

#find MSSY for BH a=1 ((production) on initially unfished
a=1
TBXBH_1<- TBst
TBXBH_1@gear_params$knife_edge_size <- TBXBH_1@species_params$w_mat
TBXBH_1@species_params$knife_edge_size <-   TBXBH_1@species_params$w_mat
gear_params(TBXBH_1)$gear <- "balanced"
species_params(TBXBH_1)$gear <- "balanced"
TBXBH_1 <- setRateFunction(TBXBH_1, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=230000, to=240000, by=2000) 
Fmort <- TBXBH_1
TBXBH_1results <- MSSY(efforts, Fmort) #c=234000, MSSY=0.01954793

#MSSY projection for a=1
TBXBH_1MSSY <- TBXBH_1
TBXBH_1MSSY <- setFishing(TBXBH_1MSSY, initial_effort = 234000) 
TBXBH_1MSSYpr <-project(TBXBH_1MSSY, t_max=100)
TotalYield((TBXBH_1MSSYpr))[101]#0.01954793

#later MSSY plots 
TBXMSSYTotalBiomass1 <- TotalBiomass((TBXBH_1MSSYpr))
TBXMSSYTotalAbundance1 <-TotalAbundance(TBXBH_1MSSYpr)
TBXMSSYTotalYield1 <- TotalYield((TBXBH_1MSSYpr))

#find MSSY for BH a=0 ((productivity) with initial state steady KE fishing 
a=0
TBBH_0<- TBXKEMSSYst 
TBBH_0@gear_params$knife_edge_size <- TBBH_0@species_params$w_mat
TBBH_0@species_params$knife_edge_size <-   TBBH_0@species_params$w_mat
gear_params(TBBH_0)$gear <- "balanced"
species_params(TBBH_0)$gear <- "balanced"
TBBH_0 <- setRateFunction(TBBH_0, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=26, to=27, by=0.5) 
Fmort <- TBBH_0 
TBBH_0results <- MSSY(efforts, Fmort) #c=26.5, MSSY=0.01815822

#MSSY projection for a=0 intially fished 
TBBH_0MSSY <- TBBH_0
TBBH_0MSSY <- setFishing(TBBH_0MSSY, initial_effort = 26.5) 
TBBH_0MSSYpr <-project(TBBH_0MSSY, t_max=100)
TotalYield((TBBH_0MSSYpr))[101]#0.01815822 
#later MSSY plots 
TBMSSYTotalBiomass0 <- TotalBiomass((TBBH_0MSSYpr))
TBMSSYTotalAbundance0 <-TotalAbundance(TBBH_0MSSYpr)
TBMSSYTotalYield0 <- TotalYield((TBBH_0MSSYpr))

#find fishing effort for a=0.25 MSSY
a=0.25
TBBH_025<- TBXKEMSSYst 
TBBH_025@gear_params$knife_edge_size <- TBBH_025@species_params$w_mat
TBBH_025@species_params$knife_edge_size <-   TBBH_025@species_params$w_mat
gear_params(TBBH_025)$gear <- "balanced"
species_params(TBBH_025)$gear <- "balanced"
TBBH_025 <- setRateFunction(TBBH_025, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=256, to=258, by=0.1) 
Fmort <- TBBH_025
TBBH_025results <- MSSY(efforts, Fmort) #c=257.2, MSSY=0.01886148

#PROJECTION
TBBH_025MSSY <- TBBH_025
TBBH_025MSSY <- setFishing(TBBH_025MSSY, initial_effort = 257.2) 
TBBH_025MSSYpr <-project(TBBH_025MSSY, t_max=100)
TotalYield((TBBH_025MSSYpr))[101] #0.01886148

#later MSSY plots
TBMSSYTotalBiomass025 <- TotalBiomass((TBBH_025MSSYpr))
TBMSSYTotalAbundance025 <-TotalAbundance(TBBH_025MSSYpr)
TBMSSYTotalYield025<- TotalYield((TBBH_025MSSYpr))

#find MSSY for BH a=0.5 (intermediate) with initial state steady KE fishing 
a=0.50
TBBH_05<- TBXKEMSSYst
TBBH_05@gear_params$knife_edge_size <- TBBH_05@species_params$w_mat
TBBH_05@species_params$knife_edge_size <-   TBBH_05@species_params$w_mat
gear_params(TBBH_05)$gear <- "balanced"
species_params(TBBH_05)$gear <- "balanced"
TBBH_05 <- setRateFunction(TBBH_05, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=2420, to=2440, by=5) 
Fmort <- TBBH_05 
TBBH_05results <- MSSY(efforts, Fmort) #c=2430, MSSY=0.01920970
#MSSY projection
TBBH_05MSSY <- TBBH_05
TBBH_05MSSY <- setFishing(TBBH_05MSSY, initial_effort = 2430) 
TBBH_05MSSYpr <-project(TBBH_05MSSY, t_max=100)
TotalYield((TBBH_05MSSYpr))[101]#0.0192097 
#later MSSY plots
TBMSSYTotalBiomass05 <- TotalBiomass((TBBH_05MSSYpr))
TBMSSYTotalAbundance05 <-TotalAbundance(TBBH_05MSSYpr)
TBMSSYTotalYield05<- TotalYield((TBBH_05MSSYpr))

#find MSSY for BH a=0 (intermediate) with initial state steady KE fishing 
a=0.75
TBBH_075<- TBXKEMSSYst
TBBH_075@gear_params$knife_edge_size <- TBBH_075@species_params$w_mat
TBBH_075@species_params$knife_edge_size <-   TBBH_075@species_params$w_mat
gear_params(TBBH_075)$gear <- "balanced"
species_params(TBBH_075)$gear <- "balanced"
TBBH_075 <- setRateFunction(TBBH_075, "FMort", "BalancedHarvest")

#Indentifying effort for MSSY
efforts <- seq(from=23500, to=23700, by=50) 
Fmort <- TBBH_075
TBBH_075results <- MSSY(efforts, Fmort) #c=23600, MSSY=0.01941576

#projection for MSSY
TBBH_075MSSY <- TBBH_075
TBBH_075MSSY <- setFishing(TBBH_075MSSY, initial_effort = 23600) 
TBBH_075MSSYpr <-project(TBBH_075MSSY, t_max=100)
TotalYield((TBBH_075MSSYpr))[101] #0.01941576 
#later plots
TBMSSYTotalBiomass075 <- TotalBiomass((TBBH_075MSSYpr))
TBMSSYTotalAbundance075 <-TotalAbundance(TBBH_075MSSYpr)
TBMSSYTotalYield075<- TotalYield((TBBH_075MSSYpr))

#find MSSY for BH a=1 ((productivity) with initial state steady KE fishing 
a=1
TBBH_1<- TBXKEMSSYst
TBBH_1@gear_params$knife_edge_size <- TBBH_1@species_params$w_mat
TBBH_1@species_params$knife_edge_size <-   TBBH_1@species_params$w_mat
gear_params(TBBH_1)$gear <- "balanced"
species_params(TBBH_1)$gear <- "balanced"
TBBH_1 <- setRateFunction(TBBH_1, "FMort", "BalancedHarvest")

efforts <- seq(from= 230000, to=240000, by=1000) 
Fmort <- TBBH_1
TBBH_1results <- MSSY(efforts, Fmort) #c=235000, MSSY=0.01954795

TBBH_1MSSY <- TBBH_1
TBBH_1MSSY <- setFishing(TBBH_1MSSY, initial_effort = 235000) 
TBBH_1MSSYpr <-project(TBBH_1MSSY, t_max=100)
TotalYield((TBBH_1MSSYpr))[101]#0.01954795

#later MSSY plots 
TBMSSYTotalBiomass1 <- TotalBiomass((TBBH_1MSSYpr))
TBMSSYTotalAbundance1 <-TotalAbundance(TBBH_1MSSYpr)
TBMSSYTotalYield1 <- TotalYield((TBBH_1MSSYpr))

#TB MSSY plots
TBMSSYdf <- data.frame(policy=rep(c("0", "0.25", "0.5", "0.75", "1", "KE"),2), Historically =rep(c("SaE fished","Unifished"),each=6),
                       Yield=c(TBMSSYTotalYield0[101], TBMSSYTotalYield025[101], TBMSSYTotalYield05[101],TBMSSYTotalYield075[101], 
                               TBMSSYTotalYield1[101], TBXMSSYTotalYieldKE[101], TBXMSSYTotalYield0[101], TBXMSSYTotalYield025[101],
                               TBXMSSYTotalYield05[101],TBXMSSYTotalYield075[101], TBXMSSYTotalYield1[101], TBXMSSYTotalYieldKE[101]),
                       Biomass= c(TBMSSYTotalBiomass0[101], TBMSSYTotalBiomass025[101], TBMSSYTotalBiomass05[101],TBMSSYTotalBiomass075[101], 
                                  TBMSSYTotalBiomass1[101], TBXMSSYTotalBiomassKE[101], TBXMSSYTotalBiomass0[101], TBXMSSYTotalBiomass025[101], 
                                  TBXMSSYTotalBiomass05[101],TBXMSSYTotalBiomass075[101], TBXMSSYTotalBiomass1[101], TBXMSSYTotalBiomassKE[101]),
                       Abundance= c(TBMSSYTotalAbundance0[101], TBMSSYTotalAbundance025[101], TBMSSYTotalAbundance05[101],TBMSSYTotalAbundance075[101], 
                                    TBMSSYTotalAbundance1[101], TBXMSSYTotalAbundanceKE[101], TBXMSSYTotalAbundance0[101], TBXMSSYTotalAbundance025[101], 
                                    TBXMSSYTotalAbundance05[101],TBXMSSYTotalAbundance075[101], TBXMSSYTotalAbundance1[101], TBXMSSYTotalAbundanceKE[101]))

TBMSSYYield <- ggplot(TBMSSYdf, aes(x=policy, y=Yield, fill=Historically)) + 
  geom_bar(stat="identity", position=position_dodge()) +coord_cartesian(ylim=c(0.018,0.022))+ labs(x=NULL, y='Yield/ g^3year^-1') +theme(legend.position="none")

TBMSSYBiomass<- ggplot(TBMSSYdf, aes(x=policy, y=Biomass, fill=Historically)) + 
  geom_bar(stat="identity", position=position_dodge()) +coord_cartesian(ylim=c(0.028,0.0325))+ labs(x=NULL, y='Biomass/ gm^3year^-1') + theme(legend.position="none")

TBMSSYAbundance<- ggplot(TBMSSYdf, aes(x=policy, y=Abundance, fill=Historically)) + 
  geom_bar(stat="identity", position=position_dodge()) +coord_cartesian(ylim=c(0.01,0.018))+ labs(x=NULL, y='Abdunance/m^3year^-1') +theme(
    legend.position = c(1, 1),
    legend.justification = c("left", "top"),
    legend.box.just = "left",
    legend.margin = margin(2,2,2,2)
  )

plot_grid(TBMSSYYield, TBMSSYBiomass, TBMSSYAbundance,ncol=2)

#############################################################################
#Now we are analysing fishing efforts which result in the same yield regardless of policy to compare policies rather than system initial conditions in a TB model
#we compare policies after KE fishing as this is the most realistic
#finding the initial state for all other policies 
TB01KE<- TBXKE
TB01KE<- setFishing(TB01KE, initial_effort = 1.030049) #0.01 
TB01KEst <-projectToSteady(TB01KE) #checking steady 
TB01KEpr <-project(TB01KEst, t_max=100)
TotalYield(TB01KEpr)[101] #0.01

#for later plots
TB01BiomassKE <- getBiomass((TB01KEpr))
TB01AbundanceKE <-getN(TB01KEpr)
TB01YieldKE <- getYield((TB01KEpr))
TB01TotalBiomassKE <- TotalBiomass((TB01KEpr))
TB01TotalAbundanceKE <-TotalAbundance(TB01KEpr)
TB01TotalYieldKE <- TotalYield((TB01KEpr))

#yield=0.01 projection for a=0 (production)
a=0
TB01BH_0<- TB01KEst
TB01BH_0@gear_params$knife_edge_size <- TB01BH_0@species_params$w_mat
TB01BH_0@species_params$knife_edge_size <-   TB01BH_0@species_params$w_mat
gear_params(TB01BH_0)$gear <- "balanced"
species_params(TB01BH_0)$gear <- "balanced"
TB01BH_0 <- setRateFunction(TB01BH_0, "FMort", "BalancedHarvest")
TB01BH_0 <- setFishing(TB01BH_0, initial_effort = 3.079272) #0.01
#TB01BH_0st <-projectToSteady(TB01BH_0)
TB01BH_0pr <- project(TB01BH_0, t_max=100)
TotalYield(TB01BH_1pr)[101] #0.01
#later plots 
TB01Yield0 <- getYield((TB01BH_0pr ))
TB01Biomass0 <- getBiomass((TB01BH_0pr ))
TB01Abundance0 <-getN(TB01BH_0pr )
TB01TotalYield0 <- TotalYield((TB01BH_0pr ))
TB01TotalBiomass0 <- TotalBiomass((TB01BH_0pr ))
TB01TotalAbundance0 <-TotalAbundance(TB01BH_0pr )

#yield=0.01 projection for a=0.25
a<-0.25
TB01BH_025<- TB01KEst
TB01BH_025@gear_params$knife_edge_size <- TB01BH_025@species_params$w_mat
TB01BH_025@species_params$knife_edge_size <-   TB01BH_025@species_params$w_mat
gear_params(TB01BH_025)$gear <- "balanced"
species_params(TB01BH_025)$gear <- "balanced"
TB01BH_025 <- setRateFunction(TB01BH_025, "FMort", "BalancedHarvest")
TB01BH_025 <- setFishing(TB01BH_025, initial_effort = 16.91983) 
TB01BH_025pr <- project(TB01BH_025, t_max=100)
TotalYield(TB01BH_025pr)[101] #0.01

#later plots 
TB01Yield025 <- getYield((TB01BH_025pr))
TB01Biomass025 <- getBiomass((TB01BH_025pr))
TB01Abundance025 <-getN(TB01BH_025pr)
TB01TotalYield025 <- TotalYield((TB01BH_025pr))
TB01TotalBiomass025 <- TotalBiomass((TB01BH_025pr))
TB01TotalAbundance025 <-TotalAbundance(TB01BH_025pr)

#projection for a=0.5 (intermediate)
a<-0.5
TB01BH_05<- TB01KEst
TB01BH_05@gear_params$knife_edge_size <- TB01BH_05@species_params$w_mat
TB01BH_05@species_params$knife_edge_size <-   TB01BH_05@species_params$w_mat
gear_params(TB01BH_05)$gear <- "balanced"
species_params(TB01BH_05)$gear <- "balanced"
TB01BH_05 <- setRateFunction(TB01BH_05, "FMort", "BalancedHarvest")
TB01BH_05 <- setFishing(TB01BH_05, initial_effort = 93.046) 
TB01BH_05pr <- project(TB01BH_05, t_max=100)
TotalYield(TB01BH_05pr)[101] #0.01
#later plots 
TB01Yield05 <- getYield((TB01BH_05pr))
TB01Biomass05 <- getBiomass((TB01BH_05pr))
TB01Abundance05 <-getN(TB01BH_05pr)
TB01TotalYield05 <- TotalYield((TB01BH_05pr))
TB01TotalBiomass05 <- TotalBiomass((TB01BH_05pr))
TB01TotalAbundance05 <-TotalAbundance(TB01BH_05pr)

#projection for a=0.75 (intermediate)
a<-0.75
TB01BH_075<- TB01KEst
TB01BH_075@gear_params$knife_edge_size <- TB01BH_075@species_params$w_mat
TB01BH_075@species_params$knife_edge_size <-   TB01BH_075@species_params$w_mat
gear_params(TB01BH_075)$gear <- "balanced"
species_params(TB01BH_075)$gear <- "balanced"
TB01BH_075 <- setRateFunction(TB01BH_075, "FMort", "BalancedHarvest")
TB01BH_075 <- setFishing(TB01BH_075, initial_effort = 512.192) 
TB01BH_075pr <- project(TB01BH_075, t_max=100)
TotalYield(TB01BH_075pr)[101] #0.01 
#later plots 
TB01Yield075 <- getYield((TB01BH_075pr))
TB01Biomass075 <- getBiomass((TB01BH_075pr))
TB01Abundance075 <-getN(TB01BH_075pr)
TB01TotalYield075 <- TotalYield((TB01BH_075pr))
TB01TotalBiomass075 <- TotalBiomass((TB01BH_075pr))
TB01TotalAbundance075 <-TotalAbundance(TB01BH_075pr)

#projection for a=1 (production)
a=1
TB01BH_1<- TB01KEst
TB01BH_1@gear_params$knife_edge_size <- TB01BH_1@species_params$w_mat
TB01BH_1@species_params$knife_edge_size <-   TB01BH_1@species_params$w_mat
gear_params(TB01BH_1)$gear <- "balanced"
species_params(TB01BH_1)$gear <- "balanced"
TB01BH_1 <- setRateFunction(TB01BH_1, "FMort", "BalancedHarvest")
TB01BH_1 <- setFishing(TB01BH_1, initial_effort = 2822.523) 
TB01BH_1pr <- project(TB01BH_1, t_max=100)
TotalYield(TB01BH_1pr)[101] #0.01
#later plots 
TB01Yield1 <- getYield((TB01BH_1pr ))
TB01Biomass1 <- getBiomass((TB01BH_1pr ))
TB01Abundance1 <-getN(TB01BH_1pr )
TB01TotalYield1 <- TotalYield((TB01BH_1pr ))
TB01TotalBiomass1 <- TotalBiomass((TB01BH_1pr ))
TB01TotalAbundance1 <-TotalAbundance(TB01BH_1pr )

#TB01 plots, #need to change the limits
matplot(cbind(TB01TotalYieldKE, TB01TotalYield0, TB01TotalYield025, TB01TotalYield05, TB01TotalYield075, TB01TotalYield1),
        log = "xy", type = "l", main = "Total Yield",
        xlab = "Year", ylab = "Yield/ gm^3year^-1", col = c(1,7,3,4,5,2), lty=c(4,1,2,2,2,1),lwd = 3, bty = "l",cex.axis=1.2,cex.lab=1.2, xlim = c(1,20)) 
legend(x='topright',legend = c('KE', 'a=1','a=0.25', 'a=0.5','a=0.75', 'a=1'), lty = c(4,1,2,2,2,1), col = c(1,7,3,4,5,2), lwd=3, cex=1)

TB01YieldKEdf <- data.frame(yield100=TB01YieldKE[101,], yield0=TB01YieldKE[1,], species=1:12)
TB01Yield0df <- data.frame(yield100=TB01Yield0[101,], yield0=TB01Yield0[1,], species=1:12)
TB01Yield025df <- data.frame(yield100=TB01Yield025[101,], yield0=TB01Yield025[1,], species=1:12)
TB01Yield05df <- data.frame(yield100=TB01Yield05[101,], yield0=TB01Yield05[1,], species=1:12)
TB01Yield075df <- data.frame(yield100=TB01Yield075[101,], yield0=TB01Yield075[1,], species=1:12)
TB01Yield1df <- data.frame(yield100=TB01Yield1[101,], yield0=TB01Yield1[1,], species=1:12)

TB01YieldKEplot <- ggplot(TB01YieldKEdf, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = NULL, y = "Yield /gm^3year^-1") +scale_y_log10(limits = c(0.3e-4, 5e-3)) 
#scale_x_discrete(breaks = 1:12, labels = paste0(1:12))+ylim(0, 0.004)

TB01Yield0plot<- ggplot(TB01Yield0df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +scale_y_log10(limits = c(0.3e-4, 5e-3)) 
#scale_x_discrete(breaks = 1:12, labels = paste0(1:12))+ylim(0, 0.004)

TB01Yield025plot<-ggplot(TB01Yield025df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +scale_y_log10(limits=c(0.3e-4, 5e-3))

#scale_x_discrete(breaks = 1:12, labels = paste0(1:12))+ylim(0, 0.004)

TB01Yield05plot<- ggplot(TB01Yield05df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = 'Species', y = 'Yield/gm^3year^-1') +scale_y_log10(limits=c(0.3e-4, 5e-3))

#scale_x_discrete(breaks = 1:12, labels = paste0(1:12))+ylim(0, 0.005)

TB01Yield075plot<- ggplot(TB01Yield075df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = 'Species', y = NULL) + scale_y_log10(limits =c(0.3e-4, 5e-3))
#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0, 0.005) 

TB01Yield1plot<- ggplot(TB01Yield1df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = 'Species', y = NULL) + scale_y_log10(limits = c(0.3e-4, 5e-3))

#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0, 0.005) 

TB01RelativeBiomass0df <- data.frame(RelativeBiomass=TB01Biomass0[101,]/TB01BiomassKE[101,], species=1:12)
TB01RelativeBiomass025df <- data.frame(RelativeBiomass=TB01Biomass025[101,]/TB01BiomassKE[101,], species=1:12)
TB01RelativeBiomass05df <- data.frame(RelativeBiomass=TB01Biomass05[101,]/TB01BiomassKE[101,], species=1:12)
TB01RelativeBiomass075df <- data.frame(RelativeBiomass=TB01Biomass075[101,]/TB01BiomassKE[101,], species=1:12)
TB01RelativeBiomass1df <- data.frame(RelativeBiomass=TB01Biomass1[101,]/TB01BiomassKE[101,], species=1:12)

TB01Biomass0plot <- ggplot(TB01RelativeBiomass0df, aes(x = factor(species)))+geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +
    labs(x = NULL, y = 'Relative Biomass') +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+
    geom_hline(yintercept = (TB01TotalBiomass0[101]/TB01TotalBiomassKE[101]),  linetype = "dashed", color = "red")

TB01Biomass025plot <- ggplot(TB01RelativeBiomass025df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2)+ 
    labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
    geom_hline(yintercept = (TB01TotalBiomass025[101]/TB01TotalBiomassKE[101]), linetype = "dashed", color = "red") 

TB01Biomass05plot <- ggplot(TB01RelativeBiomass05df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
  geom_hline(yintercept = (TB01TotalBiomass05[101]/TB01TotalBiomassKE[101]), linetype = "dashed", color = "red") 


TB01Biomass075plot <- ggplot(TB01RelativeBiomass075df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
  geom_hline(yintercept = (TB01TotalBiomass075[101]/TB01TotalBiomassKE[101]), linetype = "dashed", color = "red") 

TB01Biomass1plot <- ggplot(TB01RelativeBiomass1df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
  geom_hline(yintercept = (TB01TotalBiomass1[101]/TB01TotalBiomassKE[101]), linetype = "dashed", color = "red") 

TB01RelativeAbundance0df <- data.frame(RelativeAbundance=TB01Abundance0[101,]/TB01AbundanceKE[101,], species=1:12)
TB01RelativeAbundance025df <- data.frame(RelativeAbundance=TB01Abundance025[101,]/TB01AbundanceKE[101,], species=1:12)
TB01RelativeAbundance05df <- data.frame(RelativeAbundance=TB01Abundance05[101,]/TB01AbundanceKE[101,], species=1:12)
TB01RelativeAbundance075df <- data.frame(RelativeAbundance=TB01Abundance075[101,]/TB01AbundanceKE[101,], species=1:12)
TB01RelativeAbundance1df <- data.frame(RelativeAbundance=TB01Abundance1[101,]/TB01AbundanceKE[101,], species=1:12)

TB01Abundance0plot <- ggplot(TB01RelativeAbundance0df, aes(x = factor(species)))+ geom_point(aes(y = RelativeAbundance), shape=19, size=2) + labs(x = 'Species', y = 'Relative Abundance') +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +geom_hline(yintercept = (TB01TotalAbundance0[101]/TB01TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
TB01Abundance025plot <- ggplot(TB01RelativeAbundance025df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") + geom_hline(yintercept = (TB01TotalAbundance025[101]/TB01TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
TB01Abundance05plot <- ggplot(TB01RelativeAbundance05df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +geom_hline(yintercept = (TB01TotalAbundance05[101]/TB01TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
TB01Abundance075plot<- ggplot(TB01RelativeAbundance075df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") + geom_hline(yintercept = (TB01TotalAbundance075[101]/TB01TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
TB01Abundance1plot<- ggplot(TB01RelativeAbundance1df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") + geom_hline(yintercept = (TB01TotalAbundance1[101]/TB01TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 

plot_grid(TB01Biomass0plot, TB01Biomass025plot, TB01Biomass05plot, TB01Biomass075plot, TB01Biomass1plot, TB01Abundance0plot, TB01Abundance025plot, TB01Abundance05plot, TB01Abundance075plot, TB01Abundance1plot, ncol = 5)
plot_grid(TB01YieldKEplot, TB01Yield0plot, TB01Yield025plot, TB01Yield05plot, TB01Yield075plot, TB01Yield1plot,ncol=3)


#All analysis after this point is on the North Sea model 
#setting up the model 
NS <- NS_params
gear_params(NS)$gear <- "None"
species_params(NS)$gear <- "None"
NSst <- projectToSteady(NS, effort = 0)
NSpr <- project(NSst, t_max=100)


#First we look for the MSSY of each policy on a steady NS model whose system is initially unfished system (X denotes initially unfished)

#maturity Knife edge cut off 
NSXKE <- NSst
gear_params(NSXKE)$gear <- "Knife Edge"
species_params(NSXKE)$gear <- "Knife Edge"
gear_params(NSXKE)$knife_edge_size <- NSXKE@species_params$w_mat
species_params(NSXKE)$knife_edge_size <- NSXKE@species_params$w_mat

efforts <- seq(from=1.26, to=1.27 , by=0.01) #won't converge >1.27

Fmort <- NSXKE
NSXKE_1results <- NSMSSY(efforts, Fmort) #c=1.27, Yield= 2.877839e+12

#projection for MSSY 
NSXMSSYKE <- NSXKE
NSXMSSYKEst <- projectToSteady(NSXMSSYKE, effort=1.27)
NSXMSSYKEpr <- project(NSXMSSYKE, effort=1.27)
TotalYield(NSXMSSYKEpr)[101] #2.877839e+12
NSXMSSYTotalYieldKE <- TotalYield(NSXMSSYKEpr) 
NSXMSSYTotalBiomassKE <-TotalBiomass(NSXMSSYKEpr)
NSXMSSYTotalAbundanceKE <- TotalAbundance(NSXMSSYKEpr)


#finding MSSY for a=0
a=0
NSXBH_0<- NSst
gear_params(NSXBH_0)$gear <- "balanced"
species_params(NSXBH_0)$gear <- "balanced"
gear_params(NSXBH_0)$knife_edge_size <- NSXBH_0@species_params$w_mat
species_params(NSXBH_0)$knife_edge_size <- NSXBH_0@species_params$w_mat
NSXBH_0 <- setRateFunction(NSXBH_0, "FMort", "BalancedHarvest")

#find MSSY
efforts <- seq(from=3.9,to=4, by=0.1) #>3.9
Fmort <- NSXBH_0
NSXBH_0results <- NSMSSY(efforts, Fmort) #c=1.27, Yield= 2.916919e+12

#Projection for MSSY
NSXBH_0MSSY <-NSXBH_0
NSXBH_0MSSY <- setFishing(NSXBH_0MSSY, initial_effort = 3.9) 
NSXBH_0MSSYpr <-project(NSXBH_0MSSY, t_max=100)
TotalYield((NSXBH_0MSSYpr))[101] #2.916919e+12
#later MSSY plots 
NSXMSSYTotalBiomass0 <- TotalBiomass((NSXBH_0MSSYpr))
NSXMSSYTotalAbundance0 <-TotalAbundance(NSXBH_0MSSYpr)
NSXMSSYTotalYield0 <- TotalYield((NSXBH_0MSSYpr))


#finding MSSY for a=0.25 on an initially unfished system 
a=0.25
NSXBH_025<- NSst
gear_params(NSXBH_025)$gear <- "balanced"
species_params(NSXBH_025)$gear <- "balanced"
gear_params(NSXBH_025)$knife_edge_size <- NSXBH_025@species_params$w_mat
species_params(NSXBH_025)$knife_edge_size <- NSXBH_025@species_params$w_mat
NSXBH_025 <- setRateFunction(NSXBH_025, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=0.4,to=0.5, by=0.1) #>0.4 doesn't converge --> 4.508873e+12 
Fmort <- NSXBH_025
NSXBH_025results <- NSMSSY(efforts, Fmort) #c=0.4, Yield= 4.508873e+12

#projection for MSSY
NSXBH_025MSSY <-NSXBH_025 
NSXBH_025MSSY <- setFishing(NSXBH_025MSSY, initial_effort = 0.4) 
NSXBH_025MSSYpr <-project(NSXBH_025MSSY, t_max=100)
TotalYield((NSXBH_025MSSYpr))[101]#4.508873e+12 
#later MSSY plots
NSXMSSYTotalBiomass025 <- TotalBiomass((NSXBH_025MSSYpr))
NSXMSSYTotalAbundance025 <-TotalAbundance(NSXBH_025MSSYpr)
NSXMSSYTotalYield025<- TotalYield((NSXBH_025MSSYpr))

#finding MSSY for a=0.5
a=0.5
NSXBH_05<- NSst
gear_params(NSXBH_05)$gear <- "balanced"
species_params(NSXBH_05)$gear <- "balanced"
gear_params(NSXBH_05)$knife_edge_size <- NSXBH_05@species_params$w_mat
species_params(NSXBH_05)$knife_edge_size <- NSXBH_05@species_params$w_mat
NSXBH_05 <- setRateFunction(NSXBH_05, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=29e-3,to=30e-3, by=1e-3) #> 29e-3 doesn't converge 
Fmort <- NSXBH_05
NSXBH_05results <- NSMSSY(efforts, Fmort) #c=29e-3, Yield= 4.616958e+12

#projection for MSSY

NSXBH_05MSSY <- NSXBH_05
NSXBH_05MSSY <- setFishing(NSXBH_05MSSY, initial_effort = 29e-3) 
NSXBH_05MSSYst <-projectToSteady(NSXBH_05MSSY)
NSXBH_05MSSYpr <-project(NSXBH_05MSSY, t_max=100)
TotalYield((NSXBH_05MSSYpr))[101]#4.616958e+12 
#later MSSY plots
NSXMSSYTotalBiomass05 <- TotalBiomass((NSXBH_05MSSYpr))
NSXMSSYTotalAbundance05 <-TotalAbundance(NSXBH_05MSSYpr)
NSXMSSYTotalYield05<- TotalYield((NSXBH_05MSSYpr))


#finding MSSY for a=0.75 on an initially unfished system 
a=0.75
NSXBH_075<- NSst
gear_params(NSXBH_075)$gear <- "balanced"
species_params(NSXBH_075)$gear <- "balanced"
gear_params(NSXBH_075)$knife_edge_size <- NSXBH_075@species_params$w_mat
species_params(NSXBH_075)$knife_edge_size <- NSXBH_075@species_params$w_mat
NSXBH_075 <- setRateFunction(NSXBH_075, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=3033e-6,to=3034e-6, by=1e-6) #>3033e-6 doesnt converge 
Fmort <- NSXBH_075
NSXBH_075results <- NSMSSY(efforts, Fmort) #c=3033e-6, Yield= 4.631252e+12


#projection for MSSY
NSXBH_075MSSY <-NSXBH_075
NSXBH_075MSSY <- setFishing(NSXBH_075MSSY, initial_effort = 3033e-6) 
NSXBH_075MSSYst <-projectToSteady(NSXBH_075MSSY)
NSXBH_075MSSYpr <-project(NSXBH_075MSSY, t_max=100)
TotalYield((NSXBH_075MSSYpr))[101] #4.631252e+12 
#later plots
NSXMSSYTotalBiomass075 <- TotalBiomass((NSXBH_075MSSYpr))
NSXMSSYTotalAbundance075 <-TotalAbundance(NSXBH_075MSSYpr)
NSXMSSYTotalYield075<- TotalYield((NSXBH_075MSSYpr))

#finding MSSY for a=1
a=1
NSXBH_1<- NSst
gear_params(NSXBH_1)$gear <- "balanced"
species_params(NSXBH_1)$gear <- "balanced"
gear_params(NSXBH_1)$knife_edge_size <- NSXBH_1@species_params$w_mat
species_params(NSXBH_1)$knife_edge_size <- NSXBH_1@species_params$w_mat
NSXBH_1 <- setRateFunction(NSXBH_1, "FMort", "BalancedHarvest")

#Testing efforts 
efforts <- seq(from=3.9e-6,to=4e-6, by=0.1e-6) #doesnt converge >3.9
NSXBH_1results <- NSMSSY(efforts, Fmort) #c=3.9e-6, Yield= 4.675532e+12

#projection for MSSY
NSXBH_1MSSY <- NSXBH_1
NSXBH_1MSSY <- setFishing(NSXBH_1MSSY, initial_effort = 3.9e-6) 
NSXBH_1MSSYpr <-project(NSXBH_1MSSY, t_max=100)
TotalYield((NSXBH_1MSSYpr))[101] #4.675532e+12 
#later MSSY plots 
NSXMSSYTotalBiomass1 <- TotalBiomass((NSXBH_1MSSYpr))
NSXMSSYTotalAbundance1 <-TotalAbundance(NSXBH_1MSSYpr)
NSXMSSYTotalYield1 <- TotalYield((NSXBH_1MSSYpr))

#Now finding MSSY for each BH policy with initial conditions fished at KE MSSY
#finding MSSY for a=0 initially fished kE
a=0
NSBH_0<- NSXMSSYKEst
gear_params(NSBH_0)$gear <- "balanced"
species_params(NSBH_0)$gear <- "balanced"
gear_params(NSBH_0)$knife_edge_size <- NSBH_0@species_params$w_mat
species_params(NSBH_0)$knife_edge_size <- NSBH_0@species_params$w_mat
NSBH_0 <- setRateFunction(NSBH_0, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=2.725,to=2.726, by=0.001) #>2.725 doesnt converge 
Fmort <- NSBH_0 
NSBH_0results <- NSMSSY(efforts, Fmort) #c=2.725 Yield= 2.561979e+12

#projection for MSSY
NSBH_0MSSY <- NSBH_0
NSBH_0MSSY <- setFishing(NSBH_0MSSY, initial_effort = 2.725) 
NSBH_0MSSYpr <-project(NSBH_0MSSY, t_max=100)
TotalYield((NSBH_0MSSYpr))[101]#2.561979e+12 
#later MSSY plots 
NSMSSYTotalBiomass0 <- TotalBiomass((NSBH_0MSSYpr))
NSMSSYTotalAbundance0 <-TotalAbundance(NSBH_0MSSYpr)
NSMSSYTotalYield0 <- TotalYield((NSBH_0MSSYpr))

#finding MSSY for a=0.25 initially fished kE
a=0.25
NSBH_025<- NSXMSSYKEst
gear_params(NSBH_025)$gear <- "balanced"
species_params(NSBH_025)$gear <- "balanced"
gear_params(NSBH_025)$knife_edge_size <- NSBH_025@species_params$w_mat
species_params(NSBH_025)$knife_edge_size <- NSBH_025@species_params$w_mat
NSBH_025 <- setRateFunction(NSBH_025, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=63.6e-4,to=63.7e-4, by=0.1e-4) #>63.6e-4 doesn't converge 
Fmort <- NSBH_025
NSBH_025results <- NSMSSY(efforts, Fmort) #c=63.6e-4, yield=2.816340e+12

#projection 
NSBH_025MSSY <-NSBH_025 
NSBH_025MSSY <- setFishing(NSBH_025MSSY, initial_effort = 63.6e-4) 
NSBH_025MSSYst <-projectToSteady(NSBH_025MSSY)
NSBH_025MSSYpr <-project(NSBH_025MSSY, t_max=100)
TotalYield((NSBH_025MSSYpr))[101]#2.816340e+12
#later MSSY plots
NSMSSYTotalBiomass025 <- TotalBiomass((NSBH_025MSSYpr))
NSMSSYTotalAbundance025 <-TotalAbundance(NSBH_025MSSYpr)
NSMSSYTotalYield025<- TotalYield((NSBH_025MSSYpr))

#finding MSSY for a=0.25 initially fished kE
a=0.5
NSBH_05<- NSXMSSYKEst
gear_params(NSBH_05)$gear <- "balanced"
species_params(NSBH_05)$gear <- "balanced"
gear_params(NSBH_05)$knife_edge_size <- NSBH_05@species_params$w_mat
species_params(NSBH_05)$knife_edge_size <- NSBH_05@species_params$w_mat
NSBH_05 <- setRateFunction(NSBH_05, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=15.75e-6,to=15.76e-6, by=0.01e-6) #> 15.7doesn't converge 
Fmort <- NSBH_05
NSBH_05results <- NSMSSY(efforts, Fmort) #c=15.75e-6, yield=2.985762e+12

#projection
NSBH_05MSSY <-NSBH_05 
NSBH_05MSSY <- setFishing(NSBH_05MSSY, initial_effort = 15.75e-6) 
NSBH_05MSSYst <-projectToSteady(NSBH_05MSSY)
NSBH_05MSSYpr <-project(NSBH_05MSSY, t_max=100)
TotalYield((NSBH_05MSSYpr))[101] #2.985762e+12  
#later MSSY plots
NSMSSYTotalBiomass05 <- TotalBiomass((NSBH_05MSSYpr))
NSMSSYTotalAbundance05 <-TotalAbundance(NSBH_05MSSYpr)
NSMSSYTotalYield05<- TotalYield((NSBH_05MSSYpr))

#finding MSSY for a=0.75 initially fished kE
a=0.75
NSBH_075<- NSXMSSYKEst
gear_params(NSBH_075)$gear <- "balanced"
species_params(NSBH_075)$gear <- "balanced"
gear_params(NSBH_075)$knife_edge_size <- NSBH_075@species_params$w_mat
species_params(NSBH_075)$knife_edge_size <- NSBH_075@species_params$w_mat
NSBH_075 <- setRateFunction(NSBH_075, "FMort", "BalancedHarvest")

#Testing efforts for BH_pb
efforts <- seq(from=40.11e-9,to=40.12e-9, by=0.01e-9) #>40.11 doesnt converge
Fmort <- NSBH_075
NSBH_075results <- NSMSSY(efforts, Fmort) #c=40.11e-9, yield=3.063074e+12

#projection
NSBH_075MSSY <- NSBH_075
NSBH_075MSSY <- setFishing(NSBH_075MSSY, initial_effort = 40.11e-9) 
NSBH_075MSSYpr <-project(NSBH_075MSSY, t_max=100)
TotalYield((NSBH_075MSSYpr))[101] #3.063074e+12
#later plots
NSMSSYTotalBiomass075 <- TotalBiomass((NSBH_075MSSYpr))
NSMSSYTotalAbundance075 <-TotalAbundance(NSBH_075MSSYpr)
NSMSSYTotalYield075<- TotalYield((NSBH_075MSSYpr))


#finding MSSY for a=1 initially fished kE
a=1
NSBH_1<- NSXMSSYKEst
gear_params(NSBH_1)$gear <- "balanced"
species_params(NSBH_1)$gear <- "balanced"
gear_params(NSBH_1)$knife_edge_size <- NSBH_1@species_params$w_mat
species_params(NSBH_1)$knife_edge_size <- NSBH_1@species_params$w_mat
NSBH_1 <- setRateFunction(NSBH_1, "FMort", "BalancedHarvest")

efforts <- seq(from=103.5e-12,to=103.6e-12, by=0.1e-12) #>103.5 doesn't converge
Fmort <- NSBH_1
NSBH_1results <- NSMSSY(efforts, Fmort) #c=103.5e-12, yield=3.128320e+12

#projection
NSBH_1MSSY <- NSBH_1
NSBH_1MSSY <- setFishing(NSBH_1MSSY, initial_effort = 103.5e-12) 
NSBH_1MSSYpr <-project(NSBH_1MSSY, t_max=100)
TotalYield((NSBH_1MSSYpr))[101] #3.12832e+12

#later MSSY plots 
NSMSSYTotalBiomass1 <- TotalBiomass((NSBH_1MSSYpr))
NSMSSYTotalAbundance1 <-TotalAbundance(NSBH_1MSSYpr)
NSMSSYTotalYield1 <- TotalYield((NSBH_1MSSYpr))

#NS MSSY PLOTS
NSMSSYdf <- data.frame(policy=rep(c("0", "0.25", "0.5", "0.75", "1", "KE"),2), Historically =rep(c("SaE fished","Unifished"),each=6),
                       Yield=c(NSMSSYTotalYield0[101], NSMSSYTotalYield025[101], NSMSSYTotalYield05[101],NSMSSYTotalYield075[101], 
                               NSMSSYTotalYield1[101],NSXMSSYTotalYieldKE[101],  NSXMSSYTotalYield0[101],NSXMSSYTotalYield025[101], 
                               NSXMSSYTotalYield05[101],NSXMSSYTotalYield075[101],  NSXMSSYTotalYield1[101],  NSXMSSYTotalYieldKE[101]), 
                        Biomass= c(NSMSSYTotalBiomass0[101], NSMSSYTotalBiomass025[101], NSMSSYTotalBiomass05[101], NSMSSYTotalBiomass075[101],
                                  NSMSSYTotalBiomass1[101], NSXMSSYTotalBiomassKE[101], NSXMSSYTotalBiomass0[101], NSXMSSYTotalBiomass025[101],
                                  NSXMSSYTotalBiomass05[101], NSXMSSYTotalBiomass075[101], NSXMSSYTotalBiomass1[101], NSXMSSYTotalBiomassKE[101]), 
                       Abundance= c(NSMSSYTotalAbundance0[101], NSMSSYTotalAbundance025[101],NSMSSYTotalAbundance05[101], NSMSSYTotalAbundance075[101],
                                    NSMSSYTotalAbundance1[101], NSXMSSYTotalAbundanceKE[101],NSXMSSYTotalAbundance0[101], NSXMSSYTotalAbundance025[101],
                                    NSXMSSYTotalAbundance05[101], NSXMSSYTotalAbundance075[101], NSXMSSYTotalAbundance1[101], NSXMSSYTotalAbundanceKE[101]))


NSMSSYYield <- ggplot(NSMSSYdf, aes(x=policy, y=Yield, fill=Historically)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x=NULL, y='Yield/ gyear^-1') +theme(legend.position="none")+
  coord_cartesian(ylim=c(2e+12,5e+12)) 

NSMSSYBiomass<- ggplot(NSMSSYdf, aes(x=policy, y=Biomass, fill=Historically)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x=NULL, y='Biomass/ gyear^-1') + theme(legend.position="none")+
  coord_cartesian(ylim=c(5e+12,6.5e+12)) 

NSMSSYAbundance<- ggplot(NSMSSYdf, aes(x=policy, y=Abundance, fill=Historically)) + 
  geom_bar(stat="identity", position=position_dodge()) + labs(x=NULL, y='Abdunance/year^-1') +
  coord_cartesian(ylim=c(1e+12,8.5e+12))+
  theme(legend.position = c(1, 1),legend.justification = c("left", "top"),legend.box.just = "left",legend.margin = margin(2,2,2,2))


plot_grid(NSMSSYYield, NSMSSYBiomass, NSMSSYAbundance,ncol=2)


#Now we are analyzing fishing efforts which result in the same yield regardless of policy to compare policies rather than system initial conditions in a NS model
#we compare policies after KE fishing as this is the most realistic
#finding the initial state for all other policies 

#for KE fishing
NSe12KE <- NSst
gear_params(NSe12KE)$gear <- "knife edge"
species_params(NSe12KE)$gear <- "knife edge"
gear_params(NSe12KE)$knife_edge_size <- species_params(NSe12KE)$w_mat
species_params(NSe12KE)$knife_edge_size <- species_params(NSe12KE)$w_mat
NSe12KEst <- projectToSteady(NSe12KE, effort=0.1553028)
NSe12KEpr <- project(NSe12KEst, effort=0.1553028, t_max=100)
TotalYield(NSe12KEpr)[101] #1e+12 

#plots for later 
NSe12YieldKE<- getYield(NSe12KEpr)
NSe12BiomassKE <- getBiomass((NSe12KEpr))
NSe12AbundanceKE <-getN(NSe12KEpr)
NSe12TotalBiomassKE <- TotalBiomass((NSe12KEpr))
NSe12TotalAbundanceKE <-TotalAbundance(NSe12KEpr)
NSe12TotalYieldKE <- TotalYield(NSe12KEpr)

#for a=0
a=0
NSe12BH_0<- NSe12KEst
gear_params(NSe12BH_0)$gear <- "balanced"
species_params(NSe12BH_0)$gear <- "balanced"
gear_params(NSe12BH_0)$knife_edge_size <- NSe12BH_0@species_params$w_mat
species_params(NSe12BH_0)$knife_edge_size <- NSe12BH_0@species_params$w_mat
NSe12BH_0 <- setFishing(NSe12BH_0, initial_effort =0.5891637) 
NSe12BH_0 <- setRateFunction(NSe12BH_0, "FMort", "BalancedHarvest")
NSe12BH_0pr <- project(NSe12BH_0, t_max=100)
TotalYield(NSe12BH_0pr)[101] #1e+12 

#FOR LATER PLOTS
NSe12Yield0 <- getYield((NSe12BH_0pr))
NSe12Biomass0 <- getBiomass((NSe12BH_0pr))
NSe12Abundance0 <-getN(NSe12BH_0pr)
NSe12TotalBiomass0 <- TotalBiomass((NSe12BH_0pr))
NSe12TotalAbundance0 <-TotalAbundance(NSe12BH_0pr)
NSe12TotalYield0 <- TotalYield(NSe12BH_0pr)

a<-0.25
NSe12BH_025<-NSe12KEst
gear_params(NSe12BH_025)$gear <- "balanced"
species_params(NSe12BH_025)$gear <- "balanced"
gear_params(NSe12BH_025)$knife_edge_size <- NSe12BH_025@species_params$w_mat
species_params(NSe12BH_025)$knife_edge_size <- NSe12BH_025@species_params$w_mat
NSe12BH_025 <- setFishing(NSe12BH_025, initial_effort =7.74266e-4) 
NSe12BH_025<- setRateFunction(NSe12BH_025, "FMort", "BalancedHarvest")
NSe12BH_025pr <- project(NSe12BH_025, t_max=100)
TotalYield(NSe12BH_025pr)[101]  #1e+12 

#FOR LATER PLOTS
NSe12Yield025 <- getYield((NSe12BH_025pr))
NSe12Biomass025 <- getBiomass((NSe12BH_025pr))
NSe12Abundance025 <- getN(NSe12BH_025pr)
NSe12TotalBiomass025 <- TotalBiomass((NSe12BH_025pr))
NSe12TotalAbundance025 <-TotalAbundance(NSe12BH_025pr)
NSe12TotalYield025 <- TotalYield(NSe12BH_025pr)

#projection for a=0.5
a<-0.5
NSe12BH_05<-NSe12KEst
gear_params(NSe12BH_05)$gear <- "balanced"
species_params(NSe12BH_05)$gear <- "balanced"
gear_params(NSe12BH_05)$knife_edge_size <- NSe12BH_05@species_params$w_mat
species_params(NSe12BH_05)$knife_edge_size <- NSe12BH_05@species_params$w_mat
NSe12BH_05 <- setFishing(NSe12BH_05, initial_effort =9.68017e-7) 
NSe12BH_05<- setRateFunction(NSe12BH_05, "FMort", "BalancedHarvest")
NSe12BH_05pr <- project(NSe12BH_05, t_max=100)
TotalYield(NSe12BH_05pr)[101] #1e+12

#FOR LATER PLOTS
NSe12Yield05 <- getYield((NSe12BH_05pr))
NSe12Biomass05 <- getBiomass((NSe12BH_05pr))
NSe12Abundance05 <-getN(NSe12BH_05pr)
NSe12TotalBiomass05 <- TotalBiomass((NSe12BH_05pr))
NSe12TotalAbundance05 <-TotalAbundance(NSe12BH_05pr)
NSe12TotalYield05 <- TotalYield(NSe12BH_05pr)

#projection for a=0.75
a<-0.75
NSe12BH_075<- NSe12KEst
gear_params(NSe12BH_075)$gear <- "balanced"
species_params(NSe12BH_075)$gear <- "balanced"
gear_params(NSe12BH_075)$knife_edge_size <- NSe12BH_075@species_params$w_mat
species_params(NSe12BH_075)$knife_edge_size <- NSe12BH_075@species_params$w_mat
NSe12BH_075 <- setFishing(NSe12BH_075, initial_effort =1.166863e-9) 
NSe12BH_075<- setRateFunction(NSe12BH_075, "FMort", "BalancedHarvest")
NSe12BH_075pr <- project(NSe12BH_075, t_max=100)
TotalYield(NSe12BH_075pr)[101] #1e12

#FOR LATER PLOTS 
NSe12Yield075 <- getYield((NSe12BH_075pr))
NSe12Biomass075 <- getBiomass((NSe12BH_075pr))
NSe12Abundance075 <-getN(NSe12BH_075pr)
NSe12TotalBiomass075 <- TotalBiomass((NSe12BH_075pr))
NSe12TotalAbundance075 <-TotalAbundance(NSe12BH_075pr)
NSe12TotalYield075 <- TotalYield(NSe12BH_075pr)

#for a=1
a=1
NSe12BH_1<- NSe12KEst
gear_params(NSe12BH_1)$gear <- "balanced"
species_params(NSe12BH_1)$gear <- "balanced"
gear_params(NSe12BH_1)$knife_edge_size <- NSe12BH_1@species_params$w_mat
species_params(NSe12BH_1)$knife_edge_size <- NSe12BH_1@species_params$w_mat
NSe12BH_1 <- setFishing(NSe12BH_1, initial_effort =1.368565e-12)
NSe12BH_1 <- setRateFunction(NSe12BH_1, "FMort", "BalancedHarvest")
NSe12BH_1pr <- project(NSe12BH_1, t_max=100)
TotalYield(NSe12BH_1pr)[101] #1e+12 

#FOR LATER PLOTS 
NSe12Yield1 <- getYield((NSe12BH_1pr))
NSe12Biomass1 <- getBiomass((NSe12BH_1pr))
NSe12Abundance1 <- getN(NSe12BH_1pr)
NSe12TotalBiomass1 <- TotalBiomass((NSe12BH_1pr))
NSe12TotalAbundance1 <-TotalAbundance(NSe12BH_1pr)
NSe12TotalYield1 <- TotalYield(NSe12BH_1pr)


#NS yield=1e+12 plots 
matplot(cbind(NSe12TotalYieldKE, NSe12TotalYield0, NSe12TotalYield025, NSe12TotalYield05, NSe12TotalYield075, NSe12TotalYield1),
        log = "xy", type = "l", main = "Total Yield",
        xlab = "Year", ylab = "Yield\ gm^3year-1", col = c(1,7,3,4,5,2), lty=c(4,1,2,2,2,1),lwd = 3, bty = "l",cex.axis=1.2,cex.lab=1, xlim=c(1,30))
legend(x='topright',legend = c('KE', 'a=1','a=0.25', 'a=0.5','a=0.75', 'a=1'), lty = c(4,1,2,2,2,1), col = c(1,7,3,4,5,2), lwd=3, cex=1)

NSe12YieldKEdf <- data.frame(yield100=NSe12YieldKE[101,], yield0=NSe12YieldKE[1,], species=1:12)
NSe12Yield0df <- data.frame(yield100=NSe12Yield0[101,], yield0=NSe12Yield0[1,], species=1:12)
NSe12Yield025df <- data.frame(yield100=NSe12Yield025[101,], yield0=NSe12Yield025[1,], species=1:12)
NSe12Yield05df <- data.frame(yield100=NSe12Yield05[101,], yield0=NSe12Yield05[1,], species=1:12)
NSe12Yield075df <- data.frame(yield100=NSe12Yield075[101,], yield0=NSe12Yield075[1,], species=1:12)
NSe12Yield1df <- data.frame(yield100=NSe12Yield1[101,], yield0=NSe12Yield1[1,], species=1:12)

NSe12YieldKEplot <- ggplot(NSe12YieldKEdf, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = NULL, y = 'Yield/gyear^-1') +scale_y_log10(limits=c(1e+7,3e+12))
#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0,1e+12) 

NSe12Yield0plot<- ggplot(NSe12Yield0df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +scale_y_log10(limits=c(1e+7,3e+12))

#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0,2.1e+12)

NSe12Yield025plot<-ggplot(NSe12Yield025df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = NULL, y = NULL) +scale_y_log10(limits=c(1e+7,3e+12))
  
#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0,2.1e+12)

NSe12Yield05plot<- ggplot(NSe12Yield05df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = 'Species', y = 'Yield/gyear^-1') +scale_y_log10(limits=c(1e+7,3e+12))
  
#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0,2.1e+12)

NSe12Yield075plot<- ggplot(NSe12Yield075df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = 'Species', y = NULL) +scale_y_log10(limits=c(1e+7,3e+12))

#scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) + ylim(0,2.1e+12)

NSe12Yield1plot<- ggplot(NSe12Yield1df, aes(x = factor(species))) +
  geom_point(aes(y = yield0), shape = 21, fill = "white", size = 2) +
  geom_point(aes(y = yield100), shape = 19, size = 2) +
  labs(x = 'Species', y = NULL) +scale_y_log10(limits=c(1e+7,3e+12))

  #scale_x_discrete(breaks = 1:12, labels = paste0(1:12)) +ylim(0,2.1e+12)

NSe12RelativeBiomass0df <- data.frame(RelativeBiomass=NSe12Biomass0[101,]/NSe12BiomassKE[101,], species=1:12)
NSe12RelativeBiomass025df <- data.frame(RelativeBiomass=NSe12Biomass025[101,]/NSe12BiomassKE[101,], species=1:12)
NSe12RelativeBiomass05df <- data.frame(RelativeBiomass=NSe12Biomass05[101,]/NSe12BiomassKE[101,], species=1:12)
NSe12RelativeBiomass075df <- data.frame(RelativeBiomass=NSe12Biomass075[101,]/NSe12BiomassKE[101,], species=1:12)
NSe12RelativeBiomass1df <- data.frame(RelativeBiomass=NSe12Biomass1[101,]/NSe12BiomassKE[101,], species=1:12)

NSe12Biomass0plot <- ggplot(NSe12RelativeBiomass0df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +
  labs(x = NULL, y = 'Relative Biomass') +geom_hline(yintercept = 1, linetype = "dotted", color = "black") +
  ylim(0.5,1.5) + geom_hline(yintercept = (NSe12TotalBiomass0[101]/NSe12TotalBiomassKE[101]), linetype = "dashed", color = "red") 

NSe12Biomass025plot <- ggplot(NSe12RelativeBiomass025df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2)+
  labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+ylim(0.5,1.5) + geom_hline(yintercept = (NSe12TotalBiomass025[101]/NSe12TotalBiomassKE[101]), linetype = "dashed", color = "red") 
NSe12Biomass05plot <- ggplot(NSe12RelativeBiomass05df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+ ylim(0.5,1.5) + geom_hline(yintercept = (NSe12TotalBiomass05[101]/NSe12TotalBiomassKE[101]), linetype = "dashed", color = "red") 
NSe12Biomass075plot <- ggplot(NSe12RelativeBiomass075df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black") + ylim(0.5,1.5) + geom_hline(yintercept = (NSe12TotalBiomass075[101]/NSe12TotalBiomassKE[101]), linetype = "dashed", color = "red") 
NSe12Biomass1plot <- ggplot(NSe12RelativeBiomass1df, aes(x = factor(species))) + geom_point(aes(y = RelativeBiomass), shape = 19, size = 2) +labs(x = NULL, y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+ylim(0.5,1.5) + geom_hline(yintercept = (NSe12TotalBiomass1[101]/NSe12TotalBiomassKE[101]), linetype = "dashed", color = "red") 

NSe12RelativeAbundance0df <- data.frame(RelativeAbundance=NSe12Abundance0[101,]/NSe12AbundanceKE[101,], species=1:12)
NSe12RelativeAbundance025df <- data.frame(RelativeAbundance=NSe12Abundance025[101,]/NSe12AbundanceKE[101,], species=1:12)
NSe12RelativeAbundance05df <- data.frame(RelativeAbundance=NSe12Abundance05[101,]/NSe12AbundanceKE[101,], species=1:12)
NSe12RelativeAbundance075df <- data.frame(RelativeAbundance=NSe12Abundance075[101,]/NSe12AbundanceKE[101,], species=1:12)
NSe12RelativeAbundance1df <- data.frame(RelativeAbundance=NSe12Abundance1[101,]/NSe12AbundanceKE[101,], species=1:12)

NSe12Abundance0plot <- ggplot(NSe12RelativeAbundance0df, aes(x = factor(species)))+ geom_point(aes(y = RelativeAbundance), shape=19, size=2) + labs(x = 'Species', y = 'Relative Abundance')+geom_hline(yintercept = 1, linetype = "dotted", color = "black") +ylim(0.8,1.2) + geom_hline(yintercept = (NSe12TotalAbundance0[101]/NSe12TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
NSe12Abundance025plot <- ggplot(NSe12RelativeAbundance025df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")  +ylim(0.8,1.2) + geom_hline(yintercept = (NSe12TotalAbundance025[101]/NSe12TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
NSe12Abundance05plot <- ggplot(NSe12RelativeAbundance05df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+ylim(0.8,1.2) + geom_hline(yintercept = (NSe12TotalAbundance05[101]/NSe12TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
NSe12Abundance075plot<- ggplot(NSe12RelativeAbundance075df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+ylim(0.8,1.2) +geom_hline(yintercept = (NSe12TotalAbundance075[101]/NSe12TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 
NSe12Abundance1plot<- ggplot(NSe12RelativeAbundance1df, aes(x = factor(species))) + geom_point(aes(y = RelativeAbundance), shape = 19, size = 2) +labs(x = 'Species', y = NULL) +geom_hline(yintercept = 1, linetype = "dotted", color = "black")+ylim(0.8,1.2) + geom_hline(yintercept = (NSe12TotalAbundance1[101]/NSe12TotalAbundanceKE[101]), linetype = "dashed", color = "blue") 

plot_grid(NSe12YieldKEplot, NSe12Yield0plot, NSe12Yield025plot, NSe12Yield05plot, NSe12Yield075plot, NSe12Yield1plot, ncol= 3)
plot_grid(NSe12Biomass0plot, NSe12Biomass025plot, NSe12Biomass05plot, NSe12Biomass075plot, NSe12Biomass1plot, NSe12Abundance0plot, NSe12Abundance025plot, NSe12Abundance05plot, NSe12Abundance075plot, NSe12Abundance1plot, ncol = 3)



