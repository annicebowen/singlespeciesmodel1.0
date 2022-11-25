remotes::install_github("sizespectrum/mizerExperimental")
library(mizerExperimental)
library(tidyverse)

#size spectrum community 
mp <- newTraitParams() |> steady()
plotSpectra(mpsteady, power = 2, total = TRUE)

#get some info on community : decide want to investigate spp 10
# w_mat= 1258.925412, w_inf = 5011.87234
select(species_params(mp), w_mat, w_mat25, w_inf, m)

species_params(mp)

#want to impose knife edge fishing >500
mp_500 <- mp
gear_params(mp_500)$sel_func <- "knife_edge"
gear_params(mp_500)$knife_edge_size <- 500
initial_effort(mp_500) <- 1
#doesnt coverege why?
mp_500s <- projectToSteady(mp_500)
#Simulation run did not converge after 91.5 years.


mp_1000<- mp
gear_params(mp_1000)$sel_func <- "knife_edge"
gear_params(mp_1000)$knife_edge_size <- 1000
initial_effort(mp_1000) <- 1

mp_1000s <-projectToSteady(mp_1000)
#Convergence was achieved in 3 years.
sim_mp1000s <- project(mp_1000s, t_max = 3)
plotYieldGear(sim_mp2000s)


mp_1500<- mp
gear_params(mp_1500)$sel_func <- "knife_edge"
gear_params(mp_1500)$knife_edge_size <- 1500
initial_effort(mp_1500) <- 1

mp_1500s <-projectToSteady(mp_1500)
#Simulation run did not converge after 99 years
#sim_mp1000s <- project(mp_1500s, t_max = 3)
#plotYieldGear(sim_mp1500s)

mp_2000<- mp
gear_params(mp_2000)$sel_func <- "knife_edge"
gear_params(mp_2000)$knife_edge_size <- 2000
initial_effort(mp_2000) <- 10
#Simulation run did not converge after 73.5 years.
#mp_2000s <- projectToSteady(mp_2000)
#sim_mp2000s <- project(mp_2000s, t_max = 73.5)
#plotYieldGear(sim_mp2000s)


mp_2500<- mp
gear_params(mp_2500)$sel_func <- "knife_edge"
gear_params(mp_2500)$knife_edge_size <- 2500
initial_effort(mp_2500) <- 1

#Simulation run did not converge after 99 years.
#mp_2500s <- projectToSteady(mp_2500)
#sim_mp2000s <- project(mp_2000s, t_max = 73.5)
#plotYieldGear(sim_mp2000s)

mp_3000<- mp
gear_params(mp_3000)$sel_func <- "knife_edge"
gear_params(mp_3000)$knife_edge_size <- 3000
initial_effort(mp_3000) <- 1

#Simulation run did not converge after 99 years.
#mp_3000s <- projectToSteady(mp_3000)
#sim_mp2000s <- project(mp_2000s, t_max = 73.5)
#plotYieldGear(sim_mp2000s)


mp_3500<- mp
gear_params(mp_3500)$sel_func <- "knife_edge"
gear_params(mp_3500)$knife_edge_size <- 3500
initial_effort(mp_3500) <- 1
#Simulation run did not converge after 99 years.
#mp_3500s <- projectToSteady(mp_3500)
#sim_mp2000s <- project(mp_2000s, t_max = 73.5)
#plotYieldGear(sim_mp2000s)

mp_4000<- mp
gear_params(mp_4000)$sel_func <- "knife_edge"
gear_params(mp_4000)$knife_edge_size <- 4000
initial_effort(mp_4000) <- 1
#Simulation run did not converge after 99 years.
#mp_4000s <- projectToSteady(mp_4000)
#sim_mp2000s <- project(mp_2000s, t_max = 73.5)
#plotYieldGear(sim_mp2000s)



mp_4500<- mp
gear_params(mp_4500)$sel_func <- "knife_edge"
gear_params(mp_4500)$knife_edge_size <- 4500
initial_effort(mp_4500) <- 1
#convergence reached in 63 years 
mp_4500s <- projectToSteady(mp_4500)
sim_mp4500s <- project(mp_4500s, t_max = 63)
plotYieldGear(sim_mp4500s)


mp_5000<- mp
gear_params(mp_5000)$sel_func <- "knife_edge"
gear_params(mp_5000)$knife_edge_size <- 5000
initial_effort(mp_5000) <- 1

#CONVERGENCE IN 63 YEARS
mp_5000s <- projectToSteady(mp_5000)
sim_mp5000s <- project(mp_5000s, t_max = 63)
#fails idk why? no params?
#animateSpectra(mp_5000s, total = TRUE, power = 2, 
               #ylim = c(1e-8, NA), wlim = c(1e-3, NA))
#difference between getyieldgear 7 getyield params?
#overall yield?
getYieldGear(sim_mp5000s)
#individual yield 
getYield(sim_mp5000s)


mp_6000<- mp
gear_params(mp_6000)$sel_func <- "knife_edge"
gear_params(mp_6000)$knife_edge_size <- 6000
initial_effort(mp_6000) <- 1

#convergence reached after 10.5
mp_6000s <- projectToSteady(mp_6000)
sim_mp6000s <- project(mp_6000s, t_max = 10.5)
plotYieldGear(sim_mp6000s)



getYield(mp_5000s)
#interesting as the numebr of spp 11 decreases # of species 10 increases 
plotYieldGear(sim_mp5000s)






plotYieldVsSpecies(mp_500steady)
plotYieldVsSpecies(mp_200steady)
mp_500steady <-projectToSteady(mp_)

sim_mp500 <- project(mp_500steady, t_max = 90)

getYield(mp_500steady)
plotYieldVsSpecies(mp_500steady)


animateSpectra(sim_mp500, total = TRUE, power = 2, 
               ylim = c(1e-8, NA), wlim = c(1e-3, NA))

project
plotSpectra(mp_500, power =2, total =TRUE)
plotSpectra(mp, power=2, total=TRUE)

plotYieldVsF(mp_500, species = 10, F_max = 5)
plotYieldVsF(mp_200, species = 10, F_max =5)
