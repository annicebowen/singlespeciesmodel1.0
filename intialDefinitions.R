library(mizer)

#define function used to calculate BH_p 
productionBH_p <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth * n, 2, params@w, "/")
  production_density * usual_f_mort
}

#define function used to calculate BH_pb 
productionBH_pb <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth, 2, params@w, "/")
  production_density * usual_f_mort
}

#define function used to calculate BH_pba
productionBH_pa <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth * n^0.5, 2, params@w, "/")
  production_density * usual_f_mort
}


#define total yield
TotalYield <- function(sim) {
  y <- getYield(sim)
  y_total <- rowSums(y)
  return(y_total)
}

#########################################################################
TB <- newTraitParams(no_sp = 12, gear_names = "None", knife_edge_size = NA, min_w_inf = 30, max_w_inf = 40000)
#projecting to steady: steady=12 years 
TBst <- projectToSteady(TB) 
plotSpectra((TBst))
# Trait-based unfished model
TBPr <- project(TBst, t_max = 100, effort = 0)

#########################################################################
#investigating F=BH_pb
BH_pb <- TBst
BH_pb@gear_params$knife_edge_size <- 0.001
BH_pb@species_params$knife_edge_size <- 0.001
gear_params(BH_pb)$gear <- "balanced"
species_params(BH_pb)$gear <- "balanced"
BH_pb <- setRateFunction(BH_pb, "FMort", "productionBH_pb")

#Testing efforts for BH_pb
efforts <- c(0.01, 0.015, 0.1, 0.11, 0.2, 0.3, 0.5, 1) #above 0.12 dont coverge<99

# Create an empty list to store the results
total_yieldspb <- numeric()

# Loop over the initial effort values and run the projection
for (effort in efforts) {
  BH_pb <- setFishing(BH_pb, initial_effort = effort)
  BH_pbpr <- project(BH_pb, t_max = 100)
  total_yieldpb <- TotalYield(BH_pbpr)[101]
  total_yieldspb <- c(total_yieldspb, total_yieldpb) # Append the total yield to the vector
}

# Print the total yield values for each run
print(total_yieldspb)
plot(efforts, total_yieldspb, ylab='BH_pb', xlab='c')
#####################################################################
#######################################################################
#investigating F=BH_p
BH_p <- TBst
BH_p@gear_params$knife_edge_size <- 0.001
BH_p@species_params$knife_edge_size <- 0.001
gear_params(BH_p)$gear <- "balanced"
species_params(BH_p)$gear <- "balanced"
BH_p <- setRateFunction(BH_p, "FMort", "productionBH_p")

#Testing efforts for BH_pb
efforts <- c(1,4,8,9,10, 20, 50, 100) #c=8 best

# Create an empty list to store the results
total_yieldsp <- numeric()

# Loop over the initial effort values and run the projection
for (effort in efforts) {
  BH_p <- setFishing(BH_p, initial_effort = effort)
  BH_ppr <- project(BH_p, t_max = 100)
  total_yieldp <- TotalYield(BH_ppr)[101]
  total_yieldsp <- c(total_yieldsp, total_yieldp) # Append the total yield to the vector
}

# Print the total yield values for each run
print(total_yieldsp)
plot(efforts, total_yieldsp, xlab='c', ylab='F_BHp')

####################################################################
#investigating F=BH_pa
BH_pa <- TBst
BH_pa@gear_params$knife_edge_size <- 0.001
BH_pa@species_params$knife_edge_size <- 0.001
gear_params(BH_pa)$gear <- "balanced"
species_params(BH_pa)$gear <- "balanced"
BH_pa <- setRateFunction(BH_pa, "FMort", "productionBH_pa")

#Testing efforts for BH_pb
efforts <- c(1,10 ,100)

# Create an empty list to store the results
total_yieldspa <- numeric()

# Loop over the initial effort values and run the projection
for (effort in efforts) {
  BH_pa <- setFishing(BH_pa, initial_effort = effort)
  BH_papr <- project(BH_pa, t_max = 100)
  total_yieldpa <- TotalYield(BH_papr)[101]
  total_yieldspa <- c(total_yieldspa, total_yieldpa) # Append the total yield to the vector
}

# Print the total yield values for each run
print(total_yieldspa)
plot(efforts, total_yieldspa)


#####################################################################
#eventually will plot all curves but due to Fmort error did not finish 
yield <- data.frame(efforts, total_yieldspa, total_yieldsp)
colnames(yield) <- c('c', 'F_BHp', 'F_BHpa')
ggplot()+ geom_point(data=yield, aes(x=c, y=F_BHp), colour='red') + geom_point(data=yield, aes(x=c, y=F_BHpa))
#####################################################################

#investigtaing steady state F_mort -doesnt look right 
BH_pb <- TBst
BH_pb@gear_params$knife_edge_size <- 0.001
BH_pb@species_params$knife_edge_size <- 0.001
gear_params(BH_pb)$gear <- "balanced"
species_params(BH_pb)$gear <- "balanced"
BH_pb <- setRateFunction(BH_pb, "FMort", "productionBH_pb")



#investigating steadiness of policies 
BH_pb <- setFishing(BH_pb, initial_effort = 0.11) #max initial effort = 0.11
a<- projectToSteady(BH_pb) # converges in 42
a <- project(BH_pb, t_max=100)

plotFMort(BH_pb)
plotSpectra(a)
################################################################################
BH_p <- TBst
BH_p@gear_params$knife_edge_size <- 0.001
BH_p@species_params$knife_edge_size <- 0.001
gear_params(BH_p)$gear <- "balanced"
species_params(BH_p)$gear <- "balanced"
BH_p <- setRateFunction(BH_p, "FMort", "productionBH_p")

#investigating steadiness of policie
BH_p <- setFishing(BH_p, initial_effort = 8) #max initial effort = 0.11
b<- projectToSteady(BH_p) # converges in 30
b <- project(BH_p, t_max=100)
plotFMort(BH_p)
plotSpectra(b)

#############################################################################
BH_pa <- TBst
BH_pa@gear_params$knife_edge_size <- 0.001
BH_pa@species_params$knife_edge_size <- 0.001
gear_params(BH_p)$gear <- "balanced"
species_params(BH_p)$gear <- "balanced"
BH_pa <- setRateFunction(BH_p, "FMort", "productionBH_pa")

#investigating steadiness of policy
BH_pa <- setFishing(BH_pa, initial_effort = 0.01) #max initial effort = 0.11
c<- projectToSteady(BH_pa) # converges in 30
c <- project(BH_pa, t_max=100)
plotFMort(BH_pa)
plotSpectra(c)
##############################################################################
