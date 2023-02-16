#define function used to calculate BH_p 
productionRICHARD_p <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth * n, 2, params@w, "*")
  production_density * usual_f_mort
}

productionRICHARD_pb <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- e_growth
  production_density * usual_f_mort
}

productionRICHARD_p0.5 <- function(params, n, effort, e_growth, ...) {
  usual_f_mort <- colSums(mizerFMortGear(params, effort))
  production_density <- sweep(e_growth * n^0.5, 2, params@w, "*")
  production_density * usual_f_mort
}

#######################################################################
TB <- newTraitParams(no_sp = 12, gear_names = "None", knife_edge_size = NA, min_w_inf = 30, max_w_inf = 40000)
#projecting to steady: steady=12 years 
TBst <- projectToSteady(TB) 
# Trait-based unfished model
TBPr <- project(TBst, t_max = 100, effort = 0)
plot(TBst)

###############################################################################
BH_Rp<- TBst
BH_Rp@gear_params$knife_edge_size <- 0.001
BH_Rp@species_params$knife_edge_size <- 0.001
gear_params(BH_Rp)$gear <- "balanced"
species_params(BH_Rp)$gear <- "balanced"
BH_Rp <- setRateFunction(BH_Rp, "FMort", "productionRICHARD_p")
BH_Rp <- setFishing(BH_Rp, initial_effort = 0.11) #max initial effort = 0.11
plotFMort(BH_Rp)


####################################
BH_Rpb<- TBst
BH_Rpb@gear_params$knife_edge_size <- 0.001
BH_Rpb@species_params$knife_edge_size <- 0.001
gear_params(BH_Rpb)$gear <- "balanced"
species_params(BH_Rpb)$gear <- "balanced"
BH_Rpb <- setRateFunction(BH_Rpb, "FMort", "productionRICHARD_pb")
BH_Rpb <- setFishing(BH_Rpb, initial_effort = 0.11) 
BH_Rpb <- project(BH_Rpb, t_max=100)
plot(BH_Rpb)
TotalYield(BH_Rpb)
plotFMort(BH_Rpb)


