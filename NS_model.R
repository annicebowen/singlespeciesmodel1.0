load(dplyr)

#loading NS model 
NS_species_params

#adding in a values (lit)
NS_species_params$a <- c(0.007, 0.001, 0.009, 0.002, 0.010, 0.006, 0.008, 0.004,
                         0.007, 0.005, 0.005, 0.007)
#adding b values (lit)
NS_species_params$b <- c(3.014, 3.320, 2.941, 3.429, 2.986, 3.080, 3.019, 3.198,
                         3.101, 3.160, 3.173, 3.075)

#adding info on growth curves 
gear_params <- 
  data.frame(species = NS_species_params$species,
             gear = NS_species_params$species,
             sel_func = "sigmoid_length",
             l25 =  c(7.6, 9.8, 8.7, 10.1, 11.5, 19.8, 16.4, 19.8, 11.5,
                      19.1, 13.2, 35.3),
             l50 = c(8.1, 11.8, 12.2, 20.8, 17.0, 29.0, 25.8, 29.0, 17.0,
                     24.3, 22.9, 43.6))
gear_params


#loading fishing hitsory - gives absoulte fishing mortality 
f_location <- system.file("extdata", "NS_f_history.csv", package = "mizer")
f_history <- as(read.csv(f_location, row.names = 1), "matrix")


#start 1967
head(f_history)

#end 2010
tail(f_history)

#set catchabiity to that in year 1990 for reference 
gear_params$catchability <- as.numeric(f_history["1990", ])

#using their paramter estimates 
params <- newMultispeciesParams(NS_species_params, 
                                interaction = inter, 
                                kappa = 9.27e10,
                                gear_params = gear_params)


#making the eforts relative to year 1990
relative_effort <- sweep(f_history, 2, f_history["1990", ], "/")
relative_effort[as.character(1988:1992), ]


#projecting forward at initial fishing effort until steady to get inital ecosystem 
params <- projectToSteady(params, effort = relative_effort["1967", ])

#projecting with relative effort 
sim <- project(params, effort = relative_effort, dt = 0.25, t_save = 1)

#taking a look at biomass 
plotBiomass(sim)

sim0 <- projectToSteady(params, effort = 0, return_sim = TRUE)

plot(sim0)

#setting up BH_pb
BH_pb <- params
gear_params(BH_pb)$gear <- "balanced"
species_params(BH_pb)$gear <- "balanced"
gear_params(BH_pb)$knife_edge_size <- species_params(BH_pb)$w_min

BH_pb <- setRateFunction(BH_pb, "FMort", "productionBH_pb")
BH_pb<- setFishing(BHMinTP02, initial_effort = 8.58e2)
BH_pbpr <- project(BH_pb, t_max = 300)

plotYield(BH_pbpr)
getYield(BH_pbpr)









#setting up BH_p
BH_p <- params
gear_params(BH_p)$gear <- "balanced"
species_params(BH_p)$gear <- "balanced"
gear_params(BH_p)$knife_edge_size <- species_params(BH_p)$w_min
BH_p <- setRateFunction(BH_p, "FMort", "productionBH_p")
BH_p<- setFishing(BH_p, initial_effort = 0.00000001)
BH_ppr <- project(BH_p, t_max = 100)



plotYield(BH_ppr)

TotalYield(BH_ppr)


######################################################################
#Testing efforts for BH_p
efforts <- c(1, 10, 50, 80, 100)

# Create an empty list to store the results
total_yields <- numeric()

# Loop over the initial effort values and run the projection
for (effort in efforts) {
  BH_p <- params
  BH_p@gear_params$knife_edge_size <- 0.001
  BH_p@species_params$knife_edge_size <- 0.001
  gear_params(BH_p)$gear <- "balanced"
  species_params(BH_p)$gear <- "balanced"
  BH_p <- setRateFunction(BH_p, "FMort", "productionBH_p")
  BH_p <- setFishing(BH_p, initial_effort = effort)
  BH_ppr <- project(BH_p, t_max = 100)
  total_yield <- TotalYield(BH_ppr)[101]
  total_yields <- c(total_yields, total_yield) # Append the total yield to the vector
}

# Print the total yield values for each run
print(total_yields)

plot(efforts, total_yields, x_lab ='Inital_effort', ylab='Yield')
##############################################################################
















#Testing efforts for BH_pb
efforts <- c(1, 10, 50, 80, 100)

# Create an empty list to store the results
total_yieldspb <- numeric()

# Loop over the initial effort values and run the projection
for (effort in efforts) {
  BH_pb <- params
  BH_pb@gear_params$knife_edge_size <- 0.001
  BH_pb@species_params$knife_edge_size <- 0.001
  gear_params(BH_pb)$gear <- "balanced"
  species_params(BH_pb)$gear <- "balanced"
  BH_pb <- setRateFunction(BH_pb, "FMort", "productionBH_pb")
  BH_pb <- setFishing(BH_pb, initial_effort = effort)
  BH_pbpr <- project(BH_pb, t_max = 100)
  total_yieldpb <- TotalYield(BH_pbpr)[101]
  total_yieldspb <- c(total_yieldspb, total_yieldpb) # Append the total yield to the vector
}

# Print the total yield values for each run
print(total_yieldspb)
#####################################################################

plot(efforts, total_yieldspb)