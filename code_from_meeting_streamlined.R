library(mizerExperimental)
p <- newSingleSpeciesParams()
# We take the species parameters from this model and build a multi-species
# model with a single species, which is different from the previous single-
# species model because it involves canibalism.
sp <- species_params(p)
p1 <- newMultispeciesParams(sp, w_pp_cutoff = 1, no_w = 200)
initial_effort(p1) <- 1
p1 <- steady(p1)
plotSpectra(p1)

# As there is no predation mortality on the largest individuals, we need to
# introduce some senescence mortality
w <- w(p1) # A vector of sizes
w_sen <- 50 # size at which to start senescence
# mortality increasing as w^3 for all w > 80g
ext_mort(p1)[w > w_sen] <-  0.1 * (w / w_sen)[w > w_sen]^5
plot(w , ext_mort(p1), type = "l")

p1 <- steady(p1)
plotSpectra(p1)

plotYieldVsF(p1, species = 1, F_max = 20)
