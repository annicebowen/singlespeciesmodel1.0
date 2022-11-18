p <- newSingleSpeciesParams()
sp <- species_params(p)
p1 <- newMultispeciesParams(sp, w_pp_cutoff = 1, no_w = 200)
initial_effort(p1) <- 1
ps <- steady(p1)
ps <- setBevertonHolt(ps, reproduction_level = 0.95)

# set senescence mortality
w <- w(ps)
sel <- w > 40

ext_mort <- 0.0001 * ps@w^3

ext_mort(ps)[sel] <- ext_mort[sel]

plot(w , ext_mort(ps), type="l")

pss <- steady(ps)
plotSpectra(pss)

gear_params(pss)
plotYieldVsF(pss, species = 1, F_max = 2)

sim <- project(pss, effort = 1.2)
animateSpectra(sim)
plotBiomass(sim)

# Make predation kernel wider
species_params(pss)$sigma <- 2
pss <- steady(pss)
sim <- project(pss, effort = 1.2)
plotBiomass(sim)

plotYieldVsF(pss, species = 1, F_max = 5)
pss <- setBevertonHolt(pss, reproduction_level = 0.5)
plotYieldVsF(pss, species = 1, F_max = 30)
