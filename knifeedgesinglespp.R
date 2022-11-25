params <- newSingleSpeciesParams(lambda = 2.05)

params_fishing20 <- params

getParams(params_fishing20)

gear_params(params_fishing20)$sel_func <- "knife_edge"
gear_params(params_fishing20)$knife_edge_size <- 20
initial_effort(params_fishing20) <- 1
#plotFMort(params_fishing10)
#doesnt converge
ps20 <- projectToSteady(params_fishing20)
animateSpectra(ps20, t=20)

params_fishing40 <- params
gear_params(params_fishing40)$sel_func <- "knife_edge"
gear_params(params_fishing40)$knife_edge_size <- 40
initial_effort(params_fishing40) <- 1
#plotFMort(params_fishing10)
#doesnt converge
ps40 <- projectToSteady(params_fishing40)

params_fishing60 <- params
gear_params(params_fishing60)$sel_func <- "knife_edge"
gear_params(params_fishing60)$knife_edge_size <- 60
initial_effort(params_fishing60) <- 1
#plotFMort(params_fishing10)
#converges after 1.5 year
ps60 <- projectToSteady(params_fishing60)

params_fishing80 <- params
gear_params(params_fishing80)$sel_func <- "knife_edge"
gear_params(params_fishing80)$knife_edge_size <- 80
initial_effort(params_fishing80) <- 1
#plotFMort(params_fishing10)
#converges after 1.5 years 
ps80 <- projectToSteady(params_fishing80)

params_fishing20 <-singleSpeciesSteady(params_fishing20)
params_fishing40 <-singleSpeciesSteady(params_fishing40)
params_fishing60 <-singleSpeciesSteady(params_fishing60)
params_fishing80 <-singleSpeciesSteady(params_fishing80)

x <- c(20, 40,60,80)
y <- c(0.5, 1, 1.5, 2)
z <- cbind(c(0.000757859,0.001283268,0.001666831,0.001957775), c(0.000254075,0.000433374,0.000566636,0.000669553), c(7.02E-05,0.000169537,0.000156783,0.000185342), c(6.76E-06,1.63E-05,1.51E-05,1.79E-05))

library(plotly)
fig <- plot_ly(
  type = 'surface',
  contours = list(
    x = list(show = TRUE, start = 1.5, end = 2, size = 0.04, color = 'white'),
    z = list(show = TRUE, start = 0.5, end = 0.8, size = 0.05)),
  x = ~x,
  y = ~y,
  z = ~z)
fig <- fig %>% layout(
  scene = list(
    xaxis = list(nticks = 4),
    zaxis = list(nticks = 4),
    camera = list(eye = list(x = 0, y = -1, z = 0.5)),
    aspectratio = list(x = .9, y = .8, z = 0.2)))

fig
