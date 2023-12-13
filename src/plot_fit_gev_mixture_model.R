# library(extRemes)

source("./src/calculate_modes.R")
source("./src/calculate_gev_mixture_model_pdf.R")


plot_fit_gev_mixture_model <- function(gev_mixture_model_object,
                                       xlab = "support",
                                       ylab = "density",
                                       main = "density plot",
                                       legend_position = "topright"){
  # gev_mixture_model_object: an object associated with a result of the function "fit_gev_mixture_model()"
  # xlab: label of the x-axis
  # ylab: label of the y-axis
  # main: title of the plot
  # legend_position: position of the legend
  
  x <- gev_mixture_model_object$data
  
  modes_object <- calculate_modes(x = x)
  
  support <- modes_object$density_support
  
  empirical_density <- modes_object$denity_values
  
  parameters <- gev_mixture_model_object$cluster_gev_model_parameters
  
  weights <- gev_mixture_model_object$cluster_weights
  
  nclusters <- gev_mixture_model_object$nclusters
  
  if (nclusters == 1){
    theoretical_densities <- extRemes::devd(x = support, 
                                            loc = parameters["location", 1], 
                                            scale = parameters["scale", 1], 
                                            shape = parameters["shape", 1])
    
    densities <- c(empirical_density, theoretical_densities)
    
    plot(x = support, 
         y = empirical_density, 
         type = "l",
         ylim = range(densities),
         xlab = xlab, 
         ylab = ylab, 
         main = main, 
         lwd = 2)  
    
    lines(support, theoretical_densities, col = 3, lwd = 2)
    abline(h = 0, lty = "dotted")
    
    legend(legend_position, 
           legend = c("empirical", "gev"),
           lty = c(1, 1), col = c(1, 3))
  } else{
    theoretical_densities_1 <- calculate_gev_mixture_model_pdf(x = support,
                                                               locations = parameters["location", ],
                                                               scales =  parameters["scale", ],
                                                               shapes = parameters["shape", ],
                                                               weights = weights,
                                                               kind = c("geometric", "arithmetic")[1])
    
    theoretical_densities_2 <- calculate_gev_mixture_model_pdf(x = support,
                                                               locations = parameters["location", ],
                                                               scales =  parameters["scale", ],
                                                               shapes = parameters["shape", ],
                                                               weights = weights,
                                                               kind = c("geometric", "arithmetic")[2])
    
    densities <- c(empirical_density, theoretical_densities_1, theoretical_densities_2)
    
    plot(x = support, 
         y = empirical_density, 
         type = "l",
         ylim = range(densities),
         xlab = xlab, 
         ylab = ylab, 
         main = main, 
         lwd = 2)  
    
    lines(support, theoretical_densities_1, col = 6, lwd = 2)
    lines(support, theoretical_densities_2, col = 7, lwd = 2)
    abline(h = 0, lty = "dotted")
    
    legend(legend_position, 
           legend = c("empirical", "geometric", "arithmetic"),
           lty = c(1, 1, 1), col = c(1, 6, 7))
  }
  

  
}



# # example 1
# 
# source("./src/fit_gev_mixture_model.R")
# source("./src/fit_unimodal_gev_mixture_model_test.R")
# 
# x <- bmixture::rmixnorm(n = 3000, weight = c(1/3, 1/3, 1/3), mean = c(-5, 0, +5), sd = c(1, 1, 1))
# 
# p <- 3
# 
# gev_mixture_model_object <- fit_gev_mixture_model(x = x,
#                                                   nb_gev_models = p,
#                                                   min_cluster_size = 20,
#                                                   max_iteration = 1,
#                                                   left_cluster_extension_size = 10,
#                                                   right_cluster_extension_size = 100,
#                                                   tolerance = 10^(-3))
# 
# 
# plot_fit_gev_mixture_model(gev_mixture_model_object,
#                            xlab = "support",
#                            ylab = "density",
#                            main = "density plot",
#                            legend_position = "topright")
# 
# # example 2
# 
# source("./src/fit_gev_mixture_model.R")
# source("./src/generate_gev_mixture_model_sample.R")
# 
# weights <- c(1/3, 1/3, 1/3)
# 
# shapes <- c(0.1, 0.1, 0.1)
# scales <- c(1, 1, 1)
# locations <- c(-2, +2, +6)
# 
# n <- 3000
# 
# x <- generate_gev_mixture_model_sample(n = n,
#                                        locations,
#                                        scales,
#                                        shapes,
#                                        weights,
#                                        kind = c("geometric", "arithmetic")[2])
# 
# p <- 3
# 
# gev_mixture_model_object <- fit_gev_mixture_model(x = x,
#                                                   nb_gev_models = p,
#                                                   min_cluster_size = 20,
#                                                   max_iteration = 40,
#                                                   left_cluster_extension_size = 10,
#                                                   right_cluster_extension_size = 200,
#                                                   tolerance = 10^(-3))
# 
# plot_fit_gev_mixture_model(gev_mixture_model_object,
#                            xlab = "support",
#                            ylab = "density",
#                            main = "density plot",
#                            legend_position = "topright")

