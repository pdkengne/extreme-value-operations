# library(extRemes)

source("./src/estimate_gev_model_parameters.R")


estimate_gev_model_quantile <- function(x, 
                                        data, 
                                        alpha = NULL,
                                        do.ci = TRUE,
                                        confidence_level = 0.95,
                                        qcov = NULL, 
                                        threshold = NULL, 
                                        threshold.fun = ~1, 
                                        location.fun = ~1,
                                        scale.fun = ~1, 
                                        shape.fun = ~1, 
                                        use.phi = FALSE,
                                        type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
                                        method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1]){
  # x: vector of observations (assumed to be block maxima)
  # data: dataframe of covariates for linear modeling of the location parameter
  # alpha: order of the quantile to estimate
  # do.ci: boolean which indicates whether to return confidence interval or not
  # confidence_level: the desired confidence level for the estimated quantile
  # qcov: numeric matrix with rows the same length as q and columns equal to the number of parameters.
  #       This gives any covariate values for a nonstationary model
  # threshold.fun, location.fun, scale.fun, shape.fun: formula describing a model for each parameter using columns from data. data
  #                                                    must be supplied if any of these arguments are anything other than ~ 1.
  # use.phi: boolean which indicates whether to use the log of the scale parameter or not
  # type: type of model to use
  # method: estimation mmethod to use
  
  model <- estimate_gev_model_parameters(x = x, 
                                         data = data, 
                                         threshold = threshold, 
                                         threshold.fun = threshold.fun, 
                                         location.fun = location.fun,
                                         scale.fun = scale.fun, 
                                         shape.fun = shape.fun, 
                                         use.phi = use.phi,
                                         type = type,
                                         method = method)
  
  quantile <- return.level(x = model, 
                           return.period = 1/alpha, 
                           alpha = 1 - confidence_level, 
                           method = c("normal"), 
                           do.ci = do.ci,
                           qcov = qcov)
  
  quantile
}




fevd(x, 
     data, 
     threshold = NULL, 
     threshold.fun = ~1, 
     location.fun = ~1,
     scale.fun = ~1, 
     shape.fun = ~1, 
     use.phi = FALSE,
     type = c("GEV", "GP", "PP", "Gumbel", "Exponential")[1],
     method = c("MLE", "GMLE", "Bayesian", "Lmoments")[1])



z <- revd(100, loc=20, scale=0.5, shape=-0.2)

fit <- fevd(z)

names(fit)

fit_summary <- summary(fit, silent = TRUE)

names(fit_summary)

fit_summary$cov.theta


fit_evd <- evd::fgev(x = z)

names(fit_evd)

fit_evd$var.cov


# return level

return.level(x, 
             return.period = c(2, 20, 100), 
             alpha = 0.05, 
             method = c("normal"), 
             do.ci = FALSE, 
             verbose = TRUE,
             qcov = NULL, 
             qcov.base = NULL)

rl <- return.level(fit, return.period = 100, do.ci = TRUE)

rl

fit_evd_rl <- evd::fgev(x = z, prob = 1/1000000000)

#fit_evd_rl

confint(fit_evd_rl)


