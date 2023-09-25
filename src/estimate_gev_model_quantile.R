library(extRemes)




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
             verbose = FALSE,
             qcov = NULL, 
             qcov.base = NULL)

rl <- return.level(fit, return.period = 1000000000, do.ci = TRUE)

rl

fit_evd_rl <- evd::fgev(x = z, prob = 1/1000000000)

#fit_evd_rl

confint(fit_evd_rl)


