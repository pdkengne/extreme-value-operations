library(stringr)
library(extRemes)

data(PORTw)

x <- PORTw$TMX1

fit <- extRemes::fevd(x, PORTw, location.fun=~ MTMAX + AOindex, scale.fun=~ AOindex + STDMIN, use.phi = FALSE, units="deg C")
fit
z <- extRemes::trans(fit)
fevd(z)

names(fit)

fit$const.loc
fit$const.scale
fit$const.shape

fit_model <- fit$par.models
fit_model
names(fit_model)
fit_model$term.names

fit$par.models$term.names

cov_loc <- fit_model$term.names$location
cov_loc

cov_scale <- fit_model$term.names$scale
cov_scale

cov_shape <- fit_model$term.names$shape
cov_shape

model_covariates_names <- unique(c(cov_loc, cov_scale, cov_shape))
model_covariates_names

model_covariates_input <- as.list(model_covariates_names)
names(model_covariates_input) <- model_covariates_names

model_covariates_input

model_covariates_input["MTMAX"]


model_covariates_input <- data.frame()
names(model_covariates_input) <- model_covariates_names
model_covariates_input

model_covariates_input <- sapply(model_covariates_names, function(var) model_covariates_input[, var] = 1)


fit$results$par

par_list <- as.list(fit$results$par)

par_list

data.frame(par_list)

names(fit$results$par)

library(stringr)

?str_detect()

noms <- names(fit$results$par)

noms


pos <- stringr::str_detect(string = noms, 
                           pattern  = "mu", 
                           negate = FALSE)


cov_1 <- noms[pos]
cov_1
par_list_cov_1 <- par_list[pos]
par_list_cov_1


cov_loc


model_covariates_input[cov_loc[2]] <- par_list_cov_1[[2]]
model_covariates_input[cov_loc[3]] <- par_list_cov_1[[3]]

model_covariates_input

val <- fit_model$term.names$location


val

str_replace(string = noms[pos], 
            pattern = "mu", 
            replacement = c("constant", val))

