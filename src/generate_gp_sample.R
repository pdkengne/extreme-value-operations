generate_gp_sample <- function(n = 1, scale = 1, shape = 0, threshold = 0){
  # n: vnumber of observations to generate
  # scale, shape, threshold: scale, shape and threshold parameters of the considered gp distribution
  
  gp_sample <- extRemes::revd(n, scale, shape, threshold, type = "GP")
  
  gp_sample
}


# example 1

result <- generate_gp_sample(n = 10, scale = 0.5, shape = 0.8, threshold = 0)

result


# example 2

result <- generate_gp_sample(n = 10, scale = 0.5, shape = 0.8, threshold = 1)

result