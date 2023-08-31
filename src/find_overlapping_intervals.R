#--------------------------------------------------------------------------------------------------------
# function to identify the highest subset of intervals which overlaps
#--------------------------------------------------------------------------------------------------------    


library(future.apply)


find_overlapping_intervals<- function(table = cbind(rep(0, 100), rep(1, 100)) ){
  
  doBootstrapTest<- function(trial){

    isOverlappingIntervals(table = table, intesection_point = trial)
    
  }
  
  trials<- seq(from = min(table[, 1]), to = max(table[, 2]), length.out = 200)
  
  plan(multisession)
 
  overlaps <- future_sapply(trials, FUN = doBootstrapTest)
  
  plan(sequential)
  

  if (!is.null(dim(overlaps))){
    
    proportions<- apply(overlaps, 2 , mean, na.rm = TRUE)
    position<- which.max(proportions)[1]
    overlap_intervals_positions<- overlaps[, position]
    
  } else{
    
    overlap_intervals_positions<- overlaps
    
  }
  
  return(overlap_intervals_positions)
  
}



# example 1

result<- find_overlapping_intervals(table = cbind(rep(0, 1000), rep(1, 1000)))

result


# example 2

result<- find_overlapping_intervals(table = rbind(c(0, 1), c(0.5,2),c(1.5, 2),c(-1, -0.5), c(-0.75,1)))

result




