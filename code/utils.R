
get_distances_within_choice_set = function(design_array){
  # Computes the Euclidean distances between the two alternatives within a choice set for a design.
  
  dim_array = dim(design_array)
  J = dim_array[2]
  if(J != 2) stop("This function only works for designs with two alternatives within each choice set.")
  
  S = dim_array[3]
  distances = tibble(dist = rep(NA_real_, S))
  
  for(s in 1:S){
    square_diff = (design_array[,1,s] - design_array[,2,s])^2
    distances[s,] = sqrt(sum(square_diff))
  }
  
  distances %>%
    mutate(choice_set = 1:nrow(.)) %>%
    return()
  
}

