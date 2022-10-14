library(opdesmixr)
library(tidyverse)
library(mlogit)
library(here)


designs_folder = here("out/cornell_fishpatty_designs/")
dir.create(designs_folder, showWarnings = F)

n_cores = parallel::detectCores()



beta_prime = c(
  0.862499999975883, -0.927500000024117, 
  -0.974204545931931, -0.834204545594259, 0.35579545440574, 
  0.376119047645238, 0.106119047645238, 0.20561904757681, 
  0.642190476213381, 0.201190476213381, 0.402690476150095, 
  -0.0776904761965239, -0.0866904761965239, -0.00919047619923768, 
  0.027142857142857, 0.0010714285714284, -0.00785714285714291, 
  0, 0, 0)


names(beta_prime) = c(
  "x1","x2",
  "x1:x2","x1:x3","x2:x3",
  "x1:z1","x2:z1","x3:z1",
  "x1:z2","x2:z2","x3:z2",
  "x1:z3","x2:z3","x3:z3",
  "z1:z2","z1:z3","z2:z3",
  "I(z1^2)","I(z2^2)","I(z3^2)"
  )












q = 3
n_pv = 3
J = 2
S = 112



kappas = c(0.5, 5, 10, 30)
n_draws = 128
# n_random_initial_starts = 80
n_random_initial_starts = 4
max_it_cornell = 4


cornell_fishpatty_designs_basefilename = paste0(designs_folder, "cornell_fishpatty_experiment_maxit", max_it_cornell)


## Creating designs if they don't already exist
(start_time = Sys.time())

for(k in kappas){
  
  cat("kappa =", k, "\n")
  
  
  Sigma_prime = transform_varcov_matrix(k*diag(mnl_get_number_of_parameters(3, 3, 2)), q = 3)
  
  beta_prior_draws = get_correlated_halton_draws(beta_prime, Sigma_prime, n_draws)
  
  # Between 4000 and 4500 seconds for 4 random starts in 4 cores and maximum 4 iterations for each design
  
  
  d_opt_filename = paste0(cornell_fishpatty_designs_basefilename, "_kappa", k, "_Dopt.rds")

  if(file.exists(d_opt_filename)){
    cat("\tD optimal file exists.\n")
  }else{
    cat("\tD optimal file does not exist. Creating design.\n")
    cornell_fishpatty_beta_pseudo_bayesian_d_opt = mnl_mixture_coord_exch(
      q = q,
      J = J,
      S = S,
      n_pv = n_pv,
      verbose = 1,
      order = 2,
      n_random_starts = n_random_initial_starts,
      beta = beta_prior_draws,
      transform_beta = F,
      max_it = max_it_cornell,
      n_cores = n_cores,
      opt_crit = "D",
      plot_designs = F,
      save_all_designs = F)
    
    saveRDS(cornell_fishpatty_beta_pseudo_bayesian_d_opt, d_opt_filename)
  }
  
  
  
  i_opt_filename = paste0(cornell_fishpatty_designs_basefilename, "_kappa", k, "_Iopt.rds")
  
  if(file.exists(i_opt_filename)){
    cat("\tI optimal file exists.\n")
  }else{
    cat("\tI optimal file does not exist. Creating design.\n")
    cornell_fishpatty_beta_pseudo_bayesian_i_opt = mnl_mixture_coord_exch(
      q = q,
      J = J,
      S = S,
      n_pv = n_pv,
      verbose = 1,
      order = 2,
      n_random_starts = n_random_initial_starts,
      beta = beta_prior_draws,
      transform_beta = F,
      max_it = max_it_cornell,
      n_cores = n_cores,
      opt_crit = "I",
      plot_designs = F,
      save_all_designs = F)
    
    saveRDS(cornell_fishpatty_beta_pseudo_bayesian_i_opt, i_opt_filename)
  }
  
}





create_pv_grid = function(n_pv, n_out = 1000){
  
  # n_out is approximate
  
  df_grid = lapply(1:n_pv, function(i){
    out_tibble = tibble(z = seq(-1, 1, length.out = ceiling(n_out^(1/n_pv))))
    names(out_tibble) = paste0("z", i)
    return(out_tibble)
  }) %>% 
    bind_cols() %>% 
    expand_(paste0("z", 1:n_pv))
  
  return(df_grid)
}



scheffe_order2_q3_pv_utilities = function(beta, n_pv = 3, n_points = 500){
  
  out = opdesmixr:::create_2d_simplex_grid(n_points) %>%
    expand_grid(create_pv_grid(3, n_points)) %>% 
    mutate(utility = 
             beta[1]*x1 +
             beta[2]*x2 +
             beta[3]*x1*x2 +
             beta[4]*x1*x3 +
             beta[5]*x2*x3 +
             beta[6]*x1*z1 +
             beta[7]*x2*z1 +
             beta[8]*x3*z1 +
             beta[9]*x1*z2 +
             beta[10]*x2*z2 +
             beta[11]*x3*z2 +
             beta[12]*x1*z3 +
             beta[13]*x2*z3 +
             beta[14]*x3*z3 +
             beta[15]*z1*z2 +
             beta[16]*z1*z3 +
             beta[17]*z2*z3 +
             beta[18]*z1^2 +
             beta[19]*z2^2 +
             beta[20]*z3^2)
  
  return(out)
}


utilities = scheffe_order2_q3_pv_utilities(beta_prime, n_pv = 3, n_points = 500)

utilities %>% filter(utility == max(utility))
utilities %>% filter(utility == min(utility))







