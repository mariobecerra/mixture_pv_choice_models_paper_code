library(opdesmixr)
library(tidyverse)
library(here)


designs_folder = here("out/cocktail_pv_designs/")
dir.create(designs_folder, showWarnings = F)

n_cores = parallel::detectCores()




q = 3
J = 2
S = 140
n_pv = 1

beta_vec = c(7.562, 0.907, 5.109, 14.573, 17.1806, 19.2705, 19.2705, 19.2705, 0)
names(beta_vec) = c(
  "x1","x2",
  "x1:x2","x1:x3","x2:x3",
  "x1:z1","x2:z1","x3:z1",
  "I(z1^2)"
)

SDs = c(2, 3, 7, 6, 7, 30, 30, 30, 30)
names(SDs) = names(beta_vec)
var_cov_mat = diag(SDs^2)


n_draws = 128
beta_correlated_draws_cocktail = get_correlated_halton_draws(beta_vec, var_cov_mat, n_draws)




# # 40 seconds in 4 cores
# design_array_i_opt = opdesmixr::mnl_mixture_coord_exch(
#   q = q, J = J, S = S, n_pv = 1,
#   beta = beta_vec, seed = 2021, order = 2, opt_crit = "I",
#   transform_beta = F,
#   n_random_starts = 4, n_cores = 4, max_it = 4)





# n_random_initial_starts = 80
n_random_initial_starts = 48
max_it_cocktail = 15
# max_it_cocktail = 3
seed = 2112


cocktail_pv_d_opt_filename = paste0(designs_folder, "cocktail_pv_d_optimal_", max_it_cocktail, "iter.rds")
cocktail_pv_i_opt_filename = paste0(designs_folder, "cocktail_pv_i_optimal_", max_it_cocktail, "iter.rds")


if(file.exists(cocktail_pv_d_opt_filename)){
  cat("D_B optimal design already exists.\n")
} else{
  # 40 seconds in 4 cores with 4 random starts and 4 iterations and point estimate
  # 4 minutes in 4 cores with 4 random starts and 3 iterations and 12 draws
  # 42 minutes in 4 cores with 4 random starts and 3 iterations and 128 draws
  # 8643.5 seconds (2.5 hours) in 4 cores with 4 random starts and 10 iterations and 128 draws
  # 10174 seconds in 4 cores with 4 random starts and 15 iterations and 128 draws
  # 45,000 seconds in 12 cores with 48 random starts and 15 iterations and 128 draws
  cat("Doing D_B optimal design for cocktail experiment.\n")
  (t1D = Sys.time())
  cocktail_pv_D_opt = mnl_mixture_coord_exch(
    n_random_starts = n_random_initial_starts,
    q = q,
    J = J,
    S = S,
    n_pv = n_pv,
    order = 2,
    beta = beta_correlated_draws_cocktail,
    transform_beta = F,
    opt_method = "B",
    opt_crit = "D",
    max_it = max_it_cocktail,
    verbose = 1,
    plot_designs = F,
    seed = seed,
    n_cores = n_cores,
    save_all_designs = T
  )
  
  (t2D = Sys.time())
  t2D - t1D
  
  saveRDS(cocktail_pv_D_opt, cocktail_pv_d_opt_filename)
}



if(file.exists(cocktail_pv_i_opt_filename)){
  cat("I_B optimal design already exists.\n")
} else{
  # 9804.4 seconds in 4 cores with 4 random starts and 15 iterations and 128 draws
  # 45,000 seconds in 12 cores with 48 random starts and 15 iterations and 128 draws
  cat("Doing I_B optimal design for cocktail experiment.\n")
  (t1I = Sys.time())
  cocktail_pv_I_opt =  mnl_mixture_coord_exch(
    n_random_starts = n_random_initial_starts,
    q = q,
    J = J,
    S = S,
    n_pv = n_pv,
    order = 2,
    beta = beta_correlated_draws_cocktail,
    transform_beta = F,
    opt_method = "B",
    opt_crit = "I",
    max_it = max_it_cocktail,
    verbose = 1,
    plot_designs = F,
    seed = seed,
    n_cores = n_cores,
    save_all_designs = T
  )
  (t2I = Sys.time())
  t2I - t1I
  
  saveRDS(cocktail_pv_I_opt, cocktail_pv_i_opt_filename)
}






