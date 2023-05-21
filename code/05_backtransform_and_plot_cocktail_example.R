library(opdesmixr)
library(tidyverse)
library(here)

source(here("code/utils.R"))

designs_folder = here("out/cocktail_pv_designs/")
dir.create(designs_folder, showWarnings = F)


plots_folder = paste0(designs_folder, "plots/")
dir.create(plots_folder)



max_it_cocktail = 20
cocktail_pv_d_opt_filename = paste0(designs_folder, "cocktail_pv_d_optimal_", max_it_cocktail, "iter.rds")
cocktail_pv_i_opt_filename = paste0(designs_folder, "cocktail_pv_i_optimal_", max_it_cocktail, "iter.rds")


cocktail_pv_d_opt_all = readRDS(cocktail_pv_d_opt_filename)
cocktail_pv_i_opt_all = readRDS(cocktail_pv_i_opt_filename)


d_opt_ix = which.min(sapply(seq_along(cocktail_pv_d_opt_all), function(i) cocktail_pv_d_opt_all[[i]]$opt_crit_value))
i_opt_ix = which.min(sapply(seq_along(cocktail_pv_i_opt_all), function(i) cocktail_pv_i_opt_all[[i]]$opt_crit_value))


cocktail_pv_d_opt = cocktail_pv_d_opt_all[[d_opt_ix]]
cocktail_pv_i_opt = cocktail_pv_i_opt_all[[i_opt_ix]]



# Transforms from pseudocomponent x to original component a.
# x is a scalar or a vector, l is a scalar denoting the lower bound.
# sum_l is a scalar denoting the sum of the lower bounds.
transform_from_pseudocomp_to_comp = function(x, l, sum_l){
  a = l + (1 - sum_l)*x
  return(a)
}


transform_tibble_from_pseudocomp_to_comp = function(design_tibble, lower_bounds, var_indices){
  # Returns a tibble with p + q columns and n rows with the transformation from pseudo-components to the original components in the mixture. Here, q is the number of ingredietns proportions, p is the number of original columns in design_tibble, and n is the number of rows in design_tibble.
  sum_lower_bounds = sum(lower_bounds)
  q = length(lower_bounds)
  
  if(q != length(var_indices)) stop("Incompatible sizes of lower_bounds and var_indices")
  
  transformed_df = as_tibble(matrix(rep(NA_real_, nrow(design_tibble)*q), ncol = q)) %>%
    set_names(paste0(names(design_tibble)[var_indices], "_original"))
  
  for(i in seq_along(var_indices)){
    transformed_df[[i]] = transform_from_pseudocomp_to_comp(design_tibble[[var_indices[i]]], lower_bounds[i], sum_lower_bounds)
  }
  
  return(bind_cols(design_tibble, transformed_df))
}


lower_bounds_cocktail = c(0.3, 0.1, 0.15)


cocktail_pv_d_opt_df = mnl_design_array_to_dataframe(
  cocktail_pv_d_opt$X,
  names = c(paste0("x", 1:3), paste0("z", 1), "choice_set")
)


cocktail_pv_i_opt_df = mnl_design_array_to_dataframe(
  cocktail_pv_i_opt$X,
  names = c(paste0("x", 1:3), paste0("z", 1), "choice_set")
)

cocktail_pv_d_opt_df_backtransformed = transform_tibble_from_pseudocomp_to_comp(select(cocktail_pv_d_opt_df, x1:x3), lower_bounds_cocktail, 1:3) %>% mutate(z1 = cocktail_pv_d_opt_df$z1)


cocktail_pv_d_opt_df %>%
  ggtern::ggtern(ggtern::aes(x1, x2, x3, color = z1)) +
  geom_point(size = 1) +
  theme_minimal() + 
  ggtern::theme_showarrows() + 
  ggtern::theme_arrowlarge() +
  ggtern::theme_nomask()



cocktail_pv_d_opt_df_backtransformed %>%
  rename(`mango juice` = x1_original, `lemon juice` = x2_original, `blackcurrant syrup` = x3_original) %>% 
  ggtern::ggtern(ggtern::aes(`mango juice`, `lemon juice`, `blackcurrant syrup`, color = z1)) +
  geom_point(size = 1) +
  # theme_minimal() + 
  ggtern::theme_showarrows() + 
  ggtern::theme_arrowlarge() +
  ggtern::theme_nomask()




cocktail_pv_i_opt_df_backtransformed = transform_tibble_from_pseudocomp_to_comp(select(cocktail_pv_i_opt_df, x1:x3), lower_bounds_cocktail, 1:3) %>% mutate(z1 = cocktail_pv_i_opt_df$z1)


cocktail_pv_i_opt_df %>%
  ggtern::ggtern(ggtern::aes(x1, x2, x3, color = z1)) +
  geom_point(size = 1) +
  theme_minimal() + 
  ggtern::theme_showarrows() + 
  ggtern::theme_arrowlarge() +
  ggtern::theme_nomask()



cocktail_pv_i_opt_df_backtransformed %>%
  rename(`mango juice` = x1_original, `lemon juice` = x2_original, `blackcurrant syrup` = x3_original) %>% 
  ggtern::ggtern(ggtern::aes(`mango juice`, `lemon juice`, `blackcurrant syrup`, color = z1)) +
  geom_point(size = 1) +
  # theme_minimal() + 
  ggtern::theme_showarrows() + 
  ggtern::theme_arrowlarge() +
  ggtern::theme_nomask()
