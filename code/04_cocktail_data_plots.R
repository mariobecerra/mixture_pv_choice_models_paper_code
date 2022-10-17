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



# max_it_cocktail = 15
# cocktail_pv_d_opt_filename = paste0(designs_folder, "cocktail_pv_d_optimal_", max_it_cocktail, "iter.rds")
# cocktail_pv_i_opt_filename = paste0(designs_folder, "cocktail_pv_i_optimal_", max_it_cocktail, "iter.rds")
# 
# 
# cocktail_pv_d_opt = readRDS(cocktail_pv_d_opt_filename)
# cocktail_pv_i_opt = readRDS(cocktail_pv_i_opt_filename)






cocktail_pv_d_opt_df = mnl_design_array_to_dataframe(
  cocktail_pv_d_opt$X,
  names = c(paste0("x", 1:3), paste0("z", 1), "choice_set")
)


cocktail_pv_i_opt_df = mnl_design_array_to_dataframe(
  cocktail_pv_i_opt$X,
  names = c(paste0("x", 1:3), paste0("z", 1), "choice_set")
)



##### Simplex plots
simplex_width = 11
simplex_height = 8

(cocktail_pv_d_opt_df %>%
    ggtern::ggtern(ggtern::aes(x1, x2, x3, color = z1)) +
    geom_point(size = 1) +
    theme_minimal() + 
    ggtern::theme_showarrows() + 
    ggtern::theme_arrowlarge() +
    ggtern::theme_nomask()
) %>% 
  ggtern::ggsave(
    plot = ., 
    filename = paste0(plots_folder, "simplex_plot_cocktail_pv_D_optimal_", max_it_cocktail, "iter.jpg"),
    width = simplex_width,
    height = simplex_height,
    units = "cm"
  )



(cocktail_pv_i_opt_df %>%
    ggtern::ggtern(ggtern::aes(x1, x2, x3, color = z1)) +
    geom_point(size = 1) +
    theme_minimal() + 
    ggtern::theme_showarrows() + 
    ggtern::theme_arrowlarge() +
    ggtern::theme_nomask()
) %>% 
  ggtern::ggsave(
    plot = .,
    filename = paste0(plots_folder, "simplex_plot_cocktail_pv_I_optimal_", max_it_cocktail, "iter.jpg"),
    width = simplex_width,
    height = simplex_height,
    units = "cm"
  )

























# Settings for plots in PNG files
width_cocktail_simplex = 10
height_cocktail_simplex = 10





plot_both_all = ggtern::grid.arrange(
  cocktail_pv_d_opt_df %>%
    ggtern::ggtern(ggtern::aes(x1, x2, x3, color = z1)) +
    geom_point(size = 1) +
    theme_minimal() + 
    ggtern::theme_showarrows() + 
    ggtern::theme_arrowlarge() +
    ggtern::theme_nomask() +
    theme(legend.position = "none", legend.box = "none")
  ,
  
  cocktail_pv_i_opt_df %>%
    ggtern::ggtern(ggtern::aes(x1, x2, x3, color = z1)) +
    geom_point(size = 1) +
    theme_minimal() + 
    ggtern::theme_showarrows() + 
    ggtern::theme_arrowlarge() +
    ggtern::theme_nomask() +
    theme(legend.position = "none", legend.box = "none")
  ,
  
  ncol = 2
)


ggtern::ggsave(
  filename = paste0(plots_folder, "simplex_plot_cocktail_pv_both_", max_it_cocktail, "iter_all.jpg"),
  plot = plot_both_all,
  width = 2*width_cocktail_simplex,
  height = height_cocktail_simplex,
  units = "cm"
)








##### Dot plots

(cocktail_pv_i_opt_df %>% 
    select(z1) %>% 
    mutate(design = "Bayesian I-optimal") %>% 
    bind_rows(cocktail_pv_d_opt_df %>% 
                select(z1) %>% 
                mutate(design = "Bayesian D-optimal")) %>% 
    ggplot() +
    geom_dotplot(aes(z1, fill = design, color = design), method = 'histodot', binwidth = 0.055) +
    facet_wrap(~design) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    xlab("") +
    ylab("")
    
)  %>% 
  ggsave(
    plot = .,
    filename = paste0(plots_folder, "process_variables_dotplot_", max_it_cocktail, "iter.jpg"),
    width = 12,
    height = 8,
    units = "cm"
  )












(
  get_distances_within_choice_set(cocktail_pv_i_opt$X) %>%
    mutate(Design = "Bayesian\nI-optimal") %>%
    bind_rows(
      get_distances_within_choice_set(cocktail_pv_d_opt$X) %>%
        mutate(Design = "Bayesian\nD-optimal")
    ) %>%
    ggplot() +
    geom_boxplot(aes(x = Design, y = dist)) +
    theme_bw() +
    ylab("Distance between alternatives")
) %>%
  ggplot2::ggsave(
    filename = paste0(plots_folder, "res_cocktail_pv_distances_within_choice_set.png"),
    plot = .,
    width = 8,
    height = 9,
    units = "cm"
  )














# fds_n_points_per_alternative_cocktail_pv = 1000
fds_n_points_per_alternative_cocktail_pv = 5000

pred_vars_cocktail = mnl_get_fds_simulations(
  design_array = cocktail_pv_i_opt$X,
  beta = cocktail_pv_i_opt$beta,
  n_pv = 1,
  order = 2,
  n_points_per_alternative = fds_n_points_per_alternative_cocktail_pv,
  transform_beta = F,
  verbose = 1) %>%
  mutate(Design = "Bayesian I-optimal") %>%
  bind_rows(
    mnl_get_fds_simulations(
      design_array = cocktail_pv_d_opt$X,
      beta = cocktail_pv_d_opt$beta,
      n_pv = 1,
      order = 2,
      n_points_per_alternative = fds_n_points_per_alternative_cocktail_pv,
      transform_beta = F,
      verbose = 1) %>%
      mutate(Design = "Bayesian D-optimal")
  ) 




cocktail_fds_plots = pred_vars_cocktail %>%
  ggplot() +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.2) +
  geom_hline(yintercept = pred_vars_cocktail %>%
               group_by(Design) %>%
               summarize(
                 med = median(pred_var),
                 mean = mean(pred_var)) %>%
               pull(med),
             linetype = "dashed", size = 0.2) +
  geom_line(aes(fraction, pred_var, color = Design), size = 0.8) +
  xlab("Fraction of design space") +
  ylab("Prediction variance") +
  # ggtitle("Cocktail experiment") +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_manual(values = c("red", "blue", "dark green"))



width_cocktail_pv_fds = 20
height_cocktail_pv_fds = 8

ggplot2::ggsave(
  filename = paste0(plots_folder, "res_cocktail_pv_fds_db_vs_ib_plot.png"),
  plot = cocktail_fds_plots,
  width = width_cocktail_pv_fds,
  height = height_cocktail_pv_fds,
  units = "cm"
)
