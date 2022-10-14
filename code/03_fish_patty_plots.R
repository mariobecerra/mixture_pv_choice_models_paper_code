library(tidyverse)
library(opdesmixr)
library(stringi)
library(rgl)
library(here)

source(here("code/utils.R"))

designs_folder = here("out/cornell_fishpatty_designs/")

plots_folder = paste0(designs_folder, "plots/")
dir.create(plots_folder)

simplex_plots_folder = paste0(plots_folder, "simplex_plots/")
dir.create(simplex_plots_folder)

other_plots_folder = paste0(plots_folder, "other_plots/")
dir.create(other_plots_folder)

cube_plots_folder = paste0(plots_folder, "cube_plots/")
dir.create(cube_plots_folder)

cube_plots_png_folder = paste0(cube_plots_folder, "pngs/")
dir.create(cube_plots_png_folder)





kappas = c(0.5, 5, 10, 30)
n_random_initial_starts = 4
max_it_cornell = 10



cornell_fishpatty_designs_basefilename = paste0("cornell_fishpatty_experiment_maxit", max_it_cornell)



## Read designs with analytic_transformed betas and save them in a list
cornell_fishpatty_designs = lapply(kappas, function(k){
  
  cat("kappa =", k, "\n")
  
  D_opt_filename = paste0(designs_folder, cornell_fishpatty_designs_basefilename, "_kappa", k, "_Dopt.rds")
  I_opt_filename = paste0(designs_folder, cornell_fishpatty_designs_basefilename, "_kappa", k, "_Iopt.rds")
  
  
  
  if(file.exists(D_opt_filename)){
    cat("\tD optimal file exists. Loading.\n")
    cornell_D_opt = readRDS(D_opt_filename)
  }else{
    stop("\tD optimal file does not exist.\n")
    
  }
  
  if(file.exists(I_opt_filename)){
    cat("\tI optimal file exists. Loading.\n")
    cornell_I_opt = readRDS(I_opt_filename)
  }else{
    stop("\tI optimal file does not exist.\n")
  }
  
  
  return(list(
    D_opt = cornell_D_opt,
    I_opt = cornell_I_opt,
    kappa = k
  ))
  
})











##### Simplex plots

simplex_width = 8
simplex_height = 8

for(i in seq_along(cornell_fishpatty_designs)){
  
  kappa_i = cornell_fishpatty_designs[[i]]$kappa
  
  cornell_D_opt_i_df = mnl_design_array_to_dataframe(
    cornell_fishpatty_designs[[i]]$D_opt$X,
    names = c(paste0("x", 1:3), paste0("z", 1:3), "choice_set")
  )
  
  
  cornell_I_opt_i_df = mnl_design_array_to_dataframe(
    cornell_fishpatty_designs[[i]]$I_opt$X,
    names = c(paste0("x", 1:3), paste0("z", 1:3), "choice_set")
  )
  
  
  (cornell_D_opt_i_df %>%
      select(1:3) %>% 
      ggtern::ggtern(ggtern::aes(x1, x2, x3)) +
      geom_point(color = "blue", size = 1.5) +
      theme_minimal() +
      ggtern::theme_nomask()
  ) %>% 
    ggtern::ggsave(
      plot = .,
      filename = paste0(simplex_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_i, "_Dopt.jpg"),
      width = simplex_width,
      height = simplex_height,
      units = "cm"
    )
  
  
  
  (cornell_I_opt_i_df %>%
      select(1:3) %>% 
      ggtern::ggtern(ggtern::aes(x1, x2, x3)) +
      geom_point(color = "blue", size = 1.5) +
      theme_minimal() +
      ggtern::theme_nomask()
  ) %>% 
    ggtern::ggsave(
      plot = .,
      filename = paste0(simplex_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_i, "_Iopt.jpg"),
      width = simplex_width,
      height = simplex_height,
      units = "cm"
    )
  
}











###############################
## Distance plots
###############################

cornell_fishpatty_distances_within_choice_set = lapply(seq_along(cornell_fishpatty_designs), function(i){
  
  kappa_i = cornell_fishpatty_designs[[i]]$kappa
  
  get_distances_within_choice_set(cornell_fishpatty_designs[[i]]$I_opt$X) %>%
    mutate(Design = "Bayesian\nI-optimal") %>%
    bind_rows(
      get_distances_within_choice_set(cornell_fishpatty_designs[[i]]$D_opt$X) %>%
        mutate(Design = "Bayesian\nD-optimal")
    )  %>%
    mutate(kappa = kappa_i)
  
  
}) %>%
  bind_rows()


cornell_fishpatty_distances_within_choice_set_plot = cornell_fishpatty_distances_within_choice_set %>%
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_boxplot(aes(x = Design, y = dist)) +
  theme_bw() +
  ylab("Distance between alternatives") +
  facet_wrap(~kappa2, ncol = 4, labeller = label_parsed)


ggplot2::ggsave(
  filename = paste0(other_plots_folder, cornell_fishpatty_designs_basefilename, "_distances_within_choice_set.png"),
  plot = cornell_fishpatty_distances_within_choice_set_plot,
  width = 21,
  height = 6,
  units = "cm"
)




cornell_fishpatty_distances_within_choice_set_plot_nokappa30 = cornell_fishpatty_distances_within_choice_set %>%
  filter(kappa != 30) %>% 
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_boxplot(aes(x = Design, y = dist)) +
  theme_bw() +
  ylab("Distance between alternatives") +
  facet_wrap(~kappa2, ncol = 4, labeller = label_parsed)


ggplot2::ggsave(
  filename = paste0(other_plots_folder, cornell_fishpatty_designs_basefilename, "_distances_within_choice_set_nokappa30.png"),
  plot = cornell_fishpatty_distances_within_choice_set_plot_nokappa30,
  width = 21,
  height = 6,
  units = "cm"
)



  
  




##### FDS plots

beta_prime = c(
  0.862499999975883, -0.927500000024117, 
  -0.974204545931931, -0.834204545594259, 0.35579545440574, 
  0.376119047645238, 0.106119047645238, 0.20561904757681, 
  0.642190476213381, 0.201190476213381, 0.402690476150095, 
  -0.0776904761965239, -0.0866904761965239, -0.00919047619923768, 
  0.027142857142857, 0.0010714285714284, -0.00785714285714291, 
  0, 0, 0)


fds_n_points_per_alternative_cornell = 1000


pred_vars_fishpatty_cornell = lapply(
  seq_along(cornell_fishpatty_designs), function(i){
    
    kappa_i = cornell_fishpatty_designs[[i]]$kappa
    cat("\n\n\nkappa:", kappa_i, format(Sys.time(),'(%H:%M:%S)'), "\n")
    
    # beta_prior_draws_cornell = get_halton_draws(beta_prime, sd = sqrt(kappa_i), ndraws = 128)
    # Sigma_prime = transform_varcov_matrix(kappa_i*diag(7), 3)
    Sigma_prime = transform_varcov_matrix(kappa_i*diag(mnl_get_number_of_parameters(3, 3, 2)), q = 3)
    beta_prior_draws_cornell = get_correlated_halton_draws(beta = beta_prime, sigma = Sigma_prime, n_draws = 128)
    
    pred_vars_fishpatty_cornell_i = mnl_get_fds_simulations(
      design_array = cornell_fishpatty_designs[[i]]$I_opt$X,
      beta = beta_prior_draws_cornell,
      order = 2,
      n_pv = 3,
      n_points_per_alternative = fds_n_points_per_alternative_cornell,
      transform_beta = F,
      verbose = 1) %>%
      mutate(Design = "Bayesian I-optimal") %>%
      bind_rows(
        mnl_get_fds_simulations(
          design_array = cornell_fishpatty_designs[[i]]$D_opt$X,
          beta = beta_prior_draws_cornell,
          order = 2,
          n_pv = 3,
          n_points_per_alternative = fds_n_points_per_alternative_cornell,
          transform_beta = F,
          verbose = 1) %>%
          mutate(Design = "Bayesian D-optimal")
      )
    
    return(pred_vars_fishpatty_cornell_i %>%
             mutate(kappa = kappa_i))
    
  }) %>%
  bind_rows()



cornell_fds_plots = pred_vars_fishpatty_cornell %>%
  left_join(
    pred_vars_fishpatty_cornell %>%
      group_by(Design, kappa) %>%
      summarize(
        med = median(pred_var),
        mean = mean(pred_var))
  ) %>%
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.2) +
  geom_hline(aes(yintercept = med), linetype = "dashed", size = 0.2) +
  geom_line(aes(fraction, pred_var, color = Design), size = 0.8) +
  xlab("Fraction of design space") +
  ylab("Prediction variance") +
  # ggtitle("Cornell's experiment") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~kappa2, ncol = 2, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("red", "blue"))



width_cornell_fds = 20
height_cornell_fds = 12

ggplot2::ggsave(
  filename = paste0(other_plots_folder, cornell_fishpatty_designs_basefilename, "_FDS_plot.png"),
  plot = cornell_fds_plots,
  width = width_cornell_fds,
  height = height_cornell_fds,
  units = "cm"
)






cornell_fds_plots_nokappa30 = pred_vars_fishpatty_cornell %>%
  left_join(
    pred_vars_fishpatty_cornell %>%
      group_by(Design, kappa) %>%
      summarize(
        med = median(pred_var),
        mean = mean(pred_var))
  ) %>%
  filter(kappa != 30) %>% 
  mutate(kappa2 = paste0("kappa == ", kappa)) %>%
  mutate(kappa2 = fct_reorder(kappa2, kappa)) %>%
  ggplot() +
  geom_vline(xintercept = 0.5, linetype = "dashed", size = 0.2) +
  geom_hline(aes(yintercept = med), linetype = "dashed", size = 0.2) +
  geom_line(aes(fraction, pred_var, color = Design), size = 0.8) +
  xlab("Fraction of design space") +
  ylab("Prediction variance") +
  # ggtitle("Cornell's experiment") +
  theme_bw() +
  theme(legend.position = "bottom") +
  facet_wrap(~kappa2, ncol = 3, scales = "free_y", labeller = label_parsed) +
  scale_color_manual(values = c("red", "blue"))




ggplot2::ggsave(
  filename = paste0(other_plots_folder, cornell_fishpatty_designs_basefilename, "_FDS_nokappa30_plot.png"),
  plot = cornell_fds_plots_nokappa30,
  width = 22,
  height = 8,
  units = "cm"
)






















##### Cube plots
for(j in seq_along(cornell_fishpatty_designs)){
  
  kappa_j = cornell_fishpatty_designs[[j]]$kappa
  
  png_filename_D_opt = paste0(cube_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt_cube_static.png")
  png_filename_I_opt = paste0(cube_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Iopt_cube_static.png")
  
  
  if(file.exists(png_filename_D_opt)){
    cat("File", paste0(cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt.gif"), "already exists. Skipping.\n")
  } else{
    
    cornell_D_opt_j_df = mnl_design_array_to_dataframe(
      cornell_fishpatty_designs[[j]]$D_opt$X,
      names = c(paste0("x", 1:3), paste0("z", 1:3), "choice_set")
    )
    
    
    plot3d( 
      x = cornell_D_opt_j_df$z1, y = cornell_D_opt_j_df$z2, z = cornell_D_opt_j_df$z3,
      col = "blue",
      type = 's', 
      radius = .04,
      xlab="z1", ylab="z2", zlab="z3")
    
    rgl.snapshot(png_filename_D_opt, fmt = 'png')

    
  }
  
  
  
  if(file.exists(png_filename_I_opt)){
    cat("File", paste0(cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Iopt.gif"), "already exists. Skipping.\n")
  } else{
    
    cornell_I_opt_j_df = mnl_design_array_to_dataframe(
      cornell_fishpatty_designs[[j]]$I_opt$X,
      names = c(paste0("x", 1:3), paste0("z", 1:3), "choice_set")
    )
    
    
    plot3d( 
      x = cornell_I_opt_j_df$z1, y = cornell_I_opt_j_df$z2, z = cornell_I_opt_j_df$z3,
      col = "blue",
      type = 's', 
      radius = .04,
      xlab="z1", ylab="z2", zlab="z3")
    
    rgl.snapshot(png_filename_I_opt, fmt = 'png')
    
  }
  
}

















##### Cube plots (dynamic)
for(j in seq_along(cornell_fishpatty_designs)){
  
  kappa_j = cornell_fishpatty_designs[[j]]$kappa
  
  gif_filename_D_opt = paste0(cube_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt.gif")
  gif_filename_I_opt = paste0(cube_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Iopt.gif")
  
  
  if(file.exists(gif_filename_D_opt)){
    cat("File", paste0(cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt.gif"), "already exists. Skipping.\n")
  } else{
    
    cornell_D_opt_j_df = mnl_design_array_to_dataframe(
      cornell_fishpatty_designs[[j]]$D_opt$X,
      names = c(paste0("x", 1:3), paste0("z", 1:3), "choice_set")
    )
    
    
    plot3d( 
      x = cornell_D_opt_j_df$z1, y = cornell_D_opt_j_df$z2, z = cornell_D_opt_j_df$z3,
      col = "blue",
      type = 's', 
      radius = .04,
      xlab="z1", ylab="z2", zlab="z3")
    
    movie3d(
      movie = paste0(cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt"),
      spin3d(axis = c(0, 0, 1), rpm = 4), 
      duration = 10,
      dir = cube_plots_png_folder,
      type = "gif", 
      clean = F,
      convert = F
    )
    
    
    filenames = paste0(
      cube_plots_png_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt", 
      stri_pad(str = 0:100, pad = "0", width = 3), ".png")
    m <- magick::image_read(filenames[1])
    for(i in 2:length(filenames)) 
      m <- c(m, magick::image_read(filenames[i]))
    for(i in 2:length(filenames)) 
      m <- c(m, magick::image_read(filenames[length(filenames) - i + 2]))
    m <- magick::image_animate(m, fps = 10, loop = 0, dispose = "previous")
    magick::image_write(m, paste0(cube_plots_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Dopt.gif"))
    
    
  }
  
  
  
  if(file.exists(gif_filename_I_opt)){
    cat("File", paste0(cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Iopt.gif"), "already exists. Skipping.\n")
  } else{
    
    cornell_I_opt_j_df = mnl_design_array_to_dataframe(
      cornell_fishpatty_designs[[j]]$I_opt$X,
      names = c(paste0("x", 1:3), paste0("z", 1:3), "choice_set")
    )
    
    
    plot3d( 
      x = cornell_I_opt_j_df$z1, y = cornell_I_opt_j_df$z2, z = cornell_I_opt_j_df$z3,
      col = "blue",
      type = 's', 
      radius = .04,
      xlab="z1", ylab="z2", zlab="z3")
    
    movie3d(
      movie = paste0(cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Iopt"),
      spin3d(axis = c(0, 0, 1), rpm = 4), 
      duration = 10,
      dir = cube_plots_png_folder,
      type = "gif", 
      clean = F,
      convert = F
    )
    
    filenames = paste0(
      cube_plots_png_folder, cornell_fishpatty_designs_basefilename, "_kappa", kappa_j, "_Iopt", 
      stri_pad(str = 0:100, pad = "0", width = 3), ".png")
    m <- magick::image_read(filenames[1])
    for(i in 2:length(filenames)) 
      m <- c(m, magick::image_read(filenames[i]))
    for(i in 2:length(filenames)) 
      m <- c(m, magick::image_read(filenames[length(filenames) - i + 2]))
    m <- magick::image_animate(m, fps = 10, loop = 0, dispose = "previous")
    magick::image_write(m, gif_filename_I_opt)
    
  }
  
}




