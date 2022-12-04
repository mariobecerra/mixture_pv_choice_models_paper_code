library(opdesmixr)
library(tidyverse)
library(mlogit)
library(here)

# detach("package:opdesmixr", unload=TRUE)






# Read fish patty data from CSV from Peter's paper.
# The data on the paper is wrong, 
# the response of the sixth observation should be 1.16 instead of 1.61.
# In this CSV it has already been corrected.
data_fishp_orig = read_csv(here("data/fish_patty_experiment.csv"))

# # Data also available in mixexp package:
# data("fishp", package = "mixexp")
# head(fishp)

to_pv_binary = function(x){
  temp = as.integer(as.factor(x))
  out = ifelse(temp == 2, 1, -1)
  return(out)
}


data_2 = data_fishp_orig %>% 
  # set_names(c("pv_1", "pv_2", "pv_3", "comp_1", "comp_2", "comp_3", "y")) %>% 
  mutate(
    z1 = to_pv_binary(z1),
    z2 = to_pv_binary(z2),
    z3 = to_pv_binary(z3)
  )





cornell_formula_01 = y ~ -1 +
  x1 + x2 + x3 +
  x1:x2 + x1:x3 + x2:x3 +
  x1:z1 + x2:z1 + x3:z1 +
  x1:z2 + x2:z2 + x3:z2 +
  x1:z3 + x2:z3 + x3:z3 +
  z1:z2 + z1:z3 + z2:z3 +
  I(z1^2) + I(z2^2) + I(z3^2)




mod_01 = lm(
  cornell_formula_01,
  data = data_2)

summary(mod_01)






# Same as before but no quadratic effects on PVs
cornell_formula_02 = y ~ -1 +
  x1 + x2 + x3 +
  x1:x2 + x1:x3 + x2:x3 +
  x1:z1 + x2:z1 + x3:z1 +
  x1:z2 + x2:z2 + x3:z2 +
  x1:z3 + x2:z3 + x3:z3 +
  z1:z2 + z1:z3 + z2:z3




mod_02 = lm(
  cornell_formula_02,
  data = data_2)

summary(mod_02)



sum((predict(mod_01, data = data_2) - data_2$y)^2)
sum((predict(mod_02, data = data_2) - data_2$y)^2)



theta_vec = as.numeric(mod_02$coefficients)

theta_vec_prime = c(theta_vec[1] - theta_vec[3], 
                     theta_vec[2] - theta_vec[3],
                     theta_vec[4:length(theta_vec)]
)


print(theta_vec)
print(theta_vec_prime)


