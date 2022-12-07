# Returns an almost ready latex code for the matrix resulting from multiplying the model expansion vector of a special-cubic Scheffé model, i.e. f(a_{js}) f^T(a_{js}). It's the matrix that is then integrated to obtain the moments matrix.

library(here)

q = 3
n_pv = 1
order = 2

mnl_get_number_of_parameters = function(q, n_pv, order){
  
  if(order == 1){
    m = q
  } else{
    if(order == 2){
      m = q + (q-1)*q/2 + q*n_pv + n_pv*(n_pv-1)/2 + n_pv
    } else{
      if(order == 3){
        m = (q^3+ 5*q)/6 # = q + q*(q-1)/2 + q*(q-1)*(q-2)/6
      }  else{
        stop("Wrong order")
      }
    }
  }
  
  return(m)
}

m = mnl_get_number_of_parameters(q = q, order = order, n_pv = n_pv)



f_matrix = matrix(rep(0L, (m-1)*(q + n_pv)), ncol = q + n_pv)

counter = 0
# Fill indicators of first part of the model expansion
for(i in 1:(q-1)){
  counter = counter + 1
  f_matrix[counter, i] = 1
}

# Fill indicators of second part of the model expansion
if(order >= 2){
  for(i in 1:(q-1)){
    for(j in (i+1):q){
      counter = counter + 1
      f_matrix[counter, i] = 1
      f_matrix[counter, j] = 1
    }
  }
}


# Fill indicators of fourth part of the model expansion when n_pv > 0
for(i in 1:n_pv){
  for(k in 1:q){
    counter = counter + 1
    f_matrix[counter, q + i] = 1
    f_matrix[counter, k] = 1
  }
}

if(n_pv > 1){
  for(i in 1:(n_pv-1)){
    for(j in (i+1):n_pv){
      counter = counter + 1
      f_matrix[counter, q + i] = 1
      f_matrix[counter, q + j] = 1
    }
  }
}


for(i in 1:n_pv){
  counter = counter + 1
  f_matrix[counter, q + i] = 2
}








exponents_matrix = matrix(0L, nrow = nrow(f_matrix), ncol = ncol(f_matrix))

terms = c("x_1", "x_2", "x_3", "z")


# Two versions: One with all the exponents (including 0s and 1s), other without the 1s and 0s.
f_times_f = matrix("", ncol = m-1, nrow = m-1)
f_times_f_2 = matrix("", ncol = m-1, nrow = m-1)
for(k in 1:nrow(f_matrix)){
  for(i in 1:nrow(f_matrix)){
    exponents_matrix[i,] = f_matrix[k, ] + f_matrix[i, ]
  }
  
  
  for(i in 1:nrow(f_matrix)){
    string = ""
    string2 = ""
    for(j in 1:ncol(f_matrix)){
      if(exponents_matrix[i, j] > 1){
        string2 = paste0(string2, " ", terms[j], "^", exponents_matrix[i, j])
      }
      if(exponents_matrix[i, j] == 1){
        string2 = paste0(string2, " ", terms[j])
      }
      string = paste0(string, " ", terms[j], "^", exponents_matrix[i, j])
    }
    f_times_f[i, k] = string
    f_times_f_2[i, k] = string2
  }
}

f_times_f
f_times_f_2



write.table(
  as.data.frame(f_times_f), 
  here("out/f_times_f_01.txt"), 
  row.names = F, col.names = F, sep = " & ", quote = F)


# This is the one I use for the paper:
write.table(
  as.data.frame(f_times_f_2), 
  here("out/f_times_f_02.txt"), 
  row.names = F, col.names = F, sep = " & ", quote = F)




