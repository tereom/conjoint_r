data {
  int N;
  int n_ind;
  int n_tarea;
  int n_opc;
  int n_marca;
  int n_precio;
  int id[N];
  int marca[N];
  int precio[N];
  int concepto[N];
  int tarea[N];
  int seleccion[N];
  int indice[n_ind, n_tarea, n_opc];
}

transformed data {
  
}

parameters {
  real theta[n_marca];
  real beta[n_precio];
}

transformed parameters {
    real util[N];
    for(n in 1:N){
        util[n] = theta[marca[n]] + beta[precio[n]];
    }
}

model {
    for(i in 1:n_ind) {
        for(j in 1:n_tarea){
            real u_tarea[n_opc];
            int seleccion_tarea;
            seleccion_tarea = seleccion[indice[i,j,1]];
            for(k in 1:n_opc){
                u_tarea[k] = util[indice[i,j,k]];
            }
            target += u_tarea[seleccion_tarea] - log_sum_exp(u_tarea);
        }
    }
    theta ~ normal(0,2);
    theta[n_marca] ~ normal(0,0.01);
    beta ~ normal(0,2);
    beta[n_precio] ~ normal(0,0.01);
}

generated quantities {
  real theta_c[n_marca];
  real beta_c[n_precio];
  real mean_theta;
  real mean_beta;
  mean_theta = mean(theta);
  mean_beta = mean(beta);
  for(t in 1:n_marca){
      theta_c[t] = theta[t] - mean_theta;
  }
  for(b in 1:n_precio){
      beta_c[b] = beta[b] - mean_beta;
  }
}
