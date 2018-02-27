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
  real mu_theta[n_marca];
  real mu_beta[n_precio];
  real<lower=0> sigma_theta[n_marca];
  real<lower=0> sigma_beta[n_marca];
  real v_theta[n_ind, n_marca];
  real v_beta[n_ind, n_precio];
}

transformed parameters {
    real util[N];
    real theta[n_ind, n_marca];
    real beta[n_ind, n_precio];
    for(i in 1:n_ind){
        for(j in 1:n_marca){
            theta[i,j] = v_theta[i, j] * sigma_theta[j] + mu_theta[j];
        }
        for(j in 1:n_precio){
            beta[i,j] = v_beta[i, j] * sigma_beta[j] + mu_beta[j];
        }
    }
    for(n in 1:N){
        util[n] = theta[id[n],marca[n]] + beta[id[n],precio[n]];
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
            if(seleccion_tarea != 6){
                target += u_tarea[seleccion_tarea] - log_sum_exp(u_tarea);    
            } else {
                target += -log_sum_exp(u_tarea);
            }
        }
    }
    mu_theta ~ normal(0, 1);
    sigma_theta ~ normal(0, 2);
    mu_beta ~ normal(0, 1);
    sigma_beta ~ normal(0, 2);
    mu_theta ~ normal(0,1);
    mu_beta ~ normal(0,1);
    for(i in 1:n_ind){
        for(j in 1:n_marca){
            v_theta[i,j] ~ normal(0, 1);
        }
        for(j in 1:n_precio){
            v_beta[i,j] ~ normal(0, 1);
        }
    }
}

