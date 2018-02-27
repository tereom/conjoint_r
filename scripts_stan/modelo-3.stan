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
  real theta[n_ind, n_marca];
  real beta[n_precio];
  real mu_theta[n_marca];
  # real mu_beta;
  real<lower=0> tau_theta[n_marca];
  # real<lower=0> tau_beta;
}

transformed parameters {
    real util[N];
    for(n in 1:N){
        util[n] = theta[id[n],marca[n]] + beta[precio[n]];
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
            target += u_tarea[seleccion_tarea] - log(sum(exp(u_tarea)));
        }
    }
    mu_theta ~ normal(0, 2);
    tau_theta ~ cauchy(0, 2);
    for(i in 1:n_ind){
        for(j in 1:n_marca){
            theta[i,j] ~ normal(mu_theta[n_marca], tau_theta[n_marca]);
        }
    }
    // mu_beta ~ normal(0, 5);
    // tau_beta ~ cauchy(0, 5);
    beta ~ normal(0,2);
}

