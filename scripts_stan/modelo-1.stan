data {
  int N;
  int n_ind;
  int n_tarea;
  int n_opc;
  int n_marca;
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
  real beta;
}

transformed parameters {
  real util[N];
  for(n in 1:N){
    util[n] = theta[marca[n]] + beta*log(precio[n]);
  }
}

model{
  for(i in 1:n_ind){
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
  
  theta ~ normal(0,2);
  beta ~ normal(0,2);
}
