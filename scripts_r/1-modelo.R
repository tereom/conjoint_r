library(rstan)
library(tidyverse)
library(shinystan)

# base de datos procesada, en forma larga
load(file = "datos/cafe.Rdata")
cafe

datos_stan <- function(datos, var_precio = "precio"){
    # calcular el número de individuos, tareas y opciones por tarea
    # si se puede seleccionar "ninguno" hay una opción adicional
    # precio: columna de precio a usar, varía si es una columna numérica o categórica
    precio_quo <- enquo(var_precio)
    n_ind <- n_distinct(datos$id)
    n_tarea <- n_distinct(datos$tarea) 
    n_opc <- n_distinct(datos$concepto)
    N <- nrow(datos)
    # numero de marcas
    n_marca <- n_distinct(datos$marca) 
    n_precio <- n_distinct(pull(datos, !!precio_quo)) 
    
    # indice de datos largos
    indice <- array(0, dim = c(n_ind, n_tarea, n_opc))
    
    r <- 1
    for(i in 1:n_ind){
        for(j in 1:n_tarea){
            for(k in 1:n_opc){
                indice[i, j, k] <- r
                r <- r + 1
            }
        }
    }
    
    list(N = N, n_ind = n_ind, n_tarea = n_tarea, n_opc = n_opc, 
         n_marca = n_marca, n_precio = n_precio, 
         id = as.numeric(as.factor(datos$id)), marca = datos$marca, 
         precio = pull(datos, !!precio_quo), concepto = datos$concepto, 
         tarea = datos$tarea, seleccion = datos$seleccion, indice = indice)
}

# en el CBC existe selección "ninguna"
cafe %>% filter(seleccion == 6)
# 1. filtramos todas aquellas personas que eligieron ninguna al menos una vez
ids_ninguna <- unique(cafe$id[cafe$seleccion == 6])
cafe_1 <- filter(cafe, !(id %in% ids_ninguna))
cafe_1

datos_cafe_1 <- datos_stan(cafe_1)

# correr stan
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

set.seed(1021)

# precio variable continua, modelo no jerárquico
modelo_cafe <- stan_model("analisis_r/modelo-1.stan")
fit_prueba <- sampling(modelo_cafe, data = datos_cafe_1, iter = 100)

fit_1 <- stan(file = "analisis_r/modelo-1.stan", 
  data = datos_cafe_1,
  iter = 1000, chains = 3, cores = 3, thin = 5, 
  verbose = FALSE, seed = 457)

# save(fit_1, file = "datos_procesados/fit_precio_cont_no_jer.RData")
load(file = "datos_procesados/fit_precio_cont_no_jer.RData")
print(fit_1, digits = 1)
fit_1_summary <- summary(fit_1)$summary
launch_shinystan(fit_1)

util_1_summary <- fit_1_summary %>% 
    as.data.frame() %>% 
    mutate(parametro = rownames(fit_1_summary)) %>% 
    filter(str_detect(parametro, "util"))


#### precio categórico, modelo no jerárquico
datos_cafe_2 <- datos_stan(cafe_1, var_precio = "precio_int")
set.seed(1021)

fit_2 <- stan(file = "analisis_r/modelo-2.stan", data = datos_cafe_2, 
    iter = 5000, chains = 3, cores = 3, thin = 5, verbose = FALSE, seed = 457)

save(fit_2, file = "datos_procesados/fit_precio_cat_no_jer.RData")
print(fit_2, digits = 1)

load("datos_procesados/fit_precio_cat_no_jer.RData")

#### precio categórico, jerárquico indep
# no modelamos correlación en dist beta, theta

fit_3 <- stan(file = "analisis_r/modelo-3.stan", data = datos_cafe_2, 
    iter = 5000, chains = 3, cores = 3, thin = 5, verbose = FALSE, seed = 457)

save(fit_3, file = "datos_procesados/fit_precio_cat_jer_indep.RData")
print(fit_3, digits = 1)

launch_shinystan(fit_3)

# en el CBC existe selección "ninguna"
#### precio categórico, modelo no jerárquico

cafe_ninguno <- cafe %>% 
    select(id, version, seleccion, tarea) %>%
    distinct() %>% 
    mutate(
        concepto = as.integer(max(cafe$concepto) + 1), 
        marca = as.integer(max(cafe$marca) + 1),
        precio_int = max(cafe$precio_int) + 1
    )
cafe_aux <- select(cafe, id, version, seleccion, tarea, concepto, marca, precio_int)
cafe_ninguno_1 <- bind_rows(cafe_ninguno, cafe_aux)

datos_cafe_ninguno <- datos_stan(cafe_ninguno_1, var_precio = "precio_int")

# fijar utilidades de ninguno en cero y quitar última opción en parámetro de betas y thetas
fit_4 <- stan(file = "analisis_r/modelo-2.stan", data = datos_cafe_ninguno, 
    iter = 500, chains = 3, cores = 3, verbose = FALSE, seed = 457)
save(fit_4, file = "datos_procesados/fit_precio_cat_ninguno_indep.RData")

launch_shinystan(fit_4)

load("datos_procesados/fit_precio_cat_ninguno_indep.RData")
summary(fit_4)$summary


cafe %>% group_by(id, tarea, seleccion) %>% distinct() %>% group_by(id) %>% summarise(n_ninguno = sum(seleccion == 6) / n())%>%
    ggplot(aes(x = n_ninguno)) + geom_histogram()

# modelo jerárquico en precio y marca

fit_5 <- stan(file = "analisis_r/modelo-4.stan", data = datos_cafe_ninguno, 
    iter = 2000, chains = 3, cores = 3, verbose = FALSE, seed = 76765)

save(fit_5, file = "datos_procesados/fit_precio_cat_ninguno_indep.RData")

load("datos_procesados/fit_precio_cat_ninguno_indep.RData")

launch_shinystan(fit_5)

fit_5_matrix <- summary(fit_5)$summary
fit_5_df <- data.frame(parameter = rownames(fit_5_matrix), fit_5_matrix, 
    stringsAsFactors = FALSE) %>% 
    separate(parameter, c("parameter", "ind"), sep = "\\[")

betas <- fit_5_df %>% 
    filter(parameter == "beta") %>% 
    separate(ind, c("id_persona", "id_parametro"), ",") %>% 
    mutate(
        id_persona = as.numeric(id_persona), 
        id_parametro = parse_number(id_parametro)
        )

ggplot(betas, aes(x = factor(id_parametro), y = X50.)) +
    geom_boxplot()+ ylim(-40, -25)


parametros <- fit_5_df %>% 
    filter(parameter == "theta" | parammeter == "beta") %>% 
    separate(ind, c("id_persona", "id_parametro"), ",") %>% 
    mutate(
        id_persona = as.numeric(id_persona), 
        id_parametro = parse_number(id_parametro)
    )

ggplot(parametros, aes(x = factor(id_parametro), y = X50.)) +
    geom_boxplot() + 
    facet_wrap(~parameter, ncol = 1)
