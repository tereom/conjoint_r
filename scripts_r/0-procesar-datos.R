library(ggplot2)
library(readr)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# diseño
dis <- read_csv(file = '../datos/transformacion.csv')

# datos
datos <- read_sav(file = '../datos/base-cafe.sav')

cbc <- datos %>% select(sys_RespNum, contains('CBC'), contains('CO1_Random'))

# pegar variables de diseño

cbc.2 <- cbc %>% gather(variable, value, CO1_Random1:CO1_Random25) %>%
    mutate(task = str_sub(variable, 11)) %>%
    rename(version = sys_CBCVersion_CO1) %>%
    mutate(version = as.integer(version), task = as.integer(task)) %>%
    left_join(dis)
head(cbc.2)

precio_niveles <- data_frame(precio = 1:15, 
    precio_val = c(9, 13, 18, 22, 26, 27, 35, 42, 50, 57, 72, 89, 107, 124, 141))

cbc.3 <- cbc.2 %>% left_join(precio_niveles)
head(cbc.3)

cafe <- cbc.3 %>% 
    mutate(id = as.integer(as.factor(sys_RespNum))) %>%
    select(id, version, seleccion = value, 
    tarea = task, concepto = concept, marca, precio = precio_val) 
                  
cafe$precio_int <- as.numeric(as.factor(cafe$precio))

save(cafe, file = "datos/cafe.Rdata")

 
