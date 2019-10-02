#ACTIVAR LAS LIBRERIAS:
library(tidyr)
library(dplyr)
library(stringr)
library(tidyverse)
library(forcats)
library(mosaic)
library(summarytools)
library(readr)
library(tibble)
library(DT)
library(caret)
library(purrr)
library(ggplot2)


# Descargar los datos directamente de la pagina y ponerlos en el wd--------


if(!file.exists("datos")) {dir.create("datos")}
URL <- "https://www.datos.gov.co/api/views/5q5i-ydf8/rows.csv?accessType=DOWNLOAD"
download.file(URL, destfile = "/Users/REMOTO-NORBERTO/Dropbox (Personal)/noverea_repositorio_R/datos/descarga_salud_datos.csv")

# DECLARAR: encoding PARA EVITAR CARACTERES EXTRAÑOS ----------------------

datos <- as_tibble(read.csv("datos/descarga_salud_datos.csv", header = T, sep = ",", encoding = "UTF-8"))

# RETIRAR COLUMNAS NO NECESARIAS: -----------------------------------------

datos <- dplyr::select(datos, - contains ("fue"), - contains("Id"), - contains("obs"), - (10:33))

# TRANSPONER COLUMNAS DE YEA... A ATRIBUTO VALOR (AÑO, VALOR) -------------

datos_año_valor <- tidyr::gather(datos, "Año", "valor", 5:27)

# CONVERTIR COLUMNAS SELECCIONADAS A MAYUSCULA: ---------------------------

datos_año_valor$nomindicador <- str_to_upper(datos_año_valor$nomindicador, locale = "es")
datos_año_valor$nomdepto <- str_to_upper(datos_año_valor$nomdepto, locale = "es")
datos_año_valor$nommpio <- str_to_upper(datos_año_valor$nommpio, locale = "es")

# QUITAR YEA PARA DEJAR SOLO EL NUMERO DEL ALO ----------------------------

datos_año_valor$Año <- parse_number(datos_año_valor$Año)


# TABLA DE PROMEDIO POR AÑO -----------------------------------------------

datos_año_valor %>% 
  group_by(nomindicador, Año) %>% 
  summarize(promedio = mean(valor)) %>% 
  spread (Año, promedio)   %>%
  datatable()
  
  
PPI <- as.tibble(datos_año_valor) %>% filter(nomindicador == "PORCENTAJE PARTOS INSTITUCIONALES") %>% 
  group_by(nomdepto) %>% summarise(n = n(), promedio = round(mean(valor, na.rm = T), digits = 2))

estadisticos <- datos_año_valor %>%
  group_by(nomindicador) %>%  
  summarise(promedio = round(mean(valor, na.rm = T), digits = 2),
            mediana = round(median(valor, na.rm = T), digits = 2),
            min = round(min(valor, na.rm = T), digits = 2),
            max = round(max(valor, na.rm = T), digits = 2),
            n = dplyr::n()
  )


# DATOS AUSENTES ----------------------------------------------------------

nrow(datos_año_valor)
any(!complete.cases(datos_año_valor))
map_dbl(datos_año_valor, .f = function(x) {sum(is.na(x))})

library(dplyr)
library(ggplot2)
?ggplot

porcentaje_datos_ausentes <- datos_año_valor %>% group_by(nomindicador) %>% 
  summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) %>% 
  ggplot(aes(nomindicador, porcentaje_NA)) +
  geom_col() + labs(title = "Porcentaje de valores ausentes por variable", 
                    x = "variable", y = "Porcentaje de NAs") + theme()

porcentaje_datos_ausentes <- datos_a?o_valor %>% group_by(nomindicador) %>% 
  summarize(porcentaje_NA = 100 * sum(is.na(valor)) / length(valor)) 

ggplot(porcentaje_datos_ausentes, aes(nomindicador, porcentaje_NA)) +
  geom_col() + labs(title = "Porcentaje de valores ausentes por variable", 
                    x = "variable", y = "Porcentaje de NAs") + theme()

  
  