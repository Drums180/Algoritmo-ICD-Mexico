# Librerias
library(missForest)
library(readxl)
library(stringi)
library(openxlsx)
library(dplyr)
library(lmtest)
library(ggplot2)


# Fuentes de Datos
ruta <- "ICD - Indice de Calidad de Data.xlsx"
df <- read_excel(ruta, sheet = "Export")

# Limpiar nombres de columnas
nombres <- colnames(df)
nombres <- tolower(nombres)
nombres <- stringi::stri_trans_general(nombres, "Latin-ASCII")
nombres <- gsub(" ", "_", nombres)
colnames(df) <- nombres

# Data Aprobada
df_aprobada <- df %>%
  filter(aprobacion == "Aprobado")

head(df_aprobada)

# Creación de Parametros "Aprobados"
# Función para filtrar outliers basados en IQR
filter_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  return(x > (Q1 - 0.5 * IQR) & x < (Q3 + 0.5 * IQR))
}

# Filtrar outliers por grupo
df_filtered <- df_aprobada %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  filter(filter_outliers(frentes_total) & 
           filter_outliers(enfriador_total) & 
           filter_outliers(duration))

# Creación de Parametros sin outliers
parametros_df <- df_filtered %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(
    promedio_frentes_total = mean(frentes_total, na.rm = TRUE),
    lower_bound_frentes_total = quantile(frentes_total, 0.025, na.rm = TRUE),
    upper_bound_frentes_total = quantile(frentes_total, 0.975, na.rm = TRUE),
    
    promedio_enfriador_total = mean(enfriador_total, na.rm = TRUE),
    lower_bound_enfriador_total = pmax(0, quantile(enfriador_total, 0.025, na.rm = TRUE)),
    upper_bound_enfriador_total = quantile(enfriador_total, 0.975, na.rm = TRUE),
    
    promedio_duration = mean(duration, na.rm = TRUE),
    lower_bound_duration = quantile(duration, 0.025, na.rm = TRUE),
    upper_bound_duration = quantile(duration, 0.975, na.rm = TRUE)
  )

# Creación de Parametros "Generales"
# Filtrar outliers por grupo en el dataframe original
df_filtered_general <- df %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  filter(filter_outliers(frentes_total) & 
           filter_outliers(enfriador_total) & 
           filter_outliers(duration))

# Creación de Parametros generales sin outliers
parametros_df_general <- df_filtered_general %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(
    promedio_frentes_total = mean(frentes_total, na.rm = TRUE),
    lower_bound_frentes_total = quantile(frentes_total, 0.025, na.rm = TRUE),
    upper_bound_frentes_total = quantile(frentes_total, 0.975, na.rm = TRUE),
    
    promedio_enfriador_total = mean(enfriador_total, na.rm = TRUE),
    lower_bound_enfriador_total = pmax(0, quantile(enfriador_total, 0.025, na.rm = TRUE)), # topado a 0
    upper_bound_enfriador_total = quantile(enfriador_total, 0.975, na.rm = TRUE),
    
    promedio_duration = mean(duration, na.rm = TRUE),
    lower_bound_duration = quantile(duration, 0.025, na.rm = TRUE),
    upper_bound_duration = quantile(duration, 0.975, na.rm = TRUE)
  )

## Creación Combinaciones Posibles

# Todas las combinaciones posibles de sub_canal_isscom y tamano
todos_tamanos <- c("MI", "CH", "M", "G", "XG")
todos_sub_canal_isscom <- unique(parametros_df$sub_canal_isscom)
combinaciones_sub_canal <- expand.grid(sub_canal_isscom = todos_sub_canal_isscom, tamano = todos_tamanos)

# Extraer una tabla de correspondencia entre tradechannelcode y sub_canal_isscom
correspondencia <- dplyr::select(parametros_df, tradechannelcode, sub_canal_isscom) %>%
  unique()

# Hacer un left join de las combinaciones con la tabla de correspondencia para obtener tradechannelcode
combinaciones_con_tradechannel <- left_join(combinaciones_sub_canal, correspondencia, by = "sub_canal_isscom")

# Hacer un left join de las combinaciones con parametros_df
completado <- left_join(combinaciones_con_tradechannel, parametros_df, by = c("tradechannelcode", "sub_canal_isscom", "tamano"))

# Reemplazar NAs con 0 (o cualquier otro valor que desees)
completado[is.na(completado)] <- 0


# Imputación de Vecinos Cercanos para los registros faltantes
# Reemplazar 0 por NA en las columnas que deseas imputar
cols_to_impute <- c("promedio_frentes_total", "lower_bound_frentes_total", "upper_bound_frentes_total", 
                   "promedio_enfriador_total", "lower_bound_enfriador_total", "upper_bound_enfriador_total", 
                   "promedio_duration", "lower_bound_duration", "upper_bound_duration")

completado[cols_to_impute] <- lapply(completado[cols_to_impute], function(col) {
  replace(col, col == 0, NA)
})

# Convertir columnas categóricas a factores
completado$tradechannelcode <- as.factor(completado$tradechannelcode)
completado$sub_canal_isscom <- as.factor(completado$sub_canal_isscom)
completado$tamano <- as.factor(completado$tamano)

# Imputar valores usando missForest
set.seed(123)  # Para reproducibilidad
forest_imputed <- missForest(completado)

# Sustituir valores imputados en el dataframe original
completado_imputado <- forest_imputed$ximp


# Parametros Finales
# Hacer un left join de completado_imputado con parametros_df
parametros_df_final <- left_join(completado_imputado, 
                                 parametros_df, 
                                 by = c("tradechannelcode", "sub_canal_isscom", "tamano"), 
                                 suffix = c("_imputado", "_original"))

# Para cada columna de interés, si el valor "_original" no es NA, entonces tomar ese valor. Si no, conservar el "_imputado"
for (columna in cols_to_impute) {
  parametros_df_final[[columna]] <- ifelse(!is.na(parametros_df_final[[paste0(columna, "_original")]]), 
                                           parametros_df_final[[paste0(columna, "_original")]], 
                                           parametros_df_final[[paste0(columna, "_imputado")]])
}

# Añadir una columna de "aprobación" basada en si el valor "_original" es NA o no
parametros_df_final$aprobacion <- ifelse(!is.na(parametros_df_final$promedio_frentes_total_original), 
                                         "aprobado", 
                                         "imputado")

# Encontrar las columnas que terminan en "_original" o "_imputado"
cols_to_remove <- colnames(parametros_df_final)[grep("_original$|_imputado$", colnames(parametros_df_final))]

# Eliminar esas columnas del dataframe
parametros_df_final <- parametros_df_final[, !names(parametros_df_final) %in% cols_to_remove]

# Inflar por 3 el valor del upper bound
parametros_df_final$upper_bound_frentes_total <- parametros_df_final$upper_bound_frentes_total * 3
parametros_df_final$upper_bound_enfriador_total <- parametros_df_final$upper_bound_enfriador_total * 3
parametros_df_final$upper_bound_duration <- parametros_df_final$upper_bound_duration * 3


# 1. Calcular la cantidad de muestras para cada combinación
n_muestras <- df_aprobada %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(n = n())

# 2. Calcular el producto de cada métrica por su peso
df_ponderado <- df_aprobada %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(
    suma_ponderada_frentes_total = sum(frentes_total, na.rm = TRUE) * first(n_muestras$n),
    suma_ponderada_enfriador_total = sum(enfriador_total, na.rm = TRUE) * first(n_muestras$n),
    suma_ponderada_duration = sum(duration, na.rm = TRUE) * first(n_muestras$n)
  )

# 3. Sumar todos estos productos y dividir por la suma total de muestras para obtener el promedio ponderado
promedio_ponderado <- df_ponderado %>%
  summarise(
    promedio_ponderado_frentes_total = sum(suma_ponderada_frentes_total) / sum(n_muestras$n),
    promedio_ponderado_enfriador_total = sum(suma_ponderada_enfriador_total) / sum(n_muestras$n),
    promedio_ponderado_duration = sum(suma_ponderada_duration) / sum(n_muestras$n)
  )

# Separar por tradechannelcode
promedio_ponderado_tradicional <- filter(promedio_ponderado, tradechannelcode == "Tradicional")
promedio_ponderado_comer_beber <- filter(promedio_ponderado, tradechannelcode == "Comer y Beber")

promedio_ponderado_tradicional
promedio_ponderado_comer_beber


# Crear promedios por tradechannelcode y tamano
promedios_por_tamano <- df_aprobada %>%
  group_by(tradechannelcode, tamano) %>%
  summarise(
    promedio_frentes_total = mean(frentes_total, na.rm = TRUE),
    lower_bound_frentes_total = quantile(frentes_total, 0.35, na.rm = TRUE),
    upper_bound_frentes_total = quantile(frentes_total, 1, na.rm = TRUE)*3,
    promedio_enfriador_total = mean(enfriador_total, na.rm = TRUE),
    lower_bound_enfriador_total = quantile(enfriador_total, 0.35, na.rm = TRUE),
    upper_bound_enfriador_total = quantile(enfriador_total, 1, na.rm = TRUE)*2,
    promedio_duration = mean(duration, na.rm = TRUE),
    lower_bound_duration = quantile(duration, 0.35, na.rm = TRUE),
    upper_bound_duration = quantile(duration, 0.8, na.rm = TRUE)*3
  )

# Crear filas generales con la data anterior
general_por_tamano <- promedios_por_tamano %>%
  mutate(sub_canal_isscom = "General", aprobacion = "general")

# Añadir las filas al dataframe parametros_df_final
parametros_df_final <- rbind(parametros_df_final, general_por_tamano)


# Ruta del archivo de destino
ruta_destino <- "parametros.xlsx"

# Leer el archivo Excel existente
wb <- loadWorkbook(ruta_destino)

# Escribir datos en la hoja "data"
writeData(wb, sheet = "data", x = parametros_df_final, startRow = 1, startCol = 1, colNames = TRUE)

# Guardar los cambios
saveWorkbook(wb, ruta_destino, overwrite = TRUE)

# Mensaje de confirmación
cat("Datos exportados exitosamente a", ruta_destino)

