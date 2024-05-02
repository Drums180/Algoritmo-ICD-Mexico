
## Librerías
library(missForest)
library(readxl)
library(stringi)
library(openxlsx)
library(dplyr)
library(lmtest)
library(ggplot2)

## Fuentes de Datos
ruta <- "data_evaluada/mexico_icd3.csv"
master_calidad <- read_csv(ruta)

# Limpiar nombres de columnas
nombres <- colnames(master_calidad)
nombres <- tolower(nombres)
nombres <- stringi::stri_trans_general(nombres, "Latin-ASCII")
nombres <- gsub(" ", "_", nombres)
colnames(master_calidad) <- nombres

## Data Aprobada
master_calidad <- master_calidad %>%
  filter(aprobacion == "Aprobado")

head(master_calidad)

## Parametros Aprobados
# Calcular límites por tamaño usando IQR
master_calidad %>%
  group_by(tamano) %>%
  summarise(
    Q1 = quantile(duration, 0.25, na.rm = TRUE),
    Q3 = quantile(duration, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lower_bound_duration = Q1,
    upper_bound_duration = Q3 + (1.5 * IQR),
  ) -> bounds_by_tamano

# Crear un nuevo dataframe a partir de master_calidad que incluye solo las sesiones filtradas
master_calidad_filtered <- master_calidad 

# Paso 3: Aplicar el análisis de IQR para frentes_toni en el nuevo dataframe filtrado
master_calidad_filtered %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(
    Q1 = quantile(frentes_total, 0.125, na.rm = TRUE),
    Q3 = quantile(frentes_total, 0.875, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    # Aplica la condición aquí
    Q3_adjusted = ifelse(IQR == 0, Q3 * 4, Q3),
    lower_bound_frentes = Q1,
    upper_bound_frentes = ifelse(IQR == 0, Q3_adjusted + (8 + IQR + 50), Q3 + (8 * IQR + 50)) 
  ) -> bounds_by_tamano_subcanal_frentes

# Crear un nuevo dataframe a partir de master_calidad que incluye solo las sesiones filtradas
master_calidad_filtered <- master_calidad 

# Paso 3: Aplicar el análisis de IQR para frentes_toni en el nuevo dataframe filtrado
master_calidad_filtered %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(
    Q2 = quantile(frentes_arca, 0.45, na.rm = TRUE),
    Q3 = quantile(frentes_arca, 0.875, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    IQR = Q3 - Q2,
    # Aplica la condición aquí
    Q3_adjusted = ifelse(IQR == 0, Q3 * 4, Q3),
    lower_bound_frentes_arca = Q2,
    upper_bound_frentes_arca = ifelse(IQR == 0, Q3_adjusted + (8 + IQR + 50), Q3 + (8 * IQR + 50)) 
  ) -> bounds_by_tamano_subcanal_frentes_arca

# Suponiendo que los tamaños están en el orden 'Chico', 'Mediano', 'Grande'
tamaños <- c("MI", "CH", "M", "G", "XG")

# Calcular los promedios para cada tamaño y cada variable
avg_by_tamano <- master_calidad %>%
  filter(tamano %in% tamaños) %>%
  group_by(tamano) %>%
  summarise(
    Avg_NumScenes = mean(total_scenes, na.rm = TRUE),
    Avg_ScenesAmb = mean(ambiente_scenes, na.rm = TRUE),
    Avg_ScenesFrio = mean(frio_scenes, na.rm = TRUE)
  ) %>%
  arrange(match(tamano, tamaños))

# Calcular tasas de aumento para cada variable
tasa_aumento <- function(valores) {
  # Calcula las tasas normales para los datos disponibles
  tasas <- c(1, valores[2] / valores[1], valores[3] / valores[2])
  # Extiende la última tasa calculada para los tamaños adicionales
  if (length(valores) > 3) {
    tasas <- c(tasas, rep(tasas[length(tasas)], length(valores) - length(tasas)))
  }
  return(tasas)
}

tasas_NumScenes <- tasa_aumento(avg_by_tamano$Avg_NumScenes)
tasas_ScenesAmb <- tasa_aumento(avg_by_tamano$Avg_ScenesAmb)
tasas_ScenesFrio <- tasa_aumento(avg_by_tamano$Avg_ScenesFrio)

# Aplicar las tasas a los límites para cada tamaño y variable
bounds_by_tamano_scenes <- data.frame(
  tamano = tamaños,
  lower_bound_NumScenes = ceiling(1 * tasas_NumScenes),
  upper_bound_NumScenes = ceiling(6 * tasas_NumScenes),
  lower_bound_ScenesAmb = ceiling(1 * tasas_ScenesAmb),
  upper_bound_ScenesAmb = ceiling(6 * tasas_ScenesAmb),
  lower_bound_ScenesFrio = ceiling(1 * tasas_ScenesFrio),
  upper_bound_ScenesFrio = ceiling(6 * tasas_ScenesFrio)
)

master_calidad_filtered %>%
  group_by(tradechannelcode, sub_canal_isscom, tamano) %>%
  summarise(
    Q1 = quantile(enfriador_total, 0.35, na.rm = TRUE),
    Q3 = quantile(enfriador_total, 0.85, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    # Aplica la condición aquí
    Q3_adjusted = ifelse(IQR == 0, Q3 * 4, Q3),
    lower_bound_enfriadores = Q1,
    upper_bound_enfriadores = Q3 + 1
  ) -> bounds_by_tamano_subcanal_enfriadores

# Unir los dataframes
parametros_unificados <- bounds_by_tamano_subcanal_frentes %>%
  full_join(bounds_by_tamano_subcanal_frentes_arca, by = c("sub_canal_isscom", "tamano")) %>%
  full_join(bounds_by_tamano_subcanal_enfriadores, by = c("sub_canal_isscom", "tamano")) %>%
  full_join(bounds_by_tamano_scenes, by = c("tamano")) %>%
  full_join(bounds_by_tamano, by = c("tamano")) %>%
  select(
    tradechannelcode = tradechannelcode.x,
    sub_canal_isscom,
    tamano,
    lower_bound_duration,
    upper_bound_duration,
    lower_bound_frentes,
    upper_bound_frentes,
    lower_bound_enfriadores,
    upper_bound_enfriadores,
    lower_bound_NumScenes,
    upper_bound_NumScenes,
    lower_bound_ScenesAmb,
    upper_bound_ScenesAmb,
    lower_bound_ScenesFrio,
    upper_bound_ScenesFrio,
    lower_bound_frentes_arca,
    upper_bound_frentes_arca
  )

# Mostrando los resultados
print(parametros_unificados)

## Parametros Generales
# Calcular promedios y cuantiles para 'duration', 'frentes_total', 'enfriadores_total' y 'Scenes'
promedios_por_tamano <- master_calidad %>%
  group_by(tamano) %>%
  summarise(
    promedio_duration = mean(duration, na.rm = TRUE),
    lower_bound_duration = quantile(duration, 0.35, na.rm = TRUE),
    upper_bound_duration = quantile(duration, 0.8, na.rm = TRUE) * 3,
    promedio_frentes_total = mean(frentes_total, na.rm = TRUE),
    lower_bound_frentes_total = quantile(frentes_total, 0.5, na.rm = TRUE),
    upper_bound_frentes_total = quantile(frentes_total, 0.8, na.rm = TRUE) * 7,
    promedio_frentes_total_arca = mean(frentes_arca, na.rm = TRUE),
    lower_bound_frentes_total_arca = quantile(frentes_arca, 0.5, na.rm = TRUE),
    upper_bound_frentes_total_arca = quantile(frentes_arca, 0.8, na.rm = TRUE) * 7,
    promedio_enfriadores_total = mean(enfriador_total, na.rm = TRUE),
    lower_bound_enfriadores_total = quantile(enfriador_total, 0.35, na.rm = TRUE),
    upper_bound_enfriadores_total = quantile(enfriador_total, 0.85, na.rm = TRUE),
    lower_bound_NumScenes = quantile(total_scenes, 0.35, na.rm = TRUE),
    upper_bound_NumScenes = quantile(total_scenes, 0.8, na.rm = TRUE) * 3,
    lower_bound_ScenesAmb = quantile(ambiente_scenes, 0.35, na.rm = TRUE),
    upper_bound_ScenesAmb = quantile(ambiente_scenes, 0.8, na.rm = TRUE) * 3,
    lower_bound_ScenesFrio = quantile(frio_scenes, 0.35, na.rm = TRUE),
    upper_bound_ScenesFrio = quantile(frio_scenes, 0.8, na.rm = TRUE) * 3,
    .groups = 'drop'
  )

# Crear filas "Generales" con la data anterior
general_por_tamano <- promedios_por_tamano %>%
  mutate(tradechannelcode = "General", sub_canal_isscom = "General") %>%
  rename(
    lower_bound_duration = lower_bound_duration,
    upper_bound_duration = upper_bound_duration,
    lower_bound_frentes = lower_bound_frentes_total,
    upper_bound_frentes = upper_bound_frentes_total,
    lower_bound_frentes_arca = lower_bound_frentes_total_arca,
    upper_bound_frentes_arca = upper_bound_frentes_total_arca,
    lower_bound_enfriadores = lower_bound_enfriadores_total,
    upper_bound_enfriadores = upper_bound_enfriadores_total,
    lower_bound_NumScenes = lower_bound_NumScenes,
    upper_bound_NumScenes = upper_bound_NumScenes,
    lower_bound_ScenesAmb = lower_bound_ScenesAmb,
    upper_bound_ScenesAmb = upper_bound_ScenesAmb,
    lower_bound_ScenesFrio = lower_bound_ScenesFrio,
    upper_bound_ScenesFrio = upper_bound_ScenesFrio
  )

# Asegurarse de que 'general_por_tamano' tenga las mismas columnas que 'parametros_unificados', en el mismo orden
general_por_tamano <- general_por_tamano %>%
  select(names(parametros_unificados))

# Añadir las filas de 'general_por_tamano' al final de 'parametros_unificados'
parametros_unificados <- bind_rows(parametros_unificados, general_por_tamano)

# Mostrando los resultados
tail(parametros_unificados)

## Generales por Canal
# Calculando parámetros generales por cada tradechannelcode
general_por_tradechannelcode <- master_calidad %>%
  filter(!is.na(tradechannelcode) & tradechannelcode != "NA") %>%
  group_by(tradechannelcode, tamano) %>%
  summarise(
    promedio_duration = mean(duration, na.rm = TRUE),
    lower_bound_duration = quantile(duration, 0.35, na.rm = TRUE),
    upper_bound_duration = quantile(duration, 0.8, na.rm = TRUE) * 3,
    promedio_frentes = mean(frentes_total, na.rm = TRUE),
    lower_bound_frentes = quantile(frentes_total, 0.5, na.rm = TRUE),
    upper_bound_frentes = quantile(frentes_total, 0.8, na.rm = TRUE) * 7,
    promedio_frentes_arca = mean(frentes_arca, na.rm = TRUE),
    lower_bound_frentes_arca = quantile(frentes_arca, 0.5, na.rm = TRUE),
    upper_bound_frentes_arca = quantile(frentes_arca, 0.8, na.rm = TRUE) * 7,
    promedio_enfriadores = mean(enfriador_total, na.rm = TRUE),
    lower_bound_enfriadores = quantile(enfriador_total, 0.35, na.rm = TRUE),
    upper_bound_enfriadores = quantile(enfriador_total, 0.85, na.rm = TRUE),
    lower_bound_NumScenes = quantile(total_scenes, 0.35, na.rm = TRUE),
    upper_bound_NumScenes = quantile(total_scenes, 0.8, na.rm = TRUE) * 3,
    lower_bound_ScenesAmb = quantile(ambiente_scenes, 0.35, na.rm = TRUE),
    upper_bound_ScenesAmb = quantile(ambiente_scenes, 0.8, na.rm = TRUE) * 3,
    lower_bound_ScenesFrio = quantile(frio_scenes, 0.35, na.rm = TRUE),
    upper_bound_ScenesFrio = quantile(frio_scenes, 0.8, na.rm = TRUE) * 3,
    .groups = 'drop'
  ) %>%
  mutate(sub_canal_isscom = "General")

# Asegurarse de que 'general_por_tradechannelcode' tenga las mismas columnas que 'parametros_unificados', en el mismo orden
general_por_tradechannelcode <- general_por_tradechannelcode %>%
  select(names(parametros_unificados))

# Añadir las filas de 'general_por_tradechannelcode' al final de 'parametros_unificados'
parametros_unificados <- bind_rows(parametros_unificados, general_por_tradechannelcode)

# Eliminar filas con 'tamano' = NA
parametros_unificados <- parametros_unificados %>%
  filter(!is.na(tamano) & tamano != "NA") 

parametros_unificados <- distinct(parametros_unificados)

# Mostrando los resultados
tail(parametros_unificados)

## Exportación
# Ruta del archivo de destino
ruta_destino <- "parametros.xlsx"

# Crear un nuevo archivo de Excel con los datos
wb <- createWorkbook()

# Añadir una hoja de cálculo y escribir los datos en ella
addWorksheet(wb, "data")
writeData(wb, sheet = "data", x = parametros_unificados, startRow = 1, startCol = 1, colNames = TRUE)

# Guardar el archivo de Excel
saveWorkbook(wb, ruta_destino, overwrite = TRUE)

# Mensaje de confirmación
cat("Datos exportados exitosamente a", ruta_destino)

