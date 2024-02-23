# ---- CREACION PARAMETROS INICIALES ----

# Librerias
library(readxl)
library(dplyr)
library(purrr)

ruta <- "master_calidad.xlsx"
# Utilizar df master_calidad si esta en ambiente
df <- read_excel(ruta, sheet = "Export")

# Limpiar nombres de columnas
nombres <- colnames(df)
nombres <- tolower(nombres)
nombres <- stringi::stri_trans_general(nombres, "Latin-ASCII")
nombres <- gsub(" ", "_", nombres)
colnames(df) <- nombres

# Asegurarse de que todos los nombres de columnas sean únicos
nombres <- colnames(master_calidad)
nombres <- make.unique(tolower(nombres))
nombres <- stringi::stri_trans_general(nombres, "Latin-ASCII")
nombres <- gsub(" ", "_", nombres)
colnames(master_calidad) <- nombres

# Ahora puedes intentar filtrar nuevamente
master_calidad <- df


## Parametros de duration por tamaño
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

# Mostrando los resultados
print(bounds_by_tamano)

## Parametros de frentes por tamaño y subcanal


# Crear un nuevo dataframe a partir de master_calidad que incluye solo las sesiones filtradas
master_calidad_filtered <- master_calidad 

# Paso 3: Aplicar el análisis de IQR para frentes_toni en el nuevo dataframe filtrado
master_calidad_filtered %>%
  group_by(tamano, sub_canal_isscom, tradechannelcode) %>%
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


## Parametros de frentes arca

# Crear un nuevo dataframe a partir de master_calidad que incluye solo las sesiones filtradas
master_calidad_filtered <- master_calidad 

# Paso 3: Aplicar el análisis de IQR para frentes_toni en el nuevo dataframe filtrado
master_calidad_filtered %>%
  group_by(tamano, sub_canal_isscom, tradechannelcode) %>%
  summarise(
    Q1 = quantile(frentes_arca, 0.25, na.rm = TRUE),
    Q2 = quantile(frentes_arca, 0.45, na.rm = TRUE),
    Q3 = quantile(frentes_arca, 0.875, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    # Aplica la condición aquí
    Q3_adjusted = ifelse(IQR == 0, Q3 * 4, Q3),
    lower_bound_frentes_arca = Q2,
    upper_bound_frentes_arca = ifelse(IQR == 0, Q3_adjusted + (8 + IQR + 50), Q3 + (8 * IQR + 50)) 
  ) -> bounds_by_tamano_subcanal_frentes_arca

## Parametros de enfriadores por tamaño y subcanal
# Paso 1: Aplicar el análisis de IQR para frentes_toni en el nuevo dataframe filtrado
master_calidad_filtered %>%
  group_by(tamaño_homologado, subcanal_isscom, canal_isscom) %>%
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


# Unificar parametros en un solo df
# Unir los dataframes
parametros_unificados <- bounds_by_tamano_subcanal_frentes %>%
  full_join(bounds_by_tamano_subcanal_frentes_arca, by = "tamaño_homologado") %>%
  full_join(bounds_by_tamano_subcanal_enfriadores, by = "tamaño_homologado") %>%
  full_join(bounds_by_tamano_scenes, by = "tamaño_homologado") %>%
  full_join(bounds_by_tamano, by = "tamaño_homologado") %>%
  select(
    canal_isscom = canal_isscom.x,
    subcanal_isscom = subcanal_isscom.x,
    tamaño_homologado,
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


# Crear parametros generales 
# Calcular promedios y cuantiles para 'duration', 'frentes_total', 'enfriadores_total' y 'Scenes'
promedios_por_tamano <- master_calidad %>%
  group_by(tamaño_homologado) %>%
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
    lower_bound_NumScenes = quantile(Num.Scenes, 0.35, na.rm = TRUE),
    upper_bound_NumScenes = quantile(Num.Scenes, 0.8, na.rm = TRUE) * 3,
    lower_bound_ScenesAmb = quantile(Scenes_Amb, 0.35, na.rm = TRUE),
    upper_bound_ScenesAmb = quantile(Scenes_Amb, 0.8, na.rm = TRUE) * 3,
    lower_bound_ScenesFrio = quantile(Scenes_Frio, 0.35, na.rm = TRUE),
    upper_bound_ScenesFrio = quantile(Scenes_Frio, 0.8, na.rm = TRUE) * 3,
    .groups = 'drop'
  )

# Crear filas "Generales" con la data anterior
general_por_tamano <- promedios_por_tamano %>%
  mutate(canal_isscom = "VIV.LOCALES TRADICIONALES", subcanal_isscom = "General") %>%
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

# Eliminar filas con 'tamano' = NA
parametros_unificados <- parametros_unificados %>%
  filter(!is.na(tamaño_homologado) & tamaño_homologado != "NA") 

# Asegurarse de que 'general_por_tamano' tenga las mismas columnas que 'parametros_unificados', en el mismo orden
general_por_tamano <- general_por_tamano %>%
  select(names(parametros_unificados))

# Añadir las filas de 'general_por_tamano' al final de 'parametros_unificados'
parametros_unificados <- bind_rows(parametros_unificados, general_por_tamano)


# Exportar parametros
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

