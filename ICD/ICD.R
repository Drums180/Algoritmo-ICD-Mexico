# Llamar Librerias
library(dplyr)    # Manipulación de datos
library(readr)    # Lectura de datos tabulares
library(readxl)   # Lectura de archivos Excel
library(tidyverse) # Ciencia de datos (incluye dplyr, ggplot2, etc.)
library(fs)       # Manejo del sistema de archivos
library(purrr)    # Programación funcional
library(openxlsx) # Lectura/escritura de Excel avanzada
library(sf)       # Datos espaciales
library(httr)     # Comunicación con APIs web
library(jsonlite) # Trabajo con JSON
library(ggplot2)  # Visualización de datos
library(stringr)  # Modificación de texto

# Carga de Base de Datos

## Quality Data
setwd("fuentes_datos/")  # Establecer ruta de trabajo

# Función para leer y combinar archivos de una carpeta
combine_files <- function(path, file_type = c("csv", "excel")) {
  
  # Obtener lista de archivos en el directorio especificado
  files <- list.files(path, full.names = TRUE)
  
  df_combined <- data.frame()
  
  process_file <- function(file) {
    tryCatch({
      if (grepl("\\.csv$", file)) {
        data <- read_csv(file)
      } else if (grepl("\\.(xlsx|xls|XLSX|XLS)$", file)) {
        data <- read_excel(file)
      } else {
        stop("Tipo de archivo no soportado")
      }
      
      # Lista de columnas que deben ser double
      cols_to_double <- c("Duration(Sec)", "longitude", "latitude", "SessionEndLongitude", "SessionEndLatitude")
      
      # Convertir todas las columnas a character, excepto las especificadas
      data[] <- lapply(names(data), function(col) {
        if (col %in% cols_to_double) {
          as.numeric(as.character(data[[col]]))
        } else {
          as.character(data[[col]])
        }
      })
      
      return(data)
    }, error = function(e) {
      print(paste("Error al procesar el archivo:", file))  # Imprimir el archivo con error
      print(e)  # Imprimir el error específico
      return(NULL)
    })
  }
  
  if ("csv" %in% file_type) {
    files_csv <- grep("\\.csv$", files, value = TRUE)
    if(length(files_csv) > 0){
      df_list_csv <- lapply(files_csv, process_file)
      df_combined <- bind_rows(df_combined, do.call(bind_rows, df_list_csv))
    }
  }
  
  if ("excel" %in% file_type) {
    files_xls <- grep("\\.(xlsx|xls|XLSX|XLS)$", files, value = TRUE)
    if(length(files_xls) > 0){
      df_list_xls <- lapply(files_xls, process_file)
      df_combined <- bind_rows(df_combined, do.call(bind_rows, df_list_xls))
    }
  }
  
  # Eliminar filas duplicadas
  if(nrow(df_combined) > 0) {
    df_combined <- df_combined %>% distinct()
  }
  
  return(df_combined)
}

# Crear data frames para cada carpeta
df_actual <- combine_files("actual/")
df_session <- combine_files("session/")
df_survey <- combine_files("survey/")
df_scenes <- combine_files("scenes/")

## Query Como Dato
# Lista de todos los archivos Excel en el directorio
files <- dir("fuentes_datos/comodato", pattern = "\\.xlsx$", full.names = TRUE)

# Lee cada archivo y combínalos en un solo dataframe
df_comodato <- files %>%
  map_dfr(~ read_xlsx(.x))

# Agrupa por Código Cliente y suma # de Artículos
df_poc <- df_comodato %>%
  group_by(`Código Cliente`) %>%
  summarise(Total_Articulos = sum(`# de Articulos`, na.rm = TRUE)) %>%
  ungroup()


## Master Clientes
# Función para limpiar y estandarizar nombres de columnas
limpiar_nombres <- function(df){
  names(df) <- tolower(names(df))
  names(df) <- chartr("áéíóú", "aeiou", names(df))
  names(df) <- gsub(" ", "_", names(df))
  return(df)
}

# Leer todos los archivos en la carpeta
read_all_files <- function(path) {
  files <- list.files(path, full.names = TRUE, pattern = "\\.xlsx$")
  dfs <- lapply(files, read_xlsx)
  names(dfs) <- gsub(".xlsx", "", basename(files))
  return(dfs)
}

# Leer todos los archivos en fuentes_datos/clientes, excepto el de clientes que leeremos directamente
all_dfs <- read_all_files("fuentes_datos/clientes")

# Leer y limpiar los archivos Excel
clientes_trad <- read_xlsx("fuentes_datos/clientes/Catálogo Clientes MX TRAD + C&B.xlsx") %>% limpiar_nombres()
omd_censo <- read_xlsx("fuentes_datos/clientes/OMD Censo México NOV23.xlsx") %>% limpiar_nombres()

# Convertir PostalCode a character si existe en ambos dataframes
if("postalcode" %in% names(clientes_trad) && "postalcode" %in% names(omd_censo)) {
  clientes_trad$postalcode <- as.character(clientes_trad$postalcode)
  omd_censo$postalcode <- as.character(omd_censo$postalcode)
}

# Combinar los archivos
clientes_combinados <- bind_rows(clientes_trad, omd_censo)

# Eliminar duplicados, manteniendo el último (de omd_censo)
clientes_unicos <- clientes_combinados %>% 
  arrange(desc(customercode)) %>% 
  distinct(customercode, .keep_all = TRUE)

# Aplica limpiar nombres a las tablas en all_dfs
all_dfs <- lapply(all_dfs, function(df) {
  names(df) <- limpiar_nombres(names(df))
  return(df)
})

# Tomar el dataframe 'mds' de all_dfs:
mds <- all_dfs[["Clientes"]]  # Ajusta el nombre según corresponda si es diferente

# Elimina registros duplicados en mds basados en codigo_cliente
mds <- mds %>%
  limpiar_nombres() %>%
  group_by(codigo_cliente) %>%
  filter(row_number() == 1) %>%
  ungroup()

# Creación de la columna 'tamaño' en 'mds'
mds <- mds %>%
  mutate(
    tamaño = ifelse(canal_rgm == "Comer y Beber", 
                    tamaño_de_cliente, 
                    tamaño_de_cliente_industria)
  ) %>%
  mutate(tamaño = tolower(tamaño)) %>%
  mutate(
    tamaño = case_when(
      tamaño == "micro" ~ "MI",
      tamaño == "pequeño" ~ "CH",
      tamaño == "mediano" ~ "M",
      tamaño == "grande" ~ "G",
      tamaño == "extra grande" ~ "XG",
      TRUE ~ tamaño
    )
  )

# Hacer el left_join con clientes
master_clientes <- clientes_unicos %>%
  left_join(mds[, c("codigo_cliente", "tamaño", "sub_canal_isscom", "modelo_de_servicio_ruta", "ruta_preventa_oficial", "zona...13", "territorio...14")], 
            by = c("customercode" = "codigo_cliente"))

## Parametros
# Cargar la hoja "data" del archivo "parametros"
parametros_data <- read_excel("parametros.xlsx", sheet = "data")


# Sabana de Analisis

## Tiempo
df_survey <- df_survey %>%
  mutate(duration = `Duration(Sec)` / 60) %>%
  mutate(estatus = if_else(Status == "Complete", 1, 0))

## Coordenadas
# Función del haversine para calcular distancia entre dos puntos de latitud y longitud
haversine <- function(lon1, lat1, lon2, lat2) {
  R <- 6371000  # Radio de la Tierra en metros
  phi1 <- lat1 * (pi / 180)
  phi2 <- lat2 * (pi / 180)
  delta_phi <- (lat2 - lat1) * (pi / 180)
  delta_lambda <- (lon2 - lon1) * (pi / 180)
  
  a <- sin(delta_phi / 2)^2 + cos(phi1) * cos(phi2) * sin(delta_lambda / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  d <- R * c  # Distancia en metros
  return(d)
}

# Convertir las columnas relevantes a character
df_session$OutletCode <- as.character(df_session$OutletCode)
master_clientes$customercode <- as.character(master_clientes$customercode)

# Realizar el join y calcular la distancia usando la función haversine
df_session <- df_session %>%
  left_join(select(master_clientes, customercode, latitude, longitude),
            by = c("OutletCode" = "customercode")) %>%
  mutate(
    distance = mapply(haversine, longitude, latitude, SessionEndLongitude, SessionEndLatitude)
  )

# Calculamos los cuartiles y el rango intercuartil de la distancia
Q1 <- quantile(df_session$distance, 0.25, na.rm = TRUE)
Q3 <- quantile(df_session$distance, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

# Reemplazar valores atípicos por 0
df_session$distance[df_session$distance < (Q1 - 1.5 * IQR) | 
                      df_session$distance > (Q3 + 1.5 * IQR)] <- 0

# Identificar valores atípicos
outliers <- df_session$distance[df_session$distance < (Q1 - 1.5 * IQR) | 
                                  df_session$distance > (Q3 + 1.5 * IQR)]

## Frentes - (CHECK)
frentes_df <- df_actual %>%
  group_by(SessionUID) %>%
  summarise(
    # Cálculo de num_frentes
    num_frentes = sum(ProductName != "Foreign" & IsEmpty == FALSE),
    # Cálculo de frentes_com
    frentes_com = sum(ProductName != "Foreign" & IsForeign == TRUE),
    # Cálculo de frentes_total
    frentes_total = sum(ProductName != "Foreign" & IsEmpty == FALSE)
  ) %>%
  mutate(
    # Cálculo de frentes_arca
    frentes_arca = frentes_total - frentes_com,
    # Cálculo de sovi
    sovi = ifelse(frentes_total == 0, 0, frentes_arca / frentes_total)
  )

## Enfriadores
result_enfriadores <- df_scenes %>%
  # Agrupar por SessionUID y SubSceneType y contar
  group_by(SessionUID, SubSceneType) %>%
  count() %>%
  # Transforma valores de SubSceneType en columnas individuales
  spread(key = SubSceneType, value = n, fill = 0) %>% # fill = 0 para llenar con 0s donde no haya conteos
  ungroup()

# Funcion para sumar Enfriadores 

# Genera las columnas si no existen
if (!"Enfriador AC" %in% names(result_enfriadores)) {
  result_enfriadores$`Enfriador AC` <- 0
}
if (!"Enfriador AC CEDIDO" %in% names(result_enfriadores)) {
  result_enfriadores$`Enfriador AC CEDIDO` <- 0
}

# Suma las columnas
result_enfriadores <- result_enfriadores %>%
  mutate(enfriador_total = `Enfriador AC` + `Enfriador AC CEDIDO`)

## Scenes
# Unir df_actual con df_scenes para obtener SubSceneType en df_actual
df_actual_con_scenes <- df_actual %>%
  left_join(df_scenes %>% select(SceneUID, SubSceneType), by = "SceneUID")

# Calcular Frentes por SceneUID con SubSceneType ahora disponible
df_frentes <- df_actual_con_scenes %>%
  group_by(SceneUID, SubSceneType) %>%
  summarise(frentes = n_distinct(ProductName[ProductName != "Foreign" & IsEmpty == FALSE]),
            .groups = 'drop') %>%
  mutate(es_chuponera = grepl("Chuponera", SubSceneType))

# Filtrar escenas por número de frentes, excluyendo "Chuponeras" del criterio de más de 5 frentes
df_filtrado <- df_frentes %>%
  filter((!es_chuponera & frentes > 5) | es_chuponera)

# Paso 1: Crear df con SceneUID, MaxDoorIndex, y SessionUID basado en df_actual
df_actual_maxdoorindex <- df_actual %>%
  group_by(SceneUID) %>%
  summarise(MaxDoorIndex = max(DoorIndex, na.rm = TRUE), .groups = 'drop') %>%
  left_join(df_actual %>% select(SceneUID, SessionUID) %>% distinct(), by = "SceneUID")

# Paso 2: Hacer join con df_scenes para agregar SceneType
df_joined <- df_actual_maxdoorindex %>%
  left_join(df_scenes %>% select(SceneUID, SceneType) %>% distinct(), by = "SceneUID")

# Unir con df_filtrado que incluye el filtro de frentes
df_joined_con_frentes <- df_joined %>%
  inner_join(df_filtrado %>% select(SceneUID, es_chuponera, frentes), by = "SceneUID")

# Paso 3: Usar df_joined_con_frentes para calcular la cantidad de escenas de cada tipo y el total por SessionUID
df_summary <- df_joined_con_frentes %>%
  group_by(SessionUID) %>%
  summarise(Total_Scenes = n(),
            Ambiente_Scenes = sum(SceneType == "AMBIENTE"),
            Frio_Scenes = sum(SceneType == "FRIO"),
            .groups = 'drop') %>%
  replace_na(list(Ambiente_Scenes = 0, Frio_Scenes = 0)) # Reemplazar NA con 0


## Flags
# Creación del vector de valores de flag
flag_values <- c(2, 4, 13, 14, 15, 16, 26, 27, 32, 36, 39, 31)
#-------------------------------------------------------------

# Función para dividir la cadena en pares y añadir comas
format_image_quality <- function(value) {
  # Divide la cadena en pares de caracteres
  split_values <- str_extract_all(value, ".{1,2}")[[1]]
  # Une los valores con comas
  paste(split_values, collapse = ",")
}

# Aplica la función a la columna ImageQuality
df_scenes$ImageQuality <- sapply(df_scenes$ImageQuality, format_image_quality)

# Ahora procede con la agrupación y concatenación
result_scenes <- df_scenes %>%
  group_by(SessionUID) %>%
  summarise(ImageQuality = paste(unique(ImageQuality), collapse = ",")) %>%
  ungroup()

#--------------------------------------------------------------
# Función para detectar las flags en la columna ImageQuality
detect_flags <- function(quality_string) {
  quality_values <- as.numeric(unlist(strsplit(quality_string, ",")))  # Divide la cadena y convierte a numéricos
  detected <- quality_values[quality_values %in% flag_values]
  if(length(detected) > 0) {
    return(paste(detected, collapse = ","))
  } else {
    return(NA_character_)
  }
}

# Función para comprobar si alguno de los valores de flag está en la columna ImageQuality
check_flags <- function(quality_string) {
  quality_values <- as.numeric(unlist(strsplit(quality_string, ",")))  # Divide la cadena y convierte a numéricos
  
  if(any(quality_values %in% flag_values)) {
    return(-100)
  } else {
    return(100)
  }
}

# Aplicar ambas funciones a la columna ImageQuality de result_scenes
result_scenes$flag_trigger <- sapply(result_scenes$ImageQuality, check_flags)
result_scenes$detected_flags <- sapply(result_scenes$ImageQuality, detect_flags)


## Cuotas
master_clientes <- master_clientes %>%
  mutate(
    cuota_diaria = case_when(
      modelo_de_servicio_ruta == "PRE ESPECIALIZADA" ~ 3,
      modelo_de_servicio_ruta == "PREVENTA 24" ~ 2,
      modelo_de_servicio_ruta == "PREVENTA 48 FORANEO" ~ 1,
      modelo_de_servicio_ruta == "PRE MDO TRADICIONA" ~ 2,
      modelo_de_servicio_ruta == "PRE MICRO MERCADOS" ~ 3,
      modelo_de_servicio_ruta == "PREVENTA 72 FOR" ~ 1,
      modelo_de_servicio_ruta == "PREVENTA 48 METROPOLITANO" ~ 1,
      modelo_de_servicio_ruta == "PRE COMERCIAL" ~ 2,
      modelo_de_servicio_ruta == "PREVENTA NCBS" ~ 3,
      TRUE ~ NA_real_
    )
  )

## Carga de Data Historica
# Ruta a la carpeta con archivos históricos procesados
ruta_carpeta <- "data_procesada/"

# Verificar si hay archivos históricos
archivos_historicos <- list.files(path = ruta_carpeta, pattern = "\\.csv$", full.names = TRUE)

if (length(archivos_historicos) > 0) {
  # Cargar y combinar todos los archivos históricos en un dataframe
  datos_historicos <- map_dfr(archivos_historicos, function(file) {
    datos <- read_csv(file)
    datos$`Outlet Code` <- as.character(datos$`Outlet Code`)  # Asegurar que Outlet Code sea siempre un carácter
    select(datos,
           SessionUID, SurveyType, User, `Outlet Code`, 
           salesorganizationcode, tamaño, tradechannelcode, sub_canal_isscom,
           duration, distance, 
           frentes_total, frentes_arca, sovi, flag_trigger, enfriador_total, 
           Total_Scenes, Ambiente_Scenes, Frio_Scenes, detected_flags,
           modelo_de_servicio_ruta, salesterritorycode, territorio, `Survey End Time`, estatus
    )
  })
} else {
  # Inicializar un dataframe vacío con las columnas esperadas
  datos_historicos <- tibble(
    SessionUID = character(), SurveyType = character(), User = character(), `Outlet Code` = character(),
    salesorganizationcode = character(), tamaño = character(), tradechannelcode = character(), sub_canal_isscom = character(),
    duration = numeric(), distance = numeric(), 
    frentes_total = numeric(), frentes_arca = numeric(), sovi = numeric(), flag_trigger = numeric(), enfriador_total = numeric(),
    Total_Scenes = numeric(), Ambiente_Scenes = numeric(), Frio_Scenes = numeric(), detected_flags = character(),
    modelo_de_servicio_ruta = character(), salesterritorycode = character(), territorio = character(), `Survey End Time` = Date(), estatus = numeric()
  )
}


## Data Nueva e Histórica
# Preparar datos nuevos de df_survey con las columnas relevantes
nuevos_datos_survey <- df_survey %>%
  mutate(
    `Survey End Time` = as.Date(substr(`Survey End Time`, 1, 10), format = "%d/%m/%Y"),
    SessionUID = `Session Uid`, 
    SurveyType = `Survey Type`, 
    duration = duration
  ) %>%
  select(
    SessionUID, SurveyType, User, `Outlet Code`, duration, `Survey End Time`, estatus
  )

# Combinar datos históricos con nuevos datos de survey, ahora ambos con 'Survey End Time' como tipo Date
datos_combinados <- bind_rows(datos_historicos, nuevos_datos_survey) %>%
  distinct(SessionUID, .keep_all = TRUE)

## Sabana Calidad Semilla 
# Imputaciones desde df_session para 'distance'
datos_combinados <- datos_combinados %>%
  mutate(
    distance = ifelse(is.na(distance),
                      df_session$distance[match(SessionUID, df_session$SessionUId)],
                      distance)
  )

# Imputaciones desde frentes_df para 'frentes_total', 'frentes_arca', y 'sovi'
datos_combinados <- datos_combinados %>%
  mutate(
    frentes_total = ifelse(is.na(frentes_total),
                           frentes_df$frentes_total[match(SessionUID, frentes_df$SessionUID)],
                           frentes_total),
    frentes_arca = ifelse(is.na(frentes_arca),
                          frentes_df$frentes_arca[match(SessionUID, frentes_df$SessionUID)],
                          frentes_arca),
    sovi = ifelse(is.na(sovi),
                  frentes_df$sovi[match(SessionUID, frentes_df$SessionUID)],
                  sovi)
  )

# Imputaciones desde result_scenes para 'flag_trigger' y 'detected_flags'
datos_combinados <- datos_combinados %>%
  mutate(
    flag_trigger = ifelse(is.na(flag_trigger),
                          result_scenes$flag_trigger[match(SessionUID, result_scenes$SessionUID)],
                          flag_trigger),
    detected_flags = ifelse(is.na(detected_flags),
                            result_scenes$detected_flags[match(SessionUID, result_scenes$SessionUID)],
                            detected_flags)
  )

# Imputaciones desde result_enfriadores para 'enfriador_total'
datos_combinados <- datos_combinados %>%
  mutate(
    enfriador_total = ifelse(is.na(enfriador_total),
                             result_enfriadores$enfriador_total[match(SessionUID, result_enfriadores$SessionUID)],
                             enfriador_total)
  )

# Imputaciones para datos de clientes
datos_combinados <- datos_combinados %>%
  mutate(
    tamaño = ifelse(is.na(tamaño),
                    master_clientes$tamaño[match(`Outlet Code`, master_clientes$customercode)],
                    tamaño),
    salesorganizationcode = ifelse(is.na(salesorganizationcode),
                                   master_clientes$salesorganizationcode[match(`Outlet Code`, master_clientes$customercode)],
                                   salesorganizationcode),
    tradechannelcode = ifelse(is.na(tradechannelcode),
                              master_clientes$tradechannelcode[match(`Outlet Code`, master_clientes$customercode)],
                              tradechannelcode),
    sub_canal_isscom = ifelse(is.na(sub_canal_isscom),
                              master_clientes$sub_canal_isscom[match(`Outlet Code`, master_clientes$customercode)],
                              sub_canal_isscom),
    modelo_de_servicio_ruta = ifelse(is.na(modelo_de_servicio_ruta),
                                     master_clientes$modelo_de_servicio_ruta[match(`Outlet Code`, master_clientes$customercode)],
                                     modelo_de_servicio_ruta),
    salesterritorycode = ifelse(is.na(salesterritorycode),
                                master_clientes$salesterritorycode[match(`Outlet Code`, master_clientes$customercode)],
                                salesterritorycode),
    territorio = ifelse(is.na(territorio),
                        master_clientes$`territorio...14`[match(`Outlet Code`, master_clientes$customercode)],
                        territorio)
  )

# Imputaciones desde df_summary para 'Total_Scenes', 'Ambiente_Scenes', y 'Frio_Scenes'
datos_combinados <- datos_combinados %>%
  mutate(
    Total_Scenes = ifelse(is.na(Total_Scenes),
                          df_summary$Total_Scenes[match(SessionUID, df_summary$SessionUID)],
                          Total_Scenes),
    Ambiente_Scenes = ifelse(is.na(Ambiente_Scenes),
                             df_summary$Ambiente_Scenes[match(SessionUID, df_summary$SessionUID)],
                             Ambiente_Scenes),
    Frio_Scenes = ifelse(is.na(Frio_Scenes),
                         df_summary$Frio_Scenes[match(SessionUID,df_summary$SessionUID)],
                         Frio_Scenes)
  )

master_calidad <- datos_combinados

## Eliminar Directorio
# Función para limpiar directorio de archivos individuales, manteniendo solo el combinado
clean_directory <- function(path) {
  combined_file_path <- file.path(path, "combined.csv")
  
  # Listar todos los archivos excepto el archivo combinado
  files_to_delete <- setdiff(list.files(path, full.names = TRUE), combined_file_path)
  
  # Eliminar archivos
  file.remove(files_to_delete)
}

# Limpiar directorios de archivos individuales
clean_directory("fuentes_datos/session/")
clean_directory("fuentes_datos/survey/")
clean_directory("fuentes_datos/actual/")
clean_directory("fuentes_datos/scenes/")

# Correción de Datos

## Flags Inducidas 
# 1. Contar los SceneUID distintos para cada SessionUID
scene_count_df <- df_actual %>%
  group_by(SessionUID) %>%
  summarise(min_frentes = n_distinct(SceneUID) * 3) 

# Unir temporalmente con master_calidad para aplicar las condiciones
master_calidad_temp <- left_join(master_calidad, scene_count_df, by = "SessionUID")

# Actualizar las flags de acuerdo con las condiciones
master_calidad <- master_calidad_temp %>%
  mutate(
    # Flag para 0 frentes totales
    detected_flags = ifelse(frentes_total == 0, 
                            ifelse(is.na(detected_flags), "66", paste(detected_flags, ",66", sep="")),
                            detected_flags),
    flag_trigger = ifelse(frentes_total == 0, -100, flag_trigger),
    # Flag para frentes insuficientes
    detected_flags = ifelse(frentes_total < min_frentes, 
                            ifelse(is.na(detected_flags), "61", paste(detected_flags, ",61", sep="")),
                            detected_flags),
    flag_trigger = ifelse(frentes_total < min_frentes, -100, flag_trigger),
    # Nueva flag para enfriador_total no nulo y frentes_total nulo
    detected_flags = ifelse(!is.na(enfriador_total) & is.na(frentes_total),
                            ifelse(is.na(detected_flags), "-100", paste(detected_flags, ",-100", sep="")),
                            detected_flags),
    flag_trigger = ifelse(!is.na(enfriador_total) & is.na(frentes_total), -100, flag_trigger)
  ) %>%
  select(-min_frentes)  # Eliminamos la columna temporal min_frentes


# ----------EVALUACIÓN-----------
# Evaluación de Data
# Paso 1: Unión con parametros_data
master_evaluado <- master_calidad %>%
  left_join(parametros_data, by = c("tamaño", "tradechannelcode", "sub_canal_isscom"))

# Paso 2: Generar subconjuntos basados en combinaciones de tradechannelcode, tamaño y sub_canal_isscom "General"
comer_beber_MI <- parametros_data[parametros_data$tradechannelcode == "Comer y Beber" & parametros_data$tamaño == "MI" & parametros_data$sub_canal_isscom == "General", ]
comer_beber_CH <- parametros_data[parametros_data$tradechannelcode == "Comer y Beber" & parametros_data$tamaño == "CH" & parametros_data$sub_canal_isscom == "General", ]
comer_beber_M  <- parametros_data[parametros_data$tradechannelcode == "Comer y Beber" & parametros_data$tamaño == "M"  & parametros_data$sub_canal_isscom == "General", ]
comer_beber_G  <- parametros_data[parametros_data$tradechannelcode == "Comer y Beber" & parametros_data$tamaño == "G"  & parametros_data$sub_canal_isscom == "General", ]
comer_beber_XG <- parametros_data[parametros_data$tradechannelcode == "Comer y Beber" & parametros_data$tamaño == "XG" & parametros_data$sub_canal_isscom == "General", ]

tradicional_MI <- parametros_data[parametros_data$tradechannelcode == "Tradicional" & parametros_data$tamaño == "MI" & parametros_data$sub_canal_isscom == "General", ]
tradicional_CH <- parametros_data[parametros_data$tradechannelcode == "Tradicional" & parametros_data$tamaño == "CH" & parametros_data$sub_canal_isscom == "General", ]
tradicional_M  <- parametros_data[parametros_data$tradechannelcode == "Tradicional" & parametros_data$tamaño == "M"  & parametros_data$sub_canal_isscom == "General", ]
tradicional_G  <- parametros_data[parametros_data$tradechannelcode == "Tradicional" & parametros_data$tamaño == "G"  & parametros_data$sub_canal_isscom == "General", ]
tradicional_XG <- parametros_data[parametros_data$tradechannelcode == "Tradicional" & parametros_data$tamaño == "XG" & parametros_data$sub_canal_isscom == "General", ]
# Corrección del Paso 2 y 3: Usar bucle para iterar sobre columnas y reemplazar NAs
cols_to_modify <- c("lower_bound_frentes", "upper_bound_frentes", 
                    "lower_bound_enfriadores","upper_bound_enfriadores", 
                    "lower_bound_duration", "upper_bound_duration","lower_bound_ScenesFrio", "upper_bound_ScenesFrio",  "lower_bound_frentes_arca", "upper_bound_frentes_arca", "lower_bound_ScenesAmb", "upper_bound_ScenesAmb", "lower_bound_NumScenes", "upper_bound_NumScenes")

for(col in cols_to_modify) {
  
  # Comer y Beber
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Comer y Beber" & master_evaluado$tamaño == "MI", comer_beber_MI[[col]], master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Comer y Beber" & master_evaluado$tamaño == "CH", comer_beber_CH[[col]], master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Comer y Beber" & master_evaluado$tamaño == "M",  comer_beber_M[[col]],  master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Comer y Beber" & master_evaluado$tamaño == "G",  comer_beber_G[[col]],  master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Comer y Beber" & master_evaluado$tamaño == "XG", comer_beber_XG[[col]], master_evaluado[[col]])
  
  # Tradicional
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Tradicional" & master_evaluado$tamaño == "MI", tradicional_MI[[col]], master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Tradicional" & master_evaluado$tamaño == "CH", tradicional_CH[[col]], master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Tradicional" & master_evaluado$tamaño == "M",  tradicional_M[[col]],  master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Tradicional" & master_evaluado$tamaño == "G",  tradicional_G[[col]],  master_evaluado[[col]])
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & master_evaluado$tradechannelcode == "Tradicional" & master_evaluado$tamaño == "XG", tradicional_XG[[col]], master_evaluado[[col]])
  
  # Para los NA en tradechannelcode
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & is.na(master_evaluado$tradechannelcode), tradicional_M[[col]], master_evaluado[[col]])
  
  # Caso donde tamaño es NA pero tradechannelcode es "Comer y Beber"
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & is.na(master_evaluado$tamaño) & master_evaluado$tradechannelcode == "Comer y Beber", comer_beber_M[[col]], master_evaluado[[col]])
  
  # Caso donde tamaño es NA pero tradechannelcode es "Tradicional"
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]) & is.na(master_evaluado$tamaño) & master_evaluado$tradechannelcode == "Tradicional", tradicional_M[[col]], master_evaluado[[col]])
}

# Paso 4
# Imputación final para reemplazar cualquier NA restante con los valores de 'tradicional_M'
for(col in cols_to_modify) {
  master_evaluado[[col]] <- ifelse(is.na(master_evaluado[[col]]), tradicional_M[[col]], master_evaluado[[col]])
}

# Paso 5: Calificación
master_evaluado <- master_evaluado %>%
  mutate(
    # Calificación para frentes_total
    score_frentes_total = case_when(
      frentes_total >= lower_bound_frentes & frentes_total <= upper_bound_frentes ~ 100,
      frentes_total >= (lower_bound_frentes - 0.1 * (upper_bound_frentes - lower_bound_frentes)) &
        frentes_total <= (upper_bound_frentes + 0.1 * (upper_bound_frentes - lower_bound_frentes)) ~ 50,
      TRUE ~ 0
    ),
    # Calificación para frentes_total
    score_frentes_arca = case_when(
      frentes_arca >= lower_bound_frentes_arca & frentes_arca <= upper_bound_frentes_arca ~ 100,
      frentes_arca >= (lower_bound_frentes_arca - 0.01 * (upper_bound_frentes_arca - lower_bound_frentes_arca)) &
        frentes_arca <= (upper_bound_frentes_arca + 0.01 * (upper_bound_frentes_arca - lower_bound_frentes_arca)) ~ 50,
      TRUE ~ 0
    ),
    # Calificación para duration
    score_duration = case_when(
      duration >= lower_bound_duration & duration <= upper_bound_duration ~ 100,
      duration >= (lower_bound_duration - 0.1 * (upper_bound_duration - lower_bound_duration)) &
        duration <= (upper_bound_duration + 0.1 * (upper_bound_duration - lower_bound_duration)) ~ 50,
      TRUE ~ 0
    ),
    # Calificación para enfriador_total
    score_enfriador_total = case_when(
      enfriador_total >= lower_bound_enfriadores & enfriador_total <= upper_bound_enfriadores ~ 100,
      enfriador_total >= (lower_bound_enfriadores - 0.1 * (upper_bound_enfriadores - lower_bound_enfriadores)) &
        enfriador_total <= (upper_bound_enfriadores + 0.1 * (upper_bound_enfriadores - lower_bound_enfriadores)) ~ 50,
      TRUE ~ 0
    ),
    # Calificación para Num.Scenes
    score_NumScenes = case_when(
      Total_Scenes >= lower_bound_NumScenes & Total_Scenes <= upper_bound_NumScenes ~ 100,
      Total_Scenes >= (lower_bound_NumScenes - 0.1 * (upper_bound_NumScenes - lower_bound_NumScenes)) &
        Total_Scenes <= (upper_bound_NumScenes + 0.1 * (upper_bound_NumScenes - lower_bound_NumScenes)) ~ 50,
      TRUE ~ 0
    ),
    # Calificación para Scenes_Amb
    score_ScenesAmb = case_when(
      Ambiente_Scenes >= lower_bound_ScenesAmb & Ambiente_Scenes <= upper_bound_ScenesAmb ~ 100,
      Ambiente_Scenes >= (lower_bound_ScenesAmb - 0.1 * (upper_bound_ScenesAmb - lower_bound_ScenesAmb)) &
        Ambiente_Scenes <= (upper_bound_ScenesAmb + 0.1 * (upper_bound_ScenesAmb - lower_bound_ScenesAmb)) ~ 50,
      TRUE ~ 0
    ),
    # Calificación para Scenes_Frio
    score_ScenesFrio = case_when(
      Frio_Scenes >= lower_bound_ScenesFrio & Frio_Scenes <= upper_bound_ScenesFrio ~ 100,
      Frio_Scenes >= (lower_bound_ScenesFrio - 0.1 * (upper_bound_ScenesFrio - lower_bound_ScenesFrio)) &
        Frio_Scenes <= (upper_bound_ScenesFrio + 0.1 * (upper_bound_ScenesFrio - lower_bound_ScenesFrio)) ~ 50,
      TRUE ~ 0
    )
  )

# Asegurándonos que SessionUID es del mismo tipo en ambos dataframes
df_session$SessionUID <- as.character(df_session$SessionUId)
master_evaluado$SessionUID <- as.character(master_evaluado$SessionUID)

# Uniendo las coordenadas de df_session a master_calidad
master_evaluado <- master_evaluado %>%
  left_join(df_session %>% select(SessionUID, latitude, longitude),
            by = "SessionUID")


#----------- GUARDAR DATA PROCESADA -----------
# Asegurarse de que 'Survey End Time' está en formato de fecha
master_evaluado$`Survey End Time` <- as.Date(master_evaluado$`Survey End Time`, format = "%Y-%m-%d")

# Convertir la columna 'detected_flags' a tipo caracter para evitar problemas al combinar dataframes
master_evaluado$detected_flags <- as.character(master_evaluado$detected_flags)

# Convertir 'Outlet Code' a tipo caracter en master_evaluado para evitar problemas al combinar
master_evaluado$`Outlet Code` <- as.character(master_evaluado$`Outlet Code`)

# Extraer mes y año de 'Survey End Time' para organizar los archivos
master_evaluado$mes_año <- format(master_evaluado$`Survey End Time`, "%Y-%m")

# Dividir los datos por mes y año, y guardar cada subconjunto en un archivo CSV correspondiente
unique_meses <- unique(master_evaluado$mes_año)

for(mes in unique_meses) {
  subset_datos <- filter(master_evaluado, mes_año == mes)
  
  # Convertir 'detected_flags' en subset_datos a caracter para mantener consistencia
  subset_datos$detected_flags <- as.character(subset_datos$detected_flags)
  
  # Crear el nombre del archivo basado en el mes y año
  archivo_nombre <- paste0("data_procesada/", mes, ".csv")
  
  # Si ya existe el archivo, leerlo, convertir 'detected_flags' a caracter y combinarlo con los datos nuevos antes de escribir de nuevo
  if (file.exists(archivo_nombre)) {
    datos_existentes <- read_csv(archivo_nombre)
    datos_existentes$detected_flags <- as.character(datos_existentes$detected_flags)
    datos_existentes$`Outlet Code` <- as.character(datos_existentes$`Outlet Code`)
    subset_datos <- bind_rows(datos_existentes, subset_datos) %>%
      distinct()  # Asegurar que no hay duplicados
  }
  
  # Guardar el subconjunto de datos en el archivo
  write_csv(subset_datos, archivo_nombre)
}

cat("Los datos han sido actualizados y guardados en archivos separados por mes y año en 'data_procesada/'.\n")

# ---------- LIMPIEZA DE DATA PROCESADA -------------

# Definir la carpeta donde se encuentran los datos
folder_path <- "data_procesada"

# Leer todos los archivos CSV en la carpeta
files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

# Función para procesar cada archivo
process_file <- function(file_path) {
  # Leer el archivo
  data <- read_csv(file_path)
  
  # Calcular la cantidad de NAs y 0s por fila
  data <- data %>%
    mutate(cantidad_na = rowSums(is.na(.)),
           cantidad_ceros = rowSums(. == 0, na.rm = TRUE))
  
  # Combinar las cantidades de NAs y 0s para tener un criterio de selección
  data <- data %>%
    mutate(cantidad_na_ceros = cantidad_na + cantidad_ceros)
  
  # Seleccionar la entrada con menos NA y 0 para cada SessionUID y eliminar los contadores
  data <- data %>%
    group_by(SessionUID) %>%
    arrange(cantidad_na_ceros) %>%
    slice(1) %>%
    ungroup() %>%
    select(-c(cantidad_na, cantidad_ceros, cantidad_na_ceros))
  
  # Guardar el archivo procesado
  write_csv(data, file_path)
}

# Aplicar la función a cada archivo
walk(files, process_file)

# Mensaje de confirmación
cat("Todos los archivos en la carpeta", folder_path, "han sido procesados y limpiados considerando valores NA y 0.\n")

# -----------EXTRACTOS-----------
# Sabana de Calidad Madre
# Ruta a la carpeta con los archivos procesados
ruta_carpeta <- "data_procesada/"

# Obtener lista de todos los archivos CSV en la carpeta
archivos <- dir_ls(ruta_carpeta, regexp = "\\.csv$")

# Función para leer un archivo CSV y asegurarse de que todas las columnas sean del mismo tipo
leer_y_convertir <- function(archivo) {
  df <- read_csv(archivo)
  # Convertir todas las columnas a character para evitar conflictos
  df[] <- lapply(df, as.character)
  return(df)
}

# Leer cada archivo, convertir columnas y combinarlos en un solo dataframe
master_calidad <- map_dfr(archivos, leer_y_convertir)

# Asumiendo que master_calidad es tu dataframe
master_calidad <- master_calidad %>%
  mutate(
    duration = as.numeric(duration),
    distance = as.numeric(distance),
    frentes_total = as.numeric(frentes_total),
    frentes_arca = as.numeric(frentes_arca),
    sovi = as.numeric(sovi),
    flag_trigger = as.numeric(flag_trigger),
    enfriador_total = as.numeric(enfriador_total),
    score_frentes_total = as.numeric(score_frentes_total),
    score_frentes_arca = as.numeric(score_frentes_arca),
    score_duration = as.numeric(score_duration),
    score_enfriador_total = as.numeric(score_enfriador_total),
    score_ScenesAmb = as.numeric(score_ScenesAmb),
    score_ScenesFrio = as.numeric(score_ScenesFrio),
    score_NumScenes = as.numeric(score_NumScenes),
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    estatus = as.numeric(estatus),
    lower_bound_frentes = as.numeric(lower_bound_frentes),
    upper_bound_frentes = as.numeric(upper_bound_frentes),
    lower_bound_frentes_arca = as.numeric(lower_bound_frentes_arca),
    upper_bound_frentes_arca = as.numeric(upper_bound_frentes_arca),
    lower_bound_enfriadores = as.numeric(lower_bound_enfriadores),
    upper_bound_enfriadores = as.numeric(upper_bound_enfriadores),
    lower_bound_duration = as.numeric(lower_bound_duration),
    upper_bound_duration = as.numeric(upper_bound_duration),
    lower_bound_ScenesAmb = as.numeric(lower_bound_ScenesAmb),
    upper_bound_ScenesAmb = as.numeric(upper_bound_ScenesAmb),
    lower_bound_ScenesFrio = as.numeric(lower_bound_ScenesFrio),
    upper_bound_ScenesFrio = as.numeric(upper_bound_ScenesFrio),
    lower_bound_NumScenes = as.numeric(lower_bound_NumScenes),
    upper_bound_NumScenes = as.numeric(upper_bound_NumScenes),
  )

# Guardar el dataframe combinado como un archivo Excel
write.xlsx(master_calidad, "master_calidad.xlsx")

# Mensaje de confirmación
cat("El archivo 'master_calidad.xlsx' ha sido guardado con éxito.\n")

## Sabana Maestra
# Path a data procesada
path_data_procesada <- "master_calidad.xlsx"

# Cargar la data procesada
data_procesada <- read_xlsx(path_data_procesada)

# Crear una carpeta para la data evaluada si no existe
path_data_evaluada <- "data_evaluada"
dir_create(path_data_evaluada)

# Parámetros con sus ponderaciones
ponderacion_tiempo <- 0.10
ponderacion_escenas_totales <- 0.04
ponderacion_escenas_ambiente <- 0.04
ponderacion_escenas_frio <- 0.04
ponderacion_coordenadas <- 0.11
ponderacion_flags <- 0.20
ponderacion_enfriadores <- 0.06
ponderacion_frentes_arca <- 0.10
ponderacion_frentes <- 0.09
ponderacion_sovi <- 0.22

# Funciones auxiliares para cálculos específicos
calcular_calificacion_coordenadas <- function(distancia) {
  if (is.na(distancia)) return(0)
  if (distancia <= 75) return(100)
  max(50 - 10 * floor((distancia - 75 - 100) / 100), 0)
}

calcular_calificacion_sovi <- function(tradechannelcode, sovi) {
  # Comprueba si tradechannelcode es "Comer y Beber"
  if (!is.na(tradechannelcode) && tradechannelcode == "Comer y Beber") {
    return(100)
  }
  
  # Continúa con la lógica original para calcular la calificación basada en sovi
  if (is.na(sovi)) {
    return(0)
  }
  if (sovi <= 0.73) {
    return(100)
  }
  return(max(100 - 10 * floor((sovi - 0.73) / 0.03), 0))
}

# Aplicar las funciones auxiliares a las columnas relevantes
data_procesada <- data_procesada %>%
  mutate(
    calificacion_coordenadas = map_dbl(distance, calcular_calificacion_coordenadas),
    calificacion_sovi = pmap_dbl(list(tradechannelcode, sovi), calcular_calificacion_sovi)
  )

# Calcular ICD
data_procesada <- data_procesada %>%
  mutate(
    # Calcular ICD considerando NA como 0 solo en la suma final por variable
    ICD = pmax(0,
               (if_else(is.na(score_duration), 0, score_duration) * ponderacion_tiempo) +
                 (if_else(is.na(score_NumScenes), 0, score_NumScenes) * ponderacion_escenas_totales) +
                 (if_else(is.na(score_ScenesAmb), 0, score_ScenesAmb) * ponderacion_escenas_ambiente) +
                 (if_else(is.na(score_ScenesFrio), 0, score_ScenesFrio) * ponderacion_escenas_frio) +
                 (if_else(is.na(calificacion_coordenadas), 0, calificacion_coordenadas) * ponderacion_coordenadas) +
                 (if_else(is.na(flag_trigger), 0, flag_trigger) * ponderacion_flags) +
                 (if_else(is.na(score_enfriador_total), 0, score_enfriador_total) * ponderacion_enfriadores) +
                 (if_else(is.na(score_frentes_arca), 0, score_frentes_arca) * ponderacion_frentes_arca) +
                 (if_else(is.na(score_frentes_total), 0, score_frentes_total) * ponderacion_frentes) +
                 (if_else(is.na(calificacion_sovi), 0, calificacion_sovi) * ponderacion_sovi)
    ),
    ICD = round(ICD, 1)  # Redondeo a 1 decimal
  )

# Asegurarse de que Survey End Time es de tipo fecha
data_procesada$`Survey End Time` <- as.Date(data_procesada$`Survey End Time`)

# Calcular la diferencia en días desde la fecha de la encuesta hasta hoy
data_procesada <- data_procesada %>%
  mutate(
    dias_desde_encuesta = as.numeric(Sys.Date() - `Survey End Time`)
  )

# Calcular Aprobación con la nueva condición
data_procesada <- data_procesada %>%
  mutate(
    dias_desde_encuesta = as.numeric(Sys.Date() - as.Date(`Survey End Time`, format = "%Y-%m-%d")), # Asegurar que `Survey End Time` se trate como fecha
    NA_count = rowSums(is.na(select(., duration, distance, frentes_total, frentes_arca, sovi, enfriador_total, Total_Scenes, Ambiente_Scenes, Frio_Scenes))),
    Aprobacion = case_when(
      flag_trigger == -100 & ICD < 70 ~ "Reprobado",
      flag_trigger == -100 ~ "Aprobado",
      NA_count > 4 & dias_desde_encuesta <= 14 ~ "DF",  # Usa dias_desde_encuesta para aplicar la regla de DF solo si es dentro de los últimos 14 días
      ICD < 70 ~ "Reprobado",
      TRUE ~ "Aprobado"
    )
  ) %>%
  select(-NA_count, -dias_desde_encuesta)  # Eliminar columnas temporales después de su uso

# Calcular Notas
data_procesada <- data_procesada %>%
  mutate(
    Notas = case_when(
      Aprobacion == "DF" ~ "En Procesamiento",
      Aprobacion == "Aprobado" ~ "",
      TRUE ~ paste0(
        if_else(score_duration <= 50, "Tiempo fuera de rango, ", ""),
        if_else(calificacion_coordenadas <= 50, "Coordenadas: lejos de PDV, ", ""),
        if_else(flag_trigger != 100, "Problemas con las flags, ", ""),
        if_else(is.na(detected_flags), "",  # Evita agregar texto si detected_flags es NA
                paste0(
                  if_else(str_detect(detected_flags, "13"), "Foto de foto, ", ""),
                  if_else(str_detect(detected_flags, "14"), "Tipo de escena incorrecto, ", ""),
                  if_else(str_detect(detected_flags, "15"), "Mal ángulo, ", ""),
                  if_else(str_detect(detected_flags, "16"), "Imagen borrosa, ", ""),
                  if_else(str_detect(detected_flags, "2"), "Parte del cuerpo obstruyendo, ", ""),
                  if_else(str_detect(detected_flags, "4"), "Muy oscuro, ", ""),
                  if_else(str_detect(detected_flags, "26"), "Múltiples POCs desconectados en una escena, ", ""),
                  if_else(str_detect(detected_flags, "88"), "Frentes Insuficientes, ", ""),
                  if_else(str_detect(detected_flags, "27"), "Puerta cerrada, ", ""),
                  if_else(str_detect(detected_flags, "32"), "Imagen de objetos (No enfriadores ni frentes), ", ""),
                  if_else(str_detect(detected_flags, "61"), "Foto sin Frentes, ", ""),
                  if_else(str_detect(detected_flags, "36"), "Imagen duplicada, ", ""),
                  if_else(str_detect(detected_flags, "39"), "Objetos obstaculizando, ", ""),
                  if_else(str_detect(detected_flags, "66"), "Sesión con 0 productos (frentes) en imágenes, ", "")
                )),
        if_else(score_enfriador_total <= 50, "Enfriadores en PDV no coincide con Comodato, ", ""),
        if_else(score_NumScenes <= 50, "Número de fotografías fuera de rango, ", ""),
        if_else(score_ScenesAmb <= 50, "Número de fotos de ambiente fuera de cantidad estándar, ", ""),
        if_else(score_ScenesFrio <= 50, "Número de fotos de frío fuera de cantidad estándar, ", ""),
        if_else(score_frentes_total <= 50, "Cantidad de frentes fuera de rango, ", ""),
        if_else(calificacion_sovi <= 50, "Falta fotografiar competencia", ""),
        sep = ""
      )
    )
  )

# Aprobar automáticamente si es un "AUD"
data_procesada <- data_procesada %>%
  mutate(
    Aprobacion = ifelse(substr(User, 1, 3) == "AUD", "Aprobado", Aprobacion),
    Notas = ifelse(substr(User, 1, 3) == "AUD", "Aprobado automáticamente por ser auditoría", Notas)
  )

# Seleccion de Variables
data_evaluada <- data_procesada %>%
  select(
    SessionUID, SurveyType, User, `Outlet Code`, salesorganizationcode, tamaño, tradechannelcode, 
    sub_canal_isscom, duration, distance, frentes_total, frentes_arca, sovi, flag_trigger, 
    enfriador_total, Total_Scenes, Ambiente_Scenes, Frio_Scenes, detected_flags, 
    modelo_de_servicio_ruta, salesterritorycode, territorio, 
    `Survey End Time`, ICD, estatus, Aprobacion, Notas
  )

# Guardar los resultados
write_csv(data_evaluada, file.path(path_data_evaluada, "mexico_icd3.csv"))