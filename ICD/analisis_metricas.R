## Llamar Librerias
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)
library(purrr)
library(tidyr)
library(tidyverse)

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
df_scenes <- combine_files("scenes/")

## Crear df sku
# Asumiendo que df_actual ya está en el environment
# Filtrado de df_actual
df_actual_filtrado <- df_actual %>%
  filter(!str_detect(`IsEmpty`, "TRUE") & !str_detect(`ProductName`, "Foreign"))

# Creación de df_sku
df_sku <- df_actual_filtrado %>%
  group_by(SessionUID, SceneUID, SKU) %>%
  summarise(Total = n(), .groups = 'drop')

## Importar Manual Questions
# Lista todos los archivos en la carpeta fuentes_datos/sku_master
archivos <- list.files(path = "fuentes_datos/manual_questions", full.names = TRUE)

# Define una función para leer un archivo individual
leer_archivo <- function(archivo) {
  extension <- tools::file_ext(archivo)
  df <- switch(extension,
               xlsx = read_excel(archivo),
               XLSX = read_excel(archivo),
               csv = read_csv(archivo),
               stop("Formato de archivo no soportado"))
  # Convertir la columna 'Text' a tipo character si existe
  if ("Text" %in% names(df)) {
    df$Text <- as.character(df$Text)
  }
  # Convertir la columna 'CreatedOnTime' a tipo character si existe
  if ("CreatedOnTime" %in% names(df)) {
    df$CreatedOnTime <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("LastModifiedTime" %in% names(df)) {
    df$LastModifiedTime <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("FileCreatedTime" %in% names(df)) {
    df$FileCreatedTime <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("SliceStartTime" %in% names(df)) {
    df$SliceStartTime <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("SliceEndTime" %in% names(df)) {
    df$SliceEndTime <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("ReExportTime" %in% names(df)) {
    df$ReExportTime <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("ParentQuestionID" %in% names(df)) {
    df$ParentQuestionID <- as.character(df$Text)
  }
  # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("ReExportStatus" %in% names(df)) {
    df$ReExportStatus <- as.character(df$Text)
  }
   # Convertir la columna 'LastModifiedTime' a tipo character si existe
  if ("ReProcessedStatus" %in% names(df)) {
    df$ReProcessedStatus <- as.character(df$Text)
  }
  if("ReProcessedTime" %in% names(df)) {
    df$ReProcessedTime <- as.character(df$ReProcessedTime)
  }
  return(df)
}

# Lee y combina todos los archivos en la carpeta
df_mq <- archivos %>%
  map_dfr(leer_archivo)

## Importar Master Productos
# Lista todos los archivos en la carpeta fuentes_datos/sku_master
archivos <- list.files(path = "fuentes_datos/sku_master", full.names = TRUE)

# Define una función para leer un archivo individual
leer_archivo <- function(archivo) {
  extension <- tools::file_ext(archivo)
  switch(extension,
         xlsx = read_excel(archivo),
         XLSX = read_excel(archivo),
         csv = read_csv(archivo),
         stop("Formato de archivo no soportado")
  )
}

# Lee y combina todos los archivos en la carpeta
df_fmprs <- archivos %>%
  map_dfr(leer_archivo)

## Importar de master_icd
# Importación del archivo Excel
master_icd <- read_excel("ICD - Indice de Calidad de Data.xlsx", sheet = "Export")

## Creación de df_metricas
# Número de filas en df_sku
n_rows <- nrow(df_sku)

# Dividir df_sku en 3 partes
df_sku_part1 <- df_sku[1:floor(n_rows / 3), ]
df_sku_part2 <- df_sku[(floor(n_rows / 3) + 1):floor(2 * n_rows / 3), ]
df_sku_part3 <- df_sku[(floor(2 * n_rows / 3) + 1):n_rows, ]

process_part <- function(df_part) {
  df_part %>%
    left_join(master_icd, by = "SessionUID") %>%
    left_join(select(df_scenes, SceneUID, SubSceneType), by = "SceneUID") %>%
    left_join(df_fmprs, by = c("SKU" = "SKU ID")) %>%
    filter(ICD >= 70) %>%
    mutate(ruta = if_else(
      str_sub(User, 1, 4) == "PVTA",
      str_sub(User, -6, -1),
      str_sub(User, -5, -1)
    )) %>%
    select(SessionUID,
           User,
           ruta,  # Incluir la nueva columna ruta
           Survey_End_Time = `Survey End Time`,
           SceneUID,
           SubSceneType,
           SKU,
           Total,
           ICD,
           Outlet_Code = `Outlet Code`,
           Sales_Organization_Code = `salesorganizationcode`,
           territorio,
           salesterritorycode,
           Trade_Channel_Code = `tradechannelcode`,
           sub_canal_isscom,
           tamaño,
           modelo_de_servicio_ruta,
           Is_Foreign_Product = `Is Foreign Product?`,
           Manufacturer,
           Sub_Brand = `Sub Brand`,
           Normalize_Size = `Normalize Size`,
           Package_Type = `Package Type`,
           Product_Group = `Product Group`,
           Local_Product_Category = `Local Product Category`)
}

# Aplicar la función a cada parte
df_metricas_part1 <- process_part(df_sku_part1)
df_metricas_part2 <- process_part(df_sku_part2)
df_metricas_part3 <- process_part(df_sku_part3)

# Combinar las partes procesadas
df_metricas <- bind_rows(df_metricas_part1, df_metricas_part2, df_metricas_part3)


# Manual Questions dataframe
# Define las preguntas de interés
preguntas_interes <- c(
  "¿El cliente es enrejado?",
  "Foto Evidencia Punto de Venta",
  "¿Tiene exhibidor de Bidón de Competencia (5L-11L)?",
  "Cantidad de Puertas AC Ejecutadas Bebidas",
  "Cantidad de Puertas AC Cedidas",
  "Cantidad de Puertas PEPSI",
  "Cantidad de puertas BONAFONT",
  "Cantidad de puertas resto Competencia",
  "Cantidad de puertas CERVEZA (Heineken, Tecate, etc.)"
)

# Filtrar solo las preguntas de interés
df_filtrado <- df_mq %>% 
  filter(LocalQuestionText %in% preguntas_interes) %>%
  select(SessionUID, LocalQuestionText, AnswerValue)  # Seleccionar solo las columnas necesarias

# Reestructurar el dataframe
df_reestructurado <- df_filtrado %>%
  group_by(SessionUID, LocalQuestionText) %>%  # Agrupar por SessionUID y LocalQuestionText
  summarise(AnswerValue = first(AnswerValue, order_by = NULL), .groups = 'drop') %>%  # Tomar el primer valor de AnswerValue para cada grupo
  pivot_wider(names_from = LocalQuestionText, 
              values_from = AnswerValue)  # Usar pivot_wider para reestructurar los datos

# Listar todos los archivos CSV en la carpeta 'metricas_procesada'
archivos_csv <- list.files(path = "metricas_procesada", full.names = TRUE, pattern = "\\.csv$")

# Leer cada archivo CSV y asegurarse de que las columnas son del mismo tipo
df_lista <- lapply(archivos_csv, function(csv) {
  df_temp <- read_csv(csv, col_types = cols(
    .default = col_guess(),
    Survey_End_Time = col_date(format = "%Y-%m-%d"),
    Outlet_Code = col_character()
  ))
  
  # Calcular 'MesAno' basado en 'Survey_End_Time' si no existe
  if (!"MesAno" %in% names(df_temp)) {
    df_temp$MesAno <- format(df_temp$Survey_End_Time, "%Y-%m")
  }
  
  return(df_temp)
})

# Combinar todos los DataFrames en uno solo
df_combinado <- do.call(rbind, df_lista)

# Asegurarse de que 'df_metricas' tiene las columnas del mismo tipo
if ("Survey_End_Time" %in% names(df_metricas)) {
  df_metricas$Survey_End_Time <- as.Date(df_metricas$Survey_End_Time, format = "%Y-%m-%d")
}
if ("Outlet_Code" %in% names(df_metricas)) {
  df_metricas$Outlet_Code <- as.character(df_metricas$Outlet_Code)
}
if (!"MesAno" %in% names(df_metricas)) {
  df_metricas$MesAno <- format(df_metricas$Survey_End_Time, "%Y-%m")
}

# Combinar con df_metricas
df_metricas_combinadas <- bind_rows(df_combinado, df_metricas)

# Eliminar registros duplicados
df_metricas_unicas <- df_metricas_combinadas %>% distinct()

## Guardar Metricas Combinadas en archivos csv por mes
# Primero, asegurarse de que el directorio existe o crearlo
output_folder <- "metricas_procesada"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Dividir df_metricas_unicas por la columna MesAno y escribir cada partición en un archivo CSV
df_metricas_unicas %>%
  group_by(MesAno) %>%
  group_walk(~ write.csv(.x, file = paste0(output_folder, "/", .y$MesAno, ".csv"), row.names = FALSE))


# -----------LIMPIEZA-----------
# Evitar duplicados dentro de data mensual

# Definir la ruta de la carpeta donde se encuentran los archivos
folder_path <- "metricas_procesada"

# Listar todos los archivos CSV en la carpeta
archivos_csv <- list.files(path = folder_path, full.names = TRUE, pattern = "\\.csv$")

# Función para leer, eliminar duplicados y escribir cada archivo
process_file <- function(file_path) {
  # Leer el archivo CSV
  data <- read.csv(file_path)
  
  # Eliminar duplicados
  data_unique <- distinct(data)
  
  # Sobreescribir el archivo CSV con la versión sin duplicados
  write.csv(data_unique, file_path, row.names = FALSE)
}

# Aplicar la función a cada archivo CSV en la carpeta
lapply(archivos_csv, process_file)


# -----------EXTRACTOS-----------
# Sabana de Metricas FMPR
library(lubridate)
library(openxlsx)
library(dplyr)

# Función para dividir el dataframe en pestañas por mes, eliminar duplicados y guardar cada pestaña en un archivo Excel
guardar_por_mes <- function(df, date_column = "Survey_End_Time", path = "metricas_por_mes.xlsx") {
  # Convertir la columna de fecha a tipo fecha
  df[[date_column]] <- as.Date(df[[date_column]])
  
  # Crear una nueva columna con el mes y el año
  df$MesAno <- format(df[[date_column]], "%Y-%m")
  
  # Eliminar duplicados
  df <- df %>% distinct()
  
  # Dividir el dataframe por la nueva columna MesAno
  lista_meses <- split(df, df$MesAno)
  
  # Crear un nuevo libro de trabajo de Excel
  wb <- createWorkbook()
  
  # Iterar sobre la lista de dataframes divididos y escribir cada uno en una hoja diferente
  for (mes in names(lista_meses)) {
    addWorksheet(wb, sheetName = mes)
    writeData(wb, sheet = mes, x = lista_meses[[mes]])
  }
  
  # Guardar el libro de trabajo
  saveWorkbook(wb, file = path, overwrite = TRUE)
}

# Uso de la función
guardar_por_mes(df_metricas_unicas)


# Sabana de Manual Questions
# Guardar archivo
write.xlsx(df_reestructurado, "manual_questions.xlsx")


# -----------ELIMINAR DATA ACTUAL-----------
# Función para limpiar directorio de archivos individuales, manteniendo solo el combinado
clean_directory <- function(path) {
  combined_file_path <- file.path(path, "combined.csv")
  
  # Listar todos los archivos excepto el archivo combinado
  files_to_delete <- setdiff(list.files(path, full.names = TRUE), combined_file_path)
  
  # Eliminar archivos
  file.remove(files_to_delete)
}

# Limpiar directorios de archivos individuales
clean_directory("fuentes_datos/actual/")
clean_directory("fuentes_datos/scenes/")
