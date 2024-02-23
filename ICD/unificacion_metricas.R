#----------- UNIFICAR DATA DE METRICAS -----------

# Especifica la ruta de la carpeta donde se encuentran los archivos
ruta_carpeta <- "metricas_procesada/"
nombres_archivos <- list.files(path = ruta_carpeta, pattern = "*.csv", full.names = TRUE)

# Función para leer y transformar cada archivo CSV
leer_y_transformar <- function(nombre_archivo) {
  df <- read_csv(nombre_archivo)

  # Convertir la columna 'Outlet_Code' a character si existe
  if("Outlet_Code" %in% names(df)) {
    df$Outlet_Code <- as.character(df$Outlet_Code)
  }

  return(df)
}

# Crear una lista con los dataframes importados y transformados
lista_dataframes <- lapply(nombres_archivos, leer_y_transformar)

# Combinar todos los dataframes en uno
dataframe_combinado <- bind_rows(lista_dataframes)

# Guardar el dataframe combinado en un nuevo archivo
write_csv(dataframe_combinado, paste0(ruta_carpeta, "analitica_censo.csv"))
