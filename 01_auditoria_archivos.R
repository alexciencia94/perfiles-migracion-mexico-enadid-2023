# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 01: Auditoría inicial de archivos
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

# ------------------------------------------------------------------------------
# 1. Verificar el directorio del proyecto
# ------------------------------------------------------------------------------

cat("Directorio del proyecto:\n")
cat(getwd(), "\n\n")

# ------------------------------------------------------------------------------
# 2. Definir rutas
# ------------------------------------------------------------------------------

ruta_zip <- file.path(
  "01_datos_originales",
  "base_datos_enadid23_csv.zip"
)

ruta_extraccion <- file.path(
  "01_datos_originales",
  "enadid23_csv"
)

ruta_resultados <- "09_documentacion"

# ------------------------------------------------------------------------------
# 3. Comprobar que el ZIP existe
# ------------------------------------------------------------------------------

if (!file.exists(ruta_zip)) {
  stop(
    paste0(
      "No se encontró el archivo:\n",
      normalizePath(
        ruta_zip,
        winslash = "/",
        mustWork = FALSE
      )
    )
  )
}

cat("Archivo ZIP localizado correctamente:\n")
cat(normalizePath(ruta_zip, winslash = "/"), "\n\n")

# ------------------------------------------------------------------------------
# 4. Listar el contenido del ZIP
# ------------------------------------------------------------------------------

contenido_zip <- unzip(
  zipfile = ruta_zip,
  list = TRUE
)

print(contenido_zip)

write.csv(
  contenido_zip,
  file = file.path(
    ruta_resultados,
    "contenido_zip_enadid2023.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 5. Archivos esperados
# ------------------------------------------------------------------------------

archivos_necesarios <- c(
  "TMIGRANTE.csv",
  "THOGAR.csv",
  "TSDEM.csv",
  "TVIVIENDA.csv"
)

nombres_zip <- basename(contenido_zip$Name)

archivos_faltantes <- setdiff(
  archivos_necesarios,
  nombres_zip
)

if (length(archivos_faltantes) > 0) {
  stop(
    paste(
      "Faltan los siguientes archivos:",
      paste(archivos_faltantes, collapse = ", ")
    )
  )
}

cat("Las cuatro tablas necesarias están presentes.\n\n")

# ------------------------------------------------------------------------------
# 6. Extraer las cuatro tablas necesarias
# ------------------------------------------------------------------------------

dir.create(
  ruta_extraccion,
  recursive = TRUE,
  showWarnings = FALSE
)

# Identificar la ruta interna de cada archivo dentro del ZIP
rutas_internas <- contenido_zip$Name[
  basename(contenido_zip$Name) %in% archivos_necesarios
]

unzip(
  zipfile = ruta_zip,
  files = rutas_internas,
  exdir = ruta_extraccion,
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# 7. Localizar los CSV extraídos
# ------------------------------------------------------------------------------

csv_extraidos <- list.files(
  path = ruta_extraccion,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

tabla_archivos <- data.frame(
  archivo = basename(csv_extraidos),
  ruta = normalizePath(
    csv_extraidos,
    winslash = "/",
    mustWork = TRUE
  ),
  tamano_mb = round(
    file.info(csv_extraidos)$size / 1024^2,
    2
  )
)

print(tabla_archivos)

write.csv(
  tabla_archivos,
  file = file.path(
    ruta_resultados,
    "archivos_extraidos_enadid2023.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 8. Función para encontrar cada archivo
# ------------------------------------------------------------------------------

buscar_archivo <- function(nombre) {
  
  coincidencias <- csv_extraidos[
    toupper(basename(csv_extraidos)) == toupper(nombre)
  ]
  
  if (length(coincidencias) == 0) {
    stop("No se encontró el archivo extraído: ", nombre)
  }
  
  if (length(coincidencias) > 1) {
    stop("Se encontraron varias copias de: ", nombre)
  }
  
  coincidencias
}

ruta_tmigrante <- buscar_archivo("TMIGRANTE.csv")
ruta_thogar <- buscar_archivo("THOGAR.csv")
ruta_tsdem <- buscar_archivo("TSDEM.csv")
ruta_tvivienda <- buscar_archivo("TVIVIENDA.csv")

# ------------------------------------------------------------------------------
# 9. Leer solamente los encabezados
# ------------------------------------------------------------------------------

encabezados <- list(
  TMIGRANTE = names(
    read.csv(
      ruta_tmigrante,
      nrows = 0,
      check.names = FALSE
    )
  ),
  THOGAR = names(
    read.csv(
      ruta_thogar,
      nrows = 0,
      check.names = FALSE
    )
  ),
  TSDEM = names(
    read.csv(
      ruta_tsdem,
      nrows = 0,
      check.names = FALSE
    )
  ),
  TVIVIENDA = names(
    read.csv(
      ruta_tvivienda,
      nrows = 0,
      check.names = FALSE
    )
  )
)

cat("\nNúmero de variables por tabla:\n")

print(
  sapply(
    encabezados,
    length
  )
)

# ------------------------------------------------------------------------------
# 10. Contar filas sin cargar las tablas completas
# ------------------------------------------------------------------------------

contar_filas <- function(ruta) {
  
  conexion <- file(
    ruta,
    open = "r",
    encoding = "UTF-8"
  )
  
  on.exit(close(conexion))
  
  contador <- 0L
  
  repeat {
    
    bloque <- readLines(
      conexion,
      n = 100000,
      warn = FALSE
    )
    
    if (length(bloque) == 0) {
      break
    }
    
    contador <- contador + length(bloque)
  }
  
  # Se resta la fila de encabezados
  contador - 1L
}

dimensiones <- data.frame(
  tabla = c(
    "TMIGRANTE",
    "THOGAR",
    "TSDEM",
    "TVIVIENDA"
  ),
  filas = c(
    contar_filas(ruta_tmigrante),
    contar_filas(ruta_thogar),
    contar_filas(ruta_tsdem),
    contar_filas(ruta_tvivienda)
  ),
  columnas = c(
    length(encabezados$TMIGRANTE),
    length(encabezados$THOGAR),
    length(encabezados$TSDEM),
    length(encabezados$TVIVIENDA)
  )
)

print(dimensiones)

# ------------------------------------------------------------------------------
# 11. Validar dimensiones esperadas
# ------------------------------------------------------------------------------

dimensiones_esperadas <- data.frame(
  tabla = c(
    "TMIGRANTE",
    "THOGAR",
    "TSDEM",
    "TVIVIENDA"
  ),
  filas_esperadas = c(
    3660,
    108894,
    359018,
    106749
  ),
  columnas_esperadas = c(
    52,
    26,
    103,
    50
  )
)

auditoria <- merge(
  dimensiones,
  dimensiones_esperadas,
  by = "tabla",
  sort = FALSE
)

auditoria$filas_correctas <- (
  auditoria$filas == auditoria$filas_esperadas
)

auditoria$columnas_correctas <- (
  auditoria$columnas == auditoria$columnas_esperadas
)

print(auditoria)

write.csv(
  auditoria,
  file = file.path(
    ruta_resultados,
    "auditoria_dimensiones_enadid2023.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 12. Resultado final
# ------------------------------------------------------------------------------

if (
  all(auditoria$filas_correctas) &&
  all(auditoria$columnas_correctas)
) {
  
  cat("\n")
  cat("============================================================\n")
  cat("AUDITORÍA TERMINADA CORRECTAMENTE\n")
  cat("Las cuatro tablas tienen las dimensiones esperadas.\n")
  cat("============================================================\n")
  
} else {
  
  stop(
    paste0(
      "La auditoría detectó diferencias en las dimensiones. ",
      "Revise 09_documentacion/auditoria_dimensiones_enadid2023.csv"
    )
  )
}
