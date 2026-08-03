# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 02: Importación y validación de llaves
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

# ------------------------------------------------------------------------------
# 1. Verificar paquetes
# ------------------------------------------------------------------------------

paquetes <- c(
  "readr",
  "dplyr",
  "janitor",
  "tibble"
)

faltantes <- paquetes[
  !vapply(
    paquetes,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(faltantes) > 0) {
  
  stop(
    paste0(
      "Faltan estos paquetes: ",
      paste(faltantes, collapse = ", "),
      "\nInstálelos primero con renv::install()."
    )
  )
}

library(readr)
library(dplyr)
library(janitor)
library(tibble)

# ------------------------------------------------------------------------------
# 2. Definir rutas
# ------------------------------------------------------------------------------

ruta_extraidos <- file.path(
  "01_datos_originales",
  "enadid23_csv"
)

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_documentacion <- "09_documentacion"

dir.create(
  ruta_procesados,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  ruta_tablas,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------------------------
# 3. Localizar archivos
# ------------------------------------------------------------------------------

localizar_csv <- function(nombre_archivo) {
  
  coincidencias <- list.files(
    path = ruta_extraidos,
    pattern = paste0(
      "^",
      nombre_archivo,
      "$"
    ),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(coincidencias) == 0) {
    stop("No se encontró: ", nombre_archivo)
  }
  
  if (length(coincidencias) > 1) {
    stop(
      "Se encontraron varias copias de: ",
      nombre_archivo
    )
  }
  
  coincidencias
}

ruta_tmigrante <- localizar_csv("TMIGRANTE.csv")
ruta_thogar <- localizar_csv("THOGAR.csv")
ruta_tsdem <- localizar_csv("TSDEM.csv")
ruta_tvivienda <- localizar_csv("TVIVIENDA.csv")

# ------------------------------------------------------------------------------
# 4. Función de lectura
# ------------------------------------------------------------------------------

leer_enadid <- function(ruta) {
  
  read_csv(
    file = ruta,
    col_types = cols(
      .default = col_character()
    ),
    na = character(),
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    clean_names()
}

cat("Importando TMIGRANTE...\n")
tmigrante <- leer_enadid(ruta_tmigrante)

cat("Importando THOGAR...\n")
thogar <- leer_enadid(ruta_thogar)

cat("Importando TSDEM...\n")
tsdem <- leer_enadid(ruta_tsdem)

cat("Importando TVIVIENDA...\n")
tvivienda <- leer_enadid(ruta_tvivienda)

cat("Importación terminada.\n\n")

# ------------------------------------------------------------------------------
# 5. Validar dimensiones
# ------------------------------------------------------------------------------

dimensiones <- tibble(
  tabla = c(
    "TMIGRANTE",
    "THOGAR",
    "TSDEM",
    "TVIVIENDA"
  ),
  filas = c(
    nrow(tmigrante),
    nrow(thogar),
    nrow(tsdem),
    nrow(tvivienda)
  ),
  columnas = c(
    ncol(tmigrante),
    ncol(thogar),
    ncol(tsdem),
    ncol(tvivienda)
  )
)

print(dimensiones)

dimensiones_esperadas <- tibble(
  tabla = c(
    "TMIGRANTE",
    "THOGAR",
    "TSDEM",
    "TVIVIENDA"
  ),
  filas_esperadas = c(
    3660L,
    108894L,
    359018L,
    106749L
  ),
  columnas_esperadas = c(
    52L,
    26L,
    103L,
    50L
  )
)

validacion_dimensiones <- dimensiones |>
  left_join(
    dimensiones_esperadas,
    by = "tabla"
  ) |>
  mutate(
    filas_correctas = filas == filas_esperadas,
    columnas_correctas = columnas == columnas_esperadas
  )

if (
  !all(validacion_dimensiones$filas_correctas) ||
  !all(validacion_dimensiones$columnas_correctas)
) {
  
  stop(
    "Las dimensiones no coinciden con la auditoría inicial."
  )
}

# ------------------------------------------------------------------------------
# 6. Verificar variables llave
# ------------------------------------------------------------------------------

llaves_requeridas <- list(
  TMIGRANTE = c(
    "llave_mig",
    "llave_hog",
    "llave_viv",
    "llave_per"
  ),
  THOGAR = c(
    "llave_hog"
  ),
  TSDEM = c(
    "llave_per"
  ),
  TVIVIENDA = c(
    "llave_viv"
  )
)

validar_variables <- function(datos, variables, tabla) {
  
  ausentes <- setdiff(
    variables,
    names(datos)
  )
  
  if (length(ausentes) > 0) {
    
    stop(
      paste0(
        "En ",
        tabla,
        " faltan las variables: ",
        paste(ausentes, collapse = ", ")
      )
    )
  }
}

validar_variables(
  tmigrante,
  llaves_requeridas$TMIGRANTE,
  "TMIGRANTE"
)

validar_variables(
  thogar,
  llaves_requeridas$THOGAR,
  "THOGAR"
)

validar_variables(
  tsdem,
  llaves_requeridas$TSDEM,
  "TSDEM"
)

validar_variables(
  tvivienda,
  llaves_requeridas$TVIVIENDA,
  "TVIVIENDA"
)

# ------------------------------------------------------------------------------
# 7. Función de auditoría de llaves
# ------------------------------------------------------------------------------

auditar_llave <- function(datos, tabla, llave) {
  
  valores <- datos[[llave]]
  
  tibble(
    tabla = tabla,
    llave = llave,
    registros = nrow(datos),
    valores_vacios = sum(
      is.na(valores) |
        trimws(valores) == ""
    ),
    valores_unicos = n_distinct(
      valores[
        !is.na(valores) &
          trimws(valores) != ""
      ]
    ),
    registros_duplicados = sum(
      duplicated(valores) |
        duplicated(
          valores,
          fromLast = TRUE
        )
    )
  )
}

auditoria_llaves <- bind_rows(
  auditar_llave(
    tmigrante,
    "TMIGRANTE",
    "llave_mig"
  ),
  auditar_llave(
    thogar,
    "THOGAR",
    "llave_hog"
  ),
  auditar_llave(
    tsdem,
    "TSDEM",
    "llave_per"
  ),
  auditar_llave(
    tvivienda,
    "TVIVIENDA",
    "llave_viv"
  )
)

print(auditoria_llaves)

write_csv(
  auditoria_llaves,
  file.path(
    ruta_tablas,
    "auditoria_llaves.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Validar unicidad de llaves principales
# ------------------------------------------------------------------------------

validar_unicidad <- function(datos, llave, tabla) {
  
  llave_no_vacia <- datos[[llave]][
    !is.na(datos[[llave]]) &
      trimws(datos[[llave]]) != ""
  ]
  
  if (
    n_distinct(llave_no_vacia) !=
    length(llave_no_vacia)
  ) {
    
    stop(
      paste0(
        "La llave ",
        llave,
        " no es única en ",
        tabla,
        "."
      )
    )
  }
}

validar_unicidad(
  tmigrante,
  "llave_mig",
  "TMIGRANTE"
)

validar_unicidad(
  thogar,
  "llave_hog",
  "THOGAR"
)

validar_unicidad(
  tsdem,
  "llave_per",
  "TSDEM"
)

validar_unicidad(
  tvivienda,
  "llave_viv",
  "TVIVIENDA"
)

# ------------------------------------------------------------------------------
# 9. Validar cobertura de enlaces
# ------------------------------------------------------------------------------

enlace_hogar <- tmigrante$llave_hog %in%
  thogar$llave_hog

enlace_vivienda <- tmigrante$llave_viv %in%
  tvivienda$llave_viv

llaves_personales_disponibles <- tmigrante$llave_per[
  !is.na(tmigrante$llave_per) &
    trimws(tmigrante$llave_per) != ""
]

enlace_persona <- llaves_personales_disponibles %in%
  tsdem$llave_per

auditoria_enlaces <- tibble(
  enlace = c(
    "TMIGRANTE a THOGAR",
    "TMIGRANTE a TVIVIENDA",
    "TMIGRANTE a TSDEM, llaves personales disponibles"
  ),
  elegibles = c(
    length(enlace_hogar),
    length(enlace_vivienda),
    length(enlace_persona)
  ),
  enlazados = c(
    sum(enlace_hogar),
    sum(enlace_vivienda),
    sum(enlace_persona)
  )
) |>
  mutate(
    proporcion_enlazada = enlazados / elegibles
  )

print(auditoria_enlaces)

write_csv(
  auditoria_enlaces,
  file.path(
    ruta_tablas,
    "auditoria_enlaces.csv"
  )
)

if (!all(auditoria_enlaces$proporcion_enlazada == 1)) {
  
  stop(
    "Se detectaron registros sin correspondencia entre tablas."
  )
}

# ------------------------------------------------------------------------------
# 10. Revisar variables esenciales del diseño muestral
# ------------------------------------------------------------------------------

variables_diseno <- c(
  "fac_hog",
  "est_dis",
  "upm_dis"
)

validar_variables(
  tmigrante,
  variables_diseno,
  "TMIGRANTE"
)

auditoria_diseno <- tibble(
  variable = variables_diseno,
  faltantes = sapply(
    variables_diseno,
    function(variable) {
      
      valores <- tmigrante[[variable]]
      
      sum(
        is.na(valores) |
          trimws(valores) == ""
      )
    }
  ),
  categorias_unicas = sapply(
    variables_diseno,
    function(variable) {
      
      n_distinct(
        tmigrante[[variable]]
      )
    }
  )
)

print(auditoria_diseno)

write_csv(
  auditoria_diseno,
  file.path(
    ruta_tablas,
    "auditoria_variables_diseno.csv"
  )
)

# ------------------------------------------------------------------------------
# 11. Guardar copias importadas en formato RDS
# ------------------------------------------------------------------------------

saveRDS(
  tmigrante,
  file.path(
    ruta_procesados,
    "tmigrante_importada.rds"
  )
)

saveRDS(
  thogar,
  file.path(
    ruta_procesados,
    "thogar_importada.rds"
  )
)

saveRDS(
  tsdem,
  file.path(
    ruta_procesados,
    "tsdem_importada.rds"
  )
)

saveRDS(
  tvivienda,
  file.path(
    ruta_procesados,
    "tvivienda_importada.rds"
  )
)

# ------------------------------------------------------------------------------
# 12. Guardar información de sesión
# ------------------------------------------------------------------------------

captura_sesion <- capture.output(
  sessionInfo()
)

writeLines(
  captura_sesion,
  con = file.path(
    ruta_documentacion,
    "session_info_script_02.txt"
  )
)

# ------------------------------------------------------------------------------
# 13. Mensaje final
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("IMPORTACIÓN Y VALIDACIÓN TERMINADAS CORRECTAMENTE\n")
cat("Las cuatro tablas fueron importadas como texto.\n")
cat("Las llaves principales son únicas.\n")
cat("La cobertura de los enlaces es del 100 %.\n")
cat("Los archivos RDS fueron guardados en 04_datos_procesados.\n")
cat("============================================================\n")