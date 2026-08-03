# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 07: Generar modelos LCA de 2 a 8 clases para Mplus
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

dir.create(ruta_mplus, recursive = TRUE, showWarnings = FALSE)

archivo_datos <- "base_lca_enadid_subpop_correcta.dat"
ruta_datos <- file.path(ruta_mplus, archivo_datos)

if (!file.exists(ruta_datos)) {
  stop(
    paste0(
      "No se encontró ",
      ruta_datos,
      ". Ejecute primero el script 06C."
    )
  )
}

n_lineas <- length(
  readLines(
    ruta_datos,
    warn = FALSE
  )
)

if (n_lineas != 3660L) {
  stop(
    paste0(
      "El archivo de datos contiene ",
      n_lineas,
      " filas; se esperaban 3,660."
    )
  )
}

# ------------------------------------------------------------------------------
# 1. Función para crear una sintaxis por número de clases
# ------------------------------------------------------------------------------

crear_sintaxis <- function(k) {
  
  c(
    "TITLE:",
    paste0(
      "  ENADID 2023 - LCA de ",
      k,
      " clases con dominio EUA y diseno complejo;"
    ),
    "",
    "DATA:",
    paste0(
      "  FILE IS ",
      archivo_datos,
      ";"
    ),
    "",
    "VARIABLE:",
    "  NAMES ARE",
    "    id subpop edad paren nacim motivo docu dest",
    "    peso estrat upm;",
    "",
    "  USEVARIABLES ARE",
    "    edad paren nacim motivo docu dest;",
    "",
    "  IDVARIABLE IS id;",
    "  MISSING ARE ALL (-999);",
    "",
    "  CATEGORICAL ARE edad;",
    "  NOMINAL ARE paren nacim motivo docu dest;",
    "",
    paste0(
      "  CLASSES = c(",
      k,
      ");"
    ),
    "",
    "  WEIGHT IS peso;",
    "  STRATIFICATION IS estrat;",
    "  CLUSTER IS upm;",
    "  SUBPOPULATION IS subpop EQ 1;",
    "",
    "ANALYSIS:",
    "  TYPE = MIXTURE COMPLEX;",
    "  ESTIMATOR = MLR;",
    "",
    # Etapa de cribado. Los modelos finalistas se volverán a ejecutar
    # posteriormente con un número mayor de inicios aleatorios.
    "  STARTS = 1000 250;",
    "  STITERATIONS = 20;",
    "  PROCESSORS = 4;",
    "",
    "OUTPUT:",
    "  TECH1 TECH4 TECH8 TECH11;",
    "",
    "SAVEDATA:",
    paste0(
      "  FILE IS probabilidades_",
      k,
      "_clases.dat;"
    ),
    "  SAVE = CPROBABILITIES;"
  )
}

# ------------------------------------------------------------------------------
# 2. Generar archivos para 2 a 8 clases
# ------------------------------------------------------------------------------

clases <- 2:8

rutas_inp <- vapply(
  clases,
  function(k) {
    
    nombre_archivo <- paste0(
      "LCA_",
      k,
      "_clases.inp"
    )
    
    ruta_archivo <- file.path(
      ruta_mplus,
      nombre_archivo
    )
    
    writeLines(
      crear_sintaxis(k),
      con = ruta_archivo,
      useBytes = TRUE
    )
    
    ruta_archivo
  },
  character(1)
)

# ------------------------------------------------------------------------------
# 3. Crear guía de ejecución
# ------------------------------------------------------------------------------

guia <- c(
  "MODELOS LCA DE 2 A 8 CLASES",
  "",
  "Ejecute en Mplus, en este orden:",
  paste0(
    "1. ",
    basename(rutas_inp[1])
  ),
  paste0(
    seq_along(rutas_inp[-1]) + 1,
    ". ",
    basename(rutas_inp[-1])
  ),
  "",
  "Cada modelo usa:",
  "- TYPE = MIXTURE COMPLEX",
  "- WEIGHT, STRATIFICATION y CLUSTER",
  "- SUBPOPULATION para destino EUA",
  "- 1,000 inicios aleatorios y 250 optimizaciones finales",
  "- TECH11 como contraste complementario",
  "- probabilidades posteriores guardadas por solución",
  "",
  "Criterios que deben revisarse en cada salida:",
  "- estimación terminada normalmente",
  "- mejor loglikelihood replicado",
  "- AIC, BIC y BIC ajustado",
  "- entropía",
  "- tamaño estimado y modal de cada clase",
  "- probabilidades posteriores medias",
  "- parámetros extremos o clases con probabilidades 0/1",
  "- clases menores de 5 %",
  "- interpretabilidad sustantiva",
  "",
  "No seleccione una solución únicamente por la entropía o por TECH11.",
  "Los modelos finalistas se volverán a ejecutar con más inicios aleatorios."
)

writeLines(
  guia,
  con = file.path(
    ruta_documentacion,
    "guia_modelos_lca_2_a_8.txt"
  ),
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 4. Plantilla para registrar resultados
# ------------------------------------------------------------------------------

plantilla <- data.frame(
  clases = clases,
  convergencia_normal = NA,
  mejor_loglikelihood_replicado = NA,
  loglikelihood = NA_real_,
  parametros = NA_integer_,
  aic = NA_real_,
  bic = NA_real_,
  bic_ajustado = NA_real_,
  entropia = NA_real_,
  clase_minima_n = NA_real_,
  clase_minima_porcentaje = NA_real_,
  tech11_p = NA_real_,
  parametros_extremos = NA,
  interpretable = NA,
  observaciones = NA_character_
)

write.csv(
  plantilla,
  file = file.path(
    ruta_mplus,
    "plantilla_comparacion_modelos_lca.csv"
  ),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------------------------
# 5. Mensaje final
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("MODELOS DE 2 A 8 CLASES GENERADOS CORRECTAMENTE\n")
cat("Archivo de datos:", ruta_datos, "\n")
cat("Sintaxis creadas:\n")
cat(paste0("- ", rutas_inp, collapse = "\n"))
cat("\nPlantilla: 08_mplus/plantilla_comparacion_modelos_lca.csv\n")
cat("============================================================\n")