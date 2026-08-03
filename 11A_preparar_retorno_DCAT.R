# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 11A: Preparar retorno como resultado distal categórico (DCAT)
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

paquetes <- c("dplyr", "readr", "tibble")

faltantes <- paquetes[
  !vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)
]

if (length(faltantes) > 0) {
  stop(
    paste0(
      "Faltan estos paquetes: ",
      paste(faltantes, collapse = ", "),
      "\nInstálelos con renv::install()."
    )
  )
}

library(dplyr)
library(readr)
library(tibble)

ruta_procesados <- "04_datos_procesados"
ruta_mplus <- "08_mplus"
ruta_tablas <- "05_tablas"
ruta_documentacion <- "09_documentacion"

dir.create(ruta_mplus, recursive = TRUE, showWarnings = FALSE)

ruta_tmigrante <- file.path(
  ruta_procesados,
  "tmigrante_importada.rds"
)

if (!file.exists(ruta_tmigrante)) {
  stop(
    "No se encontró 04_datos_procesados/tmigrante_importada.rds."
  )
}

tmigrante <- readRDS(ruta_tmigrante)

if (nrow(tmigrante) != 3660L) {
  stop(
    paste0(
      "TMIGRANTE contiene ",
      nrow(tmigrante),
      " registros; se esperaban 3,660."
    )
  )
}

# ------------------------------------------------------------------------------
# 1. Construir base completa
# ------------------------------------------------------------------------------

base_dcat <- tmigrante |>
  mutate(
    id = row_number(),
    subpop = if_else(p4_11 == "1", 1L, 0L),
    
    # Los casos fuera del dominio reciben categorías válidas arbitrarias.
    # SUBPOPULATION les asignará peso analítico cero.
    edad = case_when(
      subpop == 0L ~ 1L,
      p4_8_ag2 %in% c("1", "2", "3", "4") ~ as.integer(p4_8_ag2),
      TRUE ~ -999L
    ),
    
    paren = case_when(
      subpop == 0L ~ 1L,
      p4_7 == "1" ~ 1L,
      p4_7 == "2" ~ 2L,
      p4_7 == "3" ~ 3L,
      p4_7 == "4" ~ 4L,
      p4_7 == "5" ~ 5L,
      p4_7 == "8" ~ 6L,
      TRUE ~ -999L
    ),
    
    nacim = case_when(
      subpop == 0L ~ 1L,
      p4_5 == "1" ~ 1L,
      p4_5 == "2" ~ 2L,
      p4_5 %in% c("3", "4") ~ 3L,
      TRUE ~ -999L
    ),
    
    motivo = case_when(
      subpop == 0L ~ 1L,
      p4_14 == "1" ~ 1L,
      p4_14 == "2" ~ 2L,
      p4_14 == "3" ~ 3L,
      p4_14 == "4" ~ 4L,
      p4_14 == "5" ~ 5L,
      p4_14 %in% c("6", "9") ~ 6L,
      TRUE ~ -999L
    ),
    
    docu = case_when(
      subpop == 0L ~ 1L,
      p4_13 %in% c("1", "2", "5") ~ 1L,
      p4_13 %in% c("3", "4") ~ 2L,
      p4_13 == "6" ~ 3L,
      p4_13 == "7" ~ 4L,
      p4_13 == "9" ~ -999L,
      TRUE ~ -999L
    ),
    
    dest = case_when(
      subpop == 0L ~ 1L,
      p4_12 %in% as.character(1:7) ~ as.integer(p4_12),
      TRUE ~ -999L
    ),
    
    # Resultado distal binario:
    # 1 = retornó; 2 = no retornó.
    # Los 22 casos no especificados permanecen como faltantes.
    # Fuera del dominio se asigna 1 de forma arbitraria.
    retorno = case_when(
      subpop == 0L ~ 1L,
      cond_resid == "1" ~ 1L,
      cond_resid == "2" ~ 2L,
      TRUE ~ -999L
    ),
    
    peso = as.numeric(fac_hog),
    estrat = as.integer(est_dis),
    upm = as.integer(upm_dis)
  ) |>
  select(
    id,
    subpop,
    edad,
    paren,
    nacim,
    motivo,
    docu,
    dest,
    retorno,
    peso,
    estrat,
    upm
  )

# ------------------------------------------------------------------------------
# 2. Validaciones
# ------------------------------------------------------------------------------

if (sum(base_dcat$subpop == 1L) != 3259L) {
  stop("La subpoblación EUA no contiene 3,259 personas.")
}

auditoria_retorno <- base_dcat |>
  filter(subpop == 1L) |>
  count(
    retorno,
    name = "n"
  ) |>
  arrange(retorno)

print(auditoria_retorno)

esperado <- tibble(
  retorno = c(-999L, 1L, 2L),
  n_esperado = c(22L, 679L, 2558L)
)

validacion <- auditoria_retorno |>
  left_join(
    esperado,
    by = "retorno"
  ) |>
  mutate(
    correcto = n == n_esperado
  )

if (
  nrow(validacion) != 3L ||
  !all(validacion$correcto)
) {
  stop(
    "La distribución de retorno no coincide con 22, 679 y 2,558."
  )
}

if (
  anyNA(base_dcat$peso) ||
  any(base_dcat$peso <= 0) ||
  anyNA(base_dcat$estrat) ||
  anyNA(base_dcat$upm)
) {
  stop(
    "Hay problemas en peso, estrato o UPM."
  )
}

# ------------------------------------------------------------------------------
# 3. Exportar datos
# ------------------------------------------------------------------------------

ruta_dat <- file.path(
  ruta_mplus,
  "base_lca_7_clases_retorno_dcat.dat"
)

write.table(
  base_dcat,
  file = ruta_dat,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  na = "-999",
  dec = "."
)

write_csv(
  base_dcat,
  file.path(
    ruta_mplus,
    "base_lca_7_clases_retorno_dcat_revision.csv"
  )
)

if (
  length(readLines(ruta_dat, warn = FALSE)) != 3660L
) {
  stop("El archivo DAT no contiene 3,660 filas.")
}

# ------------------------------------------------------------------------------
# 4. Crear sintaxis Mplus
# ------------------------------------------------------------------------------

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - retorno como resultado distal categórico, 7 clases;",
  "",
  "DATA:",
  "  FILE IS base_lca_7_clases_retorno_dcat.dat;",
  "",
  "VARIABLE:",
  "  NAMES ARE",
  "    id subpop edad paren nacim motivo docu dest",
  "    retorno peso estrat upm;",
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
  "  CLASSES = c(7);",
  "",
  "  AUXILIARY = retorno (DCAT);",
  "",
  "  WEIGHT IS peso;",
  "  STRATIFICATION IS estrat;",
  "  CLUSTER IS upm;",
  "  SUBPOPULATION IS subpop EQ 1;",
  "",
  "ANALYSIS:",
  "  TYPE = MIXTURE COMPLEX;",
  "  ESTIMATOR = MLR;",
  "  STARTS = 5000 1000;",
  "  STITERATIONS = 20;",
  "  PROCESSORS = 4;",
  "",
  "OUTPUT:",
  "  TECH1 TECH4 TECH8;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "LCA_7_clases_retorno_DCAT.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

write_csv(
  validacion,
  file.path(
    ruta_tablas,
    "auditoria_retorno_para_DCAT.csv"
  )
)

informe <- c(
  "ANÁLISIS DCAT PREPARADO",
  "",
  "Universo TMIGRANTE: 3,660.",
  "Dominio EUA: 3,259.",
  "Retornaron: 679.",
  "No retornaron: 2,558.",
  "Retorno no especificado: 22.",
  "",
  "Archivos:",
  "- 08_mplus/base_lca_7_clases_retorno_dcat.dat",
  "- 08_mplus/base_lca_7_clases_retorno_dcat_revision.csv",
  "- 08_mplus/LCA_7_clases_retorno_DCAT.inp"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_11A_DCAT.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")