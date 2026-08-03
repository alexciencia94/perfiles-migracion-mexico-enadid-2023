# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 06B: Exportación completa y prueba LCA con SUBPOPULATION
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
      paste(faltantes, collapse = ", ")
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
    "No se encontró tmigrante_importada.rds. Ejecute primero el script 02."
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
# 1. Construcción de la base completa
# ------------------------------------------------------------------------------

base_mplus_completa <- tmigrante |>
  mutate(
    id = row_number(),
    subpop = if_else(p4_11 == "1", 1L, 0L),
    
    # Fuera del dominio EUA, los indicadores se dejan como faltantes.
    edad = case_when(
      subpop == 1L & p4_8_ag2 %in% c("1", "2", "3", "4") ~
        as.integer(p4_8_ag2),
      TRUE ~ -999L
    ),
    
    paren = case_when(
      subpop == 1L & p4_7 == "1" ~ 1L,
      subpop == 1L & p4_7 == "2" ~ 2L,
      subpop == 1L & p4_7 == "3" ~ 3L,
      subpop == 1L & p4_7 == "4" ~ 4L,
      subpop == 1L & p4_7 == "5" ~ 5L,
      subpop == 1L & p4_7 == "8" ~ 6L,
      TRUE ~ -999L
    ),
    
    nacim = case_when(
      subpop == 1L & p4_5 == "1" ~ 1L,
      subpop == 1L & p4_5 == "2" ~ 2L,
      subpop == 1L & p4_5 %in% c("3", "4") ~ 3L,
      TRUE ~ -999L
    ),
    
    motivo = case_when(
      subpop == 1L & p4_14 == "1" ~ 1L,
      subpop == 1L & p4_14 == "2" ~ 2L,
      subpop == 1L & p4_14 == "3" ~ 3L,
      subpop == 1L & p4_14 == "4" ~ 4L,
      subpop == 1L & p4_14 == "5" ~ 5L,
      subpop == 1L & p4_14 %in% c("6", "9") ~ 6L,
      TRUE ~ -999L
    ),
    
    docu = case_when(
      subpop == 1L & p4_13 %in% c("1", "2", "5") ~ 1L,
      subpop == 1L & p4_13 %in% c("3", "4") ~ 2L,
      subpop == 1L & p4_13 == "6" ~ 3L,
      subpop == 1L & p4_13 == "7" ~ 4L,
      TRUE ~ -999L
    ),
    
    dest = case_when(
      subpop == 1L & p4_12 %in% as.character(1:7) ~
        as.integer(p4_12),
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
    peso,
    estrat,
    upm
  )

# ------------------------------------------------------------------------------
# 2. Validaciones
# ------------------------------------------------------------------------------

if (sum(base_mplus_completa$subpop == 1L) != 3259L) {
  stop(
    "La subpoblación EUA no contiene 3,259 registros."
  )
}

if (
  anyNA(base_mplus_completa$peso) ||
  any(base_mplus_completa$peso <= 0) ||
  anyNA(base_mplus_completa$estrat) ||
  anyNA(base_mplus_completa$upm)
) {
  stop(
    "Se detectaron problemas en peso, estrato o UPM."
  )
}

indicadores <- c(
  "edad",
  "paren",
  "nacim",
  "motivo",
  "docu",
  "dest"
)

fuera_dominio_no_faltante <- base_mplus_completa |>
  filter(subpop == 0L) |>
  summarise(
    across(
      all_of(indicadores),
      ~ sum(.x != -999L)
    )
  )

if (any(unlist(fuera_dominio_no_faltante) != 0L)) {
  stop(
    "Hay indicadores no faltantes fuera de la subpoblación EUA."
  )
}

# ------------------------------------------------------------------------------
# 3. Exportar datos
# ------------------------------------------------------------------------------

ruta_dat <- file.path(
  ruta_mplus,
  "base_lca_enadid_subpop.dat"
)

write.table(
  base_mplus_completa,
  file = ruta_dat,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  na = "-999",
  dec = "."
)

write_csv(
  base_mplus_completa,
  file.path(
    ruta_mplus,
    "base_lca_enadid_subpop_revision.csv"
  )
)

if (
  length(readLines(ruta_dat, warn = FALSE)) != 3660L
) {
  stop(
    "El archivo DAT no contiene las 3,660 filas esperadas."
  )
}

# ------------------------------------------------------------------------------
# 4. Sintaxis Mplus
# ------------------------------------------------------------------------------

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - LCA de 2 clases usando SUBPOPULATION;",
  "",
  "DATA:",
  "  FILE IS base_lca_enadid_subpop.dat;",
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
  "  CLASSES = c(2);",
  "",
  "  WEIGHT IS peso;",
  "  STRATIFICATION IS estrat;",
  "  CLUSTER IS upm;",
  "  SUBPOPULATION IS subpop EQ 1;",
  "",
  "ANALYSIS:",
  "  TYPE = MIXTURE COMPLEX;",
  "  ESTIMATOR = MLR;",
  "  STARTS = 500 100;",
  "  STITERATIONS = 20;",
  "  PROCESSORS = 4;",
  "",
  "OUTPUT:",
  "  TECH1 TECH4 TECH8;",
  "",
  "SAVEDATA:",
  "  FILE IS probabilidades_2_clases_subpop.dat;",
  "  SAVE = CPROBABILITIES;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "LCA_2_clases_subpop.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 5. Resumen e informe
# ------------------------------------------------------------------------------

resumen <- tibble(
  universo = c(
    "TMIGRANTE completa",
    "Subpoblación destino EUA",
    "Fuera de la subpoblación"
  ),
  n = c(
    nrow(base_mplus_completa),
    sum(base_mplus_completa$subpop == 1L),
    sum(base_mplus_completa$subpop == 0L)
  )
)

print(resumen)

write_csv(
  resumen,
  file.path(
    ruta_tablas,
    "resumen_base_mplus_subpoblacion.csv"
  )
)

informe <- c(
  "BASE COMPLETA Y SINTAXIS SUBPOPULATION CREADAS CORRECTAMENTE",
  "",
  "Filas exportadas: 3,660.",
  "Subpoblación EUA: 3,259.",
  "Indicadores del modelo: 6.",
  "",
  "Archivos:",
  "- 08_mplus/base_lca_enadid_subpop.dat",
  "- 08_mplus/base_lca_enadid_subpop_revision.csv",
  "- 08_mplus/LCA_2_clases_subpop.inp"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_06b_subpoblacion.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")