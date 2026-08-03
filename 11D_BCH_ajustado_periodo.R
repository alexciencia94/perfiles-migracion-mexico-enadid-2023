# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 11D: BCH ajustado por periodo de salida
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999
)

# ------------------------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------------------------

paquetes <- c(
  "readr",
  "dplyr",
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
      "\nInstalelos con renv::install()."
    )
  )
}

library(readr)
library(dplyr)
library(tibble)

# ------------------------------------------------------------------------------
# 2. Rutas
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

ruta_bch <- file.path(
  ruta_mplus,
  "bch_7_clases_paso1.dat"
)

ruta_tmigrante <- file.path(
  ruta_procesados,
  "tmigrante_importada.rds"
)

if (!file.exists(ruta_bch)) {
  stop(
    paste0(
      "No se encontro ",
      ruta_bch,
      ". Ejecute primero BCH_7_clases_paso1.inp."
    )
  )
}

if (!file.exists(ruta_tmigrante)) {
  stop(
    "No se encontro 04_datos_procesados/tmigrante_importada.rds."
  )
}

# ------------------------------------------------------------------------------
# 3. Leer pesos BCH
# ------------------------------------------------------------------------------

nombres_bch <- c(
  "paren_mplus",
  "nacim_mplus",
  "motivo_mplus",
  "docu_mplus",
  "dest_mplus",
  "edad_mplus",
  "retorno_mplus",
  paste0("bchw", 1:7),
  "peso_mplus_normalizado",
  "id",
  "estrato_mplus",
  "upm_mplus"
)

bch <- read_table(
  ruta_bch,
  col_names = nombres_bch,
  col_types = cols(
    .default = col_double()
  ),
  na = "*",
  progress = FALSE,
  show_col_types = FALSE
) |>
  mutate(
    id = as.integer(id)
  )

if (nrow(bch) != 3259L) {
  stop(
    paste0(
      "El archivo BCH contiene ",
      nrow(bch),
      " filas; se esperaban 3,259."
    )
  )
}

if (n_distinct(bch$id) != 3259L) {
  stop(
    "El identificador ID no es unico en el archivo BCH."
  )
}

variables_bch <- paste0("bchw", 1:7)

# ------------------------------------------------------------------------------
# 4. Preparar TMIGRANTE completa
# ------------------------------------------------------------------------------

tmigrante <- readRDS(
  ruta_tmigrante
) |>
  mutate(
    id = row_number(),
    
    subpop = if_else(
      p4_11 == "1",
      1L,
      0L
    ),
    
    retorno = case_when(
      subpop == 0L ~ 1L,
      cond_resid == "1" ~ 1L,
      cond_resid == "2" ~ 2L,
      TRUE ~ -999L
    ),
    
    anio_salida = suppressWarnings(
      as.integer(p4_10_2)
    ),
    
    periodo = case_when(
      anio_salida %in% c(2018L, 2019L) ~ "2018-2019",
      anio_salida %in% c(2020L, 2021L) ~ "2020-2021",
      anio_salida %in% c(2022L, 2023L) ~ "2022-2023",
      TRUE ~ NA_character_
    ),
    
    # Referencia: 2018-2019.
    p20 = case_when(
      subpop == 0L ~ 0L,
      periodo == "2020-2021" ~ 1L,
      !is.na(periodo) ~ 0L,
      TRUE ~ -999L
    ),
    
    p22 = case_when(
      subpop == 0L ~ 0L,
      periodo == "2022-2023" ~ 1L,
      !is.na(periodo) ~ 0L,
      TRUE ~ -999L
    ),
    
    peso = as.numeric(fac_hog),
    estrat = as.integer(est_dis),
    upm = as.integer(upm_dis)
  )

if (nrow(tmigrante) != 3660L) {
  stop(
    "TMIGRANTE no contiene las 3,660 filas esperadas."
  )
}

# ------------------------------------------------------------------------------
# 5. Distribucion comun de estandarizacion
# ------------------------------------------------------------------------------

# Se utiliza la distribucion ponderada del periodo entre las personas del
# dominio EUA con condicion de retorno conocida.
distribucion_periodo <- tmigrante |>
  filter(
    subpop == 1L,
    retorno %in% c(1L, 2L),
    !is.na(periodo)
  ) |>
  group_by(
    periodo
  ) |>
  summarise(
    n = n(),
    poblacion_expandida = sum(
      peso,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  mutate(
    proporcion = poblacion_expandida /
      sum(poblacion_expandida)
  ) |>
  arrange(
    factor(
      periodo,
      levels = c(
        "2018-2019",
        "2020-2021",
        "2022-2023"
      )
    )
  )

print(distribucion_periodo)

if (
  nrow(distribucion_periodo) != 3L ||
  abs(sum(distribucion_periodo$proporcion) - 1) > 1e-10
) {
  stop(
    "No se pudo construir correctamente la distribucion de estandarizacion."
  )
}

q18 <- distribucion_periodo$proporcion[
  distribucion_periodo$periodo == "2018-2019"
]

q20 <- distribucion_periodo$proporcion[
  distribucion_periodo$periodo == "2020-2021"
]

q22 <- distribucion_periodo$proporcion[
  distribucion_periodo$periodo == "2022-2023"
]

write_csv(
  distribucion_periodo,
  file.path(
    ruta_tablas,
    "distribucion_ponderada_periodo_estandarizacion.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Integrar pesos BCH y conservar el diseño completo
# ------------------------------------------------------------------------------

base_ajustada <- tmigrante |>
  select(
    id,
    subpop,
    retorno,
    p20,
    p22,
    peso,
    estrat,
    upm
  ) |>
  left_join(
    bch |>
      select(
        id,
        all_of(variables_bch)
      ),
    by = "id"
  )

# Todos los casos EUA deben tener pesos BCH.
faltantes_bch_dominio <- base_ajustada |>
  filter(
    subpop == 1L
  ) |>
  summarise(
    across(
      all_of(variables_bch),
      ~ sum(is.na(.x))
    )
  )

if (
  any(
    unlist(
      faltantes_bch_dominio
    ) != 0L
  )
) {
  stop(
    "Existen casos EUA sin pesos BCH."
  )
}

# Fuera del dominio, pesos de entrenamiento neutros y validos.
for (variable in variables_bch) {
  base_ajustada[[variable]][
    base_ajustada$subpop == 0L
  ] <- 1 / 7
}

if (
  anyNA(
    base_ajustada[
      variables_bch
    ]
  )
) {
  stop(
    "Quedaron pesos BCH faltantes."
  )
}

# ------------------------------------------------------------------------------
# 7. Auditoría
# ------------------------------------------------------------------------------

auditoria <- base_ajustada |>
  filter(
    subpop == 1L
  ) |>
  summarise(
    n_dominio = n(),
    retorno_conocido = sum(
      retorno %in% c(1L, 2L)
    ),
    retorno_no_especificado = sum(
      retorno == -999L
    ),
    periodo_faltante = sum(
      p20 == -999L |
        p22 == -999L
    )
  )

print(auditoria)

if (
  auditoria$n_dominio != 3259L ||
  auditoria$retorno_conocido != 3237L ||
  auditoria$retorno_no_especificado != 22L
) {
  stop(
    "La auditoria del dominio o del retorno no coincide con lo esperado."
  )
}

if (auditoria$periodo_faltante > 0L) {
  stop(
    "Existen personas del dominio con periodo de salida faltante."
  )
}

write_csv(
  auditoria,
  file.path(
    ruta_tablas,
    "auditoria_BCH_ajustado_periodo.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Exportar datos
# ------------------------------------------------------------------------------

base_exportar <- base_ajustada |>
  select(
    id,
    subpop,
    retorno,
    p20,
    p22,
    all_of(variables_bch),
    peso,
    estrat,
    upm
  )

ruta_dat <- file.path(
  ruta_mplus,
  "bch_7_clases_retorno_ajustado_periodo.dat"
)

write.table(
  base_exportar,
  file = ruta_dat,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  na = "-999",
  dec = "."
)

write_csv(
  base_exportar,
  file.path(
    ruta_mplus,
    "bch_7_clases_retorno_ajustado_periodo_revision.csv"
  )
)

if (
  length(
    readLines(
      ruta_dat,
      warn = FALSE
    )
  ) != 3660L
) {
  stop(
    "El archivo DAT no contiene 3,660 filas."
  )
}

# ------------------------------------------------------------------------------
# 9. Sintaxis Mplus
# ------------------------------------------------------------------------------

fnum <- function(x) {
  format(
    x,
    scientific = FALSE,
    digits = 12,
    trim = TRUE
  )
}

lineas_probabilidades <- unlist(
  lapply(
    1:7,
    function(k) {
      c(
        paste0(
          "  r",
          k,
          "18 = 1 / (1 + EXP(-(t",
          k,
          ")));"
        ),
        paste0(
          "  r",
          k,
          "20 = 1 / (1 + EXP(-(t",
          k,
          " - b20)));"
        ),
        paste0(
          "  r",
          k,
          "22 = 1 / (1 + EXP(-(t",
          k,
          " - b22)));"
        ),
        paste0(
          "  rs",
          k,
          " = ",
          fnum(q18),
          "*r",
          k,
          "18 + ",
          fnum(q20),
          "*r",
          k,
          "20 + ",
          fnum(q22),
          "*r",
          k,
          "22;"
        )
      )
    }
  )
)

comparaciones <- combn(
  1:7,
  2
)

nombres_diferencias <- apply(
  comparaciones,
  2,
  function(x) {
    paste0(
      "sd",
      x[1],
      x[2]
    )
  }
)

lineas_diferencias <- apply(
  comparaciones,
  2,
  function(x) {
    paste0(
      "  sd",
      x[1],
      x[2],
      " = rs",
      x[1],
      " - rs",
      x[2],
      ";"
    )
  }
)

parametros_nuevos <- c(
  unlist(
    lapply(
      1:7,
      function(k) {
        c(
          paste0("r", k, "18"),
          paste0("r", k, "20"),
          paste0("r", k, "22"),
          paste0("rs", k)
        )
      }
    )
  ),
  "or20",
  "or22",
  nombres_diferencias
)

# Mplus limita cada línea del archivo de entrada a 90 caracteres.
# Se divide la declaración NEW en líneas cortas, manteniendo un único comando.
grupos_new <- split(
  parametros_nuevos,
  ceiling(seq_along(parametros_nuevos) / 7)
)

lineas_new <- c(
  "  NEW (",
  vapply(
    grupos_new,
    function(x) {
      paste0("    ", paste(x, collapse = " "))
    },
    character(1)
  ),
  "  );"
)

if (any(nchar(lineas_new) > 90L)) {
  stop("Alguna línea de MODEL CONSTRAINT todavía supera 90 caracteres.")
}

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - BCH del retorno ajustado por periodo de salida;",
  "",
  "DATA:",
  "  FILE IS bch_7_clases_retorno_ajustado_periodo.dat;",
  "",
  "VARIABLE:",
  "  NAMES ARE",
  "    id subpop retorno p20 p22",
  "    bchw1 bchw2 bchw3 bchw4 bchw5 bchw6 bchw7",
  "    peso estrat upm;",
  "",
  "  USEVARIABLES ARE",
  "    retorno p20 p22",
  "    bchw1 bchw2 bchw3 bchw4 bchw5 bchw6 bchw7;",
  "",
  "  IDVARIABLE IS id;",
  "  MISSING ARE ALL (-999);",
  "  CATEGORICAL IS retorno;",
  "",
  "  CLASSES = c(7);",
  "  TRAINING = bchw1 bchw2 bchw3 bchw4 bchw5 bchw6 bchw7 (BCH);",
  "",
  "  WEIGHT IS peso;",
  "  STRATIFICATION IS estrat;",
  "  CLUSTER IS upm;",
  "  SUBPOPULATION IS subpop EQ 1;",
  "",
  "ANALYSIS:",
  "  TYPE = MIXTURE COMPLEX;",
  "  ESTIMATOR = MLR;",
  "  STARTS = 0;",
  "",
  "MODEL:",
  "  %OVERALL%",
  "    C ON p20 p22;",
  "    retorno ON p20 p22 (b20 b22);",
  "",
  "  %C#1%",
  "    [retorno$1] (t1);",
  "",
  "  %C#2%",
  "    [retorno$1] (t2);",
  "",
  "  %C#3%",
  "    [retorno$1] (t3);",
  "",
  "  %C#4%",
  "    [retorno$1] (t4);",
  "",
  "  %C#5%",
  "    [retorno$1] (t5);",
  "",
  "  %C#6%",
  "    [retorno$1] (t6);",
  "",
  "  %C#7%",
  "    [retorno$1] (t7);",
  "",
  "MODEL CONSTRAINT:",
  lineas_new,
  "",
  "  ! Probabilidad de RETORNO=1 en cada periodo.",
  "  ! Para categoria 1: logistic(umbral - beta*x).",
  lineas_probabilidades,
  "",
  "  ! Odds ratios para retorno, no para no-retorno.",
  "  or20 = EXP(-b20);",
  "  or22 = EXP(-b22);",
  "",
  "  ! Diferencias estandarizadas entre perfiles.",
  lineas_diferencias,
  "",
  "MODEL TEST:",
  "  t2 = t1;",
  "  t3 = t1;",
  "  t4 = t1;",
  "  t5 = t1;",
  "  t6 = t1;",
  "  t7 = t1;",
  "",
  "OUTPUT:",
  "  TECH1 TECH4 CINTERVAL;"
)

ruta_inp <- file.path(
  ruta_mplus,
  "BCH_7_clases_retorno_ajustado_periodo.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 10. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "MODELO BCH AJUSTADO POR PERIODO PREPARADO",
  "",
  "Covariables:",
  "- P20: salida en 2020-2021 frente a 2018-2019.",
  "- P22: salida en 2022-2023 frente a 2018-2019.",
  "",
  "El modelo incluye:",
  "- periodo como predictor de la clase latente;",
  "- periodo como predictor común del retorno;",
  "- umbral de retorno específico por perfil;",
  "- probabilidades de retorno por perfil y periodo;",
  "- probabilidades estandarizadas a una distribución común del periodo;",
  "- prueba global entre perfiles;",
  "- 21 diferencias estandarizadas entre perfiles.",
  "",
  paste0(
    "Distribución de estandarización: ",
    "2018-2019=",
    round(q18, 6),
    "; 2020-2021=",
    round(q20, 6),
    "; 2022-2023=",
    round(q22, 6),
    "."
  ),
  "",
  "Archivos:",
  "- 08_mplus/bch_7_clases_retorno_ajustado_periodo.dat",
  "- 08_mplus/bch_7_clases_retorno_ajustado_periodo_revision.csv",
  "- 08_mplus/BCH_7_clases_retorno_ajustado_periodo.inp"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_11D_BCH_ajustado_periodo.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")

cat("\n============================================================\n")