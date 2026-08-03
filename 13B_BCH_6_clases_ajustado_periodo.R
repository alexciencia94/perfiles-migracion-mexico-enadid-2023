# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 13B: Sensibilidad con 6 clases - BCH ajustado por periodo
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
  "bch_6_clases_sensibilidad_paso1.dat"
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
      ". Ejecute primero BCH_6_clases_sensibilidad_paso1.inp."
    )
  )
}

if (!file.exists(ruta_tmigrante)) {
  stop(
    "No se encontro 04_datos_procesados/tmigrante_importada.rds."
  )
}

# ------------------------------------------------------------------------------
# 3. Leer pesos BCH de la solucion de seis clases
# ------------------------------------------------------------------------------

nombres_bch <- c(
  "paren_mplus",
  "nacim_mplus",
  "motivo_mplus",
  "docu_mplus",
  "dest_mplus",
  "edad_mplus",
  "retorno_mplus",
  paste0("bchw", 1:6),
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

variables_bch <- paste0("bchw", 1:6)

bch <- bch |>
  mutate(
    suma_bch = rowSums(
      across(
        all_of(variables_bch)
      )
    )
  )

if (
  max(
    abs(
      bch$suma_bch - 1
    )
  ) > 0.005
) {
  stop(
    "Los seis pesos BCH no suman aproximadamente 1."
  )
}

# Los pesos BCH pueden contener valores negativos. Esto es esperado y no
# constituye un error: son pesos de correccion, no probabilidades posteriores.
auditoria_bch <- tibble(
  criterio = c(
    "Registros",
    "Identificadores unicos",
    "Suma minima de pesos BCH",
    "Suma maxima de pesos BCH",
    "Peso BCH individual minimo",
    "Peso BCH individual maximo",
    "Casos con algun peso BCH negativo"
  ),
  resultado = c(
    nrow(bch),
    n_distinct(bch$id),
    min(bch$suma_bch),
    max(bch$suma_bch),
    min(
      as.matrix(
        bch[
          variables_bch
        ]
      )
    ),
    max(
      as.matrix(
        bch[
          variables_bch
        ]
      )
    ),
    sum(
      apply(
        as.matrix(
          bch[
            variables_bch
          ]
        ),
        1,
        function(x) {
          any(x < 0)
        }
      )
    )
  )
)

print(auditoria_bch)

write_csv(
  auditoria_bch,
  file.path(
    ruta_tablas,
    "auditoria_pesos_BCH_6_clases.csv"
  )
)

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
    
    # 1 = retorno; 2 = no retorno.
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
    
    # Categoria de referencia: 2018-2019.
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
# 5. Distribucion comun para estandarizar por periodo
# ------------------------------------------------------------------------------

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
    proporcion =
      poblacion_expandida /
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

if (
  nrow(distribucion_periodo) != 3L ||
  abs(
    sum(distribucion_periodo$proporcion) -
    1
  ) > 1e-10
) {
  stop(
    "No se pudo construir la distribucion de estandarizacion."
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

print(distribucion_periodo)

write_csv(
  distribucion_periodo,
  file.path(
    ruta_tablas,
    "distribucion_periodo_sensibilidad_6_clases.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Integrar pesos BCH y preservar el diseno completo
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

# Fuera del dominio se asignan pesos de entrenamiento validos y neutros.
# SUBPOPULATION les otorga peso analitico cero.
for (variable in variables_bch) {
  base_ajustada[[variable]][
    base_ajustada$subpop == 0L
  ] <- 1 / 6
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
# 7. Auditoria analitica
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
    retornaron = sum(
      retorno == 1L
    ),
    no_retornaron = sum(
      retorno == 2L
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
  auditoria$retornaron != 679L ||
  auditoria$no_retornaron != 2558L ||
  auditoria$retorno_no_especificado != 22L
) {
  stop(
    "La auditoria no coincide con los valores esperados."
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
    "auditoria_BCH_6_clases_ajustado_periodo.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Exportar archivo para Mplus
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
  "bch_6_clases_retorno_ajustado_periodo.dat"
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
    "bch_6_clases_retorno_ajustado_periodo_revision.csv"
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
# 9. Construir sintaxis Mplus
# ------------------------------------------------------------------------------

fnum <- function(x) {
  format(
    x,
    scientific = FALSE,
    digits = 12,
    trim = TRUE
  )
}

comparaciones <- combn(
  1:6,
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

lineas_probabilidades <- unlist(
  lapply(
    1:6,
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
      1:6,
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

# Mantener todas las lineas por debajo del limite de 90 caracteres de Mplus.
grupos_new <- split(
  parametros_nuevos,
  ceiling(
    seq_along(parametros_nuevos) /
      7
  )
)

lineas_new <- c(
  "  NEW (",
  vapply(
    grupos_new,
    function(x) {
      paste0(
        "    ",
        paste(
          x,
          collapse = " "
        )
      )
    },
    character(1)
  ),
  "  );"
)

if (
  any(
    nchar(lineas_new) >
    90L
  )
) {
  stop(
    "Alguna linea de NEW supera 90 caracteres."
  )
}

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - Sensibilidad BCH 6 clases ajustada por periodo;",
  "",
  "DATA:",
  "  FILE IS bch_6_clases_retorno_ajustado_periodo.dat;",
  "",
  "VARIABLE:",
  "  NAMES ARE",
  "    id subpop retorno p20 p22",
  "    bchw1 bchw2 bchw3 bchw4 bchw5 bchw6",
  "    peso estrat upm;",
  "",
  "  USEVARIABLES ARE",
  "    retorno p20 p22",
  "    bchw1 bchw2 bchw3 bchw4 bchw5 bchw6;",
  "",
  "  IDVARIABLE IS id;",
  "  MISSING ARE ALL (-999);",
  "  CATEGORICAL IS retorno;",
  "",
  "  CLASSES = c(6);",
  "  TRAINING = bchw1 bchw2 bchw3 bchw4 bchw5 bchw6 (BCH);",
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
  "MODEL CONSTRAINT:",
  lineas_new,
  "",
  "  ! Probabilidad de RETORNO=1 por clase y periodo.",
  lineas_probabilidades,
  "",
  "  ! Odds ratios del retorno frente a 2018-2019.",
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
  "",
  "OUTPUT:",
  "  TECH1 TECH4 CINTERVAL;"
)

if (
  any(
    nchar(sintaxis) >
    90L
  )
) {
  lineas_largas <- sintaxis[
    nchar(sintaxis) >
      90L
  ]
  
  stop(
    paste0(
      "La sintaxis contiene lineas mayores de 90 caracteres:\n",
      paste(
        lineas_largas,
        collapse = "\n"
      )
    )
  )
}

ruta_inp <- file.path(
  ruta_mplus,
  "BCH_6_clases_retorno_ajustado_periodo.inp"
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
  "SENSIBILIDAD BCH CON SEIS CLASES PREPARADA",
  "",
  "Base completa: 3,660 registros.",
  "Dominio EUA: 3,259.",
  "Retorno conocido: 3,237.",
  "",
  "El modelo estimara:",
  "- probabilidad de retorno por clase y periodo;",
  "- probabilidad estandarizada de retorno en cada clase;",
  "- efecto general del periodo de salida;",
  "- prueba Wald global entre las seis clases;",
  "- 15 diferencias estandarizadas entre pares de clases.",
  "",
  "Archivos:",
  "- 08_mplus/bch_6_clases_retorno_ajustado_periodo.dat",
  "- 08_mplus/bch_6_clases_retorno_ajustado_periodo_revision.csv",
  "- 08_mplus/BCH_6_clases_retorno_ajustado_periodo.inp"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_13B_BCH_6_clases_ajustado_periodo.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")