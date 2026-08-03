# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 11C: Paso 2 BCH manual para retorno binario
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
# 3. Leer el archivo BCH
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
    "Los siete pesos BCH no suman aproximadamente 1."
  )
}

# ------------------------------------------------------------------------------
# 4. Recuperar TMIGRANTE completa y preservar el diseño
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
    # Los 22 casos no especificados quedan como -999.
    # Fuera del dominio se asigna 1 de manera arbitraria.
    retorno = case_when(
      subpop == 0L ~ 1L,
      cond_resid == "1" ~ 1L,
      cond_resid == "2" ~ 2L,
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

# Vincular los pesos BCH únicamente a la subpoblación EUA.
base_paso2 <- tmigrante |>
  select(
    id,
    subpop,
    retorno,
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

# Todos los casos EUA deben tener los siete pesos BCH.
faltantes_bch_dominio <- base_paso2 |>
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

# Fuera del dominio, asignar pesos de entrenamiento válidos y neutros.
# SUBPOPULATION les asignará peso analítico cero.
for (variable in variables_bch) {
  base_paso2[[variable]][
    base_paso2$subpop == 0L
  ] <- 1 / 7
}

if (
  anyNA(
    base_paso2[
      variables_bch
    ]
  )
) {
  stop(
    "Quedaron pesos BCH faltantes después de preparar la base completa."
  )
}

# ------------------------------------------------------------------------------
# 5. Auditoría
# ------------------------------------------------------------------------------

auditoria_retorno <- base_paso2 |>
  filter(
    subpop == 1L
  ) |>
  count(
    retorno,
    name = "n"
  ) |>
  arrange(
    retorno
  )

print(auditoria_retorno)

esperado <- tibble(
  retorno = c(
    -999L,
    1L,
    2L
  ),
  n_esperado = c(
    22L,
    679L,
    2558L
  )
)

validacion_retorno <- auditoria_retorno |>
  left_join(
    esperado,
    by = "retorno"
  ) |>
  mutate(
    correcto = n == n_esperado
  )

if (
  nrow(validacion_retorno) != 3L ||
  !all(validacion_retorno$correcto)
) {
  stop(
    "La distribución del retorno no coincide con los valores esperados."
  )
}

auditoria_diseno <- bind_rows(
  base_paso2 |>
    summarise(
      universo = "TMIGRANTE completa",
      n = n(),
      estratos = n_distinct(estrat),
      upm = n_distinct(upm)
    ),
  base_paso2 |>
    filter(
      subpop == 1L
    ) |>
    summarise(
      universo = "Dominio EUA",
      n = n(),
      estratos = n_distinct(estrat),
      upm = n_distinct(upm)
    )
)

print(auditoria_diseno)

write_csv(
  validacion_retorno,
  file.path(
    ruta_tablas,
    "auditoria_retorno_BCH_paso2.csv"
  )
)

write_csv(
  auditoria_diseno,
  file.path(
    ruta_tablas,
    "auditoria_diseno_BCH_paso2.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Exportar archivo para Mplus
# ------------------------------------------------------------------------------

base_exportar <- base_paso2 |>
  select(
    id,
    subpop,
    retorno,
    all_of(variables_bch),
    peso,
    estrat,
    upm
  )

ruta_dat <- file.path(
  ruta_mplus,
  "bch_7_clases_retorno_paso2.dat"
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
    "bch_7_clases_retorno_paso2_revision.csv"
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
# 7. Crear sintaxis Mplus del paso 2
# ------------------------------------------------------------------------------

comparaciones <- combn(
  1:7,
  2
)

nombres_diferencias <- apply(
  comparaciones,
  2,
  function(x) {
    paste0(
      "d",
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
      "  d",
      x[1],
      x[2],
      " = p",
      x[1],
      " - p",
      x[2],
      ";"
    )
  }
)

sintaxis <- c(
  "TITLE:",
  "  ENADID 2023 - Paso 2 BCH manual para retorno binario;",
  "",
  "DATA:",
  "  FILE IS bch_7_clases_retorno_paso2.dat;",
  "",
  "VARIABLE:",
  "  NAMES ARE",
  "    id subpop retorno",
  "    bchw1 bchw2 bchw3 bchw4 bchw5 bchw6 bchw7",
  "    peso estrat upm;",
  "",
  "  USEVARIABLES ARE",
  "    retorno bchw1 bchw2 bchw3 bchw4 bchw5 bchw6 bchw7;",
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
  "  NEW (",
  "    p1 p2 p3 p4 p5 p6 p7",
  "    d12 d13 d14 d15 d16 d17 d23 d24 d25 d26 d27",
  "    d34 d35 d36 d37 d45 d46 d47 d56 d57 d67",
  "  );",
  "",
  "  p1 = 1 / (1 + EXP(t1));",
  "  p2 = 1 / (1 + EXP(t2));",
  "  p3 = 1 / (1 + EXP(t3));",
  "  p4 = 1 / (1 + EXP(t4));",
  "  p5 = 1 / (1 + EXP(t5));",
  "  p6 = 1 / (1 + EXP(t6));",
  "  p7 = 1 / (1 + EXP(t7));",
  "",
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
  "BCH_7_clases_retorno_paso2.inp"
)

writeLines(
  sintaxis,
  con = ruta_inp,
  useBytes = TRUE
)

# ------------------------------------------------------------------------------
# 8. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "PASO 2 DEL BCH MANUAL PREPARADO",
  "",
  "Base completa: 3,660 registros.",
  "Dominio EUA: 3,259.",
  "Retorno conocido: 3,237.",
  "Retorno no especificado: 22.",
  "",
  "El modelo estimará:",
  "- probabilidad corregida de retorno en cada una de las siete clases;",
  "- prueba global de igualdad de probabilidades, 6 grados de libertad;",
  "- 21 diferencias absolutas entre pares de clases;",
  "- intervalos de confianza de 95 por ciento.",
  "",
  "Archivos:",
  "- 08_mplus/bch_7_clases_retorno_paso2.dat",
  "- 08_mplus/bch_7_clases_retorno_paso2_revision.csv",
  "- 08_mplus/BCH_7_clases_retorno_paso2.inp"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_11C_BCH_paso2.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")