# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 05: Recodificación definitiva de indicadores y exportación para Mplus
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

# ------------------------------------------------------------------------------
# 2. Rutas e importación
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_mplus <- "08_mplus"
ruta_documentacion <- "09_documentacion"

dir.create(ruta_mplus, recursive = TRUE, showWarnings = FALSE)

ruta_base <- file.path(
  ruta_procesados,
  "base_eua_lca.rds"
)

if (!file.exists(ruta_base)) {
  stop(
    "No se encontró base_eua_lca.rds. Ejecute primero el script 03."
  )
}

base_eua <- readRDS(ruta_base)

if (nrow(base_eua) != 3259L) {
  stop(
    paste0(
      "La base contiene ",
      nrow(base_eua),
      " registros; se esperaban 3,259."
    )
  )
}

# ------------------------------------------------------------------------------
# 3. Recodificación de indicadores
# ------------------------------------------------------------------------------

base_lca <- base_eua |>
  mutate(
    id_lca = row_number(),
    
    # 1. Edad al emigrar: ordinal.
    # 1 = 0-17; 2 = 18-29; 3 = 30-59; 4 = 60 y más.
    # Código 9 = edad no especificada -> faltante Mplus (-999).
    edad_lca = case_when(
      p4_8_ag2 %in% c("1", "2", "3", "4") ~ as.integer(p4_8_ag2),
      p4_8_ag2 == "9" ~ -999L,
      TRUE ~ -999L
    ),
    
    # 2. Parentesco: nominal.
    # Se conservan las seis categorías generales.
    # El código original 8 se renumera como 6 para obtener categorías contiguas.
    parentesco_lca = case_when(
      p4_7 == "1" ~ 1L,
      p4_7 == "2" ~ 2L,
      p4_7 == "3" ~ 3L,
      p4_7 == "4" ~ 4L,
      p4_7 == "5" ~ 5L,
      p4_7 == "8" ~ 6L,
      TRUE ~ -999L
    ),
    
    # 3. Lugar de nacimiento: nominal.
    # 1 = misma entidad; 2 = otra entidad mexicana;
    # 3 = fuera de México (EUA u otro país).
    nacimiento_lca = case_when(
      p4_5 == "1" ~ 1L,
      p4_5 == "2" ~ 2L,
      p4_5 %in% c("3", "4") ~ 3L,
      TRUE ~ -999L
    ),
    
    # 4. Motivo de emigración: nominal.
    # Se conservan los cinco motivos principales.
    # Inseguridad (6) se agrupa con otra causa (9) por su baja frecuencia.
    motivo_lca = case_when(
      p4_14 == "1" ~ 1L,                 # Buscar trabajo
      p4_14 == "2" ~ 2L,                 # Cambio u oferta de trabajo
      p4_14 == "3" ~ 3L,                 # Reunirse con la familia
      p4_14 == "4" ~ 4L,                 # Matrimonio o unión
      p4_14 == "5" ~ 5L,                 # Estudiar
      p4_14 %in% c("6", "9") ~ 6L,       # Inseguridad u otra causa
      TRUE ~ -999L
    ),
    
    # 5. Documento de ingreso: nominal.
    # 1 = residencia/trabajo/ciudadanía;
    # 2 = visa temporal (turista o estudiante);
    # 3 = otro documento legal;
    # 4 = ningún documento;
    # 9 = no sabe -> faltante.
    documento_lca = case_when(
      p4_13 %in% c("1", "2", "5") ~ 1L,
      p4_13 %in% c("3", "4") ~ 2L,
      p4_13 == "6" ~ 3L,
      p4_13 == "7" ~ 4L,
      p4_13 == "9" ~ -999L,
      TRUE ~ -999L
    ),
    
    # 6. Estado de destino en EUA: nominal.
    # Se conservan las siete categorías oficiales.
    destino_lca = case_when(
      p4_12 %in% as.character(1:7) ~ as.integer(p4_12),
      TRUE ~ -999L
    ),
    
    # Variables técnicas del diseño.
    peso_lca = as.numeric(fac_hog_num),
    estrato_lca = as.integer(est_dis_num),
    upm_lca = as.integer(upm_dis_num)
  )

# ------------------------------------------------------------------------------
# 4. Validaciones de recodificación
# ------------------------------------------------------------------------------

variables_lca <- c(
  "edad_lca",
  "parentesco_lca",
  "nacimiento_lca",
  "motivo_lca",
  "documento_lca",
  "destino_lca"
)

categorias_esperadas <- list(
  edad_lca = c(-999L, 1:4),
  parentesco_lca = 1:6,
  nacimiento_lca = 1:3,
  motivo_lca = 1:6,
  documento_lca = c(-999L, 1:4),
  destino_lca = 1:7
)

for (variable in variables_lca) {
  
  observadas <- sort(
    unique(
      base_lca[[variable]]
    )
  )
  
  no_permitidas <- setdiff(
    observadas,
    categorias_esperadas[[variable]]
  )
  
  if (length(no_permitidas) > 0) {
    stop(
      paste0(
        "La variable ",
        variable,
        " contiene categorías no permitidas: ",
        paste(no_permitidas, collapse = ", ")
      )
    )
  }
}

if (
  anyNA(base_lca$peso_lca) ||
  any(base_lca$peso_lca <= 0)
) {
  stop("Se detectaron ponderadores faltantes o no positivos.")
}

if (
  anyNA(base_lca$estrato_lca) ||
  anyNA(base_lca$upm_lca)
) {
  stop("Se detectaron estratos o UPM faltantes.")
}

if (n_distinct(base_lca$id_lca) != nrow(base_lca)) {
  stop("id_lca no es único.")
}

# ------------------------------------------------------------------------------
# 5. Tabla de distribución recodificada
# ------------------------------------------------------------------------------

auditar_recodificada <- function(datos, variable) {
  
  datos |>
    count(
      categoria = .data[[variable]],
      name = "n"
    ) |>
    mutate(
      variable = variable,
      porcentaje_no_ponderado = 100 * n / sum(n),
      es_faltante_mplus = categoria == -999L
    ) |>
    select(
      variable,
      categoria,
      n,
      porcentaje_no_ponderado,
      es_faltante_mplus
    )
}

distribucion_recodificada <- lapply(
  variables_lca,
  function(variable) {
    auditar_recodificada(
      base_lca,
      variable
    )
  }
) |>
  bind_rows()

print(
  distribucion_recodificada,
  n = Inf
)

write_csv(
  distribucion_recodificada,
  file.path(
    ruta_tablas,
    "distribucion_indicadores_lca_recodificados.csv"
  )
)

# Número de indicadores disponibles por persona.
base_lca <- base_lca |>
  mutate(
    indicadores_disponibles = rowSums(
      across(
        all_of(variables_lca),
        ~ .x != -999L
      )
    )
  )

disponibilidad <- base_lca |>
  count(
    indicadores_disponibles,
    name = "n"
  ) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  )

print(disponibilidad)

write_csv(
  disponibilidad,
  file.path(
    ruta_tablas,
    "disponibilidad_indicadores_lca.csv"
  )
)

if (any(base_lca$indicadores_disponibles == 0)) {
  stop(
    "Hay personas sin ningún indicador disponible; deben revisarse."
  )
}

# ------------------------------------------------------------------------------
# 6. Diccionario definitivo
# ------------------------------------------------------------------------------

diccionario_definitivo <- tribble(
  ~variable,          ~tipo_mplus,   ~categoria, ~etiqueta,
  "edad_lca",         "CATEGORICAL",  1L,         "0 a 17 años",
  "edad_lca",         "CATEGORICAL",  2L,         "18 a 29 años",
  "edad_lca",         "CATEGORICAL",  3L,         "30 a 59 años",
  "edad_lca",         "CATEGORICAL",  4L,         "60 años y más",
  "edad_lca",         "CATEGORICAL", -999L,       "Edad no especificada: faltante",
  
  "parentesco_lca",   "NOMINAL",      1L,         "Código general de parentesco 1",
  "parentesco_lca",   "NOMINAL",      2L,         "Código general de parentesco 2",
  "parentesco_lca",   "NOMINAL",      3L,         "Código general de parentesco 3",
  "parentesco_lca",   "NOMINAL",      4L,         "Código general de parentesco 4",
  "parentesco_lca",   "NOMINAL",      5L,         "Código general de parentesco 5",
  "parentesco_lca",   "NOMINAL",      6L,         "Código original 8 de parentesco",
  
  "nacimiento_lca",   "NOMINAL",      1L,         "Nació en la misma entidad",
  "nacimiento_lca",   "NOMINAL",      2L,         "Nació en otra entidad mexicana",
  "nacimiento_lca",   "NOMINAL",      3L,         "Nació fuera de México",
  
  "motivo_lca",       "NOMINAL",      1L,         "Buscar trabajo",
  "motivo_lca",       "NOMINAL",      2L,         "Cambio u oferta de trabajo",
  "motivo_lca",       "NOMINAL",      3L,         "Reunirse con la familia",
  "motivo_lca",       "NOMINAL",      4L,         "Matrimonio o unión",
  "motivo_lca",       "NOMINAL",      5L,         "Estudiar",
  "motivo_lca",       "NOMINAL",      6L,         "Inseguridad u otra causa",
  
  "documento_lca",    "NOMINAL",      1L,         "Residencia, trabajo o ciudadanía",
  "documento_lca",    "NOMINAL",      2L,         "Visa temporal",
  "documento_lca",    "NOMINAL",      3L,         "Otro documento",
  "documento_lca",    "NOMINAL",      4L,         "Ningún documento",
  "documento_lca",    "NOMINAL",     -999L,       "No sabe: faltante",
  
  "destino_lca",      "NOMINAL",      1L,         "California",
  "destino_lca",      "NOMINAL",      2L,         "Texas",
  "destino_lca",      "NOMINAL",      3L,         "Florida",
  "destino_lca",      "NOMINAL",      4L,         "Arizona",
  "destino_lca",      "NOMINAL",      5L,         "Nueva York",
  "destino_lca",      "NOMINAL",      6L,         "Illinois",
  "destino_lca",      "NOMINAL",      7L,         "Otro estado"
)

write_csv(
  diccionario_definitivo,
  file.path(
    ruta_tablas,
    "diccionario_definitivo_indicadores_lca.csv"
  )
)

# ------------------------------------------------------------------------------
# 7. Guardar base completa recodificada
# ------------------------------------------------------------------------------

saveRDS(
  base_lca,
  file.path(
    ruta_procesados,
    "base_eua_lca_recodificada.rds"
  )
)

# Archivo de correspondencia entre ID numérico y llave original.
mapa_id <- base_lca |>
  select(
    id_lca,
    llave_mig
  )

write_csv(
  mapa_id,
  file.path(
    ruta_mplus,
    "mapa_id_lca_llave_mig.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Exportar archivo para Mplus
# ------------------------------------------------------------------------------

base_mplus <- base_lca |>
  select(
    id_lca,
    edad_lca,
    parentesco_lca,
    nacimiento_lca,
    motivo_lca,
    documento_lca,
    destino_lca,
    peso_lca,
    estrato_lca,
    upm_lca
  )

ruta_dat <- file.path(
  ruta_mplus,
  "base_lca_enadid.dat"
)

write.table(
  base_mplus,
  file = ruta_dat,
  sep = " ",
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  na = "-999",
  dec = "."
)

# Guardar una versión CSV con encabezados solo para revisión humana.
write_csv(
  base_mplus,
  file.path(
    ruta_mplus,
    "base_lca_enadid_revision.csv"
  )
)

# Verificación de filas exportadas.
lineas_dat <- length(
  readLines(
    ruta_dat,
    warn = FALSE
  )
)

if (lineas_dat != 3259L) {
  stop(
    paste0(
      "El archivo DAT contiene ",
      lineas_dat,
      " líneas; se esperaban 3,259."
    )
  )
}

# ------------------------------------------------------------------------------
# 9. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "RECODIFICACIÓN Y EXPORTACIÓN PARA MPLUS TERMINADAS CORRECTAMENTE",
  "",
  paste0("Registros exportados: ", nrow(base_mplus), "."),
  paste0("Variables exportadas: ", ncol(base_mplus), "."),
  "",
  "Indicadores:",
  "- edad_lca: 4 categorías ordinales; código 9 tratado como faltante.",
  "- parentesco_lca: 6 categorías nominales.",
  "- nacimiento_lca: 3 categorías; EUA y otros países agrupados.",
  "- motivo_lca: 6 categorías; inseguridad agrupada con otra causa.",
  "- documento_lca: 4 categorías; no sabe tratado como faltante.",
  "- destino_lca: 7 categorías nominales.",
  "",
  "Archivos principales:",
  "- 04_datos_procesados/base_eua_lca_recodificada.rds",
  "- 08_mplus/base_lca_enadid.dat",
  "- 08_mplus/base_lca_enadid_revision.csv",
  "- 08_mplus/mapa_id_lca_llave_mig.csv",
  "- 05_tablas/diccionario_definitivo_indicadores_lca.csv"
)

writeLines(
  informe,
  con = file.path(
    ruta_documentacion,
    "informe_script_05_recodificacion_lca.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")