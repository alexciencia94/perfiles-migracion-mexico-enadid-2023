# ==============================================================================
# PROYECTO: Perfiles migratorios México–Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 04 (CORREGIDO): Auditoría de indicadores candidatos para LCA
# ==============================================================================

rm(list = ls())
gc()
options(stringsAsFactors = FALSE, scipen = 999)

paquetes <- c("dplyr", "readr", "tibble")
faltantes <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  stop("Faltan estos paquetes: ", paste(faltantes, collapse = ", "))
}
library(dplyr)
library(readr)
library(tibble)

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_documentacion <- "09_documentacion"
ruta_base <- file.path(ruta_procesados, "base_eua_lca.rds")

if (!file.exists(ruta_base)) {
  stop("No se encontró base_eua_lca.rds. Ejecute primero el script 03.")
}

base_eua <- readRDS(ruta_base)
if (nrow(base_eua) != 3259L) {
  stop("La base no contiene los 3,259 registros esperados.")
}

cat("Base importada correctamente:", nrow(base_eua),
    "migrantes hacia Estados Unidos.\n\n")

# Las variables terminadas en 'c' son subcódigos detallados, no etiquetas.
indicadores <- tribble(
  ~orden, ~nombre_analitico, ~descripcion, ~variable,
  1L, "edad_emigrar", "Grupo de edad al emigrar", "p4_8_ag2",
  2L, "parentesco", "Parentesco general con la jefatura", "p4_7",
  3L, "lugar_nacimiento", "Lugar general de nacimiento", "p4_5",
  4L, "motivo_emigracion", "Motivo principal general de emigración", "p4_14",
  5L, "documento_ingreso", "Documento utilizado para ingresar a EUA", "p4_13",
  6L, "destino_eua", "Categoría general del destino en EUA", "p4_12"
)

subcodigos <- tribble(
  ~nombre_general, ~variable_general, ~nombre_detallado, ~variable_detallada,
  "parentesco", "p4_7", "parentesco_detallado", "p4_7c",
  "lugar_nacimiento", "p4_5", "nacimiento_detallado", "p4_5c",
  "motivo_emigracion", "p4_14", "motivo_detallado", "p4_14c",
  "destino_eua", "p4_12", "destino_detallado", "p4_12c"
)

variables_requeridas <- unique(c(
  indicadores$variable,
  subcodigos$variable_general,
  subcodigos$variable_detallada,
  "fac_hog_num"
))

faltan <- setdiff(variables_requeridas, names(base_eua))
if (length(faltan) > 0) {
  stop("Faltan variables: ", paste(faltan, collapse = ", "))
}

auditar_variable <- function(datos, nombre, descripcion, variable) {
  codigo <- trimws(as.character(datos[[variable]]))
  codigo[codigo == ""] <- NA_character_
  peso <- as.numeric(datos$fac_hog_num)
  
  tibble(codigo = codigo, peso = peso) |>
    group_by(codigo) |>
    summarise(
      n = n(),
      poblacion_expandida = sum(peso, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      nombre_analitico = nombre,
      descripcion = descripcion,
      variable = variable,
      porcentaje_no_ponderado = 100 * n / nrow(datos),
      porcentaje_ponderado = 100 * poblacion_expandida /
        sum(peso, na.rm = TRUE),
      es_na_real = is.na(codigo),
      categoria_n_menor_50 = !is.na(codigo) & n < 50,
      categoria_menor_1_por_ciento =
        !is.na(codigo) & porcentaje_no_ponderado < 1,
      requiere_revision = es_na_real |
        categoria_n_menor_50 |
        categoria_menor_1_por_ciento
    ) |>
    select(
      nombre_analitico, descripcion, variable, codigo, n,
      porcentaje_no_ponderado, poblacion_expandida,
      porcentaje_ponderado, es_na_real,
      categoria_n_menor_50,
      categoria_menor_1_por_ciento,
      requiere_revision
    ) |>
    arrange(is.na(codigo), suppressWarnings(as.numeric(codigo)), codigo)
}

auditoria_principales <- lapply(seq_len(nrow(indicadores)), function(i) {
  auditar_variable(
    base_eua,
    indicadores$nombre_analitico[i],
    indicadores$descripcion[i],
    indicadores$variable[i]
  )
}) |>
  bind_rows()

print(auditoria_principales, n = Inf)

write_csv(
  auditoria_principales,
  file.path(ruta_tablas,
            "auditoria_indicadores_principales_lca_corregida.csv")
)

resumen_principales <- auditoria_principales |>
  group_by(nombre_analitico, descripcion, variable) |>
  summarise(
    categorias_observadas = n_distinct(codigo[!is.na(codigo)]),
    registros_na_reales = sum(n[is.na(codigo)]),
    porcentaje_na_real = 100 * registros_na_reales / nrow(base_eua),
    categorias_n_menor_50 = sum(categoria_n_menor_50, na.rm = TRUE),
    categorias_menor_1_por_ciento =
      sum(categoria_menor_1_por_ciento, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(match(nombre_analitico, indicadores$nombre_analitico))

print(resumen_principales, n = Inf)

write_csv(
  resumen_principales,
  file.path(ruta_tablas,
            "resumen_indicadores_principales_lca_corregido.csv")
)

categorias_revisar <- auditoria_principales |>
  filter(requiere_revision) |>
  arrange(nombre_analitico, n)

print(categorias_revisar, n = Inf)

write_csv(
  categorias_revisar,
  file.path(ruta_tablas,
            "categorias_principales_lca_a_revisar_corregidas.csv")
)

# Auditoría separada de subcódigos detallados.
auditoria_subcodigos <- lapply(seq_len(nrow(subcodigos)), function(i) {
  general <- trimws(as.character(base_eua[[subcodigos$variable_general[i]]]))
  detalle <- trimws(as.character(base_eua[[subcodigos$variable_detallada[i]]]))
  general[general == ""] <- NA_character_
  detalle[detalle == ""] <- NA_character_
  
  tibble(
    nombre_general = subcodigos$nombre_general[i],
    variable_general = subcodigos$variable_general[i],
    nombre_detallado = subcodigos$nombre_detallado[i],
    variable_detallada = subcodigos$variable_detallada[i],
    codigo_general = general,
    codigo_detallado = detalle,
    peso = as.numeric(base_eua$fac_hog_num)
  ) |>
    group_by(
      nombre_general, variable_general,
      nombre_detallado, variable_detallada,
      codigo_general, codigo_detallado
    ) |>
    summarise(
      n = n(),
      poblacion_expandida = sum(peso, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(nombre_general, codigo_general) |>
    mutate(
      porcentaje_dentro_categoria_general = 100 * n / sum(n)
    ) |>
    ungroup()
}) |>
  bind_rows()

write_csv(
  auditoria_subcodigos,
  file.path(ruta_tablas,
            "auditoria_subcodigos_detallados_lca.csv")
)

# Cruces de los seis indicadores principales.
pares <- combn(indicadores$variable, 2, simplify = FALSE)

auditoria_cruces <- lapply(pares, function(par) {
  x <- trimws(as.character(base_eua[[par[1]]]))
  y <- trimws(as.character(base_eua[[par[2]]]))
  x[x == ""] <- NA_character_
  y[y == ""] <- NA_character_
  validos <- !is.na(x) & !is.na(y)
  tabla <- table(x[validos], y[validos])
  
  tibble(
    variable_1 = par[1],
    variable_2 = par[2],
    casos_validos = sum(validos),
    niveles_variable_1 = nrow(tabla),
    niveles_variable_2 = ncol(tabla),
    celdas_totales = length(tabla),
    celdas_con_cero = sum(tabla == 0),
    celdas_con_menos_5 = sum(tabla < 5),
    porcentaje_celdas_menos_5 = 100 * sum(tabla < 5) / length(tabla),
    frecuencia_minima = min(tabla),
    frecuencia_maxima = max(tabla)
  )
}) |>
  bind_rows()

print(auditoria_cruces, n = Inf)

write_csv(
  auditoria_cruces,
  file.path(ruta_tablas,
            "auditoria_cruces_indicadores_lca_corregida.csv")
)

write_csv(
  indicadores,
  file.path(ruta_tablas, "diccionario_indicadores_lca_corregido.csv")
)

informe <- c(
  "AUDITORÍA CORREGIDA DE INDICADORES PARA LCA TERMINADA",
  "",
  paste0("Muestra auditada: ", nrow(base_eua), "."),
  paste0("Indicadores principales: ", nrow(indicadores), "."),
  paste0("Categorías principales para revisión: ", nrow(categorias_revisar), "."),
  "",
  "Corrección aplicada:",
  "- Las variables terminadas en c se trataron como subcódigos detallados.",
  "- Las categorías principales se contaron solo por su código general."
)

writeLines(
  informe,
  file.path(ruta_documentacion, "informe_script_04_corregido.txt")
)

cat("\n============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")