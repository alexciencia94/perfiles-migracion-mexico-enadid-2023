# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 12B: Tiempo hasta el retorno - base, curvas y modelo de Cox
# ==============================================================================

rm(list = ls())
gc()

options(
  stringsAsFactors = FALSE,
  scipen = 999,
  survey.lonely.psu = "adjust"
)

# ------------------------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------------------------

paquetes <- c(
  "dplyr",
  "readr",
  "tibble",
  "survival",
  "survey",
  "ggplot2"
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

library(dplyr)
library(readr)
library(tibble)
library(survival)
library(survey)
library(ggplot2)

# ------------------------------------------------------------------------------
# 2. Rutas e importacion
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_figuras <- "06_figuras"
ruta_modelos <- "07_modelos"
ruta_documentacion <- "09_documentacion"

dir.create(ruta_tablas, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_figuras, recursive = TRUE, showWarnings = FALSE)
dir.create(ruta_modelos, recursive = TRUE, showWarnings = FALSE)

ruta_base <- file.path(
  ruta_procesados,
  "base_temporal_auditoria.rds"
)

if (!file.exists(ruta_base)) {
  stop(
    paste0(
      "No se encontro ",
      ruta_base,
      ". Ejecute primero el script 12A."
    )
  )
}

base <- readRDS(ruta_base)

if (nrow(base) != 3259L) {
  stop(
    paste0(
      "La base contiene ",
      nrow(base),
      " registros; se esperaban 3,259."
    )
  )
}

variables_requeridas <- c(
  "id",
  "llave_mig",
  "evento_retorno",
  "tpo_mig_meses",
  "duracion_fechas_meses",
  "indice_salida",
  "clase_modal",
  "fac_hog_num",
  "est_dis_num",
  "upm_dis_num"
)

variables_faltantes <- setdiff(
  variables_requeridas,
  names(base)
)

if (length(variables_faltantes) > 0) {
  stop(
    paste0(
      "Faltan estas variables: ",
      paste(variables_faltantes, collapse = ", ")
    )
  )
}

# ------------------------------------------------------------------------------
# 3. Construccion del tiempo del evento y de la censura
# ------------------------------------------------------------------------------

indice_censura_septiembre <- 2023L * 12L + 9L
indice_censura_octubre <- 2023L * 12L + 10L

base_supervivencia <- base |>
  mutate(
    # Para quienes retornaron se prioriza TPO_MIG, validado contra las fechas.
    # Si TPO_MIG no es utilizable, se emplea la diferencia entre fechas.
    tiempo_evento_meses = case_when(
      evento_retorno == 1L &
        !is.na(tpo_mig_meses) &
        tpo_mig_meses >= 0L &
        tpo_mig_meses <= 72L ~
        as.numeric(tpo_mig_meses),
      
      evento_retorno == 1L &
        !is.na(duracion_fechas_meses) &
        duracion_fechas_meses >= 0L &
        duracion_fechas_meses <= 72L ~
        as.numeric(duracion_fechas_meses),
      
      TRUE ~ NA_real_
    ),
    
    fuente_tiempo_evento = case_when(
      evento_retorno == 1L &
        !is.na(tpo_mig_meses) &
        tpo_mig_meses >= 0L &
        tpo_mig_meses <= 72L ~
        "TPO_MIG",
      
      evento_retorno == 1L &
        !is.na(duracion_fechas_meses) &
        duracion_fechas_meses >= 0L &
        duracion_fechas_meses <= 72L ~
        "Diferencia entre fechas",
      
      evento_retorno == 1L ~
        "Sin tiempo valido",
      
      TRUE ~
        "No aplica"
    ),
    
    # Escenario principal: censura administrativa en octubre de 2023.
    tiempo_octubre_meses = case_when(
      evento_retorno == 1L ~
        tiempo_evento_meses,
      
      evento_retorno == 0L &
        !is.na(indice_salida) ~
        as.numeric(
          indice_censura_octubre -
            indice_salida
        ),
      
      TRUE ~ NA_real_
    ),
    
    # Sensibilidad: censura administrativa en septiembre de 2023.
    tiempo_septiembre_meses = case_when(
      evento_retorno == 1L ~
        tiempo_evento_meses,
      
      evento_retorno == 0L &
        !is.na(indice_salida) ~
        as.numeric(
          indice_censura_septiembre -
            indice_salida
        ),
      
      TRUE ~ NA_real_
    ),
    
    # Los eventos o censuras en el mismo mes se ubican a mitad del intervalo.
    tiempo_octubre_analisis = case_when(
      !is.na(tiempo_octubre_meses) &
        tiempo_octubre_meses >= 0 ~
        pmax(
          tiempo_octubre_meses,
          0.5
        ),
      TRUE ~ NA_real_
    ),
    
    tiempo_septiembre_analisis = case_when(
      !is.na(tiempo_septiembre_meses) &
        tiempo_septiembre_meses >= 0 ~
        pmax(
          tiempo_septiembre_meses,
          0.5
        ),
      TRUE ~ NA_real_
    ),
    
    perfil_num = factor(
      clase_modal,
      levels = 1:7,
      labels = paste0(
        "Clase ",
        1:7
      )
    )
  )

# ------------------------------------------------------------------------------
# 4. Auditoria de la base de supervivencia
# ------------------------------------------------------------------------------

auditoria_fuente_evento <- base_supervivencia |>
  filter(
    evento_retorno == 1L
  ) |>
  count(
    fuente_tiempo_evento,
    name = "n"
  ) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  )

print(auditoria_fuente_evento)

auditoria_inclusion <- tibble(
  escenario = c(
    "Octubre 2023",
    "Septiembre 2023"
  ),
  n_incluido = c(
    sum(
      !is.na(base_supervivencia$evento_retorno) &
        !is.na(base_supervivencia$tiempo_octubre_analisis)
    ),
    sum(
      !is.na(base_supervivencia$evento_retorno) &
        !is.na(base_supervivencia$tiempo_septiembre_analisis)
    )
  ),
  eventos_incluidos = c(
    sum(
      base_supervivencia$evento_retorno == 1L &
        !is.na(base_supervivencia$tiempo_octubre_analisis),
      na.rm = TRUE
    ),
    sum(
      base_supervivencia$evento_retorno == 1L &
        !is.na(base_supervivencia$tiempo_septiembre_analisis),
      na.rm = TRUE
    )
  ),
  censurados_incluidos = c(
    sum(
      base_supervivencia$evento_retorno == 0L &
        !is.na(base_supervivencia$tiempo_octubre_analisis),
      na.rm = TRUE
    ),
    sum(
      base_supervivencia$evento_retorno == 0L &
        !is.na(base_supervivencia$tiempo_septiembre_analisis),
      na.rm = TRUE
    )
  )
) |>
  mutate(
    excluidos_retorno_conocido =
      3237L -
      n_incluido
  )

print(auditoria_inclusion)

write_csv(
  auditoria_fuente_evento,
  file.path(
    ruta_tablas,
    "fuente_tiempo_evento_retorno.csv"
  )
)

write_csv(
  auditoria_inclusion,
  file.path(
    ruta_tablas,
    "auditoria_inclusion_supervivencia.csv"
  )
)

# ------------------------------------------------------------------------------
# 5. Bases definitivas de cada escenario
# ------------------------------------------------------------------------------

base_octubre <- base_supervivencia |>
  filter(
    !is.na(evento_retorno),
    !is.na(tiempo_octubre_analisis)
  )

base_septiembre <- base_supervivencia |>
  filter(
    !is.na(evento_retorno),
    !is.na(tiempo_septiembre_analisis)
  )

if (
  any(base_octubre$tiempo_octubre_analisis <= 0) ||
  any(base_septiembre$tiempo_septiembre_analisis <= 0)
) {
  stop(
    "Se detectaron tiempos iguales o menores que cero."
  )
}

if (
  anyNA(base_octubre$fac_hog_num) ||
  any(base_octubre$fac_hog_num <= 0)
) {
  stop(
    "Se detectaron ponderadores invalidos."
  )
}

saveRDS(
  base_octubre,
  file.path(
    ruta_procesados,
    "base_supervivencia_octubre2023.rds"
  )
)

saveRDS(
  base_septiembre,
  file.path(
    ruta_procesados,
    "base_supervivencia_septiembre2023.rds"
  )
)

# ------------------------------------------------------------------------------
# 6. Diseno complejo
# ------------------------------------------------------------------------------

diseno_octubre <- svydesign(
  ids = ~upm_dis_num,
  strata = ~est_dis_num,
  weights = ~fac_hog_num,
  nest = TRUE,
  data = base_octubre
)

diseno_septiembre <- svydesign(
  ids = ~upm_dis_num,
  strata = ~est_dis_num,
  weights = ~fac_hog_num,
  nest = TRUE,
  data = base_septiembre
)

# La clase 4 es la referencia porque presenta el menor retorno.
diseno_octubre <- update(
  diseno_octubre,
  perfil_num = relevel(
    perfil_num,
    ref = "Clase 4"
  )
)

diseno_septiembre <- update(
  diseno_septiembre,
  perfil_num = relevel(
    perfil_num,
    ref = "Clase 4"
  )
)

# ------------------------------------------------------------------------------
# 7. Modelos de Cox ponderados por el diseño
# ------------------------------------------------------------------------------

modelo_octubre <- svycoxph(
  Surv(
    tiempo_octubre_analisis,
    evento_retorno
  ) ~ perfil_num,
  design = diseno_octubre
)

modelo_septiembre <- svycoxph(
  Surv(
    tiempo_septiembre_analisis,
    evento_retorno
  ) ~ perfil_num,
  design = diseno_septiembre
)

extraer_modelo <- function(modelo, escenario) {
  
  beta <- coef(modelo)
  se <- sqrt(
    diag(
      vcov(modelo)
    )
  )
  
  z <- beta / se
  p <- 2 * pnorm(
    abs(z),
    lower.tail = FALSE
  )
  
  tibble(
    escenario = escenario,
    contraste = names(beta),
    log_hazard_ratio = as.numeric(beta),
    error_estandar = as.numeric(se),
    hazard_ratio = exp(beta),
    ic95_inferior = exp(
      beta -
        qnorm(0.975) * se
    ),
    ic95_superior = exp(
      beta +
        qnorm(0.975) * se
    ),
    z = as.numeric(z),
    p_valor = as.numeric(p)
  )
}

resultados_cox <- bind_rows(
  extraer_modelo(
    modelo_octubre,
    "Censura en octubre de 2023"
  ),
  extraer_modelo(
    modelo_septiembre,
    "Censura en septiembre de 2023"
  )
)

print(resultados_cox)

write_csv(
  resultados_cox,
  file.path(
    ruta_tablas,
    "modelo_cox_perfiles_retorno.csv"
  )
)

# Pruebas globales del perfil.
prueba_global_octubre <- regTermTest(
  modelo_octubre,
  ~perfil_num
)

prueba_global_septiembre <- regTermTest(
  modelo_septiembre,
  ~perfil_num
)

extraer_prueba_global <- function(prueba, escenario) {
  
  tibble(
    escenario = escenario,
    estadistico = as.numeric(
      prueba$chisq
    ),
    grados_libertad = as.numeric(
      prueba$df
    ),
    p_valor = as.numeric(
      prueba$p
    )
  )
}

pruebas_globales <- bind_rows(
  extraer_prueba_global(
    prueba_global_octubre,
    "Censura en octubre de 2023"
  ),
  extraer_prueba_global(
    prueba_global_septiembre,
    "Censura en septiembre de 2023"
  )
)

print(pruebas_globales)

write_csv(
  pruebas_globales,
  file.path(
    ruta_tablas,
    "prueba_global_cox_perfiles.csv"
  )
)

saveRDS(
  modelo_octubre,
  file.path(
    ruta_modelos,
    "modelo_cox_octubre2023.rds"
  )
)

saveRDS(
  modelo_septiembre,
  file.path(
    ruta_modelos,
    "modelo_cox_septiembre2023.rds"
  )
)

# ------------------------------------------------------------------------------
# 8. Curvas ponderadas descriptivas de retorno acumulado
# ------------------------------------------------------------------------------

ajuste_curvas <- survfit(
  Surv(
    tiempo_octubre_analisis,
    evento_retorno
  ) ~ perfil_num,
  data = base_octubre,
  weights = fac_hog_num
)

resumen_curvas <- summary(
  ajuste_curvas
)

curvas <- tibble(
  tiempo_meses = resumen_curvas$time,
  supervivencia_sin_retorno = resumen_curvas$surv,
  estrato = as.character(
    resumen_curvas$strata
  )
) |>
  mutate(
    clase = sub(
      "perfil_num=",
      "",
      estrato,
      fixed = TRUE
    ),
    retorno_acumulado = 1 -
      supervivencia_sin_retorno
  ) |>
  select(
    clase,
    tiempo_meses,
    supervivencia_sin_retorno,
    retorno_acumulado
  )

write_csv(
  curvas,
  file.path(
    ruta_tablas,
    "curvas_ponderadas_retorno_acumulado.csv"
  )
)

# Probabilidad descriptiva acumulada en puntos clínicamente interpretables.
tiempos_resumen <- c(
  6,
  12,
  24,
  36,
  48,
  60
)

resumen_tiempos <- summary(
  ajuste_curvas,
  times = tiempos_resumen,
  extend = TRUE
)

tabla_tiempos <- tibble(
  clase = sub(
    "perfil_num=",
    "",
    as.character(
      resumen_tiempos$strata
    ),
    fixed = TRUE
  ),
  tiempo_meses = resumen_tiempos$time,
  supervivencia_sin_retorno =
    resumen_tiempos$surv,
  retorno_acumulado =
    1 -
    resumen_tiempos$surv
)

write_csv(
  tabla_tiempos,
  file.path(
    ruta_tablas,
    "retorno_acumulado_6_12_24_36_48_60_meses.csv"
  )
)

figura <- ggplot(
  curvas,
  aes(
    x = tiempo_meses,
    y = retorno_acumulado,
    group = clase,
    linetype = clase
  )
) +
  geom_step(
    linewidth = 0.8
  ) +
  scale_y_continuous(
    labels = scales::label_percent(
      accuracy = 1
    ),
    limits = c(
      0,
      1
    )
  ) +
  labs(
    x = "Meses desde la salida",
    y = "Probabilidad acumulada de retorno",
    linetype = "Perfil",
    title = "Retorno acumulado a Mexico por perfil migratorio",
    subtitle = "Estimacion descriptiva ponderada; censura administrativa en octubre de 2023"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    legend.position = "right"
  )

ggsave(
  filename = file.path(
    ruta_figuras,
    "curvas_retorno_acumulado_7_perfiles.png"
  ),
  plot = figura,
  width = 10,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------------------------
# 9. Informe
# ------------------------------------------------------------------------------

informe <- c(
  "ANALISIS DE TIEMPO HASTA EL RETORNO TERMINADO",
  "",
  paste0(
    "Escenario principal, octubre de 2023: n=",
    nrow(base_octubre),
    "; eventos=",
    sum(base_octubre$evento_retorno),
    "."
  ),
  paste0(
    "Sensibilidad, septiembre de 2023: n=",
    nrow(base_septiembre),
    "; eventos=",
    sum(base_septiembre$evento_retorno),
    "."
  ),
  "",
  "Modelo inferencial:",
  "- Cox ponderado con estratos, UPM y FAC_HOG.",
  "- Clase 4 como categoria de referencia.",
  "",
  "Advertencia metodologica:",
  "Este analisis utiliza la clase modal y no incorpora directamente el error",
  "de clasificacion de la LCA. Debe presentarse como analisis secundario o",
  "de sensibilidad, complementario al BCH del retorno.",
  "",
  "Archivos principales:",
  "- 05_tablas/fuente_tiempo_evento_retorno.csv",
  "- 05_tablas/auditoria_inclusion_supervivencia.csv",
  "- 05_tablas/modelo_cox_perfiles_retorno.csv",
  "- 05_tablas/prueba_global_cox_perfiles.csv",
  "- 05_tablas/retorno_acumulado_6_12_24_36_48_60_meses.csv",
  "- 06_figuras/curvas_retorno_acumulado_7_perfiles.png",
  "- 07_modelos/modelo_cox_octubre2023.rds",
  "- 07_modelos/modelo_cox_septiembre2023.rds"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_12B_supervivencia.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")