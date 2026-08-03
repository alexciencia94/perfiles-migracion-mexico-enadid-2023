# ==============================================================================
# PROYECTO: Perfiles migratorios Mexico-Estados Unidos
# FUENTE: ENADID 2023
# SCRIPT 12C: Prueba global, sensibilidad y proporcionalidad de riesgos
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
  "survey"
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

# ------------------------------------------------------------------------------
# 2. Rutas e importacion
# ------------------------------------------------------------------------------

ruta_procesados <- "04_datos_procesados"
ruta_tablas <- "05_tablas"
ruta_figuras <- "06_figuras"
ruta_modelos <- "07_modelos"
ruta_documentacion <- "09_documentacion"

ruta_modelo_octubre <- file.path(
  ruta_modelos,
  "modelo_cox_octubre2023.rds"
)

ruta_modelo_septiembre <- file.path(
  ruta_modelos,
  "modelo_cox_septiembre2023.rds"
)

ruta_base_octubre <- file.path(
  ruta_procesados,
  "base_supervivencia_octubre2023.rds"
)

ruta_base_septiembre <- file.path(
  ruta_procesados,
  "base_supervivencia_septiembre2023.rds"
)

rutas_requeridas <- c(
  ruta_modelo_octubre,
  ruta_modelo_septiembre,
  ruta_base_octubre,
  ruta_base_septiembre
)

faltan_archivos <- rutas_requeridas[
  !file.exists(rutas_requeridas)
]

if (length(faltan_archivos) > 0) {
  stop(
    paste0(
      "Faltan estos archivos:\n",
      paste(
        faltan_archivos,
        collapse = "\n"
      ),
      "\nEjecute primero el script 12B."
    )
  )
}

modelo_octubre <- readRDS(
  ruta_modelo_octubre
)

modelo_septiembre <- readRDS(
  ruta_modelo_septiembre
)

base_octubre <- readRDS(
  ruta_base_octubre
)

base_septiembre <- readRDS(
  ruta_base_septiembre
)

# ------------------------------------------------------------------------------
# 3. Etiquetas
# ------------------------------------------------------------------------------

etiquetas_perfiles <- tibble(
  clase = c(
    "Clase 1",
    "Clase 2",
    "Clase 3",
    "Clase 4",
    "Clase 5",
    "Clase 6",
    "Clase 7"
  ),
  perfil = c(
    "Migracion laboral adulta sin documento",
    "Migracion laboral adulta documentada por oferta",
    "Migracion joven transnacional documentada",
    "Migracion laboral juvenil sin documento",
    "Migracion familiar de mayor edad",
    "Migracion juvenil documentada por oferta",
    "Migracion juvenil temporal por trabajo o estudio"
  )
)

write_csv(
  etiquetas_perfiles,
  file.path(
    ruta_tablas,
    "etiquetas_perfiles_supervivencia.csv"
  )
)

# ------------------------------------------------------------------------------
# 4. Prueba Wald global manual
# ------------------------------------------------------------------------------

# Se usa la matriz de varianza robusta del modelo svycoxph.
# H0: los seis coeficientes frente a la Clase 4 son simultaneamente cero.

prueba_wald_global <- function(
    modelo,
    escenario
) {
  
  beta <- coef(
    modelo
  )
  
  matriz_varianza <- vcov(
    modelo
  )
  
  if (
    length(beta) == 0L ||
    anyNA(beta) ||
    anyNA(matriz_varianza)
  ) {
    stop(
      paste0(
        "No se pudieron extraer coeficientes validos para ",
        escenario,
        "."
      )
    )
  }
  
  solucion <- tryCatch(
    qr.solve(
      matriz_varianza,
      beta
    ),
    error = function(e) {
      stop(
        paste0(
          "La matriz de varianza no pudo invertirse en ",
          escenario,
          ": ",
          conditionMessage(e)
        )
      )
    }
  )
  
  estadistico <- as.numeric(
    crossprod(
      beta,
      solucion
    )
  )
  
  grados_libertad <- length(
    beta
  )
  
  tibble(
    escenario = escenario,
    estadistico_wald = estadistico,
    grados_libertad = grados_libertad,
    p_valor = pchisq(
      estadistico,
      df = grados_libertad,
      lower.tail = FALSE
    )
  )
}

pruebas_globales <- bind_rows(
  prueba_wald_global(
    modelo_octubre,
    "Censura en octubre de 2023"
  ),
  prueba_wald_global(
    modelo_septiembre,
    "Censura en septiembre de 2023"
  )
)

print(pruebas_globales)

write_csv(
  pruebas_globales,
  file.path(
    ruta_tablas,
    "prueba_global_cox_perfiles_CORREGIDA.csv"
  )
)

# Guardar tambien la impresion oficial de regTermTest.
captura_regterm <- c(
  "OCTUBRE 2023",
  capture.output(
    print(
      regTermTest(
        modelo_octubre,
        ~perfil_num,
        df = Inf,
        method = "Wald"
      )
    )
  ),
  "",
  "SEPTIEMBRE 2023",
  capture.output(
    print(
      regTermTest(
        modelo_septiembre,
        ~perfil_num,
        df = Inf,
        method = "Wald"
      )
    )
  )
)

writeLines(
  captura_regterm,
  file.path(
    ruta_documentacion,
    "prueba_global_regTermTest_Cox.txt"
  )
)

# ------------------------------------------------------------------------------
# 5. Comparar los dos escenarios de censura
# ------------------------------------------------------------------------------

extraer_hr <- function(
    modelo,
    escenario
) {
  
  beta <- coef(
    modelo
  )
  
  se <- sqrt(
    diag(
      vcov(
        modelo
      )
    )
  )
  
  tibble(
    escenario = escenario,
    contraste = names(
      beta
    ),
    hazard_ratio = exp(
      beta
    ),
    ic95_inferior = exp(
      beta -
        qnorm(0.975) * se
    ),
    ic95_superior = exp(
      beta +
        qnorm(0.975) * se
    )
  )
}

hr_octubre <- extraer_hr(
  modelo_octubre,
  "Octubre"
)

hr_septiembre <- extraer_hr(
  modelo_septiembre,
  "Septiembre"
)

sensibilidad_hr <- hr_octubre |>
  select(
    contraste,
    hr_octubre = hazard_ratio,
    ic95_inferior_octubre =
      ic95_inferior,
    ic95_superior_octubre =
      ic95_superior
  ) |>
  inner_join(
    hr_septiembre |>
      select(
        contraste,
        hr_septiembre = hazard_ratio,
        ic95_inferior_septiembre =
          ic95_inferior,
        ic95_superior_septiembre =
          ic95_superior
      ),
    by = "contraste"
  ) |>
  mutate(
    diferencia_absoluta_hr =
      hr_septiembre -
      hr_octubre,
    diferencia_relativa_porcentaje =
      100 *
      (
        hr_septiembre -
          hr_octubre
      ) /
      hr_octubre
  )

print(sensibilidad_hr)

write_csv(
  sensibilidad_hr,
  file.path(
    ruta_tablas,
    "sensibilidad_HR_octubre_septiembre.csv"
  )
)

# ------------------------------------------------------------------------------
# 6. Diagnostico de proporcionalidad de riesgos
# ------------------------------------------------------------------------------

# cox.zph no incorpora por si mismo toda la estratificacion del diseno.
# Se usa como diagnostico secundario sobre un Cox ponderado, con varianza
# robusta por UPM. La inferencia principal de los HR sigue siendo svycoxph.

preparar_base_diagnostico <- function(
    datos
) {
  
  datos |>
    mutate(
      perfil_num = relevel(
        factor(
          perfil_num
        ),
        ref = "Clase 4"
      )
    )
}

base_diag_octubre <- preparar_base_diagnostico(
  base_octubre
)

base_diag_septiembre <- preparar_base_diagnostico(
  base_septiembre
)

modelo_diag_octubre <- coxph(
  Surv(
    tiempo_octubre_analisis,
    evento_retorno
  ) ~ perfil_num,
  data = base_diag_octubre,
  weights = fac_hog_num,
  cluster = upm_dis_num,
  robust = TRUE,
  x = TRUE,
  y = TRUE,
  model = TRUE
)

modelo_diag_septiembre <- coxph(
  Surv(
    tiempo_septiembre_analisis,
    evento_retorno
  ) ~ perfil_num,
  data = base_diag_septiembre,
  weights = fac_hog_num,
  cluster = upm_dis_num,
  robust = TRUE,
  x = TRUE,
  y = TRUE,
  model = TRUE
)

ph_octubre <- cox.zph(
  modelo_diag_octubre,
  transform = "km",
  terms = FALSE,
  global = TRUE
)

ph_septiembre <- cox.zph(
  modelo_diag_septiembre,
  transform = "km",
  terms = FALSE,
  global = TRUE
)

extraer_ph <- function(
    objeto,
    escenario
) {
  
  tabla <- as.data.frame(
    objeto$table
  )
  
  tabla$termino <- rownames(
    tabla
  )
  
  rownames(
    tabla
  ) <- NULL
  
  as_tibble(
    tabla
  ) |>
    transmute(
      escenario = escenario,
      termino = termino,
      chi_cuadrado = chisq,
      grados_libertad = df,
      p_valor = p
    )
}

tabla_ph <- bind_rows(
  extraer_ph(
    ph_octubre,
    "Censura en octubre de 2023"
  ),
  extraer_ph(
    ph_septiembre,
    "Censura en septiembre de 2023"
  )
)

print(tabla_ph)

write_csv(
  tabla_ph,
  file.path(
    ruta_tablas,
    "diagnostico_proporcionalidad_riesgos.csv"
  )
)

# Graficos de residuos de Schoenfeld.
png(
  filename = file.path(
    ruta_figuras,
    "diagnostico_PH_octubre2023.png"
  ),
  width = 3000,
  height = 2200,
  res = 300
)

par(
  mfrow = c(
    2,
    3
  ),
  mar = c(
    4,
    4,
    2,
    1
  )
)

plot(
  ph_octubre
)

dev.off()

png(
  filename = file.path(
    ruta_figuras,
    "diagnostico_PH_septiembre2023.png"
  ),
  width = 3000,
  height = 2200,
  res = 300
)

par(
  mfrow = c(
    2,
    3
  ),
  mar = c(
    4,
    4,
    2,
    1
  )
)

plot(
  ph_septiembre
)

dev.off()

# ------------------------------------------------------------------------------
# 7. Dictamen automatico
# ------------------------------------------------------------------------------

global_octubre <- tabla_ph |>
  filter(
    escenario ==
      "Censura en octubre de 2023",
    termino == "GLOBAL"
  ) |>
  pull(
    p_valor
  )

global_septiembre <- tabla_ph |>
  filter(
    escenario ==
      "Censura en septiembre de 2023",
    termino == "GLOBAL"
  ) |>
  pull(
    p_valor
  )

dictamen_ph <- case_when(
  length(global_octubre) == 1L &
    length(global_septiembre) == 1L &
    global_octubre >= 0.05 &
    global_septiembre >= 0.05 ~
    paste0(
      "No se detecto evidencia global contra el supuesto ",
      "de riesgos proporcionales en ninguno de los escenarios."
    ),
  
  TRUE ~
    paste0(
      "Se detecto posible incumplimiento del supuesto de riesgos ",
      "proporcionales en al menos un escenario. Revise la tabla y ",
      "los graficos antes de interpretar los HR como constantes."
    )
)

informe <- c(
  "VALIDACION DEL MODELO DE COX TERMINADA",
  "",
  "Se corrigio la extraccion de la prueba Wald global.",
  "Se compararon los escenarios de censura de octubre y septiembre.",
  "Se evaluo el supuesto de riesgos proporcionales mediante cox.zph.",
  "",
  paste0(
    "Dictamen PH: ",
    dictamen_ph
  ),
  "",
  "Archivos:",
  "- 05_tablas/prueba_global_cox_perfiles_CORREGIDA.csv",
  "- 05_tablas/sensibilidad_HR_octubre_septiembre.csv",
  "- 05_tablas/diagnostico_proporcionalidad_riesgos.csv",
  "- 06_figuras/diagnostico_PH_octubre2023.png",
  "- 06_figuras/diagnostico_PH_septiembre2023.png",
  "- 09_documentacion/prueba_global_regTermTest_Cox.txt"
)

writeLines(
  informe,
  file.path(
    ruta_documentacion,
    "informe_script_12C_validacion_Cox.txt"
  )
)

cat("\n")
cat("============================================================\n")
cat(paste(informe, collapse = "\n"))
cat("\n============================================================\n")