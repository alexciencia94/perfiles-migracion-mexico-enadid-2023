README: Perfiles migratorios México–Estados Unidos (ENADID 2023)
Este repositorio contiene el flujo de trabajo completo para el análisis de clases latentes (LCA) de la migración mexicana reciente hacia Estados Unidos, utilizando la Encuesta Nacional de la Dinámica Demográfica (ENADID) 2023. El estudio identifica perfiles migratorios y examina su relación con el retorno a México, incorporando el diseño muestral complejo y corrigiendo el error de clasificación mediante el procedimiento BCH.

Estructura del proyecto
El repositorio sigue una estructura de carpetas estandarizada, creada automáticamente por el script 00_Creacion_Carpetas_Comprobacion.R:

text
.
├── 01_datos_originales/         # Datos crudos (ZIP de ENADID)
├── 02_diccionarios/             # Diccionarios de variables (no incluidos)
├── 03_scripts/                  # Scripts de análisis (este repositorio)
├── 04_datos_procesados/         # Datos intermedios en formato RDS y CSV
├── 05_tablas/                   # Tablas de resultados en CSV
├── 06_figuras/                  # Figuras generadas (PNG)
├── 07_modelos/                  # Modelos guardados (RDS)
├── 08_mplus/                    # Archivos para Mplus (.dat, .inp) y salidas
└── 09_documentacion/            # Informes, logs y documentación auxiliar
Requisitos de software
R (versión 4.5.2 o superior) con los siguientes paquetes:

readr, dplyr, tidyr, tibble, janitor

survival, survey, ggplot2, scales

Mplus (versión 9.1 o superior) para la estimación de los modelos de clases latentes y el procedimiento BCH.
Nota: Los scripts de R generan los archivos de entrada para Mplus; el usuario debe ejecutar Mplus manualmente para obtener las salidas requeridas.

Datos
Los microdatos de la ENADID 2023 son públicos y se pueden descargar del portal del INEGI:
https://www.inegi.org.mx/programas/enadid/2023/

El archivo necesario es base_datos_enadid23_csv.zip. Debe colocarse dentro de la carpeta 01_datos_originales/ antes de ejecutar los scripts.

Instrucciones de ejecución
Descargar los datos y colocar el ZIP en 01_datos_originales/.

Ejecutar los scripts en orden numérico, comenzando con 00_Creacion_Carpetas_Comprobacion.R (crea la estructura de carpetas).

Scripts 01 a 05: preparan los datos, importan, validan y recodifican las variables, y exportan el archivo .dat para Mplus.

Scripts 06 a 08: generan los archivos de entrada de Mplus para las soluciones de 2 a 8 clases y para los modelos finalistas (5, 6 y 7 clases). El usuario debe ejecutar Mplus con estos archivos .inp para obtener los archivos de salida (.out) y los archivos guardados (.dat con probabilidades o pesos BCH).

Script 09: integra las probabilidades posteriores de la solución de 7 clases (requiere el archivo probabilidades_7_clases_final.dat generado por Mplus).

Scripts 10, 11A‑D, 12A‑E y 13A‑B: realizan la caracterización, el análisis BCH del retorno (ajustado por periodo), los modelos de tiempo hasta el retorno (Cox con efectos variables) y la sensibilidad con 6 clases. Algunos de estos scripts requieren archivos de salida de Mplus (por ejemplo, bch_7_clases_paso1.dat).

Script 14A: consolida todas las tablas y figuras finales en la carpeta 09_documentacion/resultados_finales/.

Importante: Asegúrese de que los archivos generados por Mplus se encuentren en la carpeta 08_mplus/ con los nombres esperados (consultar cada script para los nombres exactos). Los scripts están diseñados para leer las salidas de Mplus y continuar el análisis.

Descripción de los scripts principales
Script	Descripción
00_Creacion_Carpetas_Comprobacion.R	Crea las carpetas del proyecto y verifica el entorno.
01_auditoria_archivos.R	Descomprime el ZIP, extrae las cuatro tablas necesarias (TMIGRANTE, THOGAR, TSDEM, TVIVIENDA) y verifica dimensiones.
02_importacion_y_validacion_llaves.R	Importa las tablas como texto, valida llaves primarias y cobertura de enlaces.
03_construccion_muestra_analitica.R	Filtra migrantes con destino EE.UU., construye la muestra analítica y genera bases para LCA.
04_auditoria_indicadores_lca.R	Audita los seis indicadores candidatos (edad, parentesco, nacimiento, motivo, documento, destino) y sus subcódigos.
05_recodificacion_y_exportacion_mplus.R	Recodifica los indicadores según las categorías definitivas y exporta el archivo .dat para Mplus (sin subpoblación).
06_crear_prueba_mplus_2_clases.R	Genera el archivo .inp para una prueba de 2 clases (sin subpoblación).
06B_exportacion_mplus_con_subpoblacion.R	Exporta la base completa con la variable subpop y genera sintaxis para 2 clases usando SUBPOPULATION.
06C_subpoblacion_correcta_mplus.R	Corrige la asignación de valores para casos fuera del dominio (valores válidos arbitrarios) y genera sintaxis para 2 clases.
07_generar_modelos_lca_2_a_8.R	Genera archivos .inp para modelos de 2 a 8 clases con 1000 inicios aleatorios.
08_generar_modelos_finalistas_5_6_7.R	Genera archivos .inp para los modelos finalistas de 5, 6 y 7 clases con 5000 inicios.
09_integrar_probabilidades_mplus.R	Integra las probabilidades posteriores de la solución de 7 clases (requiere el archivo SAVEDATA de Mplus).
10_caracterizacion_ponderada_7_perfiles.R	Calcula prevalencias ponderadas y caracteriza los perfiles con variables externas.
11A_preparar_retorno_DCAT.R	Prepara la base para el análisis de retorno como resultado distal categórico (DCAT) en Mplus.
11B_generar_BCH_paso1.R	Genera el archivo .inp para el paso 1 del BCH (guardar pesos BCH).
11C_preparar_BCH_retorno_paso2.R	Lee los pesos BCH, prepara la base completa y genera el paso 2 del BCH para retorno binario (sin covariables).
11D_BCH_ajustado_periodo.R	Prepara el BCH ajustado por periodo de salida (covariables p20 y p22) y genera la sintaxis Mplus.
12A_auditoria_temporal.R	Audita la disponibilidad y calidad de las fechas de salida y retorno, y la variable tpo_mig.
12B_tiempo_hasta_retorno.R	Construye las bases de supervivencia, estima curvas de Kaplan‑Meier ponderadas y modelos de Cox con diseño complejo.
12C_validacion_modelo_Cox.R	Realiza pruebas Wald globales manuales, compara escenarios de censura y evalúa proporcionalidad de riesgos (diagnóstico).
12D_diagnostico_PH_pesos_normalizados.R	Repite el diagnóstico PH con pesos normalizados para evaluar sensibilidad.
12E_Cox_efectos_variables_intervalos.R	Estima modelos de Cox con efectos del perfil variables en el tiempo (intervalos 0-12, >12-24, >24 meses).
13A_BCH_6_clases_sensibilidad_paso1.R	Genera el paso 1 del BCH para la solución de 6 clases (sensibilidad).
13B_BCH_6_clases_ajustado_periodo.R	Prepara el BCH ajustado por periodo para la solución de 6 clases y genera la sintaxis Mplus.
14A_consolidacion_resultados_finales.R	Consolida todas las tablas, figuras y resúmenes finales en 09_documentacion/resultados_finales/.
Resultados principales
Los resultados finales se encuentran en la carpeta 09_documentacion/resultados_finales/ y comprenden:

Tabla 1: Ajuste de los modelos finalistas (AIC, BIC, entropía).

Tabla 2: Descripción sustantiva y prevalencia de los 7 perfiles.

Tabla 3: Probabilidad de retorno ajustada por perfil (BCH, diseño complejo, estandarización por periodo).

Tabla 4: Efecto del periodo de salida sobre el retorno (odds ratios).

Tabla 5: Sensibilidad de la probabilidad de retorno con 6 clases.

Figuras: Prevalencia de perfiles, retorno ajustado y sensibilidad.

Resumen analítico final: Texto con las conclusiones principales.

Contribuciones y citación
Si utiliza este código o los resultados, por favor cite el estudio correspondiente (el manuscrito está en fase de preparación). Se agradece cualquier sugerencia o reporte de errores a través de los issues de GitHub.

Licencia
Este repositorio se distribuye bajo licencia MIT (consulte el archivo LICENSE). Los datos de la ENADID son propiedad del INEGI y su uso debe ajustarse a las condiciones establecidas por dicha institución.

Contacto: alexciencia94@gmail.com
