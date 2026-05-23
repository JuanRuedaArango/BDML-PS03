# ============================================================
# DIRECTORIO DE EJECUCIÓN - PROBLEM SET 3
# Predicción de precios inmobiliarios en Bogotá D.C.
# ============================================================
#
# MASTER SCRIPT
# Ejecutar desde consola:
#   source("01_code/00_rundirectory.R")
#
# Flujo lógico:
#   01_featuring.R       -> crea train_out / test_out en memoria
#   02_multivariate.R    -> crea train2 / test2 en memoria
#   modelos              -> usan train2 / test2
#
# ============================================================

# ============================================================
# 0. RESET OPCIONAL
# ============================================================

FULL_RESET <- FALSE

if (isTRUE(FULL_RESET)) {
  rm(list = setdiff(ls(envir = .GlobalEnv), "FULL_RESET"), envir = .GlobalEnv)
}

# ============================================================
# 1. FIJAR RAÍZ DEL PROYECTO
# ============================================================

find_project_root <- function(start = getwd(), max_up = 10) {
  
  candidate <- normalizePath(start, winslash = "/", mustWork = FALSE)
  
  for (i in 0:max_up) {
    
    marker_rproj <- file.path(candidate, "BDML-PS03.Rproj")
    marker_code  <- file.path(candidate, "01_code", "01_featuring.R")
    
    if (file.exists(marker_rproj) || file.exists(marker_code)) {
      return(candidate)
    }
    
    parent <- dirname(candidate)
    
    if (identical(parent, candidate)) {
      break
    }
    
    candidate <- parent
  }
  
  stop(
    paste0(
      "No se encontró la raíz del proyecto.\n",
      "Se esperaba encontrar BDML-PS03.Rproj o 01_code/01_featuring.R.\n",
      "Directorio inicial revisado: ", start
    ),
    call. = FALSE
  )
}

set_project_wd <- function(max_up = 10) {
  
  starts <- c(getwd())
  
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    
    ctx_path <- try(rstudioapi::getActiveDocumentContext()$path, silent = TRUE)
    
    if (!inherits(ctx_path, "try-error") &&
        length(ctx_path) == 1 &&
        !is.na(ctx_path) &&
        nzchar(ctx_path)) {
      starts <- c(dirname(ctx_path), starts)
    }
  }
  
  starts <- unique(starts)
  
  for (start in starts) {
    
    root <- try(find_project_root(start, max_up = max_up), silent = TRUE)
    
    if (!inherits(root, "try-error")) {
      setwd(root)
      options(project_dir = root)
      return(invisible(TRUE))
    }
  }
  
  stop(
    "No se pudo fijar el working directory en la raíz del repositorio.",
    call. = FALSE
  )
}

set_project_wd()

cat("\n[INFO] Working directory del repositorio:\n")
print(getwd())

# ============================================================
# 2. RUTA A LA CARPETA DE DATOS
# ============================================================

BASES_DIR <- file.path(getwd(), "02_data")

BASES_DIR <- normalizePath(
  BASES_DIR,
  winslash = "/",
  mustWork = FALSE
)

if (!dir.exists(BASES_DIR)) {
  stop(
    paste0(
      "No se encontró la carpeta 02_data.\n\n",
      "Ruta esperada:\n",
      BASES_DIR
    ),
    call. = FALSE
  )
}

options(bases_dir = BASES_DIR)

cat("\n[INFO] Carpeta de datos encontrada en:\n")
print(BASES_DIR)

# ============================================================
# 3. PAQUETES
# ============================================================

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  # Base de trabajo
  rio,
  readr,
  readxl,
  writexl,
  dplyr,
  tidyr,
  stringr,
  purrr,
  forcats,
  tibble,
  tidyverse,
  
  # Tidymodels y modelamiento
  tidymodels,
  recipes,
  rsample,
  yardstick,
  workflows,
  parsnip,
  tune,
  dials,
  embed,
  
  # Modelos
  xgboost,
  ranger,
  rpart,
  glmnet,
  nnet,
  
  # Validación espacial
  spatialsample,
  
  # Espacial
  sf,
  osmdata,
  FNN,
  
  # Texto y visualización
  tidytext,
  ggplot2,
  gganimate,
  gifski,
  gt,
  scales,
  
  # Paralelización
  doParallel,
  parallel,
  
  # Utilidades
  jsonlite,
  fastDummies
)

# ============================================================
# 4. CARPETAS DE SALIDA
# ============================================================

output_dirs <- c(
  "03_outputs",
  "03_outputs/01_results",
  "03_outputs/02_plots",
  "03_outputs/03_datasets",
  "03_outputs/figures",
  "03_outputs/tables"
)

purrr::walk(
  output_dirs,
  ~ dir.create(.x, recursive = TRUE, showWarnings = FALSE)
)

# ============================================================
# 5.1 FUNCIÓN PARA EJECUTAR SCRIPTS
# ============================================================

scripts_dir <- "01_code"

run_script <- function(filename, descripcion) {
  
  ruta <- file.path(scripts_dir, filename)
  
  if (!file.exists(ruta)) {
    cat("\n[ERROR] No se encontró el archivo:", ruta, "\n")
    return(invisible(FALSE))
  }
  
  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("  Ejecutando:", descripcion, "\n")
  cat("  Archivo   :", ruta, "\n")
  cat(strrep("=", 60), "\n\n", sep = "")
  
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  
  t0 <- proc.time()
  
  source(
    ruta,
    encoding = "UTF-8",
    local = .GlobalEnv
  )
  
  setwd(getOption("project_dir", old_wd))
  
  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  
  cat("\n[OK]", descripcion, "completado en", elapsed, "segundos.\n")
  
  invisible(TRUE)
}

# ============================================================
# SECTION 5.2: Preparación de datos
# ==============================================================

FORZAR_PREPROCESAMIENTO <- FALSE

preparar_datos <- function() {
  
  preprocesamiento_ya_cargado <- isTRUE(
    get0(".PREPROCESAMIENTO_PS03_OK", envir = .GlobalEnv, ifnotfound = FALSE)
  )
  
  existen_bases_modelo <- exists("train2", envir = .GlobalEnv) &&
    exists("test2", envir = .GlobalEnv)
  
  if (!preprocesamiento_ya_cargado ||
      !existen_bases_modelo ||
      isTRUE(FORZAR_PREPROCESAMIENTO)) {
    
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat("Corriendo preprocesamiento base\n")
    cat(strrep("=", 70), "\n", sep = "")
    
    run_script("01_featuring.R", "Feature Engineering")
    run_script("02_multivariate.R", "PCA + Clustering")
    
    if (!exists("train2", envir = .GlobalEnv) ||
        !exists("test2", envir = .GlobalEnv)) {
      stop(
        "\n[ERROR] No se crearon train2 y test2.\n",
        "Revisa 01_featuring.R y 02_multivariate.R.",
        call. = FALSE
      )
    }
    
    assign(".PREPROCESAMIENTO_PS03_OK", TRUE, envir = .GlobalEnv)
    
    cat("\nPreprocesamiento base completado.\n")
    cat("Objeto disponible: train2 | Dim:", dim(train2), "\n")
    cat("Objeto disponible: test2  | Dim:", dim(test2),  "\n")
    
  } else {
    
    cat("\n", strrep("=", 70), "\n", sep = "")
    cat("Preprocesamiento base ya cargado\n")
    cat(strrep("=", 70), "\n", sep = "")
    cat("No se vuelven a correr 01_featuring.R ni 02_multivariate.R.\n")
    cat("Para forzar el reproceso, cambia FORZAR_PREPROCESAMIENTO <- TRUE.\n")
  }
}


verificar_datos_modelo <- function() {
  
  if (!exists("train2", envir = .GlobalEnv) ||
      !exists("test2", envir = .GlobalEnv)) {
    preparar_datos()
  }
  
  exists("train2", envir = .GlobalEnv) &&
    exists("test2", envir = .GlobalEnv)
}


# ============================================================
# SECTION 6: Selección de algoritmos
# ============================================================

opciones <- c(
  "Preprocesamiento completo (01_featuring + 02_multivariate)",
  "Estadísticas descriptivas",
  "Regresión Lineal (LM)",
  "GBM",
  "XGBoost",
  "Random Forest",
  "CART",
  "Elastic Net",
  "SuperLearner",
  "LightGBM",
  "Todos los modelos",
  "Pipeline completo (preprocesamiento + descriptivas + modelos)",
  "Salir"
)


# ============================================================
# SECTION 7: Running scripts
# ============================================================

correr_modelos <- function(seleccion) {
  
  RUN_PREPROCESAMIENTO <- seleccion %in% c(1, 12)
  RUN_DESCRIPTIVAS     <- seleccion %in% c(2, 12)
  RUN_LM               <- seleccion %in% c(3, 11, 12)
  RUN_GBM              <- seleccion %in% c(4, 11, 12)
  RUN_XGBOOST          <- seleccion %in% c(5, 11, 12)
  RUN_RANDOM_FOREST    <- seleccion %in% c(6, 11, 12)
  RUN_CART             <- seleccion %in% c(7, 11, 12)
  RUN_ELASTIC_NET      <- seleccion %in% c(8, 11, 12)
  RUN_SUPERLEARNER     <- seleccion %in% c(9, 11, 12)
  RUN_LIGHTGBM         <- seleccion %in% c(10, 11, 12)
  
  cat("\nModelos seleccionados:\n")
  cat("  Preprocesamiento :", RUN_PREPROCESAMIENTO, "\n")
  cat("  Descriptivas     :", RUN_DESCRIPTIVAS,     "\n")
  cat("  LM               :", RUN_LM,               "\n")
  cat("  GBM              :", RUN_GBM,              "\n")
  cat("  XGBoost          :", RUN_XGBOOST,          "\n")
  cat("  Random Forest    :", RUN_RANDOM_FOREST,    "\n")
  cat("  CART             :", RUN_CART,             "\n")
  cat("  Elastic Net      :", RUN_ELASTIC_NET,      "\n")
  cat("  SuperLearner     :", RUN_SUPERLEARNER,     "\n")
  cat("  LightGBM         :", RUN_LIGHTGBM,         "\n\n")
  
  if (RUN_PREPROCESAMIENTO) {
    preparar_datos()
  }
  
  if (
    RUN_DESCRIPTIVAS ||
    RUN_LM ||
    RUN_GBM ||
    RUN_XGBOOST ||
    RUN_RANDOM_FOREST ||
    RUN_CART ||
    RUN_ELASTIC_NET ||
    RUN_SUPERLEARNER ||
    RUN_LIGHTGBM
  ) {
    verificar_datos_modelo()
  }
  
  if (RUN_DESCRIPTIVAS) {
    run_script("Descriptivas.R", "Estadísticas descriptivas")
  }
  
  if (RUN_LM) {
    run_script("03_LM.R", "Regresión Lineal")
  }
  
  if (RUN_GBM) {
    run_script("04_GBM.R", "Gradient Boosting Machine")
  }
  
  if (RUN_XGBOOST) {
    run_script("05_XGBoost.R", "XGBoost")
  }
  
  if (RUN_RANDOM_FOREST) {
    run_script("06_Random Forest.R", "Random Forest")
  }
  
  if (RUN_CART) {
    run_script("07_CARTS.R", "CART")
  }
  
  if (RUN_ELASTIC_NET) {
    run_script("08_Elastic Nets.R", "Elastic Net")
  }
  
  if (RUN_SUPERLEARNER) {
    run_script("09_Superlearner.R", "SuperLearner")
  }
  
  if (RUN_LIGHTGBM) {
    run_script("11_LightGBM.R", "LightGBM")
  }
  
  cat("\nEjecución finalizada para la selección actual.\n")
}


# ============================================================
# SECTION 8: Menú principal
# ============================================================

repeat {
  
  cat("\n", strrep("=", 70), "\n", sep = "")
  cat("MENÚ PRINCIPAL DE EJECUCIÓN - PROBLEM SET 3\n")
  cat(strrep("=", 70), "\n", sep = "")
  cat("Selecciona qué deseas correr.\n\n")
  
  seleccion <- menu(opciones)
  
  if (seleccion == 0) {
    cat("\nNo se seleccionó ninguna opción. Saliendo del menú.\n")
    break
  }
  
  if (seleccion == length(opciones)) {
    cat("\nSaliendo del menú de ejecución.\n")
    break
  }
  
  correr_modelos(seleccion)
  
  cat("\n¿Deseas volver al menú para correr otra opción?\n")
  volver <- menu(c("Sí, volver al menú", "No, terminar"))
  
  if (volver != 1) {
    cat("\nProceso terminado por el usuario.\n")
    break
  }
}