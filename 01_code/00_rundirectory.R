# ============================================================
# DIRECTORIO DE EJECUCIÓN - PROBLEM SET 3
# Predicción de precios inmobiliarios en Bogotá D.C.
# ============================================================
#
# Instrucciones:
#   1. Abrir el proyecto BDML-PS03.Rproj
#   2. Correr este script desde la consola:
#        source("01_code/00_rundirectory.R")
#   3. Seleccionar el módulo que desea ejecutar
#
# Orden de dependencias:
#   01 → 02 → (03 | 04 | 05 | 06 | 07 | 08)
#
# ============================================================


# ============================================================
# 0. CONFIGURACIÓN INICIAL
# ============================================================

# Verificar que el working directory sea la raíz del proyecto
if (!file.exists("BDML-PS03.Rproj")) {
  stop(
    "\n[ERROR] El working directory no es la raíz del proyecto.\n",
    "Abre el archivo BDML-PS03.Rproj antes de correr este script.\n",
    "Working directory actual: ", getwd()
  )
}

scripts_dir <- "01_code"

# ============================================================
# 1. FUNCIONES AUXILIARES
# ============================================================

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
  t0 <- proc.time()
  source(ruta, encoding = "UTF-8")
  elapsed <- round((proc.time() - t0)[["elapsed"]], 1)
  cat("\n[OK]", descripcion, "completado en", elapsed, "segundos.\n")
  invisible(TRUE)
}

verificar_datos_base <- function() {
  # Los modelos necesitan train2 / test2 en el entorno global
  if (!exists("train2") || !exists("test2")) {
    cat(
      "\n[ADVERTENCIA] Los objetos 'train2' y 'test2' no están en memoria.\n",
      "Asegúrate de haber ejecutado primero:\n",
      "  [1] Feature Engineering\n",
      "  [2] PCA + Clustering\n\n"
    )
    resp <- readline("¿Deseas ejecutarlos ahora? (s/n): ")
    if (tolower(trimws(resp)) == "s") {
      run_script("01_featuring.R",    "Feature Engineering")
      run_script("02_multivariate.R", "PCA + Clustering")
    } else {
      cat("[INFO] Abortando. Ejecuta los pasos 1 y 2 primero.\n")
      return(FALSE)
    }
  }
  TRUE
}

# ============================================================
# 2. MENÚ PRINCIPAL
# ============================================================

repeat {

  cat("\n", strrep("=", 60), "\n", sep = "")
  cat("  PROBLEM SET 3 – MENÚ DE EJECUCIÓN\n")
  cat(strrep("=", 60), "\n\n", sep = "")
  cat("  ── Preprocesamiento ───────────────────────────────\n")
  cat("  [1]  Feature Engineering\n")
  cat("  [2]  PCA + Clustering\n\n")
  cat("  ── Modelos ────────────────────────────────────────\n")
  cat("  [3]  Regresión Lineal (LM)\n")
  cat("  [4]  Gradient Boosting Machine (GBM)\n")
  cat("  [5]  XGBoost\n")
  cat("  [6]  Random Forest\n")
  cat("  [7]  CART\n")
  cat("  [8]  Elastic Nets\n\n")
  cat("  ── Análisis ────────────────────────────────────────\n")
  cat("  [9]  Estadísticas Descriptivas\n\n")
  cat("  ── Pipelines completos ─────────────────────────────\n")
  cat("  [10] Preprocesamiento completo (pasos 1 → 2)\n")
  cat("  [11] Todos los modelos (pasos 3 → 8)\n")
  cat("  [12] Pipeline completo (pasos 1 → 9)\n\n")
  cat("  [0]  Salir\n")
  cat(strrep("-", 60), "\n", sep = "")

  seleccion <- readline("  Selecciona una opción: ")
  seleccion <- trimws(seleccion)

  # ── Salir ──────────────────────────────────────────────────
  if (seleccion == "0") {
    cat("\n[INFO] Sesión finalizada. ¡Hasta luego!\n\n")
    break
  }

  # ── Preprocesamiento ───────────────────────────────────────
  if (seleccion == "1") {
    run_script("01_featuring.R", "Feature Engineering")

  } else if (seleccion == "2") {
    run_script("02_multivariate.R", "PCA + Clustering")

  # ── Modelos individuales ───────────────────────────────────
  } else if (seleccion == "3") {
    if (verificar_datos_base()) run_script("03_LM.R", "Regresión Lineal (LM)")

  } else if (seleccion == "4") {
    if (verificar_datos_base()) run_script("04_GBM.R", "Gradient Boosting Machine (GBM)")

  } else if (seleccion == "5") {
    if (verificar_datos_base()) run_script("05_XGBoost.R", "XGBoost")

  } else if (seleccion == "6") {
    if (verificar_datos_base()) run_script("06_Random Forest.R", "Random Forest")

  } else if (seleccion == "7") {
    if (verificar_datos_base()) run_script("07_CARTS.R", "CART")

  } else if (seleccion == "8") {
    if (verificar_datos_base()) run_script("08_Elastic Nets.R", "Elastic Nets")

  # ── Análisis ───────────────────────────────────────────────
  } else if (seleccion == "9") {
    run_script("Descriptivas.R", "Estadísticas Descriptivas")

  # ── Pipelines completos ────────────────────────────────────
  } else if (seleccion == "10") {
    cat("\n[INFO] Ejecutando preprocesamiento completo...\n")
    run_script("01_featuring.R",    "Feature Engineering")
    run_script("02_multivariate.R", "PCA + Clustering")
    cat("\n[OK] Preprocesamiento completo finalizado.\n")

  } else if (seleccion == "11") {
    if (verificar_datos_base()) {
      cat("\n[INFO] Ejecutando todos los modelos...\n")
      run_script("03_LM.R",             "Regresión Lineal (LM)")
      run_script("04_GBM.R",            "Gradient Boosting Machine (GBM)")
      run_script("05_XGBoost.R",        "XGBoost")
      run_script("06_Random Forest.R",  "Random Forest")
      run_script("07_CARTS.R",          "CART")
      run_script("08_Elastic Nets.R",   "Elastic Nets")
      cat("\n[OK] Todos los modelos finalizados.\n")
    }

  } else if (seleccion == "12") {
    cat("\n[INFO] Ejecutando pipeline completo...\n")
    run_script("01_featuring.R",        "Feature Engineering")
    run_script("02_multivariate.R",     "PCA + Clustering")
    run_script("03_LM.R",               "Regresión Lineal (LM)")
    run_script("04_GBM.R",              "Gradient Boosting Machine (GBM)")
    run_script("05_XGBoost.R",          "XGBoost")
    run_script("06_Random Forest.R",    "Random Forest")
    run_script("07_CARTS.R",            "CART")
    run_script("08_Elastic Nets.R",     "Elastic Nets")
    run_script("Descriptivas.R",        "Estadísticas Descriptivas")
    cat("\n[OK] Pipeline completo finalizado.\n")

  } else {
    cat("\n[ADVERTENCIA] Opción inválida. Ingresa un número entre 0 y 12.\n")
  }

}
