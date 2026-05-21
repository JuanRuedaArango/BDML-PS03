# ============================================================
# ESTADÍSTICAS DESCRIPTIVAS - PROBLEM SET 3
# Bases finales: Train y Test
# ============================================================

# ============================================================
# 0. Librerías y paths
# ============================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  readxl, dplyr, tidyr, ggplot2, gt, scales,
  stringr, forcats, purrr, readr
)

path_figures <- "03_outputs/figures"
path_tables  <- "03_outputs/tables"

dir.create(path_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(path_tables,  recursive = TRUE, showWarnings = FALSE)

# Rutas directas en el computador
train_path <- "C:/Users/herna/Downloads/Base_Train_Final.xlsx"
test_path  <- "C:/Users/herna/Downloads/Base_Test_Final.xlsx"

if (!file.exists(train_path)) stop("No encontré la base Train en: ", train_path)
if (!file.exists(test_path))  stop("No encontré la base Test en: ", test_path)

train_raw <- read_excel(train_path)
test_raw  <- read_excel(test_path)

cat("Archivo train:", train_path, "\n")
cat("Archivo test :", test_path,  "\n")
cat("Dim train:", dim(train_raw), "\n")
cat("Dim test :", dim(test_raw),  "\n\n")


# ============================================================
# 1. Variables solicitadas
# ============================================================

vars_texto <- c(
  "tiene_parqueadero",
  "tiene_terraza",
  "tiene_balcon",
  "tiene_deposito",
  "tiene_gimnasio",
  "tiene_piscina",
  "tiene_seguridad",
  "cocina_integral",
  "remodelado",
  "es_lujo",
  "es_penthouse",
  "es_duplex",
  "cerca_transporte",
  "zonas_verdes"
)

vars_base <- c(
  "Numero_banos",
  "Numero_bedrooms",
  "Numero_rooms",
  "superficie"
)

vars_osm <- c(
  "dist_parque",
  "dist_hospital",
  "dist_cai",
  "dist_aeropuerto",
  "dist_transmilenio",
  "n_supermercados_1km",
  "n_colegios_1km",
  "dist_centro",
  "n_estaciones_1km",
  "n_tiendas_1km"
)

vars_datos_abiertos <- c(
  "MED_VALOR_",
  "NO_PREDIOS",
  "GRUPOUSOEC",
  "V_REF",
  "ESTRATO",
  "seguridad"
)

vars_todas <- unique(c(
  vars_texto,
  vars_base,
  vars_osm,
  vars_datos_abiertos
))


# ============================================================
# 2. Etiquetas y grupos
# ============================================================

labels_vars <- c(
  # Texto
  tiene_parqueadero  = "Tiene parqueadero",
  tiene_terraza      = "Tiene terraza",
  tiene_balcon       = "Tiene balcón",
  tiene_deposito     = "Tiene depósito",
  tiene_gimnasio     = "Tiene gimnasio",
  tiene_piscina      = "Tiene piscina",
  tiene_seguridad    = "Tiene seguridad",
  cocina_integral    = "Cocina integral",
  remodelado         = "Remodelado",
  es_lujo            = "Señal de lujo",
  es_penthouse       = "Penthouse",
  es_duplex          = "Dúplex",
  cerca_transporte   = "Menciona transporte",
  zonas_verdes       = "Menciona zonas verdes",
  
  # Base
  Numero_banos       = "Número de baños",
  Numero_bedrooms    = "Número de habitaciones",
  Numero_rooms       = "Número de cuartos",
  superficie         = "Superficie",
  
  # OSM
  dist_parque        = "Distancia a parque",
  dist_hospital      = "Distancia a hospital",
  dist_cai           = "Distancia a CAI / policía",
  dist_aeropuerto    = "Distancia a aeropuerto",
  dist_transmilenio  = "Distancia a estación de transporte",
  n_supermercados_1km = "Supermercados en 1 km",
  n_colegios_1km      = "Colegios en 1 km",
  dist_centro         = "Distancia al centro",
  n_estaciones_1km    = "Estaciones en 1 km",
  n_tiendas_1km       = "Tiendas en 1 km",
  
  # Datos abiertos
  MED_VALOR_         = "Mediana valor referencia",
  NO_PREDIOS         = "Número de predios",
  GRUPOUSOEC         = "Grupo de uso económico",
  V_REF              = "Valor de referencia",
  ESTRATO            = "Estrato",
  seguridad          = "Seguridad"
)

get_label <- function(v) {
  out <- ifelse(v %in% names(labels_vars), labels_vars[v], v)
  unname(out)
}

grupo_variable <- bind_rows(
  tibble(variable = vars_texto, grupo = "Texto del anuncio"),
  tibble(variable = vars_base, grupo = "Características estructurales"),
  tibble(variable = vars_osm, grupo = "OSM: distancias y conteos"),
  tibble(variable = vars_datos_abiertos, grupo = "Datos Abiertos Bogotá")
) %>%
  distinct(variable, .keep_all = TRUE)


# ============================================================
# 3. Unión Train / Test
# ============================================================

datos <- bind_rows(
  train_raw %>% mutate(.base = "Train"),
  test_raw  %>% mutate(.base = "Test")
)

vars_presentes <- vars_todas[vars_todas %in% names(datos)]
vars_faltantes <- setdiff(vars_todas, names(datos))

cat("Variables solicitadas:", length(vars_todas), "\n")
cat("Variables encontradas :", length(vars_presentes), "\n")
cat("Variables faltantes   :", length(vars_faltantes), "\n\n")

if (length(vars_faltantes) > 0) {
  cat("Variables faltantes:\n")
  print(vars_faltantes)
  cat("\n")
}

diccionario_vars <- tibble(variable = vars_todas) %>%
  left_join(grupo_variable, by = "variable") %>%
  mutate(
    etiqueta = get_label(variable),
    disponible = variable %in% names(datos)
  )

write_csv(
  diccionario_vars,
  file.path(path_tables, "diccionario_variables_descriptivas.csv")
)


# ============================================================
# 4. Funciones auxiliares
# ============================================================

normalizar_binaria <- function(x) {
  
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  
  if (is.numeric(x)) {
    return(ifelse(x %in% c(0, 1), as.numeric(x), NA_real_))
  }
  
  x_chr <- str_squish(str_to_lower(as.character(x)))
  x_chr[x_chr %in% c("", "na", "nan", "null")] <- NA_character_
  x_num <- suppressWarnings(parse_number(x_chr))
  
  case_when(
    is.na(x_chr) ~ NA_real_,
    x_chr %in% c("true", "t", "si", "sí", "yes", "y") ~ 1,
    x_chr %in% c("false", "f", "no", "n") ~ 0,
    !is.na(x_num) & x_num %in% c(0, 1) ~ as.numeric(x_num),
    TRUE ~ NA_real_
  )
}

to_numeric_safe <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(parse_number(as.character(x)))
}

es_numeric_like <- function(x) {
  if (is.numeric(x)) return(TRUE)
  
  x_chr <- str_squish(as.character(x))
  validos <- !is.na(x_chr) & x_chr != ""
  
  if (sum(validos) == 0) return(FALSE)
  
  x_num <- suppressWarnings(parse_number(x_chr[validos]))
  mean(!is.na(x_num)) >= 0.80
}

safe_mean <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sd(x, na.rm = TRUE)
}

safe_min <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  min(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  max(x, na.rm = TRUE)
}

safe_quantile <- function(x, p) {
  if (all(is.na(x))) return(NA_real_)
  quantile(x, probs = p, na.rm = TRUE, names = FALSE)
}

safe_prop_1 <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  mean(x == 1, na.rm = TRUE)
}

guardar_gt <- function(tabla, nombre_base) {
  
  html_path <- file.path(path_tables, paste0(nombre_base, ".html"))
  png_path  <- file.path(path_figures, paste0(nombre_base, ".png"))
  
  gt::gtsave(tabla, html_path)
  
  tryCatch(
    gt::gtsave(tabla, png_path),
    error = function(e) {
      message("No se pudo guardar PNG para ", nombre_base, ". Se guardó HTML.")
    }
  )
}

tema_base <- theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank()
  )


# ============================================================
# 5. Missing values Train vs Test
# ============================================================

missing_df <- datos %>%
  select(.base, all_of(vars_presentes)) %>%
  pivot_longer(
    cols = - .base,
    names_to = "variable",
    values_to = "valor",
    values_transform = list(valor = as.character)
  ) %>%
  group_by(.base, variable) %>%
  summarise(
    n_total = n(),
    n_missing = sum(is.na(valor)),
    pct_missing = 100 * mean(is.na(valor)),
    .groups = "drop"
  ) %>%
  left_join(grupo_variable, by = "variable") %>%
  mutate(variable_label = get_label(variable))

write_csv(
  missing_df,
  file.path(path_tables, "missing_variables_train_test.csv")
)

p_missing <- ggplot(
  missing_df,
  aes(
    x = fct_reorder(variable_label, pct_missing),
    y = pct_missing,
    fill = .base
  )
) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  facet_wrap(~ grupo, scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "Porcentaje de datos faltantes por variable",
    subtitle = "Comparación entre Train y Test",
    x = NULL,
    y = "% de valores faltantes",
    fill = "",
    caption = "Fuente: Properati, OSM y Datos Abiertos de Bogotá. Elaboración propia."
  ) +
  tema_base

ggsave(
  file.path(path_figures, "missing_variables_train_test.png"),
  p_missing,
  width = 12,
  height = 8,
  dpi = 300
)


# ============================================================
# 6. Descriptivas de variables binarias de texto
# ============================================================

vars_texto_pres <- vars_texto[vars_texto %in% names(datos)]

if (length(vars_texto_pres) > 0) {
  
  datos_texto <- datos %>%
    mutate(across(all_of(vars_texto_pres), normalizar_binaria))
  
  stats_texto <- datos_texto %>%
    select(.base, all_of(vars_texto_pres)) %>%
    pivot_longer(
      cols = - .base,
      names_to = "variable",
      values_to = "valor"
    ) %>%
    group_by(.base, variable) %>%
    summarise(
      n_total = n(),
      n_validos = sum(!is.na(valor)),
      n_missing = sum(is.na(valor)),
      pct_missing = 100 * mean(is.na(valor)),
      n_1 = sum(valor == 1, na.rm = TRUE),
      prop_1 = safe_prop_1(valor),
      .groups = "drop"
    ) %>%
    mutate(
      variable_label = get_label(variable),
      grupo = "Texto del anuncio"
    )
  
  stats_texto_wide <- stats_texto %>%
    select(.base, variable, variable_label, prop_1, pct_missing, n_validos) %>%
    pivot_wider(
      names_from = .base,
      values_from = c(prop_1, pct_missing, n_validos),
      names_sep = "_"
    ) %>%
    mutate(
      brecha_pp_Test_Train = 100 * (prop_1_Test - prop_1_Train)
    ) %>%
    arrange(desc(abs(brecha_pp_Test_Train)))
  
  write_csv(
    stats_texto,
    file.path(path_tables, "descriptivas_texto_binarias_largo.csv")
  )
  
  write_csv(
    stats_texto_wide,
    file.path(path_tables, "descriptivas_texto_binarias_comparacion.csv")
  )
  
  tabla_texto <- stats_texto_wide %>%
    select(
      variable_label,
      prop_1_Train,
      prop_1_Test,
      brecha_pp_Test_Train,
      pct_missing_Train,
      pct_missing_Test
    ) %>%
    gt() %>%
    tab_header(
      title = md("**Estadísticas descriptivas — variables extraídas del texto**"),
      subtitle = "Proporción de anuncios donde aparece cada característica"
    ) %>%
    cols_label(
      variable_label = "Variable",
      prop_1_Train = "% Train",
      prop_1_Test = "% Test",
      brecha_pp_Test_Train = "Brecha Test - Train (pp)",
      pct_missing_Train = "% Missing Train",
      pct_missing_Test = "% Missing Test"
    ) %>%
    fmt_percent(columns = c(prop_1_Train, prop_1_Test), decimals = 1) %>%
    fmt_number(
      columns = c(brecha_pp_Test_Train, pct_missing_Train, pct_missing_Test),
      decimals = 2
    ) %>%
    opt_row_striping() %>%
    tab_source_note(
      source_note = md("Fuente: Properati. Elaboración propia a partir de título y descripción.")
    ) %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))
  
  guardar_gt(tabla_texto, "tabla_descriptiva_texto_binarias")
  
  p_texto <- ggplot(
    stats_texto,
    aes(
      x = fct_reorder(variable_label, prop_1),
      y = prop_1,
      fill = .base
    )
  ) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Características detectadas en el texto de los anuncios",
      subtitle = "Proporción de anuncios con cada atributo",
      x = NULL,
      y = "Proporción de anuncios",
      fill = "",
      caption = "Fuente: Properati. Elaboración propia."
    ) +
    tema_base
  
  ggsave(
    file.path(path_figures, "texto_binarias_proporciones_train_test.png"),
    p_texto,
    width = 11,
    height = 7,
    dpi = 300
  )
}


# ============================================================
# 7. Variables numéricas
# ============================================================

vars_datos_abiertos_num_base <- c("MED_VALOR_", "NO_PREDIOS", "V_REF")

vars_num_candidatas <- unique(c(
  vars_base,
  vars_osm,
  vars_datos_abiertos_num_base
))

# La variable seguridad entra como numérica solo si realmente tiene forma numérica.
if ("seguridad" %in% names(datos) && es_numeric_like(datos$seguridad)) {
  vars_num_candidatas <- unique(c(vars_num_candidatas, "seguridad"))
}

vars_num_pres <- vars_num_candidatas[vars_num_candidatas %in% names(datos)]

if (length(vars_num_pres) > 0) {
  
  datos_num <- datos %>%
    mutate(across(all_of(vars_num_pres), to_numeric_safe))
  
  stats_num_largo <- datos_num %>%
    select(.base, all_of(vars_num_pres)) %>%
    pivot_longer(
      cols = - .base,
      names_to = "variable",
      values_to = "valor"
    ) %>%
    group_by(.base, variable) %>%
    summarise(
      n_total = n(),
      n_validos = sum(!is.na(valor)),
      n_missing = sum(is.na(valor)),
      pct_missing = 100 * mean(is.na(valor)),
      media = safe_mean(valor),
      sd = safe_sd(valor),
      min = safe_min(valor),
      p25 = safe_quantile(valor, 0.25),
      mediana = safe_quantile(valor, 0.50),
      p75 = safe_quantile(valor, 0.75),
      max = safe_max(valor),
      .groups = "drop"
    ) %>%
    left_join(grupo_variable, by = "variable") %>%
    mutate(variable_label = get_label(variable))
  
  stats_num_wide <- stats_num_largo %>%
    select(
      .base, grupo, variable, variable_label,
      n_validos, pct_missing, media, sd, min, p25, mediana, p75, max
    ) %>%
    pivot_wider(
      names_from = .base,
      values_from = c(n_validos, pct_missing, media, sd, min, p25, mediana, p75, max),
      names_sep = "_"
    ) %>%
    mutate(
      dif_media_Test_Train = media_Test - media_Train,
      dif_mediana_Test_Train = mediana_Test - mediana_Train
    ) %>%
    arrange(grupo, variable)
  
  write_csv(
    stats_num_largo,
    file.path(path_tables, "descriptivas_numericas_largo.csv")
  )
  
  write_csv(
    stats_num_wide,
    file.path(path_tables, "descriptivas_numericas_comparacion.csv")
  )
  
  tabla_num <- stats_num_wide %>%
    select(
      grupo,
      variable_label,
      media_Train,
      media_Test,
      dif_media_Test_Train,
      mediana_Train,
      mediana_Test,
      dif_mediana_Test_Train,
      pct_missing_Train,
      pct_missing_Test
    ) %>%
    gt(groupname_col = "grupo") %>%
    tab_header(
      title = md("**Estadísticas descriptivas — variables numéricas**"),
      subtitle = "Comparación entre Train y Test"
    ) %>%
    cols_label(
      variable_label = "Variable",
      media_Train = "Media Train",
      media_Test = "Media Test",
      dif_media_Test_Train = "Dif. media",
      mediana_Train = "Mediana Train",
      mediana_Test = "Mediana Test",
      dif_mediana_Test_Train = "Dif. mediana",
      pct_missing_Train = "% Missing Train",
      pct_missing_Test = "% Missing Test"
    ) %>%
    fmt_number(columns = where(is.numeric), decimals = 2) %>%
    opt_row_striping() %>%
    tab_source_note(
      source_note = md("Fuente: Properati, OSM y Datos Abiertos de Bogotá. Elaboración propia.")
    ) %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))
  
  guardar_gt(tabla_num, "tabla_descriptiva_numericas_train_test")
}


# ============================================================
# 8. Gráficos de densidad para variables numéricas
# ============================================================

guardar_densidades <- function(vars, nombre_archivo, titulo, vars_por_pagina = 9) {
  
  vars <- vars[vars %in% vars_num_pres]
  
  if (length(vars) == 0) return(invisible(NULL))
  
  paginas <- split(
    vars,
    ceiling(seq_along(vars) / vars_por_pagina)
  )
  
  for (i in seq_along(paginas)) {
    
    dens_df <- datos_num %>%
      select(.base, all_of(paginas[[i]])) %>%
      pivot_longer(
        cols = - .base,
        names_to = "variable",
        values_to = "valor"
      ) %>%
      group_by(variable) %>%
      mutate(
        q01 = safe_quantile(valor, 0.01),
        q99 = safe_quantile(valor, 0.99),
        valor_plot = ifelse(
          !is.na(valor) & !is.na(q01) & !is.na(q99) & valor >= q01 & valor <= q99,
          valor,
          NA_real_
        ),
        variable_label = get_label(variable)
      ) %>%
      ungroup()
    
    p <- ggplot(
      dens_df,
      aes(x = valor_plot, fill = .base, color = .base)
    ) +
      geom_density(alpha = 0.25, linewidth = 0.8, na.rm = TRUE) +
      facet_wrap(~ variable_label, scales = "free", ncol = 3) +
      labs(
        title = titulo,
        subtitle = paste0("Comparación Train vs Test — página ", i, " de ", length(paginas)),
        x = NULL,
        y = "Densidad",
        fill = "",
        color = "",
        caption = "Nota: para mejorar legibilidad, se recortan colas al percentil 1 y 99 por variable."
      ) +
      tema_base
    
    ggsave(
      file.path(path_figures, sprintf("%s_p%02d.png", nombre_archivo, i)),
      p,
      width = 11,
      height = 8,
      dpi = 300
    )
  }
}

guardar_densidades(
  vars = vars_base,
  nombre_archivo = "base_estructural_densidades",
  titulo = "Distribución de variables estructurales del inmueble"
)

guardar_densidades(
  vars = vars_osm,
  nombre_archivo = "osm_densidades",
  titulo = "Distribución de variables OSM: distancias y conteos"
)

guardar_densidades(
  vars = vars_datos_abiertos_num_base,
  nombre_archivo = "datos_abiertos_densidades",
  titulo = "Distribución de variables numéricas de Datos Abiertos Bogotá"
)

if ("seguridad" %in% vars_num_pres) {
  guardar_densidades(
    vars = "seguridad",
    nombre_archivo = "seguridad_densidades",
    titulo = "Distribución de la variable seguridad"
  )
}


# ============================================================
# 9. Variables categóricas
# ============================================================

vars_cat_candidatas <- c("GRUPOUSOEC", "ESTRATO")

# Seguridad entra como categórica si no tiene forma numérica.
if ("seguridad" %in% names(datos) && !es_numeric_like(datos$seguridad)) {
  vars_cat_candidatas <- unique(c(vars_cat_candidatas, "seguridad"))
}

vars_cat_pres <- vars_cat_candidatas[vars_cat_candidatas %in% names(datos)]

if (length(vars_cat_pres) > 0) {
  
  datos_cat <- datos %>%
    select(.base, all_of(vars_cat_pres)) %>%
    pivot_longer(
      cols = - .base,
      names_to = "variable",
      values_to = "valor",
      values_transform = list(valor = as.character)
    ) %>%
    mutate(
      valor = str_squish(valor),
      valor = if_else(is.na(valor) | valor == "", "Sin dato", valor),
      variable_label = get_label(variable)
    )
  
  stats_cat_niveles <- datos_cat %>%
    count(.base, variable, variable_label, valor, name = "n") %>%
    group_by(.base, variable) %>%
    mutate(
      total = sum(n),
      pct = n / total
    ) %>%
    ungroup()
  
  stats_cat_resumen <- stats_cat_niveles %>%
    group_by(.base, variable, variable_label) %>%
    arrange(desc(n), .by_group = TRUE) %>%
    summarise(
      niveles_sin_missing = sum(valor != "Sin dato"),
      moda = first(valor),
      pct_moda = first(pct),
      n_missing = sum(if_else(valor == "Sin dato", n, 0L)),
      pct_missing = 100 * sum(if_else(valor == "Sin dato", n, 0L)) / sum(n),
      .groups = "drop"
    )
  
  stats_cat_resumen_wide <- stats_cat_resumen %>%
    select(.base, variable, variable_label, niveles_sin_missing, moda, pct_moda, pct_missing) %>%
    pivot_wider(
      names_from = .base,
      values_from = c(niveles_sin_missing, moda, pct_moda, pct_missing),
      names_sep = "_"
    )
  
  write_csv(
    stats_cat_niveles,
    file.path(path_tables, "descriptivas_categoricas_niveles.csv")
  )
  
  write_csv(
    stats_cat_resumen_wide,
    file.path(path_tables, "descriptivas_categoricas_resumen.csv")
  )
  
  tabla_cat <- stats_cat_resumen_wide %>%
    gt() %>%
    tab_header(
      title = md("**Estadísticas descriptivas — variables categóricas**"),
      subtitle = "Resumen de niveles, moda y valores faltantes"
    ) %>%
    cols_label(
      variable_label = "Variable",
      niveles_sin_missing_Train = "Niveles Train",
      niveles_sin_missing_Test = "Niveles Test",
      moda_Train = "Moda Train",
      moda_Test = "Moda Test",
      pct_moda_Train = "% Moda Train",
      pct_moda_Test = "% Moda Test",
      pct_missing_Train = "% Missing Train",
      pct_missing_Test = "% Missing Test"
    ) %>%
    fmt_percent(columns = starts_with("pct_moda"), decimals = 1) %>%
    fmt_number(columns = starts_with("pct_missing"), decimals = 2) %>%
    opt_row_striping() %>%
    tab_source_note(
      source_note = md("Fuente: Datos Abiertos de Bogotá. Elaboración propia.")
    ) %>%
    tab_options(table.font.size = 10, data_row.padding = px(3))
  
  guardar_gt(tabla_cat, "tabla_descriptiva_categoricas")
  
  top_cat <- stats_cat_niveles %>%
    group_by(variable, variable_label, valor) %>%
    summarise(n_total = sum(n), .groups = "drop") %>%
    group_by(variable) %>%
    slice_max(n_total, n = 8, with_ties = FALSE) %>%
    ungroup()
  
  plot_cat_df <- stats_cat_niveles %>%
    semi_join(top_cat, by = c("variable", "variable_label", "valor"))
  
  p_cat <- ggplot(
    plot_cat_df,
    aes(
      x = fct_reorder(valor, pct),
      y = pct,
      fill = .base
    )
  ) +
    geom_col(position = position_dodge(width = 0.8)) +
    coord_flip() +
    facet_wrap(~ variable_label, scales = "free_y") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Distribución de variables categóricas",
      subtitle = "Top niveles por variable — comparación Train vs Test",
      x = NULL,
      y = "Participación",
      fill = "",
      caption = "Fuente: Datos Abiertos de Bogotá. Elaboración propia."
    ) +
    tema_base
  
  ggsave(
    file.path(path_figures, "categoricas_top_niveles_train_test.png"),
    p_cat,
    width = 11,
    height = 7,
    dpi = 300
  )
}


# ============================================================
# 10. Justificación económica
# ============================================================

justificacion <- c(
  "# Justificación económica de las estadísticas descriptivas",
  "",
  "Las estadísticas descriptivas permiten revisar si las variables construidas tienen sentido económico y si existen diferencias relevantes entre la base de entrenamiento y la base de predicción. Esto es especialmente importante porque el modelo se entrena con información de propiedades de Bogotá, pero debe predecir precios en Chapinero. Por tanto, comparar Train y Test ayuda a identificar posibles problemas de extrapolación espacial.",
  "",
  "Las variables extraídas del texto del anuncio capturan atributos de calidad del inmueble que no siempre aparecen en las columnas estructuradas. Características como parqueadero, terraza, balcón, depósito, gimnasio, piscina, seguridad, cocina integral, remodelación, lujo, penthouse o dúplex pueden aumentar la disposición a pagar porque representan comodidad, exclusividad, mejor dotación o mayor calidad percibida de la vivienda.",
  "",
  "Las variables estructurales de la base, como número de baños, habitaciones, cuartos y superficie, representan los atributos físicos centrales del inmueble. Desde un enfoque de precios hedónicos, estas características deberían explicar una parte importante del precio porque reflejan tamaño, capacidad, funcionalidad y condiciones habitacionales básicas.",
  "",
  "Las variables OSM incorporan información del entorno urbano. Las distancias a parques, hospitales, CAI, aeropuerto, estaciones de transporte y centro, junto con conteos de supermercados, colegios, estaciones y tiendas en un radio cercano, aproximan accesibilidad, disponibilidad de servicios y calidad de localización. En economía urbana, estos atributos pueden capitalizarse en el precio de la vivienda porque afectan tiempos de desplazamiento, calidad de vida y conveniencia del entorno.",
  "",
  "Las variables de Datos Abiertos de Bogotá permiten capturar condiciones institucionales y territoriales. Variables como valor de referencia, número de predios, grupo de uso económico, estrato y seguridad aproximan características del mercado local, intensidad de uso del suelo, entorno socioeconómico y riesgo percibido. Estas dimensiones son relevantes para explicar diferencias de precios entre zonas de la ciudad.",
  "",
  "En conjunto, estas descriptivas no solo resumen los datos, sino que permiten evaluar si las variables incluidas en los modelos son consistentes con la teoría económica de precios hedónicos y si el modelo enfrenta un cambio de distribución importante entre Train y Test."
)

writeLines(
  justificacion,
  file.path(path_tables, "justificacion_economica_descriptivos.md")
)


# ============================================================
# 11. Resumen de outputs
# ============================================================

cat("\n============================================================\n")
cat("OUTPUTS GENERADOS\n")
cat("============================================================\n")

cat("\nTablas guardadas en:\n")
cat("  ", path_tables, "\n")

cat("\nFiguras guardadas en:\n")
cat("  ", path_figures, "\n")

cat("\nArchivos principales:\n")
cat("  - diccionario_variables_descriptivas.csv\n")
cat("  - missing_variables_train_test.csv\n")
cat("  - descriptivas_texto_binarias_comparacion.csv\n")
cat("  - descriptivas_numericas_comparacion.csv\n")
cat("  - descriptivas_categoricas_resumen.csv\n")
cat("  - justificacion_economica_descriptivos.md\n")
cat("  - missing_variables_train_test.png\n")
cat("  - texto_binarias_proporciones_train_test.png\n")
cat("  - base_estructural_densidades_p01.png\n")
cat("  - osm_densidades_p01.png\n")
cat("  - datos_abiertos_densidades_p01.png\n")
cat("  - categoricas_top_niveles_train_test.png\n")

cat("\nProceso finalizado correctamente.\n")
