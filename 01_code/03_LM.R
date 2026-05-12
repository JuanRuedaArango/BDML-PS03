# ============================================================
# LIBRERÍAS
# ============================================================

library(tidyverse)
library(tidymodels)

tidymodels_prefer()

# ============================================================
# 1. FEATURE ENGINEERING CONSISTENTE
# ============================================================

prep_data <- function(df) {
  
  df %>%
    mutate(
      # -------------------------
      # TARGET (solo si existe)
      # -------------------------
      log_price = ifelse("price" %in% names(df), log(price), NA),
      
      # -------------------------
      # SUPERFICIE LIMPIA
      # -------------------------
      surface_total = case_when(
        surface_total < 20 ~ NA_real_,
        surface_total > 600 ~ NA_real_,
        TRUE ~ surface_total
      ),
      
      surface_total = ifelse(
        is.na(surface_total),
        median(surface_total, na.rm = TRUE),
        surface_total
      ),
      
      log_surface = log1p(surface_total),
      
      # -------------------------
      # FEATURES ESPACIALES
      # -------------------------
      lat_lon_interaction = lat * lon,
      lat2 = lat^2,
      lon2 = lon^2,
      
      # -------------------------
      # RATIOS
      # -------------------------
      banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
      
      densidad_servicios =
        n_supermercados_1km +
        n_colegios_1km +
        n_estaciones_1km +
        n_tiendas_1km,
      
      # -------------------------
      # INTERACCIONES
      # -------------------------
      area_x_lujo = log_surface * es_lujo
    )
}

# ============================================================
# 2. APLICAR TRANSFORMACIÓN
# ============================================================

train_p <- prep_data(train)
test_p  <- prep_data(test)

# eliminar price del train después de crear log_price
train_p <- train_p %>% select(-price)

# ============================================================
# 3. RECETA LIMPIA
# ============================================================

rec <- recipe(log_price ~ ., data = train_p) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  # no linealidad clave
  step_ns(log_surface, deg_free = 4) %>%
  step_ns(Numero_banos, deg_free = 3) %>%
  step_ns(Numero_bedrooms, deg_free = 3) %>%
  step_ns(log_dist_centro, deg_free = 4) %>%
  step_ns(log_dist_tm, deg_free = 3) %>%
  
  # limpieza
  step_zv(all_predictors()) %>%
  step_corr(all_numeric_predictors(), threshold = 0.9)

# ============================================================
# 4. MODELO (RIDGE)
# ============================================================

model_spec <- linear_reg(
  penalty = 0.001,
  mixture = 0
) %>%
  set_engine("glmnet")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_spec)

# ============================================================
# 5. SPATIAL CV (UPZ)
# ============================================================

set.seed(123)

folds <- group_vfold_cv(
  train_p,
  group = UPZ,
  v = 5
)

cv_results <- fit_resamples(
  wf,
  resamples = folds,
  metrics = metric_set(mae)
)

collect_metrics(cv_results)

# ============================================================
# 6. MODELO FINAL
# ============================================================

final_model <- fit(wf, data = train_p)

# ============================================================
# 7. PREDICCIÓN
# ============================================================

pred_log <- predict(final_model, test_p)

# ============================================================
# 8. SUBMISSION
# ============================================================

submission <- data.frame(
  property_id = test$property_id,
  price = exp(pred_log$.pred)
)

# ============================================================
# 9. EXPORT
# ============================================================

write.csv(submission, "ridge_spatial_final.csv", row.names = FALSE)