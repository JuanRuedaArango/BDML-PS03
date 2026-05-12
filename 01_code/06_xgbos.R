# ============================================================
# 0. LIBRERÍAS
# ============================================================

library(tidymodels)
library(doParallel)
library(embed)

tidymodels_prefer()

# ============================================================
# 1. PARALLEL
# ============================================================

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# ============================================================
# 2. FEATURE ENGINEERING (SEGURO)
# ============================================================

train2 <- train %>%
  mutate(
    log_price = log(price),
    
    # espaciales
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    # ratios útiles
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
    densidad_servicios = n_supermercados_1km + n_colegios_1km
  ) %>%
  select(-price)

test2 <- test %>%
  mutate(
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
    densidad_servicios = n_supermercados_1km + n_colegios_1km
  )

# ============================================================
# 3. RECIPE (ROBUSTO)
# ============================================================

rec <- recipe(log_price ~ ., data = train2) %>%
  
  # imputación (CLAVE)
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  
  # target encoding SOLO UPZ
  step_lencode_glm(UPZ, outcome = vars(log_price)) %>%
  step_rm(UPZ) %>%
  
  # logical → numeric
  step_mutate(across(where(is.logical), as.integer)) %>%
  
  # dummies
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  
  # limpieza
  step_zv(all_predictors())

# ============================================================
# 4. MODELO XGBOOST
# ============================================================

gbm_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  min_n = tune()
) %>%
  set_engine(
    "xgboost",
    eval_metric = "mae"
  ) %>%
  set_mode("regression")

# ============================================================
# 5. WORKFLOW
# ============================================================

gbm_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(gbm_spec)

# ============================================================
# 6. GRID (BUENO Y ESTABLE)
# ============================================================

library(dials)

gbm_grid <- grid_space_filling(
  trees(range = c(800, 1800)),
  tree_depth(range = c(4, 10)),
  learn_rate(range = c(-3, -1)),   # 0.001 - 0.1
  loss_reduction(),
  sample_size = sample_prop(range = c(0.6, 1)),
  min_n(range = c(5, 25)),
  size = 40
)

# ============================================================
# 7. SPATIAL CV (EL IMPORTANTE)
# ============================================================

set.seed(123)

folds_spatial <- group_vfold_cv(
  train2,
  group = UPZ,
  v = 5
)

gbm_spatial <- tune_grid(
  gbm_wf,
  resamples = folds_spatial,
  grid = gbm_grid,
  metrics = metric_set(mae),
  control = control_grid(save_pred = TRUE)
)

spatial_results <- collect_metrics(gbm_spatial)

print(spatial_results)

# ============================================================
# 8. MEJOR MODELO
# ============================================================

best_params <- select_best(gbm_spatial, metric = "mae")

final_gbm <- finalize_workflow(gbm_wf, best_params)

# ============================================================
# 9. FIT FINAL
# ============================================================

final_model <- fit(final_gbm, data = train2)

# ============================================================
# 10. PREDICCIÓN
# ============================================================

pred_log <- predict(final_model, test2)

# sanity check
print(sum(is.na(pred_log)))  # debe ser 0

# ============================================================
# 11. SUBMISSION
# ============================================================

submission <- data.frame(
  property_id = test_feat$property_id,
  price = exp(pred_log$.pred)
)

# opcional: recorte de outliers extremos
submission$price <- pmax(submission$price, 50000000)

# guardar con nombre inteligente
file_name <- sprintf(
  "XGB_spatial_t%d_d%d_lr%.4f_min%d_s%.2f.csv",
  best_params$trees,
  best_params$tree_depth,
  best_params$learn_rate,
  best_params$min_n,
  best_params$sample_size
)

write.csv(submission, file_name, row.names = FALSE)

# ============================================================
# 12. STOP PARALLEL
# ============================================================

stopCluster(cl)
registerDoSEQ()