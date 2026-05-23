# ============================================================
# 1. LIBRERÍAS
# ============================================================

library(tidymodels)
library(spatialsample)
library(sf)
library(dplyr)
library(ggplot2)
library(doParallel)

# ============================================================
# 2. COPIAS
# ============================================================

train3 <- train2
test3  <- test2

# ============================================================
# 3. PREPROCESAMIENTO
# ============================================================

train3 <- train3 %>%
  mutate(across(where(is.logical), as.factor))

test3 <- test3 %>%
  mutate(across(where(is.logical), as.factor))

train3 <- st_drop_geometry(train3)
test3  <- st_drop_geometry(test3)

# ============================================================
# 4. RECIPE
# ============================================================

rf_recipe <- recipe(log_price ~ ., data = train3) %>%
  
  step_rm(
    property_id,
    UPZ,
    LocNombre
  ) %>%
  
  step_novel(all_nominal_predictors()) %>%
  
  step_unknown(all_nominal_predictors()) %>%
  
  step_impute_median(all_numeric_predictors()) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_zv(all_predictors()) %>%
  
  step_corr(all_numeric_predictors(), threshold = 0.95) %>%
  
  step_lincomb(all_numeric_predictors())

# ============================================================
# 5. MODELO RF
# ============================================================

rf_spec <- rand_forest(
  
  trees = 500,
  
  mtry = tune(),
  
  min_n = tune()
  
) %>%
  
  set_engine(
    
    "ranger",
    
    importance = "none",
    
    num.threads = parallel::detectCores() - 1
    
  ) %>%
  
  set_mode("regression")

# ============================================================
# 6. WORKFLOW
# ============================================================

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

# ============================================================
# 7. ESQUEMAS CV
# ============================================================

set.seed(123)

# -----------------------------
# CV clásico
# -----------------------------

folds_cv <- vfold_cv(
  train3,
  v = 5
)

# -----------------------------
# Spatial folds UPZ
# -----------------------------

folds_spatial <- group_vfold_cv(
  train3,
  group = UPZ,
  v = 5
)

# -----------------------------
# Spatial blocks
# -----------------------------

train_sf <- st_as_sf(
  train3,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

train_sf <- st_transform(train_sf, 3116)

spatial_blocks <- spatial_block_cv(
  train_sf,
  v = 5
)

# ============================================================
# 8. GRID RF
# ============================================================

rf_grid <- crossing(
  
  mtry = c(0.15, 0.25, 0.4),
  
  min_n = c(5L, 10L, 20L)
)

# ============================================================
# 9. PARALELIZACIÓN
# ============================================================

cl <- makePSOCKcluster(
  parallel::detectCores() - 1
)

registerDoParallel(cl)

# ============================================================
# 10. CV NORMAL
# ============================================================

rf_cv <- tune_grid(
  
  rf_workflow,
  
  resamples = folds_cv,
  
  grid = rf_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)

cv_metrics <- collect_metrics(rf_cv) %>%
  mutate(validacion = "CV")

# ============================================================
# 11. SPATIAL CV
# ============================================================

rf_spatial <- tune_grid(
  
  rf_workflow,
  
  resamples = folds_spatial,
  
  grid = rf_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)

spatial_metrics <- collect_metrics(rf_spatial) %>%
  mutate(validacion = "Spatial")

# ============================================================
# 12. SPATIAL BLOCKS
# ============================================================

rf_blocks <- tune_grid(
  
  rf_workflow,
  
  resamples = spatial_blocks,
  
  grid = rf_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)

blocks_metrics <- collect_metrics(rf_blocks) %>%
  mutate(validacion = "Spatial Blocks")

# ============================================================
# 13. RESULTADOS
# ============================================================

resultados_finales <- bind_rows(
  cv_metrics,
  spatial_metrics,
  blocks_metrics
)

print(
  resultados_finales %>%
    arrange(mean)
)

View(resultados_finales)
# ============================================================
# 14. MEJORES HIPERPARÁMETROS
# ============================================================

best_cv <- select_best(
  rf_cv,
  metric = "mae"
)

best_spatial <- select_best(
  rf_spatial,
  metric = "mae"
)

best_blocks <- select_best(
  rf_blocks,
  metric = "mae"
)

print(best_cv)
print(best_spatial)
print(best_blocks)

# ============================================================
# 15. WORKFLOWS FINALES
# ============================================================

final_cv_wf <- finalize_workflow(
  rf_workflow,
  best_cv
)

final_spatial_wf <- finalize_workflow(
  rf_workflow,
  best_spatial
)

final_blocks_wf <- finalize_workflow(
  rf_workflow,
  best_blocks
)

# ============================================================
# 16. FIT FINAL
# ============================================================

final_cv <- fit(
  final_cv_wf,
  data = train3
)

final_spatial <- fit(
  final_spatial_wf,
  data = train3
)

final_blocks <- fit(
  final_blocks_wf,
  data = train3
)

# ============================================================
# 17. PREDICCIONES
# ============================================================

pred_cv <- predict(
  final_cv,
  new_data = test3
)

pred_spatial <- predict(
  final_spatial,
  new_data = test3
)

pred_blocks <- predict(
  final_blocks,
  new_data = test3
)

# ============================================================
# 18. SUBMISSIONS
# ============================================================

submission_cv <- data.frame(
  property_id = test2$property_id,
  price = round(exp(pred_cv$.pred), 0)
)

submission_spatial <- data.frame(
  property_id = test2$property_id,
  price = round(exp(pred_spatial$.pred), 0)
)

submission_blocks <- data.frame(
  property_id = test2$property_id,
  price = round(exp(pred_blocks$.pred), 0)
)

# ============================================================
# 19. EXPORTAR CSV
# ============================================================

write.csv(
  submission_cv,
  "03_outputs/01_results/RF_cv.csv",
  row.names = FALSE
)

write.csv(
  submission_spatial,
  "03_outputs/01_results/RF_spatial.csv",
  row.names = FALSE
)

write.csv(
  submission_blocks,
  "03_outputs/01_results/RF_blocks.csv",
  row.names = FALSE
)

# ============================================================
# 20. HISTOGRAMA
# ============================================================

preds_plot <- bind_rows(
  
  submission_cv %>%
    mutate(modelo = "CV"),
  
  submission_spatial %>%
    mutate(modelo = "Spatial"),
  
  submission_blocks %>%
    mutate(modelo = "Spatial Blocks")
)

hist_RF <- ggplot(
  preds_plot,
  aes(x = price, fill = modelo)
) +
  
  geom_histogram(
    bins = 50,
    alpha = 0.45,
    position = "identity"
  ) +
  
  scale_x_log10() +
  
  labs(
    title = "Distribución de predicciones Random Forest",
    x = "Precio",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  
  theme_minimal()

hist_RF

# ============================================================
# 21. GUARDAR HISTOGRAMA
# ============================================================

ggsave(
  filename = "03_outputs/02_plots/hist_RF.png",
  plot = hist_RF,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 22. DETENER CLUSTER
# ============================================================

stopCluster(cl)

print("✅ Random Forest finalizado correctamente")
