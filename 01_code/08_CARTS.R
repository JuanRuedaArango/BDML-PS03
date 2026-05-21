# =========================
# CART
# =========================

library(tidymodels)

cart_spec <- decision_tree(
  
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
  
) %>%
  
  set_engine("rpart") %>%
  
  set_mode("regression")

# =========================
# RECIPE
# =========================

cart_recipe <- recipe(log_price ~ ., data = train2) %>%
  
  step_rm(
    UPZ,
    precio_m2_kernel,
    precio_vecinos_kernel,
    vref_ratio
  ) %>%
  
  step_unknown(all_nominal_predictors()) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_zv(all_predictors())

# =========================
# WORKFLOW
# =========================

library(doParallel)
cl <- makePSOCKcluster(parallel::detectCores() - 1)

registerDoParallel(cl)

cart_workflow <- workflow() %>%
  add_recipe(cart_recipe) %>%
  add_model(cart_spec)

# =========================
# GRID
# =========================

cart_grid <- grid_space_filling(
  
  cost_complexity(),
  
  tree_depth(range = c(2L, 20L)),
  
  min_n(range = c(2L, 40L)),
  
  size = 20
)

# =========================
# FOLDS ESPACIALES
# =========================

folds_spatial <- group_vfold_cv(
  train2,
  group = UPZ,
  v = 5
)

# =========================
# TUNING
# =========================

cart_res <- tune_grid(
  
  cart_workflow,
  
  resamples = folds_spatial,
  
  grid = cart_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )
)

# =========================
# RESULTADOS
# =========================

show_best(cart_res, metric = "mae")

# =========================
# MEJOR MODELO
# =========================

best_cart <- select_best(cart_res, metric = "mae")

final_cart <- finalize_workflow(
  cart_workflow,
  best_cart
)

# =========================
# FIT FINAL
# =========================

cart_fit <- fit(final_cart, data = train2)

# =========================
# PREDICCIONES
# =========================

preds_cart <- predict(cart_fit, new_data = test2) %>%
  pull(.pred)

preds_price <- round(exp(preds_cart), 0)

hist(preds_price)

# =========================
# SUBMISSION
# =========================

submission <- data.frame(
  property_id = test$property_id,
  price = preds_price
)

write.csv(
  submission,
  "CART_cost0.0001_t9_n40.csv",
  row.names = FALSE
)



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
# 2. PARALELIZACIÓN
# ============================================================

cl <- makePSOCKcluster(parallel::detectCores() - 2)

registerDoParallel(cl)

# ============================================================
# 3. PREPROCESAMIENTO
# ============================================================

train2 <- train2 %>%
  mutate(across(where(is.logical), as.factor))

test2 <- test2 %>%
  mutate(across(where(is.logical), as.factor))

# ============================================================
# 4. RECIPE
# ============================================================

cart_recipe <- recipe(log_price ~ ., data = train2) %>%
  
  step_rm(
    property_id, UPZ,LocNombre
  ) %>%
  
  step_novel(all_nominal_predictors()) %>%
  
  step_unknown(all_nominal_predictors()) %>%
  
  step_impute_median(all_numeric_predictors()) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_zv(all_predictors())

# ============================================================
# 5. MODELO CART
# ============================================================

cart_spec <- decision_tree(
  
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
  
) %>%
  
  set_engine("rpart") %>%
  
  set_mode("regression")

# ============================================================
# 6. WORKFLOW
# ============================================================

cart_workflow <- workflow() %>%
  add_recipe(cart_recipe) %>%
  add_model(cart_spec)

# ============================================================
# 7. ESQUEMAS DE VALIDACIÓN
# ============================================================

set.seed(123)

# -----------------------------
# CV clásico
# -----------------------------

folds_cv <- vfold_cv(
  train2,
  v = 5
)

# -----------------------------
# CV espacial por UPZ
# -----------------------------

folds_spatial <- group_vfold_cv(
  train2,
  group = UPZ,
  v = 5
)

# -----------------------------
# Spatial blocks
# -----------------------------

train_sf <- st_as_sf(
  train2,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)

spatial_blocks <- spatial_block_cv(
  train_sf,
  v = 5
)

# ============================================================
# 8. GRID HIPERPARÁMETROS
# ============================================================

cart_grid <- grid_space_filling(
  
  cost_complexity(),
  
  tree_depth(range = c(2L, 20L)),
  
  min_n(range = c(2L, 40L)),
  
  size = 20
)

# ============================================================
# 9. TUNING CV CLÁSICO
# ============================================================

cart_cv_res <- tune_grid(
  
  cart_workflow,
  
  resamples = folds_cv,
  
  grid = cart_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )
)

# ============================================================
# 10. TUNING GROUP UPZ
# ============================================================

cart_spatial_res <- tune_grid(
  
  cart_workflow,
  
  resamples = folds_spatial,
  
  grid = cart_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )
)

# ============================================================
# 11. TUNING SPATIAL BLOCKS
# ============================================================

cart_blocks_res <- tune_grid(
  
  cart_workflow,
  
  resamples = spatial_blocks,
  
  grid = cart_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    verbose = TRUE
  )
)

# ============================================================
# 12. MEJORES HIPERPARÁMETROS
# ============================================================

best_cv <- select_best(
  cart_cv_res,
  metric = "mae"
)

best_spatial <- select_best(
  cart_spatial_res,
  metric = "mae"
)

best_blocks <- select_best(
  cart_blocks_res,
  metric = "mae"
)

# ============================================================
# 13. FINALIZAR WORKFLOWS
# ============================================================

wf_cv <- finalize_workflow(
  cart_workflow,
  best_cv
)

wf_spatial <- finalize_workflow(
  cart_workflow,
  best_spatial
)

wf_blocks <- finalize_workflow(
  cart_workflow,
  best_blocks
)

# ============================================================
# 14. FIT FINAL
# ============================================================

final_cv <- fit(
  wf_cv,
  data = train2
)

final_spatial <- fit(
  wf_spatial,
  data = train2
)

final_blocks <- fit(
  wf_blocks,
  data = train2
)

# ============================================================
# 15. PREDICCIONES
# ============================================================

pred_cv <- predict(
  final_cv,
  new_data = test2
) %>%
  pull(.pred)

pred_spatial <- predict(
  final_spatial,
  new_data = test2
) %>%
  pull(.pred)

pred_blocks <- predict(
  final_blocks,
  new_data = test2
) %>%
  pull(.pred)

# ============================================================
# 16. TRANSFORMAR A PRECIO
# ============================================================

pred_price_cv <- round(exp(pred_cv), 0)

pred_price_spatial <- round(exp(pred_spatial), 0)

pred_price_blocks <- round(exp(pred_blocks), 0)

# ============================================================
# 17. SUBMISSIONS
# ============================================================

submission_cv <- data.frame(
  property_id = test2$property_id,
  price = pred_price_cv
)

submission_spatial <- data.frame(
  property_id = test2$property_id,
  price = pred_price_spatial
)

submission_blocks <- data.frame(
  property_id = test2$property_id,
  price = pred_price_blocks
)

# ============================================================
# 18. EXPORTAR CSV
# ============================================================

write.csv(
  submission_cv,
  "03_outputs/01_results/CART_cv.csv",
  row.names = FALSE
)

write.csv(
  submission_spatial,
  "03_outputs/01_results/CART_spatial.csv",
  row.names = FALSE
)

write.csv(
  submission_blocks,
  "03_outputs/01_results/CART_blocks.csv",
  row.names = FALSE
)

# ============================================================
# 19. MÉTRICAS
# ============================================================

cv_metrics <- collect_metrics(cart_cv_res) %>%
  mutate(validacion = "cv")

spatial_metrics <- collect_metrics(cart_spatial_res) %>%
  mutate(validacion = "group_upz")

blocks_metrics <- collect_metrics(cart_blocks_res) %>%
  mutate(validacion = "spatial_blocks")

resultados_finales <- bind_rows(
  cv_metrics,
  spatial_metrics,
  blocks_metrics
)

print(resultados_finales)

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

hist_CART <- ggplot(preds_plot, aes(x = price, fill = modelo)) +
  
  geom_histogram(
    bins = 50,
    alpha = 0.45,
    position = "identity"
  ) +
  
  scale_x_log10() +
  
  labs(
    title = "Distribución de predicciones CART",
    x = "Precio",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  
  theme_minimal()

hist_CART

# ============================================================
# 21. GUARDAR HISTOGRAMA
# ============================================================

ggsave(
  filename = "03_outputs/02_plots/hist_CART.png",
  plot = hist_CART,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 22. DETENER CLUSTER
# ============================================================

stopCluster(cl)

print("CART finalizado correctamente")
