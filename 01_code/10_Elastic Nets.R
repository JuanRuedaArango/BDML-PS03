# ============================================================
# 1. LIBRERÍAS
# ============================================================

library(tidymodels)
library(spatialsample)
library(sf)
library(dplyr)
library(ggplot2)

# ============================================================
# 2. PREPROCESAMIENTO
# ============================================================

# convertir lógicas a factor
train2 <- train2 %>%
  mutate(across(where(is.logical), as.factor))

test2 <- test2 %>%
  mutate(across(where(is.logical), as.factor))

# ============================================================
# 3. RECIPE
# ============================================================

rec <- recipe(log_price ~ ., data = train2) %>%
  
  step_rm(property_id, UPZ, LocNombre) %>%
  
  # manejar niveles nuevos
  step_novel(all_nominal_predictors()) %>%
  
  # manejar NA categóricos
  step_unknown(all_nominal_predictors()) %>%
  
  # imputación numérica
  step_impute_median(all_numeric_predictors()) %>%
  
  # convertir categóricas a dummies
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  
  # eliminar columnas constantes
  step_zv(all_predictors())

# ============================================================
# 4. MODELO ELASTIC NET
# ============================================================

elastic_net_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")

# ============================================================
# 5. WORKFLOW
# ============================================================

workflow_1 <- workflow() %>% 
  add_recipe(rec) %>%
  add_model(elastic_net_spec)

# ============================================================
# 6. ESQUEMAS DE VALIDACIÓN
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
  v = 10
)

# ============================================================
# 7. GRID HIPERPARÁMETROS
# ============================================================

grid_values <- crossing(
  penalty = c(
    0.0001,
    0.0005,
    0.001,
    0.005,
    0.01
  ),
  
  mixture = c(
    0,
    0.25,
    0.5,
    0.75,
    1
  )
)

# ============================================================
# 8. TUNING CV CLÁSICO
# ============================================================

cv_tuned <- tune_grid(
  workflow_1,
  resamples = folds_cv,
  grid = grid_values,
  metrics = metric_set(mae)
)

# ============================================================
# 9. TUNING GROUP UPZ
# ============================================================

spatial_tuned <- tune_grid(
  workflow_1,
  resamples = folds_spatial,
  grid = grid_values,
  metrics = metric_set(mae)
)

# ============================================================
# 10. TUNING SPATIAL BLOCKS
# ============================================================

blocks_tuned <- tune_grid(
  workflow_1,
  resamples = spatial_blocks,
  grid = grid_values,
  metrics = metric_set(mae)
)

# ============================================================
# 11. MEJORES HIPERPARÁMETROS
# ============================================================

best_cv <- select_best(
  cv_tuned,
  metric = "mae"
)

best_spatial <- select_best(
  spatial_tuned,
  metric = "mae"
)

best_blocks <- select_best(
  blocks_tuned,
  metric = "mae"
)


# ============================================================
# 12. FINALIZAR WORKFLOWS
# ============================================================

wf_cv <- finalize_workflow(
  workflow_1,
  best_cv
)

wf_spatial <- finalize_workflow(
  workflow_1,
  best_spatial
)

wf_blocks <- finalize_workflow(
  workflow_1,
  best_blocks
)

# ============================================================
# 13. ENTRENAR MODELOS FINALES
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
# 14. PREDICCIONES
# ============================================================

pred_cv <- predict(
  final_cv,
  new_data = test2
)

pred_spatial <- predict(
  final_spatial,
  new_data = test2
)

pred_blocks <- predict(
  final_blocks,
  new_data = test2
)

# ============================================================
# 15. SUBMISSIONS
# ============================================================

submission_cv <- data.frame(
  property_id = test2$property_id,
  price = exp(pred_cv$.pred)
)

submission_spatial <- data.frame(
  property_id = test2$property_id,
  price = exp(pred_spatial$.pred)
)

submission_blocks <- data.frame(
  property_id = test2$property_id,
  price = exp(pred_blocks$.pred)
)

# ============================================================
# 16. EXPORTAR CSV
# ============================================================

write.csv(
  submission_cv,
  "03_outputs/01_results/EN_cv.csv",
  row.names = FALSE
)

write.csv(
  submission_spatial,
  "03_outputs/01_results/EN_spatial.csv",
  row.names = FALSE
)

write.csv(
  submission_blocks,
  "03_outputs/01_results/EN_blocks.csv",
  row.names = FALSE
)

# ============================================================
# 17. RESULTADOS MÉTRICAS
# ============================================================

cv_metrics <- collect_metrics(cv_tuned) %>%
  mutate(validacion = "cv")

spatial_metrics <- collect_metrics(spatial_tuned) %>%
  mutate(validacion = "group_upz")

blocks_metrics <- collect_metrics(blocks_tuned) %>%
  mutate(validacion = "spatial_blocks")

resultados_finales <- bind_rows(
  cv_metrics,
  spatial_metrics,
  blocks_metrics
)

print(resultados_finales)

# ============================================================
# 18. HISTOGRAMA PREDICCIONES
# ============================================================

preds_plot <- bind_rows(
  
  submission_cv %>%
    mutate(modelo = "CV"),
  
  submission_spatial %>%
    mutate(modelo = "Spatial"),
  
  submission_blocks %>%
    mutate(modelo = "Spatial Blocks")
)

hist_EN <- ggplot(preds_plot, aes(x = price, fill = modelo)) +
  
  geom_histogram(
    bins = 50,
    alpha = 0.45,
    position = "identity"
  ) +
  
  scale_x_log10() +
  
  labs(
    title = "Distribución de predicciones Elastic Net",
    x = "Precio",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  
  theme_minimal()

hist_EN

# ============================================================
# 19. GUARDAR HISTOGRAMA
# ============================================================

ggsave(
  filename = "03_outputs/02_plots/hist_EN.png",
  plot = hist_EN,
  width = 10,
  height = 6,
  dpi = 300
)

print("Elastic Net finalizado correctamente")

