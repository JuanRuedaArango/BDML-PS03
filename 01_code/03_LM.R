# ============================================================
# LIBRERÍAS
# ============================================================

library(tidyverse)
library(tidymodels)
library(tune)
library(dials)
library(yardstick)
library(workflows)
library(parsnip)
library(spatialsample)
library(readxl)
library(sf)
library(embed)
library(rsample)


# ============================================================
# 2. PREPROCESAMIENTO
# ============================================================

# convertir variables lógicas a factor
train2 <- train2 %>%
  mutate(across(where(is.logical), as.factor))

test2 <- test2 %>%
  mutate(across(where(is.logical), as.factor))

# ============================================================
# 3. RECIPE
# ============================================================

rec <- recipe(log_price ~ ., data = train2) %>%
  
  step_rm(property_id, UPZ) %>%
  
  # manejar niveles nuevos
  step_novel(all_nominal_predictors()) %>%
  
  # manejar NA categóricos
  step_unknown(all_nominal_predictors()) %>%
  
  # imputación numérica
  step_impute_median(all_numeric_predictors()) %>%
  
  # convertir categóricas a dummies
  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%
  
  # eliminar columnas constantes
  step_zv(all_predictors())%>%
  
  # eliminar correlaciones entre variables
  step_corr(all_numeric_predictors(), threshold = 0.95)%>%
  
  # elimina combinaciones lineales
  step_lincomb(all_numeric_predictors())

# ============================================================
# 4. MODELO TUNEABLE
# ============================================================

model_spec <- linear_reg() %>%
  set_engine("lm")

# ============================================================
# 5. WORKFLOW
# ============================================================

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_spec)

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
# CV por UPZ
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

train_sf <- st_transform(train_sf, 3116)

spatial_blocks <- spatial_block_cv(
  train_sf,
  v = 10
)

# ============================================================
# 8. TUNING CV CLÁSICO
# ============================================================

cv_results <- fit_resamples(
  wf,
  resamples = folds_cv,
  metrics = metric_set(mae)
)

# ============================================================
# 9. TUNING GROUP UPZ
# ============================================================

spatial_results <- fit_resamples(
  wf,
  resamples = folds_spatial,
  metrics = metric_set(mae)
)

# ============================================================
# 10. TUNING SPATIAL BLOCKS
# ============================================================

blocks_results <- fit_resamples(
  wf,
  resamples = spatial_blocks,
  metrics = metric_set(mae)
)

# ============================================================
# 11. MEJORES HIPERPARÁMETROS
# ============================================================

best_cv <- select_best(
  cv_results,
  metric = "mae"
)

best_spatial <- select_best(
  spatial_results,
  metric = "mae"
)

best_blocks <- select_best(
  blocks_results,
  metric = "mae"
)

# ============================================================
# 12. FINALIZAR WORKFLOWS
# ============================================================

wf_cv <- finalize_workflow(
  wf,
  best_cv
)

wf_spatial <- finalize_workflow(
  wf,
  best_spatial
)

wf_blocks <- finalize_workflow(
  wf,
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
# 15. MÉTRICAS FINALES
# ============================================================

cv_metrics <- collect_metrics(cv_results) %>%
  mutate(validacion = "cv")

spatial_metrics <- collect_metrics(spatial_results) %>%
  mutate(validacion = "group_upz")

blocks_metrics <- collect_metrics(blocks_results) %>%
  mutate(validacion = "spatial_blocks")

resultados_finales <- bind_rows(
  cv_metrics,
  spatial_metrics,
  blocks_metrics
)

print(resultados_finales)

# ============================================================
# 16. SUBMISSIONS
# ============================================================

submission_cv <- data.frame(
  property_id = test$property_id,
  price = exp(pred_cv$.pred)
)

submission_spatial <- data.frame(
  property_id = test$property_id,
  price = exp(pred_spatial$.pred)
)

submission_blocks <- data.frame(
  property_id = test$property_id,
  price = exp(pred_blocks$.pred)
)

preds_plot <- bind_rows(
  
  submission_cv %>%
    mutate(modelo = "CV"),
  
  submission_spatial %>%
    mutate(modelo = "Spatial"),
  
  submission_blocks %>%
    mutate(modelo = "Spatial Blocks")
)

hist_LM<-ggplot(preds_plot, aes(x = price, fill = modelo)) +
  
  geom_histogram(
    bins = 50,
    alpha = 0.45,
    position = "identity"
  ) +
  
  scale_x_log10() +
  
  labs(
    title = "Distribución de predicciones LM",
    x = "Precio",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  
  theme_minimal()

ggsave(
  filename = "03_outputs/02_plots/hist_LM.png",
  plot = hist_LM,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 17. EXPORTAR
# ============================================================

write.csv(
  submission_cv,
  "03_outputs/01_results/LM_cv.csv",
  row.names = FALSE
)

write.csv(
  submission_spatial,
  "03_outputs/01_results/LM_spatial.csv",
  row.names = FALSE
)

write.csv(
  submission_blocks,
  "03_outputs/01_results/LM_blocks.csv",
  row.names = FALSE
)
