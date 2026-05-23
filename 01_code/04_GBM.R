# =========================
# GBM
# =========================


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

# quitar geometría si existe
train3 <- st_drop_geometry(train3)
test3  <- st_drop_geometry(test3)

# ============================================================
# 4. RECIPE PARA LIMPIEZA
# ============================================================

prep_rec <- recipe(log_price ~ ., data = train3) %>%

  step_rm(
    property_id,
    UPZ,
    LocNombre
  ) %>%

  step_novel(all_nominal_predictors()) %>%  

  step_impute_median(all_numeric_predictors()) %>%

  step_dummy(all_nominal_predictors(), one_hot = FALSE) %>%

  step_zv(all_predictors()) %>%

  step_corr(all_numeric_predictors(), threshold = 0.95) %>%

  step_lincomb(all_numeric_predictors()) %>%

  prep()

# aplicar recipe

train3 <- bake(prep_rec, new_data = train3)

test3 <- bake(prep_rec, new_data = test3)

# ============================================================
# 5. CONTROLES CV
# ============================================================

# -----------------------------
# CV clásico
# -----------------------------

ctrl_cv <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  allowParallel = TRUE
)

# -----------------------------
# CV espacial por UPZ
# -----------------------------

ctrl_spatial <- trainControl(
  method = "cv",
  index = groupKFold(train2$UPZ, k = 5),
  savePredictions = "final",
  allowParallel = TRUE
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

block_index <- lapply(
  spatial_blocks$splits,
  function(x) x$in_id
)

ctrl_blocks <- trainControl(
  method = "cv",
  index = block_index,
  savePredictions = "final",
  allowParallel = TRUE
)

# ============================================================
# 6. GRID GBM
# ============================================================

gbm_grid <- expand.grid(
  
  interaction.depth = c(5, 6, 8),
  
  n.trees = c(8000, 1800),
  
  shrinkage = c(0.005, 0.01),
  
  n.minobsinnode = c(20, 25, 30)
)

# ============================================================
# 7. PARALELIZACIÓN
# ============================================================

cl <- makePSOCKcluster(
  parallel::detectCores() - 2
)

registerDoParallel(cl)

# ============================================================
# 8. CV NORMAL
# ============================================================

gbm_cv <- train(
  
  log_price ~ .,
  
  data = train3,
  
  method = "gbm",
  
  metric = "MAE",
  
  trControl = ctrl_cv,
  
  tuneGrid = gbm_grid,
  
  verbose = TRUE
)

cv_results <- gbm_cv$results %>%
  mutate(tipo = "CV normal")

# ============================================================
# 9. SPATIAL CV
# ============================================================

gbm_spatial <- train(

  log_price ~ .,
 
  data = train3,

  method = "gbm",
 
  metric = "MAE",
 
  trControl = ctrl_spatial,
 
  tuneGrid = gbm_grid,
 
  verbose = FALSE
)

spatial_results <- gbm_spatial$results %>%
  mutate(tipo = "Spatial CV")

# ============================================================
# 10. SPATIAL BLOCKS
# ============================================================

gbm_blocks <- train(

  log_price ~ .,

  data = train3,
 
  method = "gbm",
 
  metric = "MAE",
 
  trControl = ctrl_blocks,
 
  tuneGrid = gbm_grid,
 
  verbose = FALSE
)

blocks_results <- gbm_blocks$results %>%
  mutate(tipo = "Spatial Blocks")

# ============================================================
# 11. COMPARACIÓN
# ============================================================

results_compare <- bind_rows(
  cv_results,
  spatial_results,
  blocks_results
)

print(
  results_compare %>%
    arrange(MAE)
)

# ============================================================
# 12. MEJORES HIPERPARÁMETROS
# ============================================================

best_cv <- gbm_cv$bestTune

best_spatial <- gbm_spatial$bestTune

best_blocks <- gbm_blocks$bestTune

# ============================================================
# 13. MODELOS FINALES
# ============================================================

final_cv <- train(
  
  log_price ~ .,
  
  data = train3,
  
  method = "gbm",
  
  metric = "MAE",
  
  trControl = ctrl_cv,
  
  tuneGrid = best_cv,
  
  verbose = FALSE
)

final_spatial <- train(
  
  log_price ~ .,
  
  data = train3,
  
  method = "gbm",
  
  metric = "MAE",
  
  trControl = ctrl_spatial,
  
  tuneGrid = best_spatial,
  
  verbose = FALSE
)

final_blocks <- train(
  
  log_price ~ .,
  
  data = train3,
  
  method = "gbm",
  
  metric = "MAE",
  
  trControl = ctrl_blocks,
  
  tuneGrid = best_blocks,
  
  verbose = FALSE
)

# ============================================================
# 14. PREDICCIONES
# ============================================================

pred_cv <- predict(
  final_cv,
  newdata = test3
)

pred_spatial <- predict(
  final_spatial,
  newdata = test3
)

pred_blocks <- predict(
  final_blocks,
  newdata = test3
)

# ============================================================
# 15. SUBMISSIONS
# ============================================================

submission_cv <- data.frame(
  property_id = test2$property_id,
  price = round(exp(pred_cv), 0)
)

submission_spatial <- data.frame(
  property_id = test2$property_id,
  price = round(exp(pred_spatial), 0)
)

submission_blocks <- data.frame(
  property_id = test2$property_id,
  price = round(exp(pred_blocks), 0)
)

# ============================================================
# 16. EXPORTAR CSV
# ============================================================

write.csv(
  submission_cv,
  "03_outputs/01_results/GBM_cv.csv",
  row.names = FALSE
)

write.csv(
  submission_spatial,
  "03_outputs/01_results/GBM_spatial.csv",
  row.names = FALSE
)

write.csv(
  submission_blocks,
  "03_outputs/01_results/GBM_blocks.csv",
  row.names = FALSE
)

# ============================================================
# 17. HISTOGRAMA
# ============================================================

preds_plot <- bind_rows(
  
  submission_cv %>%
    mutate(modelo = "CV"),
  
  submission_spatial %>%
    mutate(modelo = "Spatial"),
  
  submission_blocks %>%
    mutate(modelo = "Spatial Blocks")
)

hist_GBM <- ggplot(
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
    title = "Distribución de predicciones GBM",
    x = "Precio",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  
  theme_minimal()

hist_GBM

# ============================================================
# 18. GUARDAR HISTOGRAMA
# ============================================================

ggsave(
  filename = "03_outputs/02_plots/hist_GBM.png",
  plot = hist_GBM,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 19. DETENER CLUSTER
# ============================================================

stopCluster(cl)

print("✅ GBM finalizado correctamente")
