
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

lgbm_recipe <- recipe(log_price ~ ., data = train3) %>%
  
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
# 5. MODELO LIGHTGBM
# ============================================================

lgbm_spec <- boost_tree(
  
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune()
  
) %>%
  set_engine(
    "lightgbm",
    counts = FALSE,
    num_leaves = tune()
  ) %>%
  set_mode("regression")

# ============================================================
# 6. WORKFLOW
# ============================================================

lgbm_wf <- workflow() %>%
  add_recipe(lgbm_recipe) %>%
  add_model(lgbm_spec)

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

spatial_blocks <- spatial_block_cv(
  train_sf,
  v = 5
)

# ============================================================
# 8. GRID LIGHTGBM
# ============================================================

lgbm_grid <- crossing(
  
  trees = c(600L, 900L),
  
  tree_depth = c(5L, 6L),
  
  min_n = c(25L, 40L),
  
  learn_rate = c(0.003, 0.005),
  
  loss_reduction = c(0),
  
  sample_size = c(0.7, 0.8),
  
  mtry = c(0.8, 0.9),
  
  num_leaves = c(31L, 50L)
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

lgbm_cv <- tune_grid(
  
  lgbm_wf,
  
  resamples = folds_cv,
  
  grid = lgbm_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)

cv_metrics <- collect_metrics(lgbm_cv) %>%
  mutate(validacion = "CV")

# ============================================================
# 11. SPATIAL CV
# ============================================================

lgbm_spatial <- tune_grid(
  
  lgbm_wf,
  
  resamples = folds_spatial,
  
  grid = lgbm_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)

spatial_metrics <- collect_metrics(lgbm_spatial) %>%
  mutate(validacion = "Spatial")

# ============================================================
# 12. SPATIAL BLOCKS
# ============================================================

lgbm_blocks <- tune_grid(
  
  lgbm_wf,
  
  resamples = spatial_blocks,
  
  grid = lgbm_grid,
  
  metrics = metric_set(mae),
  
  control = control_grid(
    save_pred = TRUE,
    verbose = TRUE
  )
)

blocks_metrics <- collect_metrics(lgbm_blocks) %>%
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
  lgbm_cv,
  metric = "mae"
)

best_spatial <- select_best(
  lgbm_spatial,
  metric = "mae"
)

best_blocks <- select_best(
  lgbm_blocks,
  metric = "mae"
)

print(best_cv)
print(best_spatial)
print(best_blocks)

# ============================================================
# 15. WORKFLOWS FINALES
# ============================================================

final_cv_wf <- finalize_workflow(
  lgbm_wf,
  best_cv
)

final_spatial_wf <- finalize_workflow(
  lgbm_wf,
  best_spatial
)

final_blocks_wf <- finalize_workflow(
  lgbm_wf,
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
  "03_outputs/01_results/LGBM_cv.csv",
  row.names = FALSE
)

write.csv(
  submission_spatial,
  "03_outputs/01_results/LGBM_spatial.csv",
  row.names = FALSE
)

write.csv(
  submission_blocks,
  "03_outputs/01_results/LGBM_blocks.csv",
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

hist_LGBM <- ggplot(
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
    title = "Distribución de predicciones LightGBM",
    x = "Precio",
    y = "Frecuencia",
    fill = "Modelo"
  ) +
  
  theme_minimal()

hist_LGBM

# ============================================================
# 21. GUARDAR HISTOGRAMA
# ============================================================

ggsave(
  filename = "03_outputs/02_plots/hist_LGBM.png",
  plot = hist_LGBM,
  width = 10,
  height = 6,
  dpi = 300
)

# ============================================================
# 22. DETENER CLUSTER
# ============================================================

stopCluster(cl)

print("✅ LightGBM finalizado correctamente")
