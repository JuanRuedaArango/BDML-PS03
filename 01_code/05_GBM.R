# =========================================================
# GBM + SPATIAL CV
# =========================================================

library(caret)
library(dplyr)
library(doParallel)

set.seed(123)

# =========================================================
# 1. DATA
# =========================================================

train2 <- train3
test2  <- test3

# factores
train2 <- train2 %>%
  mutate(
    property_type = as.factor(property_type),
    UPZ = as.factor(UPZ)
  )

test2 <- test2 %>%
  mutate(
    property_type = as.factor(property_type),
    UPZ = as.factor(UPZ)
  )

# =========================================================
# 2. CONTROLES CV
# =========================================================

ctrl_cv <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

ctrl_spatial <- trainControl(
  method = "cv",
  index = groupKFold(train2$UPZ, k = 5),
  savePredictions = "final"
)

# =========================================================
# 3. GRID INICIAL
# =========================================================

gbm_grid2 <- expand.grid(
  interaction.depth = c(5, 6),
  n.trees = c(800, 1200),
  shrinkage = c(0.03),
  n.minobsinnode = c(15)
)

# =========================================================
# 4. PARALLEL
# =========================================================

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

# =========================================================
# 5. CV NORMAL
# =========================================================

gbm_cv2 <- train(
  log_price ~ .,
  data = train2,
  method = "gbm",
  metric = "MAE",
  trControl = ctrl_cv,
  tuneGrid = gbm_grid2,
  verbose = TRUE
)

cv_results <- gbm_cv2$results %>%
  mutate(tipo = "CV normal")

# =========================================================
# 6. SPATIAL CV
# =========================================================

gbm_spatial2 <- train(
  log_price ~ .,
  data = train2,
  method = "gbm",
  metric = "MAE",
  trControl = ctrl_spatial,
  tuneGrid = gbm_grid2,
  verbose = FALSE
)

spatial_results <- gbm_spatial2$results %>%
  mutate(tipo = "Spatial CV")

# =========================================================
# 7. COMPARACIÓN
# =========================================================

results_compare <- bind_rows(cv_results, spatial_results)

print(results_compare %>% arrange(RMSE))

# =========================================================
# 8. MODELO FINAL
# =========================================================

best_model <- gbm_spatial2$bestTune

final_model <- train(
  log_price ~ .,
  data = train3,
  method = "gbm",
  metric = "MAE",
  trControl = trainControl(method = "none"),
  tuneGrid = best_model,
  verbose = FALSE
)

# =========================================================
# 9. PREDICCIÓN
# =========================================================

pred_log <- predict(final_model, test3)

submission <- data.frame(
  property_id = test_feat$property_id,
  price = round(exp(pred_log), 0)
)

hist(submission$price)

# =========================================================
# 10. EXPORT
# =========================================================

write.csv(
  submission,
  "gbm_spatial_t1200_d5_lr003_min15.csv",
  row.names = FALSE
)

# =========================================================
# 11. IMPUTACIÓN
# =========================================================

impute_data <- function(df){
  
  df %>%
    mutate(
      across(where(is.numeric),
             ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
      
      across(where(is.factor), ~ {
        moda <- names(sort(table(.), decreasing = TRUE))[1]
        replace(., is.na(.), moda)
      })
    )
}

train2 <- impute_data(train2)
test2  <- impute_data(test2)

# =========================================================
# 12. GRID COMPLETO
# =========================================================

gbm_grid <- expand.grid(
  interaction.depth = c(5, 6),
  n.trees = c(800, 1200),
  shrinkage = c(0.02, 0.03),
  n.minobsinnode = c(10, 15)
)

# =========================================================
# 13. MODELO SPATIAL DEFINITIVO
# =========================================================

set.seed(123)

gbm_spatial <- train(
  log_price ~ .,
  data = train2,
  method = "gbm",
  metric = "MAE",
  trControl = ctrl_spatial,
  tuneGrid = gbm_grid,
  verbose = FALSE
)

spatial_results <- gbm_spatial$results %>%
  mutate(tipo = "Spatial CV")

print(spatial_results)

# =========================================================
# 14. MODELO FINAL DEFINITIVO
# =========================================================

best_spatial <- gbm_spatial$bestTune

final_gbm <- train(
  log_price ~ .,
  data = train2,
  method = "gbm",
  metric = "MAE",
  trControl = trainControl(method = "none"),
  tuneGrid = best_spatial,
  verbose = FALSE
)

# =========================================================
# 15. PREDICCIÓN FINAL
# =========================================================

pred_log <- predict(final_gbm, newdata = test2)

submission <- data.frame(
  property_id = test$property_id,
  price = exp(pred_log)
)

# =========================================================
# 16. GUARDAR
# =========================================================

write.csv(
  submission,
  "GBM_spatial_final.csv",
  row.names = FALSE
)

stopCluster(cl)

# =========================================================
# 17. MODELO REDUCIDO (MENOS VARIABLES)
# =========================================================

vars_model <- c(
  "log_price", "year",
  "property_type", "zona_cara",
  "Numero_banos", "Numero_bedrooms", "banos_por_habitacion",
  "log_superficie",
  "log_dist_centro", "log_dist_tm", "log_dist_parque",
  "dist_commercial", "dist_industrial", "dist_residential", "dist_cai",
  "texto_lujo_score",
  "PC1", "PC2", "PC3", "PC4", "PC5",
  "cluster"
)

train_m <- train2 %>%
  select(all_of(vars_model)) %>%
  mutate(
    property_type = as.factor(property_type),
    cluster = as.factor(cluster)
  )

test_m <- test2 %>%
  mutate(
    property_type = as.factor(property_type),
    cluster = as.factor(cluster)
  ) %>%
  select(names(train_m)[names(train_m) != "log_price"])

# nuevas features

train_m <- train_m %>%
  mutate(banos_m2 = Numero_banos / log_superficie)

test_m <- test_m %>%
  mutate(banos_m2 = Numero_banos / log_superficie)

# =========================================================
# 18. SPATIAL CV FINAL (SIN UPZ COMO FEATURE)
# =========================================================

ctrl_spatial <- trainControl(
  method = "cv",
  index = groupKFold(train2$UPZ, k = 5),
  savePredictions = "final"
)

gbm_grid <- expand.grid(
  interaction.depth = c(4, 5, 6, 7),
  n.trees = c(800, 1200, 1500, 1800, 2200),
  shrinkage = c(0.01, 0.02, 0.03),
  n.minobsinnode = c(10, 15, 20)
)

set.seed(123)

gbm_spatial <- train(
  log_price ~ .,
  data = train_m,
  method = "gbm",
  metric = "MAE",
  trControl = ctrl_spatial,
  tuneGrid = gbm_grid,
  verbose = TRUE
)

print(gbm_spatial$results)