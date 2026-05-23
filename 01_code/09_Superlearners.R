
# ============================================================
# 1. LIBRERÍAS
# ============================================================

library(sl3)
library(dplyr)
library(recipes)
library(rsample)
library(spatialsample)
library(sf)
library(tibble)
library(origami)

# ============================================================
# 2. PREPROCESAMIENTO
# ============================================================

train3 <- st_drop_geometry(train2)
test3  <- st_drop_geometry(test2)

# ============================================================
# 3. RECIPE
# ============================================================

prep_rec <- recipe(log_price ~ ., data = train3) %>%
  
  step_rm(property_id, UPZ, LocNombre) %>%
  
  step_novel(all_nominal_predictors()) %>%
  
  step_unknown(all_nominal_predictors()) %>%
  
  step_impute_median(all_numeric_predictors()) %>%
  
  step_dummy(all_nominal_predictors()) %>%
  
  step_zv(all_predictors()) %>%
  
  step_corr(all_numeric_predictors(), threshold = 0.95) %>%
  
  step_lincomb(all_numeric_predictors()) %>%
  
  prep()

# bake train/test
train_sl <- bake(prep_rec, new_data = train3)
test_sl  <- bake(prep_rec, new_data = test3)

# ============================================================
# 4. X / Y
# ============================================================

Y <- train_sl$log_price

X <- train_sl %>%
  select(-log_price)

# ============================================================
# 5. SPATIAL FOLDS (UPZ)
# ============================================================

folds_rs <- group_vfold_cv(
  train2,
  group = UPZ,
  v = 5
)

# ============================================================
# CONVERTIR FOLDS A FORMATO sl3/origami
# ============================================================

folds <- lapply(seq_along(folds_rs$splits), function(i) {
  
  split_i <- folds_rs$splits[[i]]
  
  valid_ids <- as.integer(
    rownames(
      assessment(split_i)
    )
  )
  
  train_ids <- setdiff(
    seq_len(nrow(train_sl)),
    valid_ids
  )
  
  origami::make_fold(
    training_set = train_ids,
    validation_set = valid_ids,
    v = i
  )
})

# ============================================================
# 6. LEARNERS
# ============================================================

learners <- Stack$new(
  
  # baseline
  Lrnr_mean$new(),
  
  # GLM
  Lrnr_glm$new(),
  
  # Elastic Net
  Lrnr_glmnet$new(alpha = 0),
  Lrnr_glmnet$new(alpha = 0.5),
  Lrnr_glmnet$new(alpha = 1),
  
  # GBM BEST
  Lrnr_gbm$new(
    n.trees = 1100,
    interaction.depth = 7,
    shrinkage = 0.02,
    n.minobsinnode = 10,
    bag.fraction = 0.6
  ),
  
  # GBM REGULARIZADO
  Lrnr_gbm$new(
    n.trees = 600,
    interaction.depth = 4,
    shrinkage = 0.05,
    n.minobsinnode = 25,
    bag.fraction = 0.8
  ),
  
  # RANDOM FOREST
  Lrnr_ranger$new(
    num.trees = 800,
    min.node.size = 5
  )
)

# ============================================================
# 7. METALEARNER
# ============================================================

metalearner <- Lrnr_nnls$new()

# ============================================================
# 8. TASK
# ============================================================

task <- sl3_Task$new(
  
  data = data.frame(
    log_price = Y,
    X
  ),
  
  covariates = names(X),
  
  outcome = "log_price",
  
  folds = folds
)

# ============================================================
# 9. SUPERLEARNER
# ============================================================

sl <- Lrnr_sl$new(
  
  learners = learners,
  
  metalearner = metalearner
)

# ============================================================
# 10. TRAIN
# ============================================================

set.seed(123)

sl_fit <- sl$train(task)

# ============================================================
# 11. IMPORTANCIA / RIESGO
# ============================================================

sl_fit$cv_risk(loss_squared_error)[, 1:3]

# ============================================================
# 12. TASK TEST
# ============================================================

test_sl$log_price <- NA

task_test <- sl3_Task$new(
  
  data = test_sl,
  
  covariates = setdiff(names(test_sl), "log_price"),
  
  outcome = "log_price"
)

# ============================================================
# 13. PREDICCIONES
# ============================================================

preds_sl <- sl_fit$predict(task_test)

summary(preds_sl)

# ============================================================
# 14. PASAR A PRECIO
# ============================================================

preds_price <- round(exp(preds_sl), 0)

# ============================================================
# 15. SUBMISSION
# ============================================================

submission <- tibble(
  
  property_id = test2$property_id,
  
  price = preds_price
)

# ============================================================
# 16. HISTOGRAMA
# ============================================================

hist(
  submission$price,
  breaks = 50,
  main = "Distribución predicciones SuperLearner",
  xlab = "Precio"
)

# ============================================================
# 17. EXPORT
# ============================================================

write.csv(
  submission,
  "SL_GLM_EN_RF_GBM_SPATIAL.csv",
  row.names = FALSE
)
