# =========================================================
# PCA + CLUSTERING
# =========================================================

library(dplyr)

# =========================================================
# 1. PREPARACIÓN DE DATOS
# =========================================================

train_no_target <- train_m %>%
  select(-log_price)

full_data <- bind_rows(train_no_target, test_m)

# =========================================================
# 2. VARIABLES PARA PCA
# =========================================================

vars_pca <- full_data %>%
  select(
    Numero_banos,
    Numero_bedrooms,
    banos_por_habitacion,
    superficie,
    log_superficie,
    log_dist_tm,
    log_dist_centro,
    log_dist_parque,
    dist_mall,
    dist_food,
    dist_industrial,
    dist_commercial,
    dist_residential,
    dist_cai,
    n_supermercados_1km,
    n_colegios_1km,
    n_estaciones_1km,
    texto_lujo_score
  ) %>%
  mutate(
    across(everything(), ~ ifelse(is.infinite(.), NA, .))
  ) %>%
  mutate(
    across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))
  ) %>%
  scale()

# =========================================================
# 3. PCA
# =========================================================

pca <- prcomp(vars_pca, center = TRUE, scale. = TRUE)

summary(pca)

# =========================================================
# 4. COMPONENTES PRINCIPALES
# =========================================================

full_data <- full_data %>%
  mutate(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    PC3 = pca$x[, 3],
    PC4 = pca$x[, 4],
    PC5 = pca$x[, 5]
  )

# =========================================================
# 5. CLUSTERING (KMEANS)
# =========================================================

set.seed(123)

k <- 8

clusters <- kmeans(pca$x[, 1:5], centers = k)

full_data$cluster <- clusters$cluster

# =========================================================
# 6. SPLIT TRAIN / TEST
# =========================================================

n_train <- nrow(train_m)

# -------------------------
# TRAIN
# -------------------------

train_m <- train_m %>%
  mutate(
    PC1 = full_data$PC1[1:n_train],
    PC2 = full_data$PC2[1:n_train],
    PC3 = full_data$PC3[1:n_train],
    PC4 = full_data$PC4[1:n_train],
    PC5 = full_data$PC5[1:n_train],
    cluster = full_data$cluster[1:n_train]
  )

# -------------------------
# TEST
# -------------------------

test_m <- test_m %>%
  mutate(
    PC1 = full_data$PC1[(n_train + 1):nrow(full_data)],
    PC2 = full_data$PC2[(n_train + 1):nrow(full_data)],
    PC3 = full_data$PC3[(n_train + 1):nrow(full_data)],
    PC4 = full_data$PC4[(n_train + 1):nrow(full_data)],
    PC5 = full_data$PC5[(n_train + 1):nrow(full_data)],
    cluster = full_data$cluster[(n_train + 1):nrow(full_data)]
  )