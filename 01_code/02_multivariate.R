# =========================================================
# PCA + CLUSTERING
# =========================================================
#
# Descripción:
#   Este script realiza reducción de dimensionalidad y
#   agrupamiento sobre variables generadas en el proceso
#   de feature engineering para el modelamiento de precios
#   inmobiliarios en Bogotá D.C.
#
# Inputs:
#   - train_out.xlsx
#   - test_out.xlsx
#
# Outputs:
#   - train2.xlsx
#   - test2.xlsx
#
# Procesos realizados:
#
#   1. Integración de bases train y test
#   2. Limpieza y transformación de variables
#   3. Estandarización de variables numéricas
#   4. Análisis de Componentes Principales (PCA)
#   5. Generación de clusters mediante K-Means
#   6. Incorporación de componentes principales
#      a las bases finales
#
# Componentes construidos:
#
#   • PCA - Tenencias:
#       Variables relacionadas con amenidades y
#       características del inmueble.
#
#   • PCA - Espacio:
#       Variables urbanas y categóricas espaciales.
#
#   • PCA - Distancias:
#       Variables de accesibilidad y cercanía a
#       servicios urbanos.
#
#   • PCA - Variables urbanas:
#       Indicadores socioeconómicos y catastrales.
#
#   • PCA - Tamaño:
#       Variables físicas y dimensionales del inmueble.
#
# =========================================================
# LIBRERÍAS
# =========================================================

library(dplyr)
library(fastDummies)

# =========================================================
# 1. PREPARACIÓN DE DATOS
# =========================================================

setwd("D:/Users/Usuario/Documents/BDML-PS03")

train_out <- read_xlsx("03_outputs/03_datasets/train_out.xlsx")
test_out  <- read_xlsx("03_outputs/03_datasets/test_out.xlsx")

train_no_target <- train_out
test_out$price <- as.numeric(as.character(test_out$price))
full_data <- bind_rows(train_no_target, test_out)

# =========================================================
# 2. PCA - TENENCIAS
# =========================================================

vars_pca <- full_data %>%
  select(
    tiene_parqueadero,
    tiene_terraza,
    tiene_balcon,
    tiene_deposito,
    tiene_gimnasio,
    tiene_piscina,
    tiene_seguridad
  ) %>%
  mutate(across(everything(), ~ . == "TRUE")) %>%
  mutate(across(everything(), as.integer)) %>%
  scale()

pca <- prcomp(vars_pca, center = TRUE, scale. = TRUE)
summary(pca)

full_data <- full_data %>%
  mutate(
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    PC3 = pca$x[, 3],
    PC4 = pca$x[, 4]
  )

set.seed(123)
k <- 10
clusters <- kmeans(pca$x[, 1:5], centers = k)
full_data$cluster <- clusters$cluster

n_train <- nrow(train_out)

train_out <- train_out %>%
  mutate(
    Tenencias1 = full_data$PC1[1:n_train],
    Tenencias2 = full_data$PC2[1:n_train],
    Tenencias3 = full_data$PC3[1:n_train],
    Tenencias4 = full_data$PC4[1:n_train]
  )

test_out <- test_out %>%
  mutate(
    Tenencias1 = full_data$PC1[(n_train + 1):nrow(full_data)],
    Tenencias2 = full_data$PC2[(n_train + 1):nrow(full_data)],
    Tenencias3 = full_data$PC3[(n_train + 1):nrow(full_data)],
    Tenencias4 = full_data$PC4[(n_train + 1):nrow(full_data)]
  )

# =========================================================
# 3. PCA - ESPACIO (DUMMIES)
# =========================================================

vars_pca2 <- full_data %>%
  select(GRUPOUSOEC, property_type, NO_PREDIOS) %>%
  dummy_cols(
    select_columns = c("GRUPOUSOEC", "property_type"),
    remove_first_dummy = TRUE,
    remove_selected_columns = TRUE
  ) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  scale()

pca <- prcomp(vars_pca2, center = TRUE, scale. = TRUE)
summary(pca)

full_data <- full_data %>%
  mutate(
    PC12 = pca$x[, 1],
    PC22 = pca$x[, 2],
    PC32 = pca$x[, 3],
    PC42 = pca$x[, 4]
  )

set.seed(123)
k <- 10
clusters <- kmeans(pca$x[, 1:5], centers = k)
full_data$cluster <- clusters$cluster

train_out <- train_out %>%
  mutate(
    Espacio1 = full_data$PC12[1:n_train],
    Espacio2 = full_data$PC22[1:n_train],
    Espacio3 = full_data$PC32[1:n_train],
    Espacio4 = full_data$PC42[1:n_train]
  )

test_out <- test_out %>%
  mutate(
    Espacio1 = full_data$PC12[(n_train + 1):nrow(full_data)],
    Espacio2 = full_data$PC22[(n_train + 1):nrow(full_data)],
    Espacio3 = full_data$PC32[(n_train + 1):nrow(full_data)],
    Espacio4 = full_data$PC42[(n_train + 1):nrow(full_data)]
  )

# =========================================================
# 4. PCA - DISTANCIAS
# =========================================================

vars_pca2 <- full_data %>%
  select(starts_with("dist")) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  scale()

pca <- prcomp(vars_pca2, center = TRUE, scale. = TRUE)
summary(pca)

full_data <- full_data %>%
  mutate(
    PC122 = pca$x[, 1],
    PC222 = pca$x[, 2],
    PC322 = pca$x[, 3],
    PC422 = pca$x[, 4]
  )

set.seed(123)
k <- 10
clusters <- kmeans(pca$x[, 1:5], centers = k)
full_data$cluster <- clusters$cluster

train_out <- train_out %>%
  mutate(
    Distancia1 = full_data$PC122[1:n_train],
    Distancia2 = full_data$PC222[1:n_train],
    Distancia3 = full_data$PC322[1:n_train],
    Distancia4 = full_data$PC422[1:n_train]
  )

test_out <- test_out %>%
  mutate(
    Distancia1 = full_data$PC122[(n_train + 1):nrow(full_data)],
    Distancia2 = full_data$PC222[(n_train + 1):nrow(full_data)],
    Distancia3 = full_data$PC322[(n_train + 1):nrow(full_data)],
    Distancia4 = full_data$PC422[(n_train + 1):nrow(full_data)]
  )

# =========================================================
# 5. PCA - VARIABLES URBANAS
# =========================================================

vars_pca2 <- full_data %>%
  select(MED_VALOR_, NO_PREDIOS, ESTRATO, V_REF, Numero_banos) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  scale()

pca <- prcomp(vars_pca2, center = TRUE, scale. = TRUE)
summary(pca)

full_data <- full_data %>%
  mutate(
    PC12222 = pca$x[, 1],
    PC22222 = pca$x[, 2],
    PC32222 = pca$x[, 3]
  )

set.seed(123)
k <- 10
clusters <- kmeans(pca$x[, 1:5], centers = k)
full_data$cluster <- clusters$cluster

train_out <- train_out %>%
  mutate(
    Valor1 = full_data$PC12222[1:n_train],
    Valor2 = full_data$PC22222[1:n_train],
    Valor3 = full_data$PC32222[1:n_train]
  )

test_out <- test_out %>%
  mutate(
    Valor1 = full_data$PC12222[(n_train + 1):nrow(full_data)],
    Valor2 = full_data$PC22222[(n_train + 1):nrow(full_data)],
    Valor3 = full_data$PC32222[(n_train + 1):nrow(full_data)]
  )

# =========================================================
# 6. PCA - TAMAÑO
# =========================================================

vars_pca2 <- full_data %>%
  select(
    superficie,
    Numero_rooms,
    Numero_bedrooms,
    Numero_banos,
    surface_total,
    surface_covered
  ) %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .))) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  scale()

pca <- prcomp(vars_pca2, center = TRUE, scale. = TRUE)
summary(pca)

full_data <- full_data %>%
  mutate(
    PC122222 = pca$x[, 1],
    PC222222 = pca$x[, 2],
    PC322222 = pca$x[, 3]
  )

set.seed(123)
k <- 10
clusters <- kmeans(pca$x[, 1:5], centers = k)
full_data$cluster <- clusters$cluster

train_out <- train_out %>%
  mutate(
    Tamaño1 = full_data$PC122222[1:n_train],
    Tamaño2 = full_data$PC222222[1:n_train],
    Tamaño3 = full_data$PC322222[1:n_train]
  )

test_out <- test_out %>%
  mutate(
    Tamaño1 = full_data$PC122222[(n_train + 1):nrow(full_data)],
    Tamaño2 = full_data$PC222222[(n_train + 1):nrow(full_data)],
    Tamaño3 = full_data$PC322222[(n_train + 1):nrow(full_data)]
  )

train2<-train_out
test2<-test_out

write_xlsx(
  train2,
  "D:/Users/Usuario/Documents/BDML-PS03/03_outputs/03_datasets/train2.xlsx"
)

write_xlsx(
  test2,
  "D:/Users/Usuario/Documents/BDML-PS03/03_outputs/03_datasets/test2.xlsx"
)
