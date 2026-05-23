# =========================================================
# FEATURE ENGINEERING - PREDICCIÓN DE PRECIOS INMOBILIARIOS
# =========================================================
#
# Featuring:
#   Generación de variables auxiliares para modelamiento
#   de precios de vivienda en Bogotá D.C.
#
# Inputs:
#   - Base train de precios inmobiliarios
#   - Base test de precios inmobiliarios
#
# Outputs:
#   - Base train enriquecida con variables auxiliares
#   - Base test enriquecida con variables auxiliares
#
# Descripción:
#   Este script realiza procesos de feature engineering
#   combinando información geoespacial, análisis de texto
#   y fuentes externas de información.
#
#   Se incorporan variables provenientes de:
#
#   • Base de datos de precios:
#       - Limpieza y transformación de variables
#       - Extracción de características desde texto
#       - Generación de métricas agregadas por ubicación
#       - Transformación logarítmica del precio
#
#   • Datos Abiertos Bogotá:
#       - Variables catastrales y socioeconómicas
#       - Información territorial y urbanística
#       - Indicadores por manzana y sector
#
#   • OpenStreetMap (OSM):
#       - Distancias a puntos de interés
#       - Conteo de servicios cercanos
#       - Accesibilidad y equipamientos urbanos
#
# Procesamientos realizados:
#
#   1. Análisis de texto:
#       - Cocina integral, dúplex, remodelado
#       - Seguridad, piscina, zonas verdes, terraza
#       - Amenidades y características del inmueble
#
#   2. Limpieza y transformación:
#       - Número de baños
#       - Superficie total y cubierta
#       - Número de habitaciones
#       - Estandarización de variables
#
#   3. Variables espaciales:
#       - Distancia euclidiana y KNN
#       - Distancia a hospitales, colegios, comercio,
#         parques, estaciones y vías principales
#       - Conteo de establecimientos cercanos
#
#   4. Variables territoriales:
#       - Localidad
#       - UPZ
#       - Manzana
#       - Estrato y grupo socioeconómico
#       - Valor medio del m²
#       - Nivel de seguridad
#       - Número de predios
#
# =========================================================
# LIBRERÍAS
# =========================================================

library(pacman)

p_load(
  rio,
  tidyverse,
  tidymodels,
  recipes,
  workflows,
  nnet,
  utsf,
  sf,
  jsonlite,
  ggplot2,
  gganimate,
  gifski,
  tidytext,
  stringr,
  tidyr,
  osmdata,
  FNN,
  writexl,
  readr,
  dplyr
)
# =========================================================
# RUTA Y DATOS
# =========================================================
setwd("D:/Users/Usuario/Documents/BDML-PS03/02_data")

train <- read.csv("train.csv")
test  <- read.csv("test.csv")

crs_use <- 4326

# =========================================================
# FUNCIONES AUXILIARES 🔥
# =========================================================

fix_na_nearest <- function(data, ref, var_name, ref_var){
  idx <- which(is.na(data[[var_name]]))
  
  if(length(idx) > 0){
    nearest <- st_nearest_feature(data[idx, ], ref)
    data[[var_name]][idx] <- ref[[ref_var]][nearest]
  }
  
  return(data)
}

# =========================================================
# 1. CARGA DE SHAPEFILES
# =========================================================

Localidades <- st_read("Loca.json", quiet = TRUE) %>%
  st_make_valid() %>%
  st_set_crs(4326) %>%   # 👈 ESTO ES LA CLAVE
  st_transform(crs_use) %>%
  filter(!LocNombre %in% c("SUMAPAZ", "USME"))

UPZ <- st_read("UPZ.json", quiet = TRUE) %>%
  st_make_valid() %>%
  st_set_crs(4326) %>%   # 👈 MISMO ARREGLO
  st_transform(crs_use)

# =========================================================
# 2. FUNCIÓN PRINCIPAL PARA TRAIN Y TEST 🔥
# =========================================================

procesar_base <- function(df){
  
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = crs_use, remove = FALSE)
  
  # ---------------- UPZ ----------------
  df_sf <- st_join(
    df_sf,
    UPZ %>% select(NOMBRE, UPLCODIGO),
    left = TRUE
  )
  
  df_sf <- fix_na_nearest(df_sf, UPZ, "NOMBRE", "NOMBRE")
  df_sf <- fix_na_nearest(df_sf, UPZ, "UPLCODIGO", "UPLCODIGO")
  
  # ---------------- LOCALIDAD ----------------
  df_sf <- st_join(
    df_sf,
    Localidades %>% select(LocNombre, LocCodigo),
    left = TRUE
  )
  
  df_sf <- fix_na_nearest(df_sf, Localidades, "LocNombre", "LocNombre")
  df_sf <- fix_na_nearest(df_sf, Localidades, "LocCodigo", "LocCodigo")
  
  # ---------------- LIMPIEZA ----------------
  df_sf <- df_sf %>%
    group_by(property_id) %>%
    slice(1) %>%
    ungroup()
  
  return(df_sf)
}

# =========================================================
# 3. APLICAR A TRAIN Y TEST
# =========================================================

train_full <- procesar_base(train)
test_full  <- procesar_base(test)


# =========================================================
# 4. DATOS DE MANZANA
# =========================================================

fix_sf <- function(x){
  x <- st_make_valid(x)
  if(is.na(st_crs(x))){
    x <- st_set_crs(x, 4326)
  }
  x <- st_transform(x, crs_use)
  return(x)
}

AVAL2021 <- st_read("AvalCatMz2021.json", quiet = TRUE) %>%
  fix_sf() %>%
  select(MANCODIGO, MED_VALOR_, NO_PREDIOS)

GUS2021 <- st_read("GUsoMZ2021.json", quiet = TRUE) %>%
  fix_sf() %>%
  select(MANCODIGO, GRUPOUSOEC)

VALOR2021 <- st_read("Valor_Ref_M_2021.json", quiet = TRUE) %>%
  fix_sf() %>%
  select(MANCODIGO, V_REF)

ESTRATO <- st_read("manzanaestratificacion.gpkg") %>%
  st_make_valid()

ESTRATO <- ESTRATO %>%
  st_transform(crs_use) %>%  
  select(CODIGO_MANZANA, ESTRATO)

# =========================================================
# 5. DATOS DE INCIDENTES (UPZ)
# =========================================================

IR <- st_read("IRUPZ.gpkg") %>%
  st_make_valid()

if(is.na(st_crs(IR))){
  IR <- st_set_crs(IR, 4326)
}

IR <- st_transform(IR, crs_use)

IR_filtrado <- IR %>%
  select(CMIUUPLA, CMNOMUPLA, contains("19"))

# =========================================================
# 6. SEGURIDAD NOCTURNA
# =========================================================

SEGNOCT <- st_read("PuntosSeguridadNocturna.json", quiet = TRUE) %>%
  st_set_crs(4326) %>%
  st_transform(crs_use) %>%
  select(INDICE_SEG)

# =========================================================
# 7. FUNCIÓN PARA AGREGAR VARIABLES ESPACIALES 🔥
# =========================================================

sf_use_s2(FALSE)

agregar_variables <- function(df){
  
  # ================================
  # MANZANA (JOIN + NEAREST 🔥)
  # ================================
  
  # AVAL
  df <- st_join(df, AVAL2021, left = TRUE)
  
  idx_na <- which(is.na(df$MED_VALOR_))
  if(length(idx_na) > 0){
    nearest <- st_nearest_feature(df[idx_na, ], AVAL2021)
    df$MED_VALOR_[idx_na] <- AVAL2021$MED_VALOR_[nearest]
    df$NO_PREDIOS[idx_na] <- AVAL2021$NO_PREDIOS[nearest]
  }
  
  # GUS
  df <- st_join(df, GUS2021, left = TRUE)
  
  idx_na <- which(is.na(df$GRUPOUSOEC))
  if(length(idx_na) > 0){
    nearest <- st_nearest_feature(df[idx_na, ], GUS2021)
    df$GRUPOUSOEC[idx_na] <- GUS2021$GRUPOUSOEC[nearest]
  }
  
  # VALOR
  df <- st_join(df, VALOR2021, left = TRUE)
  
  idx_na <- which(is.na(df$V_REF))
  if(length(idx_na) > 0){
    nearest <- st_nearest_feature(df[idx_na, ], VALOR2021)
    df$V_REF[idx_na] <- VALOR2021$V_REF[nearest]
  }
  
  # ESTRATO
  idx <- st_nearest_feature(df, ESTRATO)
  df$ESTRATO <- ESTRATO$ESTRATO[idx]
  df$CODIGO_MANZANA <- ESTRATO$CODIGO_MANZANA[idx]
  # ================================
  # INCIDENTES (UPZ)
  # ================================
  
  df <- st_join(df, IR_filtrado, left = TRUE)
  
  # ================================
  # SEGURIDAD (NEAREST 🔥)
  # ================================
  
  idx <- st_nearest_feature(df, SEGNOCT)
  df$seguridad <- SEGNOCT$INDICE_SEG[idx]
  
  return(df)
}

# =========================================================
# 8. APLICAR VARIABLES
# =========================================================

train_full <- agregar_variables(train_full)
test_full  <- agregar_variables(test_full)

# =========================================================
# 9. OUTPUT FINAL
# =========================================================
# -------- TRAIN --------
train_full<- train_full %>%
  st_drop_geometry() %>%
  select(
    -starts_with("MANCODIGO")
  )

# -------- TEST --------
test_full <- test_full %>%
  st_drop_geometry() %>%
  select(
    -starts_with("MANCODIGO")
  )

# =========================================================
# 4. TEXTO FEATURE ENGINEERING (TRAIN + TEST FUNCIÓN)
# =========================================================
crear_features_texto <- function(df){
  
  df %>%
    mutate(texto = str_to_lower(paste(title, description))) %>%
    mutate(
      
      # longitud
      title_len = str_length(title),
      desc_len  = str_length(description),
      
      # amenities
      tiene_parqueadero = str_detect(texto, "parqueadero(s)?|garaje(s)?|garage(s)?"),
      tiene_terraza     = str_detect(texto, "terraza(s)?|roof|azotea"),
      tiene_balcon      = str_detect(texto, "balcon(es)?"),
      tiene_deposito    = str_detect(texto, "deposito(s)?|bodega(s)?"),
      tiene_gimnasio    = str_detect(texto, "gimnasio|gym"),
      tiene_piscina     = str_detect(texto, "piscina(s)?|pool"),
      tiene_seguridad   = str_detect(texto, "vigilancia|seguridad|porter[íi]a|conjunto cerrado"),
      
      cocina_integral = str_detect(texto, "cocina integral|cocina equipada"),
      remodelado      = str_detect(texto, "remodelado|renovado|reformado"),
      
      # segmento
      es_lujo      = str_detect(texto, "lujo|exclusiv[oa]s?|premium|alta gama"),
      es_penthouse = str_detect(texto, "penthouse|ph"),
      es_duplex    = str_detect(texto, "duplex|d[úu]plex"),
      
      # entorno
      cerca_transporte = str_detect(texto, "transporte|estacion|metro|transmilenio|sitp"),
      zonas_verdes     = str_detect(texto, "parque(s)?|zona(s)? verde(s)?")
    )
}

train_feat <- crear_features_texto(train_full)
test_feat  <- crear_features_texto(test_full)

# =========================================================
# 5. EXTRACCIÓN NÚMERICA DESDE TEXTO
# =========================================================
extraer_numeros <- function(df){
  
  df %>%
    mutate(
      # =========================
      # BAÑOS / HABITACIONES
      # =========================
      bathrooms_text = parse_number(str_extract(texto, "(\\d+\\.?\\d*)\\s*(bañ|ban)")),
      bedrooms_text  = parse_number(str_extract(texto, "(\\d+\\.?\\d*)\\s*(habitaciones|alcobas|cuartos|rooms)")),
      rooms_text     = parse_number(str_extract(texto, "(\\d+\\.?\\d*)\\s*(habitaciones|cuartos|rooms)")),
      
      # =========================
      # 🔥 SUPERFICIE
      # =========================
      superficie_text = parse_number(
        str_extract(
          texto,
          "(\\d+\\.?\\d*)\\s*(m2|m²|mts2|mt2|metros cuadrados|metros|mtrs)"
        )
      )
    ) %>%
    
    # =========================
  # LIMPIEZA
  # =========================
  mutate(
    bathrooms_text = if_else(bathrooms_text > 10, NA_real_, bathrooms_text),
    bedrooms_text  = if_else(bedrooms_text  > 10, NA_real_, bedrooms_text),
    rooms_text     = if_else(rooms_text     > 10, NA_real_, rooms_text),
    
    # 🔥 superficie razonable
    superficie_text = if_else(superficie_text > 1000, NA_real_, superficie_text),
    superficie_text = if_else(superficie_text < 10, NA_real_, superficie_text)
  ) %>%
    
    # =========================
  # COALESCE FINAL
  # =========================
  mutate(
    Numero_banos    = coalesce(bathrooms, bathrooms_text),
    Numero_bedrooms = coalesce(bedrooms, bedrooms_text),
    Numero_rooms    = coalesce(rooms, rooms_text),
    
    superficie = coalesce(surface_total, superficie_text,surface_covered)  # 👈 ajusta "area" si tu variable se llama distinto
  ) %>%
    
    select(-bathrooms_text, -bedrooms_text, -rooms_text, -superficie_text)
}

train_feat <- extraer_numeros(train_feat)
test_feat  <- extraer_numeros(test_feat)

# =========================================================
# 6. OSM FEATURES (SE HACE SOLO UNA VEZ)
# =========================================================
props_train <- st_as_sf(train_feat, coords = c("lon", "lat"), crs = 4326) %>% st_transform(3857)
props_test  <- st_as_sf(test_feat, coords = c("lon", "lat"), crs = 4326) %>% st_transform(3857)

min_dist <- function(props, points){
  knn <- get.knnx(st_coordinates(points), st_coordinates(props), k = 1)
  knn$nn.dist[,1]
}



library(sf)
library(osmdata)
library(FNN)
library(dplyr)
bbox <- getbb("Bogotá Colombia")
options(osmdata.overpass_timeout = 180)
# =========================================================
# 1. PARQUES
# =========================================================
parks <- opq(bbox) |>
  add_osm_feature(key = "leisure", value = "park") |>
  osmdata_sf()

parks_sf <- parks$osm_points |>
  st_transform(3857)

parks_poly <- parks$osm_polygons |>
  st_transform(3857)

parks_poly$area <- st_area(parks_poly)

# =========================================================
# 2. HOSPITALES
# =========================================================
hospitals <- opq(bbox) |>
  add_osm_feature(key = "amenity", value = "hospital") |>
  osmdata_sf()

hosp_sf <- hospitals$osm_points |>
  st_transform(3857)

# =========================================================
# 3. POLICÍA / CAI
# =========================================================
police <- opq(bbox) |>
  add_osm_feature(key = "amenity", value = "police") |>
  osmdata_sf()

police_sf <- police$osm_points |>
  st_transform(3857)

# =========================================================
# 4. AEROPUERTO
# =========================================================
airport <- opq(bbox) |>
  add_osm_feature(key = "aeroway", value = "aerodrome") |>
  osmdata_sf()

airport_sf <- airport$osm_points |>
  st_transform(3857)

# =========================================================
# 5. TRANSPORTE (ESTACIONES)
# =========================================================
transmilenio <- opq(bbox) |>
  add_osm_feature(key = "railway", value = "station") |>
  osmdata_sf()

tm_sf <- transmilenio$osm_points |>
  st_transform(3857)

# =========================================================
# 6. EDUCACIÓN
# =========================================================
edu <- opq(bbox) |>
  add_osm_feature(key = "amenity", value = c("school", "university")) |>
  osmdata_sf()

edu_sf <- edu$osm_points |>
  st_transform(3857)

schools_sf <- edu_sf |> filter(amenity == "school")
univ_sf <- edu_sf |> filter(amenity == "university")

# =========================================================
# 7. COMERCIO
# =========================================================
commerce <- opq(bbox) |>
  add_osm_feature(key = "shop") |>
  osmdata_sf()

commerce_sf <- commerce$osm_points |>
  st_transform(3857)

supermarkets_sf <- commerce_sf |>
  filter(shop == "supermarket")

# =========================================================
# 8. PROPIEDADES
# =========================================================
props_sf <- st_as_sf(train_feat, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(3857)

# =========================================================
# 9. Highway
# =========================================================

highways <- opq(bbox) |>
  add_osm_feature(
    key = "highway",
    value = c("primary", "secondary", "tertiary")
  ) |>
  osmdata_sf()

highways_sf <- highways$osm_lines |>
  st_transform(3857) |>
  st_centroid()   # 🔥 CLAVE

# =========================================================
# 9. Uso del suelo
# =========================================================

landuse <- opq(bbox) |>
  add_osm_feature(
    key = "landuse",
    value = c("residential", "commercial", "industrial")
  ) |>
  osmdata_sf()

landuse_sf <- landuse$osm_polygons |>
  st_transform(3857)

# separar
residential_sf <- landuse_sf |>
  filter(landuse == "residential") |>
  st_centroid()

commercial_sf <- landuse_sf |>
  filter(landuse == "commercial") |>
  st_centroid()

industrial_sf <- landuse_sf |>
  filter(landuse == "industrial") |>
  st_centroid()
# =========================================================
# 9. Centros comerciales
# =========================================================

malls <- opq(bbox) |>
  add_osm_feature(key = "shop", value = "mall") |>
  osmdata_sf()

malls_sf <- malls$osm_points |>
  st_transform(3857)

# =========================================================
# 9. Restaurantes
# =========================================================

food_places <- opq(bbox) |>
  add_osm_feature(
    key = "amenity",
    value = c("restaurant", "cafe", "bar")
  ) |>
  osmdata_sf()

food_sf <- food_places$osm_points |>
  st_transform(3857)

# =========================================================
# 7. DISTANCIAS (TRAIN + TEST USAN MISMO OSM)
# =========================================================

train_feat$dist_parque <- min_dist(props_train, parks_sf)
train_feat$dist_hospital <- min_dist(props_train, hosp_sf)
train_feat$dist_cai <- min_dist(props_train, police_sf)
train_feat$dist_aeropuerto <- min_dist(props_train, airport_sf)
train_feat$dist_transmilenio <- min_dist(props_train, tm_sf)

test_feat$dist_parque <- min_dist(props_test, parks_sf)
test_feat$dist_hospital <- min_dist(props_test, hosp_sf)
test_feat$dist_cai <- min_dist(props_test, police_sf)
test_feat$dist_aeropuerto <- min_dist(props_test, airport_sf)
test_feat$dist_transmilenio <- min_dist(props_test, tm_sf)

train_feat$dist_highway <- min_dist(props_train, highways_sf)
test_feat$dist_highway  <- min_dist(props_test, highways_sf)

train_feat$dist_residential <- min_dist(props_train, residential_sf)
train_feat$dist_commercial  <- min_dist(props_train, commercial_sf)
train_feat$dist_industrial  <- min_dist(props_train, industrial_sf)

test_feat$dist_residential <- min_dist(props_test, residential_sf)
test_feat$dist_commercial  <- min_dist(props_test, commercial_sf)
test_feat$dist_industrial  <- min_dist(props_test, industrial_sf)

train_feat$dist_mall <- min_dist(props_train, malls_sf)
test_feat$dist_mall  <- min_dist(props_test, malls_sf)

train_feat$dist_food <- min_dist(props_train, food_sf)
test_feat$dist_food  <- min_dist(props_test, food_sf)

# =========================================================
# 8. COUNTS 1-2 KM
# =========================================================

train_feat$n_supermercados_1km <- lengths(st_is_within_distance(props_train, supermarkets_sf, 1000))
train_feat$n_colegios_1km <- lengths(st_is_within_distance(props_train, schools_sf, 1000))
train_feat$n_tiendas_1km <- lengths(
  st_is_within_distance(props_train, commerce_sf, 1000)
)
train_feat$n_estaciones_1km <- lengths(
  st_is_within_distance(props_train, tm_sf, 1000)
)


test_feat$n_supermercados_1km <- lengths(st_is_within_distance(props_test, supermarkets_sf, 1000))
test_feat$n_colegios_1km <- lengths(st_is_within_distance(props_test, schools_sf, 1000))
test_feat$n_tiendas_1km <- lengths(
  st_is_within_distance(props_test, commerce_sf, 1000)
)
test_feat$n_estaciones_1km <- lengths(
  st_is_within_distance(props_test, tm_sf, 1000)
)

### NUEVAS ##

# =========================================================
# 🔥 FEATURES AVANZADAS (TRAIN + TEST)
# =========================================================

library(dplyr)
library(sf)
library(FNN)
library(stringr)

# =========================================================
# 0. OBJETOS ESPACIALES
# =========================================================

props_train_sf <- props_sf  # ya lo tienes
props_test_sf <- st_as_sf(test_feat, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(3857)

# =========================================================
# 1. DISTANCIA AL CENTRO (CBD)
# =========================================================

centro <- st_sfc(st_point(c(-74.0721, 4.7110)), crs = 4326) |>
  st_transform(3857)

train_feat$dist_centro <- as.numeric(
  st_distance(props_train_sf, centro)
)

test_feat$dist_centro <- as.numeric(
  st_distance(props_test_sf, centro)
)

# =========================================================
# 2. DENSIDAD TOTAL DE SERVICIOS
# =========================================================

train_feat <- train_feat %>%
  mutate(
    densidad_total_1km =
      n_supermercados_1km +
      n_tiendas_1km +
      n_colegios_1km +
      n_estaciones_1km
  )

test_feat <- test_feat %>%
  mutate(
    densidad_total_1km =
      n_supermercados_1km +
      n_tiendas_1km +
      n_colegios_1km +
      n_estaciones_1km
  )

# =========================================================
# 3. INTERACCIONES CLAVE
# =========================================================

train_feat <- train_feat %>%
  mutate(
    banos_por_habitacion = Numero_banos / (Numero_bedrooms + 1),
    densidad_servicios = n_supermercados_1km / (dist_transmilenio + 1)
  )

test_feat <- test_feat %>%
  mutate(
    banos_por_habitacion = Numero_banos / (Numero_bedrooms + 1),
    densidad_servicios = n_supermercados_1km / (dist_transmilenio + 1)
  )

# =========================================================
# 4. CLUSTERING GEOGRÁFICO (TRAIN)
# =========================================================

coords_train <- st_coordinates(props_train_sf)

set.seed(123)
k <- 20

kmeans_model <- kmeans(coords_train, centers = k)

train_feat$cluster_geo <- as.factor(kmeans_model$cluster)

# =========================================================
# 5. CLUSTERING GEOGRÁFICO (TEST)
# =========================================================

coords_test <- st_coordinates(props_test_sf)

knn <- get.knnx(
  kmeans_model$centers,
  coords_test,
  k = 1
)

test_feat$cluster_geo <- as.factor(knn$nn.index[,1])

# asegurar mismos niveles
test_feat$cluster_geo <- factor(
  test_feat$cluster_geo,
  levels = levels(train_feat$cluster_geo)
)

# =========================================================
# 6. FEATURES DE TEXTO
# =========================================================

train_feat <- train_feat %>%
  mutate(
    texto_lujo_score = str_count(
      texto,
      "lujo|premium|exclusivo|moderno|espectacular"
    ),
    
    problemas = str_detect(
      texto,
      "sin ascensor|sin parqueadero|necesita remodelar|antiguo"
    )
  )

test_feat <- test_feat %>%
  mutate(
    texto_lujo_score = str_count(
      texto,
      "lujo|premium|exclusivo|moderno|espectacular"
    ),
    
    problemas = str_detect(
      texto,
      "sin ascensor|sin parqueadero|necesita remodelar|antiguo"
    )
  )

# =========================================================
# 7. TRANSFORMACIONES LOG
# =========================================================

train_feat <- train_feat %>%
  mutate(
    log_dist_parque = log1p(dist_parque),
    log_dist_hospital = log1p(dist_hospital),
    log_dist_tm = log1p(dist_transmilenio),
    log_dist_centro = log1p(dist_centro)
  )

test_feat <- test_feat %>%
  mutate(
    log_dist_parque = log1p(dist_parque),
    log_dist_hospital = log1p(dist_hospital),
    log_dist_tm = log1p(dist_transmilenio),
    log_dist_centro = log1p(dist_centro)
  )

# =========================================================
# 8. LIMPIEZA FINAL
# =========================================================

train_feat <- train_feat %>%
  mutate(across(where(is.logical), as.factor))

test_feat <- test_feat %>%
  mutate(across(where(is.logical), as.factor))

# =========================================================
# 9. CHECK FINAL
# =========================================================

cat("Train dim:", dim(train_feat), "\n")
cat("Test dim:", dim(test_feat), "\n")

setdiff(names(train_feat), names(test_feat))
setdiff(names(test_feat), names(train_feat))

summary(train_feat)


limpiar_final <- function(df){
  
  df %>%
    
    # =========================================================
  # 1. renombre
  # =========================================================
  rename(UPZ = NOMBRE) %>%
    
    # =========================================================
  # 2. imputación Numero_banos (robusta)
  # =========================================================
  
  group_by(UPZ) %>%
    mutate(
      Numero_banos = ifelse(
        is.na(Numero_banos),
        median(Numero_banos, na.rm = TRUE),
        Numero_banos
      )
    ) %>%
    ungroup() %>%
    
    # fallback global (MUY IMPORTANTE)
    mutate(
      Numero_banos = ifelse(
        is.na(Numero_banos),
        median(Numero_banos, na.rm = TRUE),
        Numero_banos
      )
    ) %>%
    
    # =========================================================
  # 3. limpieza final
  # =========================================================
  
  select(
    -rooms,
    -bedrooms,
    -bathrooms,
    -banos_por_habitacion
  )
}

train_feat<-limpiar_final(train_feat)
test_feat<-limpiar_final(test_feat)

# =========================================================
# FEATURES FINALES
# =========================================================

test_feat$UPZ[test_feat$UPZ == "SAN ISIDRO - PATIOS"] <- "CHAPINERO"

train <- train_feat %>%
  mutate(
    log_price = log(price),
    
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1)
  ) %>%
  select(-price)

test <- test_feat %>%
  mutate(
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1)
  )

# =========================================================
# 7. TRANSFORMACIONES LOG
# =========================================================

train_feat <- train_feat %>%
  mutate(
    log_dist_parque   = log1p(dist_parque),
    log_dist_hospital = log1p(dist_hospital),
    log_dist_tm       = log1p(dist_transmilenio)
  )

test_feat <- test_feat %>%
  mutate(
    log_dist_parque   = log1p(dist_parque),
    log_dist_hospital = log1p(dist_hospital),
    log_dist_tm       = log1p(dist_transmilenio)
  )

# =========================================================
# 8. LIMPIEZA FINAL (TIPOS)
# =========================================================

train_feat <- train_feat %>%
  mutate(across(where(is.logical), as.factor))

test_feat <- test_feat %>%
  mutate(across(where(is.logical), as.factor))

# =========================================================
# 9. CHECK FINAL
# =========================================================

cat("Train dim:", dim(train_feat), "\n")
cat("Test dim:", dim(test_feat), "\n")

setdiff(names(train_feat), names(test_feat))
setdiff(names(test_feat), names(train_feat))

# =========================================================
# 10. FUNCIÓN LIMPIEZA FINAL
# =========================================================

limpiar_final <- function(df){
  
  df %>%
    # -------------------------
  # 1. Renombrar
  # -------------------------
  # -------------------------
  # 2. Imputación por grupo
  # -------------------------
  group_by(UPZ) %>%
    mutate(
      Numero_banos = ifelse(
        is.na(Numero_banos),
        median(Numero_banos, na.rm = TRUE),
        Numero_banos
      )
    ) %>%
    ungroup() %>%
    
    # -------------------------
  # 3. Fallback global
  # -------------------------
  mutate(
    Numero_banos = ifelse(
      is.na(Numero_banos),
      median(Numero_banos, na.rm = TRUE),
      Numero_banos
    )
  )
}

train_feat <- limpiar_final(train_feat)
test_feat  <- limpiar_final(test_feat)

# =========================================================
# 11. AJUSTES ESPECÍFICOS
# =========================================================

test_feat$UPZ[test_feat$UPZ == "SAN ISIDRO - PATIOS"] <- "CHAPINERO"

# =========================================================
# 12. FEATURES FINALES
# =========================================================

train <- train_feat %>%
  mutate(
    log_price = log(price),
    
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1)
  ) %>%
  select(-price)

test <- test_feat %>%
  mutate(
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1)
  )

# =========================================================
# 13. TARGET Y GLOBAL MEAN
# =========================================================

train_feat <- train_feat %>%
  mutate(log_price = log(price))

global_mean <- mean(train_feat$log_price, na.rm = TRUE)

# =========================================================
# 14. TARGET ENCODING UPZ - PROPERTY TYPE
# =========================================================

tabla_upz_prop <- train_feat %>%
  group_by(UPZ, property_type) %>%
  summarise(
    n = n(),
    mean_log_price = mean(log_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    precio_upz_property = (n * mean_log_price + 20 * global_mean) / (n + 20)
  )

train_feat <- train_feat %>%
  st_drop_geometry() %>%
  left_join(
    tabla_upz_prop %>% select(UPZ, property_type, precio_upz_property) %>% st_drop_geometry(),
    by = c("UPZ", "property_type")
  )

test_feat <- test_feat %>%
  st_drop_geometry() %>%
  left_join(
    tabla_upz_prop %>% select(UPZ, property_type, precio_upz_property),
    by = c("UPZ", "property_type")
  )

# =========================================================
# 15. FALLBACK FINAL
# =========================================================

test_feat$precio_upz_property[
  is.na(test2$precio_upz_property)
] <- global_mean

# =========================================================
# 13. EXPORTACIÓN
# =========================================================

train_out <- train_feat %>% st_drop_geometry()
test_out  <- test_feat %>% st_drop_geometry()

write_xlsx(
  train_out,
  "D:/Users/Usuario/Documents/BDML-PS03/03_outputs/03_datasets/train_out.xlsx"
)

write_xlsx(
  test_out,
  "D:/Users/Usuario/Documents/BDML-PS03/03_outputs/03_datasets/test_out.xlsx"
)