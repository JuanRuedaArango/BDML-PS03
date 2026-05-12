# =========================================================
# SETUP
# =========================================================

library(pacman)
p_load(
  rio, tidyverse, tidymodels, nnet,
  recipes, workflows, utsf
)

library(sf)
library(jsonlite)
library(ggplot2)
library(gganimate)
library(gifski)
library(tidytext)
library(stringr)
library(tidyr)
library(osmdata)
library(FNN)
library(writexl)
library(readr)
library(dplyr)

# =========================================================
# PATHS Y DATOS
# =========================================================

setwd("D:/Users/Usuario/Documents/PRoblem set 3")

train <- read.csv("~/PRoblem set 3/train.csv")
test  <- read.csv("~/PRoblem set 3/test.csv")

# =========================================================
# 1. SHAPEFILES
# =========================================================

Localidades <- st_read("Loca.json", quiet = TRUE) %>%
  st_make_valid() %>%
  st_set_crs(4326) %>%
  filter(!LocNombre %in% c("SUMAPAZ", "USME"))

UPZ <- st_read("UPZ.json", quiet = TRUE) %>%
  st_make_valid() %>%
  st_set_crs(4326)

# =========================================================
# 2. TRAIN SF
# =========================================================

train_sf <- st_as_sf(train, coords = c("lon", "lat"),
                     crs = 4326, remove = FALSE)

# =========================================================
# 3. JOIN UPZ
# =========================================================

train_upz <- st_join(
  train_sf,
  UPZ %>% select(NOMBRE, UPLCODIGO),
  join = st_intersects,
  left = TRUE
)

# =========================================================
# 4. FIX NA UPZ
# =========================================================

idx_na_upz <- which(is.na(train_upz$NOMBRE))

if (length(idx_na_upz) > 0) {
  nearest_upz <- st_nearest_feature(train_upz[idx_na_upz, ], UPZ)
  
  train_upz$NOMBRE[idx_na_upz]   <- UPZ$NOMBRE[nearest_upz]
  train_upz$UPLCODIGO[idx_na_upz] <- UPZ$UPLCODIGO[nearest_upz]
}

# =========================================================
# 5. JOIN LOCALIDADES
# =========================================================

train_full <- st_join(
  train_upz,
  Localidades %>% select(LocNombre, LocCodigo),
  join = st_intersects,
  left = TRUE
)

# =========================================================
# 6. FIX NA LOCALIDADES
# =========================================================

idx_na_loc <- which(is.na(train_full$LocNombre))

if (length(idx_na_loc) > 0) {
  nearest_loc <- st_nearest_feature(train_full[idx_na_loc, ], Localidades)
  
  train_full$LocNombre[idx_na_loc] <- Localidades$LocNombre[nearest_loc]
  train_full$LocCodigo[idx_na_loc] <- Localidades$LocCodigo[nearest_loc]
}

# =========================================================
# 7. CLEAN TRAIN
# =========================================================

train_full <- train_full %>%
  group_by(property_id) %>%
  slice(1) %>%
  ungroup()

# =========================================================
# 8. TEST SF
# =========================================================

test_sf <- st_as_sf(test, coords = c("lon", "lat"),
                    crs = 4326, remove = FALSE)

# =========================================================
# 9. TEST UPZ
# =========================================================

test_upz <- st_join(
  test_sf,
  UPZ %>% select(NOMBRE, UPLCODIGO),
  join = st_intersects,
  left = TRUE
)

idx_na_upz_t <- which(is.na(test_upz$NOMBRE))

if (length(idx_na_upz_t) > 0) {
  nearest_upz_t <- st_nearest_feature(test_upz[idx_na_upz_t, ], UPZ)
  
  test_upz$NOMBRE[idx_na_upz_t]   <- UPZ$NOMBRE[nearest_upz_t]
  test_upz$UPLCODIGO[idx_na_upz_t] <- UPZ$UPLCODIGO[nearest_upz_t]
}

# =========================================================
# 10. TEST LOCALIDADES
# =========================================================

test_full <- st_join(
  test_upz,
  Localidades %>% select(LocNombre, LocCodigo),
  join = st_intersects,
  left = TRUE
)

idx_na_loc_t <- which(is.na(test_full$LocNombre))

if (length(idx_na_loc_t) > 0) {
  nearest_loc_t <- st_nearest_feature(test_full[idx_na_loc_t, ], Localidades)
  
  test_full$LocNombre[idx_na_loc_t] <- Localidades$LocNombre[nearest_loc_t]
  test_full$LocCodigo[idx_na_loc_t] <- Localidades$LocCodigo[nearest_loc_t]
}

# =========================================================
# 11. CLEAN TEST
# =========================================================

test_full <- test_full %>%
  group_by(property_id) %>%
  slice(1) %>%
  ungroup()

# =========================================================
# TEXT FEATURES
# =========================================================

crear_features_texto <- function(df){
  df %>%
    mutate(texto = str_to_lower(paste(title, description))) %>%
    mutate(
      title_len = str_length(title),
      desc_len  = str_length(description),
      
      tiene_parqueadero = str_detect(texto, "parqueadero(s)?|garaje(s)?|garage(s)?"),
      tiene_terraza     = str_detect(texto, "terraza(s)?|roof|azotea"),
      tiene_balcon      = str_detect(texto, "balcon(es)?"),
      tiene_deposito    = str_detect(texto, "deposito(s)?|bodega(s)?"),
      tiene_gimnasio    = str_detect(texto, "gimnasio|gym"),
      tiene_piscina     = str_detect(texto, "piscina(s)?|pool"),
      tiene_seguridad   = str_detect(texto, "vigilancia|seguridad|porter[íi]a|conjunto cerrado"),
      
      cocina_integral = str_detect(texto, "cocina integral|cocina equipada"),
      remodelado      = str_detect(texto, "remodelado|renovado|reformado"),
      
      es_lujo      = str_detect(texto, "lujo|exclusiv[oa]s?|premium|alta gama"),
      es_penthouse = str_detect(texto, "penthouse|ph"),
      es_duplex    = str_detect(texto, "duplex|d[úu]plex"),
      
      cerca_transporte = str_detect(texto, "transporte|estacion|metro|transmilenio|sitp"),
      zonas_verdes     = str_detect(texto, "parque(s)?|zona(s)? verde(s)?")
    )
}

train_feat <- crear_features_texto(train_full)
test_feat  <- crear_features_texto(test_full)

# =========================================================
# 5. EXTRACCIÓN NUMÉRICA DESDE TEXTO
# =========================================================

extraer_numeros <- function(df){
  
  df %>%
    mutate(
      # =========================
      # BAÑOS / HABITACIONES
      # =========================
      bathrooms_text = parse_number(
        str_extract(texto, "(\\d+\\.?\\d*)\\s*(bañ|ban)")
      ),
      bedrooms_text  = parse_number(
        str_extract(texto, "(\\d+\\.?\\d*)\\s*(habitaciones|alcobas|cuartos|rooms)")
      ),
      rooms_text     = parse_number(
        str_extract(texto, "(\\d+\\.?\\d*)\\s*(habitaciones|cuartos|rooms)")
      ),
      
      # =========================
      # SUPERFICIE
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
    
    superficie_text = if_else(superficie_text > 1000, NA_real_, superficie_text),
    superficie_text = if_else(superficie_text < 10,  NA_real_, superficie_text)
  ) %>%
    
    # =========================
  # COALESCE FINAL
  # =========================
  mutate(
    Numero_banos    = coalesce(bathrooms, bathrooms_text),
    Numero_bedrooms = coalesce(bedrooms,  bedrooms_text),
    Numero_rooms    = coalesce(rooms,     rooms_text),
    
    superficie = coalesce(
      surface_total,
      superficie_text,
      surface_covered
    )
  ) %>%
    
    select(-bathrooms_text, -bedrooms_text, -rooms_text, -superficie_text)
}

train_feat <- extraer_numeros(train_feat)
test_feat  <- extraer_numeros(test_feat)

# =========================================================
# 6. OSM FEATURES
# =========================================================

props_train <- st_as_sf(train_feat, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857)

props_test <- st_as_sf(test_feat, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(3857)

min_dist <- function(props, points){
  knn <- get.knnx(st_coordinates(points), st_coordinates(props), k = 1)
  knn$nn.dist[, 1]
}

bbox <- getbb("Bogotá Colombia")
options(osmdata.overpass_timeout = 180)

# =========================================================
# OSM: PARQUES
# =========================================================

parks <- opq(bbox) %>%
  add_osm_feature(key = "leisure", value = "park") %>%
  osmdata_sf()

parks_sf <- parks$osm_points %>%
  st_transform(3857)

parks_poly <- parks$osm_polygons %>%
  st_transform(3857)

parks_poly$area <- st_area(parks_poly)

# =========================================================
# OSM: HOSPITALES
# =========================================================

hospitals <- opq(bbox) %>%
  add_osm_feature(key = "amenity", value = "hospital") %>%
  osmdata_sf()

hosp_sf <- hospitals$osm_points %>%
  st_transform(3857)

# =========================================================
# OSM: POLICÍA
# =========================================================

police <- opq(bbox) %>%
  add_osm_feature(key = "amenity", value = "police") %>%
  osmdata_sf()

police_sf <- police$osm_points %>%
  st_transform(3857)

# =========================================================
# OSM: AEROPUERTO
# =========================================================

airport <- opq(bbox) %>%
  add_osm_feature(key = "aeroway", value = "aerodrome") %>%
  osmdata_sf()

airport_sf <- airport$osm_points %>%
  st_transform(3857)

# =========================================================
# OSM: TRANSPORTE
# =========================================================

transmilenio <- opq(bbox) %>%
  add_osm_feature(key = "railway", value = "station") %>%
  osmdata_sf()

tm_sf <- transmilenio$osm_points %>%
  st_transform(3857)

# =========================================================
# DISTANCIAS
# =========================================================

train_feat <- train_feat %>%
  mutate(
    dist_parque       = min_dist(props_train, parks_sf),
    dist_hospital     = min_dist(props_train, hosp_sf),
    dist_cai          = min_dist(props_train, police_sf),
    dist_aeropuerto   = min_dist(props_train, airport_sf),
    dist_transmilenio = min_dist(props_train, tm_sf)
  )

test_feat <- test_feat %>%
  mutate(
    dist_parque       = min_dist(props_test, parks_sf),
    dist_hospital     = min_dist(props_test, hosp_sf),
    dist_cai          = min_dist(props_test, police_sf),
    dist_aeropuerto   = min_dist(props_test, airport_sf),
    dist_transmilenio = min_dist(props_test, tm_sf)
  )

# =========================================================
# LIMPIEZA FINAL
# =========================================================

limpiar_final <- function(df){
  
  df %>%
    rename(UPZ = NOMBRE) %>%
    
    group_by(UPZ) %>%
    mutate(
      Numero_banos = ifelse(
        is.na(Numero_banos),
        median(Numero_banos, na.rm = TRUE),
        Numero_banos
      )
    ) %>%
    ungroup() %>%
    
    mutate(
      Numero_banos = ifelse(
        is.na(Numero_banos),
        median(Numero_banos, na.rm = TRUE),
        Numero_banos
      )
    ) %>%
    
    select(
      -rooms,
      -bedrooms,
      -bathrooms,
      -banos_por_habitacion
    )
}

train_feat <- limpiar_final(train_feat)
test_feat  <- limpiar_final(test_feat)

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
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
    
    densidad_servicios =
      n_supermercados_1km +
      n_colegios_1km
  ) %>%
  select(-price)

test <- test_feat %>%
  mutate(
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
    
    densidad_servicios =
      n_supermercados_1km +
      n_colegios_1km
  )

# =========================================================
# 7. TRANSFORMACIONES LOG
# =========================================================

train_feat <- train_feat %>%
  mutate(
    log_dist_parque   = log1p(dist_parque),
    log_dist_hospital = log1p(dist_hospital),
    log_dist_tm       = log1p(dist_transmilenio),
    log_dist_centro   = log1p(dist_centro)
  )

test_feat <- test_feat %>%
  mutate(
    log_dist_parque   = log1p(dist_parque),
    log_dist_hospital = log1p(dist_hospital),
    log_dist_tm       = log1p(dist_transmilenio),
    log_dist_centro   = log1p(dist_centro)
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

summary(train_feat)

# =========================================================
# 10. FUNCIÓN LIMPIEZA FINAL
# =========================================================

limpiar_final <- function(df){
  
  df %>%
    # -------------------------
  # 1. Renombrar
  # -------------------------
  rename(UPZ = NOMBRE) %>%
    
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
  ) %>%
    
    # -------------------------
  # 4. Variables innecesarias
  # -------------------------
  select(
    -rooms,
    -bedrooms,
    -bathrooms,
    -banos_por_habitacion
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
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
    
    densidad_servicios =
      n_supermercados_1km +
      n_colegios_1km
  ) %>%
  select(-price)

test <- test_feat %>%
  mutate(
    lat_lon_interaction = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    banos_por_habitacion = Numero_banos / pmax(Numero_bedrooms, 1),
    
    densidad_servicios =
      n_supermercados_1km +
      n_colegios_1km
  )

# =========================================================
# 13. EXPORTACIÓN
# =========================================================

train_out <- train_feat %>% st_drop_geometry()
test_out  <- test_feat %>% st_drop_geometry()

write_xlsx(train_out, "train_feat_FF.xlsx")
write_xlsx(test_out, "test_feat_FF.xlsx")