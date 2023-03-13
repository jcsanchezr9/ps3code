require(pacman)
p_load(
  tidyverse, rio, skimr, viridis, osmdata,
  ggsn, ## scale bar
  raster, stars, ## datos raster
  ggmap, ## get_stamenmap
  sf, ## Leer/escribir/manipular datos espaciales
  leaflet
) ## Visualizaciones dinámicas

p_load(mapview)

setwd("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3")

train<-read.csv(file = 'train.csv')

test<-read.csv(file = 'test.csv')

houses_preproc <- function(houses) {
  houses <- houses %>% dplyr::select(-c(rooms, title, operation_type))
  # Create binary variable for property type to replace property_type variable
  houses <- houses %>% dplyr::mutate(
    house = ifelse(property_type == "Casa", 1, 0)
  )
  houses <- houses %>% dplyr::select(-property_type)
  # Impute missing data in surface_total using surface_covered
  houses <- houses %>% dplyr::mutate(
    surface_total = dplyr::if_else(
      is.na(surface_total), surface_covered, surface_total
    )
  )
  houses <- houses %>% dplyr::select(-surface_covered)
  
  # Normalize descriptions to lower case
  houses$description <- tolower(houses$description)
  # Replace decimal comma with decimal point in descriptions
  houses$description <- stringr::str_replace_all(
    houses$description,
    "(\\d),(\\d)",
    "\\1.\\2"
  )
  
  # Use descriptions to retrieve property areas
  areas_retrieved <- stringr::str_match(
    houses$description,
    " (\\d*\\.?\\d+)\\s?m(t|etro|2|\\s|:punct:)"
  )[, 2]
  
  # Detect cases where points have been used to mark thousands
  point_thousands <- stringr::str_detect(areas_retrieved, "^\\d\\.\\d{3}")
  point_thousands[is.na(point_thousands)] <- FALSE
  # Remove points marking thousands
  areas_retrieved[point_thousands] <- stringr::str_replace_all(
    areas_retrieved[point_thousands],
    "\\.",
    ""
  )
  # Convert values to numerical
  areas_retrieved <- as.numeric(areas_retrieved)
  # Remove values less than 15 (potential errors in parsing)
  areas_retrieved[areas_retrieved < 15] <- NA
  # Use only 1 decimal figure
  houses$areas_retrieved <- round(areas_retrieved, 1)
  houses <- houses %>% dplyr::mutate(
    surface_total = dplyr::if_else(
      is.na(surface_total), areas_retrieved, surface_total
    )
  )
  houses <- houses %>% dplyr::select(-areas_retrieved)
  
  houses <- houses %>% dplyr::mutate(
    sala_com = dplyr::if_else(
      stringr::str_detect(
        description, "sala|comedor"
      ), 1, 0
    ),
    upgrade_in = dplyr::if_else(
      stringr::str_detect(
        description,
        "chimenea|terraza|social|balc.?n|balcã.n|balc&\\w{6};n"
      ), 1, 0
    ),
    upgrade_out = dplyr::if_else(
      stringr::str_detect(
        description,
        "gimnasio|gym|infantil|ni.?os|jard.?n|niã.os|jardã.n|ni&\\w{6};os|jard&\\w{6};n"
      ), 1, 0
    ),
    garage = dplyr::if_else(
      stringr::str_detect(description, "garaje|garage|parqueadero"), 1, 0
    ),
    light = dplyr::if_else(
      stringr::str_detect(
        description,
        "iluminado|iluminaci.?n|iluminaciã.n|iluminaci&\\w{6};n|luz natural"
      ),
      1, 0
    )
  )
  
  houses <- houses %>% dplyr::select(-c(description))
  houses
}

# Transform training data
train <- houses_preproc(train)
# Transfom test data
test2 <- houses_preproc(test)

mapview(test2, xcol = "lon", ycol = "lat", crs = 4326, grid = FALSE)
mapview(train, xcol = "lon", ycol = "lat", crs = 4326, grid = FALSE)
install.packages("Rcpp")
install.packages("mapview")
library(mapview)
library(Rcpp)

install.packages("spdep")
library("tidyverse")
library("sf")
library("osmdata")
library("spdep")
library("dplyr")


train_geo <- st_as_sf(x = train, coords = c("lon", "lat"), crs = 4326)
test_geo <- st_as_sf(x = test2, coords = c("lon", "lat"), crs = 4326)



# Get neighbors for points in a df
get_nb <- function(df, dist) {
  # Get buffer around points
  df_sp <- df %>%
    st_buffer(dist) %>%
    as_Spatial()
  # Get neighbors
  df_nb <- poly2nb(pl = df_sp, queen = TRUE)
  df_nb
}

# Neighbors to houses in Bogota - 150 meters
train_nb_150 <- get_nb(train_geo, 150)
saveRDS(train_nb_150, file = "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_train_150.Rds")


test_nb_150 <- get_nb(test_geo, 150)
saveRDS(test_nb_150, file = "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_test_150.Rds")


# Get a specific set of polygons corresponding to the geographic feature of interest
get_feature <- function(place, key, value) {
  feature <- opq(bbox = getbb(place)) %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf() %>%
    .$osm_polygons %>%
    dplyr::select(osm_id)
  
  feature
}

# Load matrix of pairs of selected key-value pairs
keyvals <- read.csv("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/st_maps_key_val.csv")

# Get and store polygons for the previously defined set of geographic features - train Bogota
for (i in 1:nrow(keyvals)) {
  feature <- get_feature("Bogota Colombia", keyvals$key[i], keyvals$value[i])
  dist_feature <- st_distance(x = train_geo, y = feature)
  path <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_train_", keyvals$value[i], ".Rds")
  saveRDS(dist_feature, path)
}

# Get and store polygons for the previously defined set of geographic features - test
for (i in 1:nrow(keyvals)) {
  feature <- get_feature("Bogota Colombia", keyvals$key[i], keyvals$value[i])
  dist_feature <- st_distance(x = test_geo, y = feature)
  path <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_test_", keyvals$value[i], ".Rds")
  saveRDS(dist_feature, path)
}

p_load(tidyverse,rio,skimr,viridis,
       sf, ## leer/escribir/manipular datos espaciales
       nngeo, ## st_nn function
       spdep, ## Construct neighbours list from polygon list 
       osmdata) ## Get OSM's data

## houses from geo data


houses_train <- train_geo
houses_test <- test_geo

# obtener poligono 
sf_use_s2(FALSE)

select <- dplyr::select

get_estrato <- function(base){
  if(base == "train"){
    houses <- houses_train
    df <- getbb(place_name = "Bogota", featuretype = "", format_out = "sf_polygon") %>% .$multipolygon
    mnz <- st_read("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/MGN2018_URB_MANZANA/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_MGN_A2_11.CSV")
    viv <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_1VIV_A2_11.CSV")
  }
  
  if(base == "test"){
    houses <- houses_test
    df <- getbb(place_name = "Bogota", featuretype = "", format_out = "sf_polygon") %>% .$multipolygon
    mnz <- st_read("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/MGN2018_URB_MANZANA/MGN_URB_MANZANA.shp") %>% select(MANZ_CCNCT) %>% .[df,]
    mgn <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_MGN_A2_11.CSV")
    viv <- import("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/11_Bogota_CSV/CNPV2018_1VIV_A2_11.CSV")
  }
  
  ### Unir datos imputados con manzanas DANE
  houses <- st_join(x=houses , y=mnz)
  
  
  ### Crear data estratos
  ## data manzanas
  mgn <- mgn %>% select(COD_DANE_ANM,UA_CLASE,COD_ENCUESTAS,U_VIVIENDA)
  
  ## data vivienda
  viv <- viv %>% select(COD_ENCUESTAS,UA_CLASE,U_VIVIENDA,V_TOT_HOG,VA1_ESTRATO)
  
  ## joing mnz-hogar-vivienda
  viv_mgn <- left_join(viv, mgn, by=c("UA_CLASE","COD_ENCUESTAS","U_VIVIENDA"))
  
  ##=== collapse data ===##
  db <- viv_mgn %>%
    group_by(COD_DANE_ANM) %>% 
    summarise(med_VA1_ESTRATO=median(VA1_ESTRATO,na.rm=T))
  
  
  ### UNir estratos hogar por manzana
  
  houses <- left_join(houses,db,by=c("MANZ_CCNCT"="COD_DANE_ANM"))
  
  estrato <- houses %>% select(property_id, med_VA1_ESTRATO)
  estrato <- st_drop_geometry(estrato)
  path_exp <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/estrato_", base, ".rds")
  export(estrato, path_exp)
}

get_estrato("train")
get_estrato("test")

get_mode <- function(x) { # Create mode function
  unique_x <- unique(x)
  tabulate_x <- tabulate(match(x, unique_x))
  if (length(tabulate_x) > 1) {
    unique_x <- na.omit(unique(x))
    tabulate_x <- tabulate(match(x, unique_x))
    modes <- unique_x[tabulate_x == max(tabulate_x)]
    if (length(modes > 1)) {
      set.seed(10)
      modes <- sample(modes, size = 1)
      return(modes)
    } else {
      return(modes)
    }
  } else {
    return(unique_x[tabulate_x == max(tabulate_x)])
  }
}

#estratos

estrato_train <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/estrato_train.rds")
estrato_test <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/estrato_test.rds")


houses_train <- houses_train %>% left_join(estrato_train, by = "property_id")
houses_test <- houses_test %>% left_join(estrato_test, by = "property_id")


houses_train <- houses_train %>% dplyr::rename(estrato = med_VA1_ESTRATO)
houses_test <- houses_test %>% dplyr::rename(estrato = med_VA1_ESTRATO)


nei_train_150 <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_train_150.Rds")
nei_test_150 <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/nei_test_150.Rds")

# Impute values using custom mode function that retrieves most repeated values
# in a vecinity
mode_imputer <- function(df, neighbors) {
  # Variables that have values to be imputed
  vars_to_impute <- c(
    "bathrooms",
    "sala_com",
    "upgrade_in",
    "upgrade_out",
    "garage",
    "light",
    "estrato"
  )
  
  for (variable in vars_to_impute) {
    # Create empty column to fill with imputed values
    imputed_var <- paste0("imputed_", variable)
    df[, imputed_var] <- numeric(nrow(df))
    
    for (value in seq_len(nrow(df))) { # For each property in the newly created column
      # Get indices for its neighbors
      values_neighbors <- df[neighbors[[value]], variable][[1]]
      # Apply custom mode function on the currently iterated variable and set of neighbors
      imputed <- get_mode(values_neighbors)
      # Impute the obtained mode to the currently iterated property
      df[value, imputed_var] <- imputed
    }
  }
  df
}


# Repeat same algorithm as in mode_imputer, but using mean instead of mode
mean_imputer <- function(df, neighbors) {
  vars_to_impute <- c("surface_total")
  for (variable in vars_to_impute) {
    imputed_var <- paste0("imputed_", variable)
    df[, imputed_var] <- numeric(nrow(df))
    for (value in seq_len(nrow(df))) {
      values_neighbors <- df[neighbors[[value]], variable][[1]]
      imputed <- mean(values_neighbors, na.rm = TRUE)
      if (is.nan(imputed)) {
        imputed <- NA
      }
      df[value, imputed_var] <- imputed
    }
  }
  df
}


# Use the columns created by the two previous functions to fill missing values
# in their original variables

tidy_base <- function(df) {
  df <- df %>%
    mutate(
      bathrooms = if_else(
        bathrooms == 0 | is.na(bathrooms), imputed_bathrooms, bathrooms
      ),
      sala_com = if_else(
        sala_com == 0 | is.na(sala_com), imputed_sala_com, sala_com
      ),
      upgrade_in = if_else(
        upgrade_in == 0 | is.na(upgrade_in), imputed_upgrade_in, upgrade_in
      ),
      upgrade_out = if_else(
        upgrade_out == 0 | is.na(upgrade_out), imputed_upgrade_out, upgrade_out
      ),
      garage = if_else(
        garage == 0 | is.na(garage), imputed_garage, garage
      ),
      light = if_else(
        light == 0 | is.na(light), imputed_light, light
      ),
      surface_total = if_else(
        surface_total == 0 | is.na(surface_total), imputed_surface_total, surface_total
      ),
      estrato = if_else(
        estrato == 0 | is.na(estrato), imputed_estrato, estrato
      )
    ) %>%
    select(-starts_with("imputed"))
  
  df
}


# Use search perimeter of 150 meters to impute remaining missing values - train
houses_train <- mode_imputer(houses_train, nei_train_150)
houses_train <- mean_imputer(houses_train, nei_train_150)
houses_train <- tidy_base(houses_train)

saveRDS(houses_train, "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/imputed_train.Rds")


# Use search perimeter of 150 meters to impute remaining missing values - test
houses_test <- mode_imputer(houses_test, nei_test_150)
houses_test <- mean_imputer(houses_test, nei_test_150)
houses_test <- tidy_base(houses_test)

saveRDS(houses_test, "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/imputed_test.Rds")

# Get two values with respect of a geograpghic feature:
# 1) Number of points of that kind within 500 meters from a specific house
# 2) Distance to the closest point of that kind from a specific house
vars_from_dist <- function(base, value) {
  # Load set of polygons created by get_dist.R for a specific feature
  path <- paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_", base, "_", value, ".Rds")
  data <- readRDS(path)
  # Create an empty dataframe to store vales of two variables
  less_500m_name <- paste0("less_500m_", value)
  closest_name <- paste0("closest_", value)
  grouped_vars <- data.frame(numeric(nrow(data)), numeric(nrow(data)))
  colnames(grouped_vars) <- c(less_500m_name, closest_name)
  
  for (i in seq_len(nrow(data))) { # For each house
    # Get all distances from the house to the polygons that belong to a category
    y <- data[i, ] %>% as.numeric()
    # Count how many of those polygons are within 500 meters from the house
    less_500m <- y[y < 500] %>% length()
    # Determine the closest polygon
    closest <- min(y)
    # Associate retrieved values to the house being iterated on
    grouped_vars[i, less_500m_name] <- less_500m
    grouped_vars[i, closest_name] <- closest
  }
  grouped_vars
}

houses_train <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/imputed_train.Rds")
houses_test <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/imputed_test.Rds")


# Iterate through each key-value pair from the matrix of variables of interest
# and find the values that were difined by the function vars_from_dist
# for a given base
append_dist_vars <- function(df, base) {
  for (i in keyvals[, "value"]) {
    vars <- vars_from_dist(base, i)
    df <- cbind(df, vars)
  }
  df
}

houses_train <- append_dist_vars(houses_train, "train")
saveRDS(houses_train, "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_vars_imputed_train.Rds")
houses_test <- append_dist_vars(houses_test, "test")
saveRDS(houses_test, "C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_vars_imputed_test.Rds")

houses_train <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_vars_imputed_train.Rds")
houses_test <- readRDS("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/dist_vars_imputed_test.Rds")

data_train <- houses_train
data_test <- houses_test

###Limpieza de últimos NAs en test (cerca de 12-15 observaciones en algunas variables)
median_surface_total <- median(data_test$surface_total, na.rm = TRUE)

data_test$surface_total <- ifelse(is.na(data_test$surface_total), median_surface_total, data_test$surface_total)


# Calcular la moda de un vector excluyendo los valores faltantes (NA)
get_mode2 <- function(x){
  x <- x[!is.na(x)]  # excluir los valores faltantes
  tab <- table(x)  # contar la frecuencia de cada valor
  tab_max <- tab[which.max(tab)]  # seleccionar la frecuencia máxima
  mode_val <- as.numeric(names(tab)[tab == tab_max])  # seleccionar el valor correspondiente
  return(mode_val)
}

# Crear una lista con los nombres de las columnas a modificar
cols_to_impute <- c("bathrooms", "sala_com", "upgrade_in", "upgrade_out", "garage", "light", "estrato")

# Iterar sobre las columnas y reemplazar los valores faltantes con la moda
for(col in cols_to_impute){
  mode_col <- get_mode2(data_test[[col]])
  data_test[[col]] <- ifelse(is.na(data_test[[col]]), mode_col, data_test[[col]])
}

data_test <- st_drop_geometry(data_test)

# Histograma log(precio)
histo_precio_train <- ggplot(data_train, aes(x = log(price))) +
  geom_histogram(bins = 50,color = "grey30", fill = "darkblue") +
  ggtitle("Precio vivienda Bogotá") +
  labs(x = "Log(precio vivienda)", y = "Cantidad") +
  theme_bw()
ggsave("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/Log_precio_train.png", histo_precio_train)


# Histogrma área vivienda
#data_bog <- data_bog%>% mutate_at (c ('surface_total'), ~ ( scale (.)%>% as.vector ))


histo_area_train <- ggplot(data_train, aes(x = surface_total)) +
  xlim(0, 500) +  geom_histogram(bins = 100,color = "gray30", fill = "green") +
  ggtitle("Área vivienda Bogotá") + 
  labs(x = "Metros cuadrados (M2)", y = "Cantidad") +
  theme_bw()
ggsave("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/area_train.png", histo_area_train)

histo_area_test <- ggplot(data_test, aes(x = surface_total)) +
  xlim(0, 500) + geom_histogram(bins = 100,color = "grey30", fill = "red") +
  ggtitle("Área vivienda Chapinero") +
  labs(x = "Metros cuadrados (M2)", y = "Cantidad") +
  theme_bw()
ggsave("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/area_test.png", histo_area_test)

# Tablas descriptivas
data_bog <- data_train[,!names(data_train) %in% c("property_id", "city")]
data_chap <- data_test[,!names(data_test) %in% c("property_id", "city")]

p_load(vtable)

get_descriptives <- function(city){
  if(city == "bog"){
    data <- data_bog
  }
  
  if(city == "chap"){
    data <- data_chap
  }
  
  data <- data %>% rename(
    'area M2' = surface_total,
    "Numero de habitaciones" = bedrooms,
    "Numero de baños" = bathrooms,
    "Distancia gasolinería mas cercana" =  closest_fuel,
    "Distancia hospital mas cercano" = closest_hospital,
    "Distancia estacion policia mas cercana" = closest_police,
    "Distancia parque mas cercano" = closest_park,
    "Distancia rio mas cercano" = closest_river,
    "Distancia universidad mas cercana" = closest_university,
    "Distancia estacion transporte mas cercana" = closest_station,
    "Distancia supermercado mas cercano" = closest_supermarket,
    "Distancia C.Comercial mas cercano" = closest_mall,
    "Tipo de  vivienda (1 si es casa)" = house,
    "Cuenta con sala o  comedor" = sala_com,
    "Servicios interiores adicional" = upgrade_in,
    "Servicios exteriores adicional" = upgrade_out,
    "Luz natural" = light,
    "Numero de gasolineras a 500 mts" = less_500m_fuel,
    "Numero de hospitales a 500 mts" = less_500m_hospital,
    "Numero de estaciones policia a 500 mts" = less_500m_police,
    "Numero de parques a 500 mts" = less_500m_park,
    "Numero de rios a 500 mts" = less_500m_river,
    "Numero de universidades a 500 mts" = less_500m_university,
    "Numero de estaciones transporte a 500 mts" = less_500m_station,
    "Numero de Supermercado a 500 mts" = less_500m_supermarket,
    "Numero de C.comercial a 500 mts" = less_500m_mall,
  )
  sumtable(data, out='latex',file= paste0("C:/Users/juanc/OneDrive - Universidad Nacional de Colombia/Big data ML/ps3/descriptive_", city, ".tex"))
}

get_descriptives("bog")
get_descriptives("chap")

##Preparing for regressions

houses_train <- houses_train %>% mutate(
  price = as.numeric(scale(price)),
  estrato = floor(estrato)
)

data2 <- na.omit(houses_train)
data2 <- data2 %>% mutate(
  across(
    c(
      city,
      house,
      sala_com,
      upgrade_in,
      upgrade_out,
      garage,
      light,
      estrato
    ),
    as.factor
  )
)


data_test_f <- data_test %>% mutate(
  estrato = floor(estrato)
)

data_test_f <- data_test_f %>% mutate(
  across(
    c(
      city,
      house,
      sala_com,
      upgrade_in,
      upgrade_out,
      garage,
      light,
      estrato
    ),
    as.factor
  )
)


# # Selector de muestra!!!
# set.seed(10)
# data <- data %>%
#     slice_sample(prop = 0.5)

data2<- st_drop_geometry(data2)

predictors2 <- data2 %>%
  select(-price) %>%
  names()
p_load("caret")
set.seed(666)
train_index <- createDataPartition(data2$price, p = .8)$Resample1
train_boost <- data2[train_index,]
test_boost <- data2[-train_index,]

# Creamos las particiones para hacer validación cruzada
cv5 <- trainControl(number = 5, method = "cv")
cv3 <- trainControl(number = 3, method = "cv")

#Random Forest
# Creamos una grilla para tunear el random forest
tunegrid_rf <- expand.grid(mtry = c(3, 5, 10), 
                           min.node.size = c(10, 30, 50,
                                             70, 100),
                           splitrule = "variance"
                            )
p_load("parallel")
n_cores <- detectCores()
print(paste("Mi PC tiene", n_cores, "nucleos"))

p_load("doParallel")
cl <- makePSOCKcluster(n_cores - 5) 
registerDoParallel(cl)



modelorf <- train(price ~ surface_total+bedrooms + bathrooms +house+sala_com+upgrade_in +upgrade_out+garage+light+
                   estrato+less_500m_fuel+closest_fuel+less_500m_hospital+closest_hospital+less_500m_police+closest_police+
                   less_500m_park+closest_park+less_500m_river+closest_river+less_500m_university+closest_university+less_500m_station+
                   closest_station+less_500m_supermarket+closest_supermarket+less_500m_mall+closest_mall,
                 data = train_boost, 
                 method = "ranger", 
                 trControl = cv5,
                 metric = 'RMSE', 
                 tuneGrid = tunegrid_rf)

summary(modelorf)
p_load("ranger")
var_importance_rf <- importance(modelorf)


y_hat_insample_rf = predict(modelorf, newdata = train_boost)
y_hat_outsamplerf = predict(modelorf, newdata = test_boost)

MAE(y_hat_insample_rf,train_boost$price)

MAE(y_hat_outsamplerf,test_boost$price)




precio_pred_rf = predict(modelorf, newdata = data_test_f)
mean_orig <- mean(houses_train$price)
sd_orig <- sd(houses_train$price)


predic_orig_rf = precio_pred_rf * sd_orig + mean_orig
test_rf <- cbind(data_test_f, predic_orig_rf)

# Crear data frame con variables property_id y predic_orig_rf
pred_rf <- data.frame(property_id = test_rf$property_id, price = predic_orig_rf)

# Renombrar columna predic_orig_rf como price
names(pred_rf)[2] <- "price"

# Guardar data frame en un archivo csv
write.csv(pred_rf, "pred_rf.csv", row.names = FALSE)



write.csv(pred_rf, file = "pred_rf.csv")

#XGB

tunegrid_xgb <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.05, 0.1),
  gamma = c(0, 1, 5),
  colsample_bytree = seq(0.1, 1, 0.1),
  min_child_weight = c(1, 3, 5),
  subsample = seq(0.5, 1, 0.1)
)

library(xgboost)
model_xgb <- train(price ~ surface_total + bedrooms + bathrooms + less_500m_hospital + less_500m_police +
                     less_500m_park + less_500m_river + less_500m_university +
                     less_500m_supermarket + less_500m_mall,
  data = train_boost,
  method = "xgbTree",
  trControl = cv5,
  metric = 'RMSE',
  tuneGrid = tunegrid_xgb,
  verbose = FALSE,
  nthread = 1 # para evitar problemas con el paralelismo
)

y_hat_insample_xgb = predict(modelxgb, newdata = train_boost)
y_hat_outsample_xgb = predict(model_xgb, newdata = test_boost)

MAE(y_hat_insample_xgb,train_boost$price)

MAE(y_hat_outsample_xgb,test_boost$price)




precio_pred_xgb = predict(model_xgb, newdata = data_test_f)
mean_orig <- mean(houses_train$price)
sd_orig <- sd(houses_train$price)


predic_orig_xgb = precio_pred_xgb * sd_orig + mean_orig
test_xgb <- cbind(data_test_f, predic_orig_xgb)

# Crear data frame con variables property_id y predic_orig_rf
pred_xgb <- data.frame(property_id = test_xgb$property_id, price = predic_orig_xgb)

# Renombrar columna predic_orig_rf como price
names(pred_xgb)[2] <- "price"

# Guardar data frame en un archivo csv
write.csv(pred_xgb, "pred_xgb.csv", row.names = FALSE)



write.csv(pred_xgb, file = "pred_xgb.csv")

####


######

lm_model <- train(price ~ surface_total + bedrooms + bathrooms + less_500m_hospital + less_500m_police +
                    less_500m_park + less_500m_river + less_500m_university +
                    less_500m_supermarket + less_500m_mall, data = train_boost, method = "lm", trControl = cv5, metric = "RMSE")

y_hat_insample_lm = predict(lm_model, newdata = train_boost)
y_hat_outsample_lm = predict(lm_model, newdata = test_boost)

MAE(y_hat_insample_lm,train_boost$price)

MAE(y_hat_outsample_lm,test_boost$price)




precio_pred_lm = predict(lm_model, newdata = data_test_f)
mean_orig <- mean(houses_train$price)
sd_orig <- sd(houses_train$price)


predic_orig_lm = precio_pred_lm * sd_orig + mean_orig
test_lm <- cbind(data_test_f, predic_orig_lm)

# Crear data frame con variables property_id y predic_orig_rf
pred_lm <- data.frame(property_id = test_lm$property_id, price = predic_orig_lm)

# Renombrar columna predic_orig_rf como price
names(pred_lm)[2] <- "price"

# Guardar data frame en un archivo csv
write.csv(pred_lm, "pred_lm.csv", row.names = FALSE)



write.csv(pred_lm, file = "pred_lm.csv")


######

lasso_model <- train(price ~ surface_total + bedrooms + bathrooms + house + sala_com + upgrade_in +
                       upgrade_out + garage + light + estrato + less_500m_fuel + closest_fuel + 
                       less_500m_hospital + closest_hospital + less_500m_police + closest_police + 
                       less_500m_park + closest_park + less_500m_river + closest_river + 
                       less_500m_university + closest_university + less_500m_station + closest_station + 
                       less_500m_supermarket + closest_supermarket + less_500m_mall + closest_mall, data = train_boost, method = "glmnet", trControl = cv5, 
                     metric = "RMSE", tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, by = 0.01)))

y_hat_insample_lasso = predict(lasso_model, newdata = train_boost)
y_hat_outsample_lasso = predict(lasso_model, newdata = test_boost)

MAE(y_hat_insample_lasso,train_boost$price)

MAE(y_hat_outsample_lasso,test_boost$price)




precio_pred_lasso = predict(lasso_model, newdata = data_test_f)
mean_orig <- mean(houses_train$price)
sd_orig <- sd(houses_train$price)


predic_orig_lasso = precio_pred_lasso * sd_orig + mean_orig
test_lasso <- cbind(data_test_f, predic_orig_lasso)

# Crear data frame con variables property_id y predic_orig_rf
pred_lasso <- data.frame(property_id = test_lasso$property_id, price = predic_orig_lasso)

# Renombrar columna predic_orig_rf como price
names(pred_lasso)[2] <- "price"

# Guardar data frame en un archivo csv
write.csv(pred_lasso, "pred_lasso.csv", row.names = FALSE)



write.csv(pred_lasso, file = "pred_lasso.csv")


########

ridge_model <- train(price ~ surface_total + bedrooms + bathrooms + house + sala_com + upgrade_in +
                       upgrade_out + garage + light + estrato + less_500m_fuel + closest_fuel + 
                       less_500m_hospital + closest_hospital + less_500m_police + closest_police + 
                       less_500m_park + closest_park + less_500m_river + closest_river + 
                       less_500m_university + closest_university + less_500m_station + closest_station + 
                       less_500m_supermarket + closest_supermarket + less_500m_mall + closest_mall, data = train_boost, method = "glmnet", trControl = cv5, metric = "RMSE", tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 1, by = 0.01)))




y_hat_insample_ridge = predict(ridge_model, newdata = train_boost)
y_hat_outsample_ridge = predict(ridge_model, newdata = test_boost)

MAE(y_hat_insample_ridge,train_boost$price)

MAE(y_hat_outsample_ridge,test_boost$price)




precio_pred_ridge = predict(ridge_model, newdata = data_test_f)
mean_orig <- mean(houses_train$price)
sd_orig <- sd(houses_train$price)


predic_orig_ridge = precio_pred_ridge * sd_orig + mean_orig
test_ridge <- cbind(data_test_f, predic_orig_ridge)

# Crear data frame con variables property_id y predic_orig_rf
pred_ridge <- data.frame(property_id = test_ridge$property_id, price = predic_orig_ridge)

# Renombrar columna predic_orig_rf como price
names(pred_ridge)[2] <- "price"

# Guardar data frame en un archivo csv
write.csv(pred_ridge, "pred_ridge.csv", row.names = FALSE)



write.csv(pred_ridge, file = "pred_ridge.csv")

#########################################

en_model <- train(price ~ surface_total + bedrooms + bathrooms + house + sala_com + upgrade_in +
                       upgrade_out + garage + light + estrato + less_500m_fuel + closest_fuel + 
                       less_500m_hospital + closest_hospital + less_500m_police + closest_police + 
                       less_500m_park + closest_park + less_500m_river + closest_river + 
                       less_500m_university + closest_university + less_500m_station + closest_station + 
                       less_500m_supermarket + closest_supermarket + less_500m_mall + closest_mall, 
                     data = train_boost, 
                     method = "glmnet", 
                     trControl = cv5, 
                     metric = "RMSE", 
                     tuneGrid = expand.grid(alpha = 0.5, lambda = seq(0, 1, by = 0.01)))

y_hat_insample_en = predict(en_model, newdata = train_boost)
y_hat_outsample_en = predict(en_model, newdata = test_boost)

MAE(y_hat_insample_en,train_boost$price)

MAE(y_hat_outsample_en,test_boost$price)




precio_pred_en = predict(en_model, newdata = data_test_f)



predic_orig_en = precio_pred_en * sd_orig + mean_orig
test_en <- cbind(data_test_f, predic_orig_en)

# Crear data frame con variables property_id y predic_orig_rf
pred_en <- data.frame(property_id = test_en$property_id, price = predic_orig_en)

# Renombrar columna predic_orig_rf como price
names(pred_en)[2] <- "price"

# Guardar data frame en un archivo csv
write.csv(pred_en, "pred_en.csv", row.names = FALSE)



write.csv(pred_ridge, file = "pred_ridge.csv")

######





