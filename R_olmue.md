---
title: "OLMUE"
author: "Sebastian Yanez"
date: "2024-07-09"
output: html_document
---
##Reiniciar 
```{r}
rm(list=ls())         # Limpia la lista de objetos 
graphics.off()        # Limpia la lista de gráficos
```


```{r}
#install.packages("mapview")
library(mapview)
```

```{r}
library(dplyr) 
library(ggplot2)
library(knitr)
library(cowplot)
library(kableExtra)
library(modelsummary)
library(mlogit)
library(lubridate)
library(tidyverse)
#library(Hmisc) 
library(tidyverse)
library(prettydoc)
library(readr)
library(haven)
library(corrplot)
library(ggcorrplot)
library(utils)
library(stringr)
library(openxlsx)
library(lubridate)
library(gridExtra)
```

##Leer csv
```{r}
setwd("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS")
data <- read.csv2("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS/data_terrenos_menos_5000_desde_2021.csv")
```
#####################################LIMPIAR DATA####################################################################

#sacar comillas de los datos
```{r}
data <- data.frame(lapply(data, function(x) {
  if(is.character(x)) return(gsub("'", "", x))
  else return(x)
}))
```

#renombrar las variables
```{r}
names(data) <- gsub("^X.|.$", "", names(data))
```

#rectificar columna descripción quedan 98009
```{r}
is.numeric.string <- function(s) {
  return(!any(is.na(suppressWarnings(as.numeric(s)))))
}

data$descripcion_2 <- NA

for (i in 2:nrow(data)) {
  if (!is.numeric.string(data$id[i])) {
    data$descripcion_2[i-1] <- data$id[i]
  }
}
text_rows <- sapply(data$id, function(x) !is.numeric.string(x))
data <- data[!text_rows, ]
```

######################################FORMATO DE VARIABLES#####################################################################

#Rectificar nombres **(REVISAR)**
```{r}
correcciones <- c(
  "Concepcion" = "ConcepciÃ³n",
  "Conepcion" = "ConcepciÃ³n",
  "Viña Del Mar " = "ViÃ±a del Mar",
  "Cochamo" = "CochamÃ³",
  "Donihue" = "DoÃ±ihue",
  "Hualaihue" = "HualaihuÃ©",
  "LaReina" = "La Reina",
  "Llaillay" = "Llayllay",
  "Llay Llay" = "Llayllay",
  "Los Alamos" = "Los Ãlamos",
  "Mostazal" = "San Francisco De Mostazal",
  "Francisco Mostazal" = "San Francisco De Mostazal",
  "Licanten" = "LicantÃ©n",
  "Ohiggins" = "OHiggins",
  "Natales" = "Puerto Natales",
  "Queilen" = "QueilÃ©n",
  "Quinta de Tilcoco" = "Quinta De Tilcoco",
  "Rio Ibanez" = "RÃ­o IbÃ¡Ã±ez",
  "San Jose De La Mariquina" = "San JosÃ© De La Mariquina",
  "San Jose  de Maipo" = "San JosÃ© De Maipo",
  "San Juan de la Costa" = "San Juan De La Costa",
  "San Juan de La Costa" = "San Juan De La Costa",
  "San Pedro de la Paz" = "San Pedro De La Paz",
  "San Vicente" = "San Vicente De Tagua Tagua",
  "Chol Chol" = "Cholchol",
  "Copiapo" = "CopiapÃ³",
  "Hualane" = "HualaÃ±Ã©",
  "Longuimay" = "Lonquimay"
)
for (correccion in names(correcciones)) {
  data$region <- gsub(correccion, correcciones[correccion], data$region)
}
```

#Mayúscula y sin tildes
```{r}
data$comuna <- toupper(data$comuna)
data$region <- toupper(data$region)
data$descripcion <- toupper(data$descripcion)
data$direccion <- toupper(data$direccion)
data$concat <- toupper(data$concat)

data$comuna <- iconv(data$comuna, to = "ASCII//TRANSLIT")
data$region <- iconv(data$region, to = "ASCII//TRANSLIT")
data$descripcion <- iconv(data$descripcion, to = "ASCII//TRANSLIT")
data$direccion <- iconv(data$direccion, to = "ASCII//TRANSLIT")
data$concat <- iconv(data$concat, to = "ASCII//TRANSLIT")
```

#Conteo NULL(20729), NA(0) en columna comuna
```{r}
sum(data$comuna == "NULL")
sum(data$comuna == 'NA')
sum(data$region == 'NULL')
sum(data$region == 'NA')
```
#Regiones y comunas
```{r}
regiones_comunas <- list(
  "ARICA Y PARINACOTA" = c("ARICA", "CAMARONES", "GENERAL LAGOS", "PUTRE"),
  "TARAPACA" = c("ALTO HOSPICIO", "IQUIQUE", "HUARA", "CAMINA", "COLCHANE", "PICA", "POZO ALMONTE"),
  "ANTOFAGASTA" = c("ANTOFAGASTA", "CALAMA", "MARIA ELENA", "MEJILLONES", "OLLAGUE", "SAN PEDRO DE ATACAMA", "SIERRA GORDA", "TALTAL", "TOCOPILLA"),
  "ATACAMA" = c("ALTO DEL CARMEN", "CALDERA", "CHANARAL", "COPIAPO", "DIEGO DE ALMAGRO", "FREIRINA", "HUASCO", "TIERRA AMARILLA", "VALLENAR"),
  "COQUIMBO" = c("ANDACOLLO", "CANELA", "COMBARBALA", "COQUIMBO", "ILLAPEL", "LA HIGUERA", "LA SERENA", "LOS VILOS", "MONTE PATRIA", "OVALLE", "PAIHUANO", "PUNITAQUI", "RIO HURTADO", "SALAMANCA", "VICUNA"),
  "VALPARAISO" = c("ALGARROBO", "CABILDO", "CALLE LARGA", "CARTAGENA", "CASABLANCA", "CATEMU", "CONCON", "EL QUISCO", "EL TABO", "HIJUELAS", "ISLA DE PASCUA", "JUAN FERNANDEZ", "LA CALERA", "LA CRUZ", "LA LIGUA", "LIMACHE", "LLAILLAY", "LOS ANDES", "NOGALES", "OLMUE", "PANQUEHUE", "PAPUDO", "PETORCA", "PUCHUNCAVI", "PUTAENDO", "QUILLOTA", "QUILPUE", "QUINTERO", "RINCONADA", "SAN ANTONIO", "SAN ESTEBAN", "SAN FELIPE", "SANTA MARIA", "SANTO DOMINGO", "VALPARAISO", "VILLA ALEMANA", "VINA DEL MAR", "ZAPALLAR"),
  "METROPOLITANA" = c("ALHUE", "BUIN", "CALERA DE TANGO", "CERRILLOS", "CERRO NAVIA", "COLINA", "CONCHALI", "CURACAVI", "EL BOSQUE", "EL MONTE", "ESTACION CENTRAL", "HUECHURABA", "INDEPENDENCIA", "ISLA DE MAIPO", "LA CISTERNA", "LA FLORIDA", "LA GRANJA", "LAMPA", "LA PINTANA", "LA REINA", "LAS CONDES", "LO BARNECHEA", "LO ESPEJO", "LO PRADO", "MACUL", "MAIPU", "MARIA PINTO", "MELIPILLA", "NUNOA", "PADRE HURTADO", "PAINE", "PEDRO AGUIRRE CERDA", "PENAFLOR", "PENALOLEN", "PIRQUE", "PROVIDENCIA", "PUDAHUEL", "PUENTE ALTO", "QUILICURA", "QUINTA NORMAL", "RECOLETA", "RENCA", "SAN BERNARDO", "SAN JOAQUIN", "SAN JOSE DE MAIPO", "SAN MIGUEL", "SAN PEDRO", "SAN RAMON", "SANTIAGO", "TALAGANTE", "TILTIL", "VITACURA"),
  "LIBERTADOR GENERAL BERNARDO OHIGGINS" = c("CHEPICA", "CHIMBARONGO", "CODEGUA", "COINCO", "COLTAUCO", "DONIHUE", "GRANEROS", "LA ESTRELLA", "LAS CABRAS", "LITUECHE", "LOLOL", "MACHALI", "MALLOA", "MARCHIHUE", "MOSTAZAL", "NANCAGUA", "NAVIDAD", "OLIVAR", "PALMILLA", "PAREDONES", "PERALILLO", "PEUMO", "PICHIDEGUA", "PICHILEMU", "PLACILLA", "PUMANQUE", "QUINTA DE TILCOCO", "RANCAGUA", "RENGO", "REQUINOA", "SAN FERNANDO", "SANTA CRUZ", "SAN VICENTE"),
  "MAULE" = c("CAUQUENES", "CHANCO", "COLBUN", "CONSTITUCION", "CUREPTO", "CURICO", "EMPEDRADO", "HUALANE", "LICANTEN", "LINARES", "LONGAVI", "MAULE", "MOLINA", "PARRAL", "PELARCO", "PELLUHUE", "PENCAHUE", "RAUCO", "RETIRO", "RIO CLARO", "ROMERAL", "SAGRADA FAMILIA", "SAN CLEMENTE", "SAN JAVIER", "SAN RAFAEL", "TALCA", "TENO", "VICHUQUEN", "VILLA ALEGRE", "YERBAS BUENAS"),
  "NUBLE" = c("BULNES", "CHILLAN", "CHILLAN VIEJO", "COBQUECURA", "COELEMU", "COIHUECO", "EL CARMEN", "NINHUE", "NIQUEN", "PEMUCO", "PINTO", "PORTEZUELO", "QUILLON", "QUIRIHUE", "SAN CARLOS", "SAN FABIAN", "SAN IGNACIO", "SAN NICOLAS", "TREGUACO", "YUNGAY"),
  "BIOBIO" = c("ALTO BIOBIO", "ANTUCO", "ARAUCO", "CABRERO", "CANETE", "CHIGUAYANTE", "CONCEPCION", "CONTULMO", "CORONEL", "CURANILAHUE", "FLORIDA", "HUALPEN", "HUALQUI", "LAJA", "LEBU", "LOS ALAMOS", "LOS ANGELES", "LOTA", "MULCHEN", "NACIMIENTO", "NEGRETE", "PENCO", "QUILACO", "QUILLECO", "SAN PEDRO DE LA PAZ", "SAN ROSENDO", "SANTA BARBARA", "SANTA JUANA", "TALCAHUANO", "TIRUA", "TOME", "TUCAPEL", "YUMBEL"),
  "ARAUCANIA" = c("ANGOL", "CARAHUE", "CHOLCHOL", "COLLIPULLI", "CUNCO", "CURACAUTIN", "CURARREHUE", "ERCILLA", "FREIRE", "GALVARINO", "GORBEA", "LAUTARO", "LONCOCHE", "LONQUIMAY", "LOS SAUCES", "LUMACO", "MELIPEUCO", "NUEVA IMPERIAL", "PADRE LAS CASAS", "PERQUENCO", "PITRUFQUEN", "PUCON", "PUREN", "RENAICO", "SAAVEDRA", "TEMUCO", "TEODORO SCHMIDT", "TOLTEN", "TRAIGUEN", "VICTORIA", "VILCUN", "VILLARRICA"),
  "LOS RIOS" = c("CORRAL", "FUTRONO", "LAGO RANCO", "LANCO", "LA UNION", "LOS LAGOS", "MAFIL", "MARIQUINA", "PAILLACO", "PANGUIPULLI", "RIO BUENO", "VALDIVIA"),
  "LOS LAGOS" = c("ANCUD", "CALBUCO", "CASTRO", "CHAITEN", "CHONCHI", "COCHAMO", "CURACO DE VELEZ", "DALCAHUE", "FRESIA", "FRUTILLAR", "FUTALEUFU", "HUALAIHUE", "LLANQUIHUE", "LOS MUERMOS", "MAULLIN", "OSORNO", "PALENA", "PUERTO MONTT", "PUERTO OCTAY", "PUERTO VARAS", "PUQUELDON", "PURRANQUE", "PUYEHUE", "QUEILEN", "QUELLON", "QUEMCHI", "QUINCHAO", "RIO NEGRO", "SAN JUAN DE LA COSTA", "SAN PABLO"),
  "AYSEN" = c("AYSEN", "CHILE CHICO", "CISNES", "COCHRANE", "COYHAIQUE", "GUAITECAS", "LAGO VERDE", "OHIGGINS", "RIO IBANEZ", "TORTEL"),
  "MAGALLANES" = c("ANTARTICA", "CABO DE HORNOS", "LAGUNA BLANCA", "NATALES", "PORVENIR", "PRIMAVERA", "PUNTA ARENAS", "RIO VERDE", "SAN GREGORIO", "TIMAUKEL", "TORRES DEL PAINE")
)
```

```{r}
regiones <- names(regiones_comunas)
regiones_regex <- paste(regiones, collapse = "|")

comunas <- unlist(regiones_comunas)
comunas_regex <- paste(comunas, collapse = "|")

names(data) <- make.names(names(data), unique = TRUE)
```

#Filtrar filas donde comuna es cadena vacía ""
```{r}
conteo0 <- data %>%
  filter(comuna == "")
```

#Eliminar obs df original
```{r}
data <- anti_join(data, conteo0)
```
#Reemplazo
```{r}
conteo0$comuna <- str_extract(conteo0$descripcion, comunas_regex)
```

#Nuevo df
```{r}
conteo0_new <- conteo0 %>%
  filter(!is.na(comuna))
```

#Eliminar obs df original
```{r}
conteo0 <- anti_join(conteo0, conteo0_new)
```
#Conteo1 usar "descripción" para rellenar se reemplazaron 5122
```{r}
conteo1 <- data %>%
  filter(comuna == "NULL")
```

#Eliminar obs df original
```{r}
data <- anti_join(data, conteo1)
```
#Reemplazo
```{r}
conteo1$comuna <- str_extract(conteo1$descripcion, comunas_regex)
```

#Contar obs no reemplazadas
```{r}
na_count <- sum(is.na(conteo1$comuna))
```

#Conteo2 usar "concat" para rellenar
```{r}
conteo_2 <- subset(conteo1, is.na(comuna))
```

#Eliminar obs df original
```{r}
conteo1 <- anti_join(conteo1, conteo_2)
```
#Reemplazo
```{r}
conteo_2$comuna <- str_extract(conteo_2$concat, comunas_regex)
```

#Contar obs no reemplazadas
```{r}
na_count_2 <- sum(is.na(conteo_2$comuna))
```

#Conteo3
```{r}
conteo_3 <- subset(conteo_2, is.na(comuna))
```

#Eliminar obs df original
```{r}
conteo_2 <- anti_join(conteo_2, conteo_3)
```
#Resultados sin NA
```{r}
df <- rbind(data,conteo0_new, conteo1, conteo_2)

asignar_region <- function(comuna) {
  for (region in names(regiones_comunas)) {
    if (comuna %in% regiones_comunas[[region]]) {
      return(region)
    }
  }
  return(NA)  # Si no se encuentra la comuna en ninguna regiÃ³n
}
```

#Reemplazar valores "NULL" en "region", usando la función asignar_region
```{r}
df$region <- ifelse(df$region == 'NULL', sapply(df$comuna, asignar_region), df$region)
```

```{r}
datos <- df 
```

#Tamaño
```{r}
pattern <- "\\b\\d+[.,]?\\d*\\s*(M2|MTS|MS2|MTS2|HAS|HA|HECTAREA|HECTAREAS|METROS CUADRADOS)\\b"
datos$tamaño <- str_extract(datos$descripcion, pattern)

words <- "\\b(M2|MTS|MS2|MTS2|HAS|HA|HECTAREA|HECTAREAS|METROS CUADRADOS)\\b"
datos$word_1 <- str_extract(datos$tamaño, "M2|MTS|MS2|MTS2|HAS|HA|HECTAREAS|HECTAREA|METROS CUADRADOS")

datos$number <- str_extract(datos$tamaño, "\\d+[.,]?\\d*")
datos$number <- gsub("\\.", "", datos$number)
datos$number  <- gsub(",", ".", datos$number)
datos$number <- as.numeric(datos$number)

datos <- datos %>%
  mutate(size_m2 = case_when(
    word_1 %in% c('HAS', 'HA', 'HECTAREA', 'HECTAREAS') ~ number * 10000,
    word_1 %in% c('M2', 'MS2','MTS', 'MTS2', 'METROS CUADRADOS') ~ number,
    TRUE ~ NA_real_
  ))

datos$tamaño_2 <- str_extract(datos$direccion, pattern)
datos$word_2 <- str_extract(datos$tamaño_2, "M2|MTS|MS2|MTS2|HAS|HA|HECTAREA|HECTAREAS|METROS CUADRADOS")
datos$number_2 <- str_extract(datos$tamaño_2, "\\d+[.,]?\\d*")
datos$number_2 <- gsub("\\.", "", datos$number_2)
datos$number_2  <- gsub(",", ".", datos$number_2)
datos$number_2 <- as.numeric(datos$number_2)


datos <- datos %>%
  mutate(size_m2_2 = case_when(
    word_2 %in% c('HAS', 'HA', 'HECTAREA', 'HECTAREAS') ~ number_2 * 10000,
    word_2 %in% c('M2', 'MS2', 'MTS', 'MTS2', 'METROS CUADRADOS') ~ number_2,
    TRUE ~ NA_real_
  ))

datos$size_final_size <- ifelse(is.na(datos$size_m2), datos$size_m2_2,
                             ifelse(is.na(datos$size_m2_2), datos$size_m2,
                                    ifelse(datos$size_m2 == datos$size_m2_2, datos$size_m2, datos$size_m2)))



datos$size_final_size <- ifelse(is.na(datos$size_final_size), datos$total_o_terreno, datos$size_final_size)
datos$size_final_size <- as.numeric(datos$size_final_size)
```

*La base "datos" es la que quedó limpia*
#################################################################################################################################
#################################################################################################################################

#Filtramos Olmué
```{r}
comunasO <- c('OLMUAC','OLMUE')

df_olmue <- datos  %>%
  filter(comuna == comunasO)
```

*Se trabajará con la base df_olmue*
#Separar año
```{r}
df_olmue <- df_olmue %>%
  mutate(fecha = as.Date(.[[3]], format = "%Y-%m-%d")) %>%
  mutate(año_publicacion = year(fecha))
```

#Tamaño final
```{r}
df_olmue$size_final_size[35] <- 12000
df_olmue$size_final_size[145] <- 200000
df_olmue$size_final_size[195] <- 3225
df_olmue$size_final_size[241] <- 200000
df_olmue$size_final_size[242] <- 3600
```

#Sacar valores 0
```{r}
df_olmue <- df_olmue %>%
  filter( size_final_size!= 0)
```

#Ajuste tamaño de terrenos <100 
```{r}
df_olmue$size_final_size[179] <- 4250
df_olmue$size_final_size[44] <- 4375
df_olmue$size_final_size[26] <- 20000
df_olmue$size_final_size[48] <- 22705
df_olmue$size_final_size[76] <- 28000
df_olmue$size_final_size[35] <- 1200
df_olmue$size_final_size[36] <- 1100
df_olmue$size_final_size[114] <- 1100
df_olmue$size_final_size[73] <- 62100
df_olmue$size_final_size[93] <- 4840
df_olmue$size_final_size[164] <- 1092
df_olmue$size_final_size[150] <- 112000
df_olmue$size_final_size[33] <- 30600
df_olmue$size_final_size[20] <- 4927
df_olmue$size_final_size[81] <- 4250
df_olmue$size_final_size[111] <- 4840
df_olmue$size_final_size[124] <- 4840
df_olmue$size_final_size[134] <- 4840
df_olmue$size_final_size[142] <- 4840
df_olmue$size_final_size[144] <- 4840

```

#Filtrando outliers en metros cuadrados y precio UF
```{r}
df_olmue <- subset(df_olmue, size_final_size >= 100)
df_olmue <- subset(df_olmue, size_final_size <= 25000)

df_olmue <- subset(df_olmue, precio_uf >= 10)
```

#Crear columna que describe si los Terrenos son lotes o no
```{r}
df_olmue$contains_lot <- grepl("LOT", df_olmue$descripcion, ignore.case = TRUE)
```

#Se calculará las UF por m2 
```{r}
df_olmue$precio_uf <- as.numeric(df_olmue$precio_uf)

df_olmue$uf_m2 <- df_olmue$precio_uf/df_olmue$size_final_size
df_olmue$uf_m2 <- as.numeric(df_olmue$uf_m2)
```

#Terrenos publicados cada año
```{r}
df_agrupado <- df_olmue %>%
  group_by(año_publicacion) %>%
  summarise(numero_terrenos = n())
  
ggplot(data = df_agrupado, aes(x = año_publicacion, y = numero_terrenos)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Terrenos por Año",
       x = "Año",
       y = "Número de Terrenos") +
  theme_minimal()
```

#Grafico de dispersión
```{r}
library(scales)
```

```{r}
ggplot(df_olmue, aes(x = size_final_size, y = precio_uf)) + 
  geom_point(alpha = 0.5, size = 2) +
  labs(x = "Metros cuadrados", y = "Valor UF", title = "Gráfico de dispersión m² por valor UF") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +  # Ajusta el número de interrupciones según sea necesario
  theme_minimal() +
  theme(text = element_text(size = 12))
```

#Grafico de dispersión que diferencia si los terrenos son lotes, ya que hay publicaciones de terrenos grandes pero que en la descripción se especifica que estos están subdivididos en cierto numero de loteos. 
```{r}
ggplot(df_olmue, aes(x = size_final_size, y = precio_uf, color = contains_lot)) + 
  geom_point(alpha = 0.5, size = 2) +
  labs(x = "Metros cuadrados", y = "Valor UF", title = "Gráfico de dispersión m² por valor UF") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +  # Ajusta los colores según tus preferencias
  theme_minimal() +
  theme(text = element_text(size = 12))
```

#Para poder mapear las ubicaciones es necesario filtrar las propiedades que no tengan el valor de la latitud o longitud 
```{r}
df_olmue <- subset(df_olmue, longitude != 0 & latitude != 0)
```

################################################### Mapas ##############################################################################

#Filtrar propiedades publicadas en 2020-2021 
```{r}
df_olmue21 <- subset(df_olmue, año_publicacion %in% c(2020, 2021))
```

#Filtramos las propiedades que según sus coordenadas se encuentran fuera de Olmue
```{r}
df_olmue21 <- df_olmue21[-c(3, 13), ]
```

#Propiedades publicadas 2020-2021
#Se importa el archivo KML extraído de la pagina de la municipalidad de Olmué y se definen los puntos donde se ubican las propiedades a partir de su información de latitud y longitud
```{r}
library(mapview)
library(sf)
kml_layer <- st_read("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS/doc.kml")
mapview(kml_layer)
points <- st_as_sf(df_olmue21, coords = c("longitude", "latitude"), crs = 4326)
mapview(kml_layer) + mapview(points)

#mapshot(map, file = "mapa_olmue_20_21.html")
```

########################################################################################################################################

#Filtrar propiedades publicadas en 2022 
```{r}
df_olmue22 <- subset(df_olmue, año_publicacion %in% c(2022))
```

#Filtramos las propiedades que según sus coordenadas se encuentran fuera de Olmue
```{r}
df_olmue22 <- df_olmue22[-c(16, 29), ]
```

#Mapa publicaciones 2022
```{r}
library(mapview)
library(sf)
kml_layer <- st_read("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS/doc.kml")
mapview(kml_layer)
points <- st_as_sf(df_olmue22, coords = c("longitude", "latitude"), crs = 4326)
mapview(kml_layer) + mapview(points)
```

########################################################################################################################################

#Filtrar propiedades publicadas en 2023 
```{r}
df_olmue23 <- subset(df_olmue, año_publicacion %in% c(2023))
```

#Mapa publicaciones 2023
```{r}
library(mapview)
library(sf)
kml_layer <- st_read("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS/doc.kml")
mapview(kml_layer)
points <- st_as_sf(df_olmue23, coords = c("longitude", "latitude"), crs = 4326)
mapview(kml_layer) + mapview(points)
```
########################################################################################################################################

#Filtrar propiedades publicadas en 2024 
```{r}
df_olmue24 <- subset(df_olmue, año_publicacion %in% c(2024))
```

#Mapa publicaciones 2024
```{r}
library(mapview)
library(sf)
kml_layer <- st_read("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS/doc.kml")
mapview(kml_layer)
points <- st_as_sf(df_olmue24, coords = c("longitude", "latitude"), crs = 4326)
mapview(kml_layer) + mapview(points)
```


########################################################################################################################################

#Filtrar propiedades publicadas en 2023-2024 que siguen en venta
```{r}
df_olmue_act <- subset(df_olmue, año_publicacion %in% c(2023, 2024) & active == "True")
```

#Mapa publicaciones 2023-2024 que siguen activas y puntos rojos no corresponden a loteos
```{r}
library(sf)
library(mapview)

# Leer el archivo KML
kml_layer <- st_read("C:/Users/franc/OneDrive - Universidad de Chile/Escritorio/Lares/Patricio/TODAS LAS COMUNAS/doc.kml")

# Convertir df_olmue24 en un objeto sf
points <- st_as_sf(df_olmue_act, coords = c("longitude", "latitude"), crs = 4326)

# Visualizar el mapa con puntos coloreados por la columna contains_lot
mapview(kml_layer, color = "grey") + 
  mapview(points, zcol = "contains_lot", 
          col.regions = c("red", "blue"), 
          legend = TRUE)
```



























