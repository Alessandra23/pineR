## Create a tibble with the sites and coordinates
library(tidyverse)
library(sf)

df <- tibble(
  sites = c(
    "Hortland", "Ballinagee","Oakwood", "Glendine", "Lackenrea 1", "Lackenrea 2", "Summerhill","Deerpark",
    "Ballymacshaneboy","Kilduff","Rossnagad", "Ballyroan 1", "Ballyroan 2","Ballybrittas","Emo","Donadea",
    "Glendalough", "Glendine trial 1", "Glendine trial 2", "Knockeen","The Rodneys","Killnaconnigan",
    "Cloondara","Knockaville","Killurney","Doon", "Clonoghil","Tigroney", "Gurtnapisha","Rickardstown",
    "Longfordpass", "Cashelduff","Woodford", "Corracloon","Corrakyle", "Annalecka"
  ),
  Easting = c(
    276187,320402,303812, 224303, 213024,213024,283758,326609,160106,244952,243350,248305,248305, 255415,
    253231, 283467,313063,224303, 224303,257434,136307,266278, 207204,250306, 227148,211437,225136, 321000,
    230028,253640,224426, 152830,170183,159162,162844, 305109
  ),
  Northing = c(
    237482, 214375,201215,199898, 101205,101205,246294,211019, 118416,233062,203335,199495,199495,236791,
    205382,232596,195655,199898,199898,106876,111288,255198,275580,248333,129158,231856,192099,182996,
    132387,263408,160680,298829,197082,191153,190777, 201042
  )
)




#> Linking to GEOS 3.6.1, GDAL 2.2.3, proj.4 4.9.3
latLongSites <- df %>%
  st_as_sf(coords = c("Easting", "Northing"), crs = 29902) %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as.data.frame() %>%
  bind_cols(sites = df$sites) %>%
  rename(c(Longitude = X, Latitude = Y))

save(latLongSites,file="Data/latLongSites.Rda")

save(df,file="Data/xySites.Rda")




