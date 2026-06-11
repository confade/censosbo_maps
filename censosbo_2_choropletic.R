library(dplyr)
library(ggplot2)
library(classInt)
library(censosbo)

##
personas_nacional <- get_personas_2024(variables = "p26_edad") |>
  group_by(idep, iprov, imun) |>
  summarise(
    edad_prom = mean(p26_edad, na.rm = TRUE),
    conteo = n(),
    .groups = "drop"
  ) |>
  collect()

## Cortes naturales de Jenks
breaks_jenks <- classIntervals(
  (personas_nacional$edad_prom),
  n = 5,  # Número de clases
  style = "jenks")

## Quantile
breaks_quintiles <- classIntervals(
  personas_nacional$edad_prom,
  n = 5,
  style = "quantile")

##
personas_nacional <- personas_nacional %>%
  mutate(edad_prom_cat = cut(edad_prom, 
                             breaks = breaks_jenks$brks,
                             include.lowest = TRUE))

## para verificar la discretización
table(personas_nacional$edad_prom_cat)

##
m <- mapa_mun(personas_nacional, "edad_prom_cat", paleta = 'OrRd',
         titulo = "Edad promedio por municipio (CPV-2024)") +
theme(
  plot.title = element_text(
    size = 10,
    face = "bold"
    )
  )
m +
  geom_sf(data = geo_departamentos,
          fill = NA,
          color = "grey30",
          linewidth = 0.2)

## help
#?mapa_mun
