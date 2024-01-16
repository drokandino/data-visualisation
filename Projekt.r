library(rvest)
library(stringr)
library(sf)
library(raster)
library(maps)
library(ggplot2)
library(ggvoronoi)
library(rworldmap)
library(dplyr)
require(viridis)

url = "http://meteo.hr/naslovnica_aktpod.php?tab=aktpod,tablica"

#dohvacanje html dokumenta
page = read_html(url) 

#filtriranje tablica
tables = html_nodes(page, "table")

#citanje podataka iz 3. tablice (u njoj su podaci)
table = html_table(tables[3], trim = TRUE, convert = TRUE, dec = ".", fill = TRUE)

#pretvaranje u data frame
dfOriginal = as.data.frame(table)
#uklanjanje svih stupaca osim imena mjesta i tlaka
dfmod = dfOriginal[,-c(2,3,4,5,7,8)]
#uklanjanje aerodroma
dfmod = dfmod[!grepl("-aero", dfmod[[1]], fixed = TRUE),]
#uklanjanje svih mjesta na kojima tlak nije izmjeren
dfmod = dfmod[!grepl("-", dfmod[[2]], fixed = TRUE),]
#uklanjanje zvjezdice iz podataka o tlaku
dfmod[[2]] = str_remove(dfmod[[2]], "\\*")
#pretvaranje tlakova u numericke podatke
dfmod[[2]] = as.numeric(dfmod[[2]])


#ucitavanje koordinata
loc = read.csv('/Users/dino/Documents/Diplomski/Lokacije/Projekt/locations.csv', header = TRUE, encoding = "UTF-8")

LAT = c()
LNG = c()
REMOVEROWS = c()

#pridruzivanje koordinata mjestima u tablici mjerenja
for (i in 1:dim(dfmod)[1])
{
  idx = match(dfmod[i,1], loc[[1]])
  if (!is.na(idx))
  {
    LAT = c(LAT, loc[idx,2])
    LNG = c(LNG, loc[idx,3])
  }
  else
  {
    print(dfmod[i,1])
    REMOVEROWS = c(REMOVEROWS, i)
  }
}

#uklanjanje svih redova za koje nema odgovarajuca lokacija
if (!is.null(REMOVEROWS))
{
  dfmod2 = dfmod2[-REMOVEROWS,]
}
dfmod2 = cbind(dfmod, LAT, LNG)

#Interpilacija metodom najblizeg susjeda

#convert dataframe to simple collection
tlakSc =  st_as_sf(dfmod2, coords = c('LNG', 'LAT'), remove = F)

#Napravi prediction gird, tocke predikcije
nodes = st_make_grid(st_buffer(tlakSc$geometry, 0.1), cellsize = 0.1, what = "centers")
plot(nodes, axes = TRUE)
plot(tlakSc$geometry, add = TRUE, color="red")

#cast
nodes = as(nodes, "Spatial")
class(nodes)

#izracunaj udaljenost izmedu svake tocke mjerne i postje i tocke grida
#Stupci su mjerne postaje, retci su tocke predikcije
udaljenosti = pointDistance(nodes, as(tlakSc$geometry, "Spatial"), lonlat=TRUE)

#za svaku tocku predikcije pronadi najblizu mjernu postaju
#Pornade za svaki redak udaljenosti, index najblize postaje
najblize = apply(udaljenosti, 1, function(x) which(x == min(x)))

#Rename 
colnames(tlakSc)[2] = "Tlak"

#Dodaj vrijednost tlaka svakoj tocki predikcije
tlakovi = tlakSc$Tlak[najblize]

#Napravi kopiju tocki predikcije
predikcija = nodes
#Napravi novi stupac sa vrijednostima tlaka
predikcija$tlak = tlakovi

gridded(predikcija) = TRUE

predikcija = raster(predikcija)
plot(predikcija)

#create new map
newmap = getMap(resolution = "low")
plot(newmap, add=TRUE, xlim = c(13.48, 19.45), ylim = c(42.391, 46.555))

voronoi = ggplot(tlakSc, aes(LNG, LAT, fill = Tlak)) + geom_voronoi() + geom_point()

theme_set(
  theme_void()
)
cro <- map_data("world", region =  "Croatia")
region.lab.data = cro %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


finalPlot = voronoi + geom_path(data = cro, aes(x = long, y = lat, fill = group)) + theme_void() + theme(legend.position = "none")

plot(voronoi)
plot(finalPlot)