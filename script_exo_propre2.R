setwd("C:/Users/travail/Desktop/datactivist/")

library(readxl)
library(rgdal)
library(rgeos)
library(rjson)
library(RCurl)
library(tmap)
library(stringdist)


# contours pays et subregions
load("pays.rda")
# subregions = gUnaryUnion(pays, id=pays$subregion)
# save(subregions, file="subregions.rda")
load("subregions.rda")

# fichier à géocoder
path_to_f = "DATA/AboFreemiumBrut.xls"
iSheet = 2
# index des colonnes avec les villes et les pays dans le fichier à géocoder
iCity = 1
iCountry = 4

# fichier de correspondance pays-code  iso 3166
path_to_f_iso = "iso3166paysmonde.xls"

# titre de la carte
mapTitle = "nombre d'abonnés"

makeMaps = function(path_to_f, iSheet, path_to_f_iso, iCity, iCountry, mapTitle) {
  
f = read_excel(path_to_f, sheet=iSheet)
names(f) = tolower(gsub(" ","_",names(f)))
  
# ISO
cat("-->  ASSOCIATION NOM DE PAYS <-> ISO\n")
  
f_iso = read_excel(path_to_f_iso, sheet=1)
names(f_iso) = c("id","pays","char2","char3","nombre")

isoCodes = list()
for (country in unique(f[, iCountry][[1]])) {
  
  v = stringdist(country, f_iso$pays, method="lv")
  isoCodes[[country]] = list(f_iso$pays[which.min(v)],
                             f_iso$char2[which.min(v)])
  
} 

cat("Voici les associations pays-iso qui ont été trouvées.\n")
cat("Si elles ne correspondent pas, veuillez modifier le nom des pays dans le fichier", path_to_f_iso, "\n")
print(data.frame(assoc_nom = sapply(isoCodes, function(x) x[[1]]), 
            code_iso = sapply(isoCodes, function(x) x[[2]])
            ))

f$iso_a2 = sapply(f[, iCountry][[1]], function(x) isoCodes[[x]][[2]])


# DATA > PAYS
cat("--> PREPARATION DONNEES PAYS\n")

f$foo = 1
f_agg = aggregate(f$foo, by=list(iso_a2 = f$iso_a2),FUN="sum")
names(f_agg)[2] = "nb"
m = match(pays$iso_a2, f_agg$iso_a2)

pays$nb = f_agg$nb[m]
pays$nb[is.na(pays$nb)] = 0


# DATA > subregions
cat("--> PREPARATION DONNEES SOUS REGIONS\n")

subregions_nb = aggregate(pays@data$nb,
                        by=list(subregion=pays$subregion),
                        FUN="sum")
names(subregions_nb)[2] = "nb"

m = match(row.names(subregions), subregions_nb$subregion)

subregions.df = SpatialPolygonsDataFrame(subregions,
                                        data=data.frame(subregions_nb[m, ],
                                                        row.names=row.names(subregions)))

# DATA > VILLES
cat("--> PREPARATION DONNEES VILLES\n")

f$x = NA
f$y = NA
f$geonameId = NA

for (i in 1:nrow(f)) {
    
  ville = gsub("^(.*) [1-9]*$", "\\1", f[, iCity][[1]][i])
  iso_pays = f$iso_a2[i]
  
  url = URLencode(paste0("http://api.geonames.org/searchJSON?q=",ville,"&maxRows=1&lang=fr&username=mathieu_r&style=short&orderby=relevance&country=",iso_pays))
    
  json = fromJSON(getURL(url))
  
  if (length(json$geonames) == 0) {
    
    url = URLencode(paste0("http://api.geonames.org/searchJSON?q=",ville,"&maxRows=1&lang=fr&username=mathieu_r&style=short&orderby=relevance"))
    json = fromJSON(getURL(url))
    
    if (length(json$geonames) == 0) {
      ok = FALSE
    } else {
      ok =  TRUE
    }
    
  } else {
    ok = TRUE
  }
  
  if (ok == TRUE) {
    
    f$x[i] = as.numeric(json$geonames[[1]]$lng)
    f$y[i] = as.numeric(json$geonames[[1]]$lat)
    f$geonameId[i] = json$geonames[[1]]$geonameId
    
  } 
  
  out[[i]] = df
  
}

cat("Pas de coordonnées trouvées dans geonames pour : ", 
    paste(f[which(is.na(f$geonameId)),iCity][[1]], collapse=","),"\n")
cat("Veuillez remplacer le nom de ces villes par des noms valides dans le fichier source.\n")

w = which(!is.na(f$x))

f2 = f[w, ]

villes = aggregate(f2$foo, by=list(geonameId = f2$geonameId), FUN="sum")
names(villes)[2] = "nb"
m = match(villes$geonameId, f2$geonameId)

villes$city = f2[m, iCity][[1]]
villes$country = f2[m, iCountry][[1]]
villes$x = f2$x[m]
villes$y = f2$y[m]

coordinates(villes) =~ x+y

# CARTES
cat("-->  CREATION DES CARTES\n")

entites = list("pays" = pays, 
               "subregions" = subregions.df,
               "villes" = villes)

for (i in 1:length(entites)) {
  
  entite = entites[[i]]
  libelle_entite = names(entites)[i]
  
  cat("carte des", libelle_entite, "en cours de création\n")

 if(libelle_entite %in% c("pays","subregions"))  {
  
    tm = tm_shape(entite) + tm_polygons() +
      tm_shape(entite) +
      tm_bubbles(size="nb", 
                 col=rgb(1,0,0),
                 alpha=0.5,
                 border.col = "black", 
                 border.lwd=1, 
                 size.lim = c(min(entite@data$nb), max(entite@data$nb)),
                 # sizes.legend = c(1e6, 2e6, 4e6, 6e6, 10e6), 
                 title.size=mapTitle)
 } else {

}
    save_tmap(tm, paste0("carte_",libelle_entite,".png"),width=1000,height=600) 
    
}
}

makeMaps(path_to_f, iSheet, path_to_f_iso, iCity, iCountry, mapTitle)
