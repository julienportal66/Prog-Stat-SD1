#Exercice sur les Fonctions en R
#Les fonctions read_excel() et as.factor().
#Importer le jeu de données pokemon.xlsx à l’aide du package readxl.
install.packages("readxl")
library(readxl)
pokemon = read_excel(path = "C:/Users/Nicolas/Desktop/COURS BUT/R/TD3/pokemon.xlsx",sheet = "pokemon")
View((pokemon))

#Combien de lignes, colonnes sont présentes dans ce dataset 
dim(pokemon)
ncol(pokemon)
nrow(pokemon)

#Affichez un résumé des données avec la fonction adaptée.
summary(pokemon) #Attention les données Char n'ont pas de stats

#Modif des variable quali en variable factor
pokemon$is_legendary = as.factor(pokemon$is_legendary)
pokemon$generation = as.factor(pokemon$generation)
pokemon$type = as.factor(pokemon$type)

#Nouvel affichage avec variable modif
summary(pokemon)



#Exercice 2 - Création de colonne
#Création d'un colonne avec ifelse
#Bien mettre nomdataframe$colonne quand on veut travailler dedans
med = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= med, "attack+","attack-")
pokemon$attack_group <-as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

#Créer une colonne water_fire avec la valeur yes si le type est water ou fire, sinon renseigner la valeur no.
pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"), "yes","no" )
pokemon$water_fire = as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

#Création de la colonne best
q3_attack = quantile(pokemon$attack, probs = 0.75)
q3_defense = quantile(pokemon$defense, probs = 0.75)
q3_speed = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(pokemon$attack > q3_attack &
                        pokemon$defense > q3_defense &
                        pokemon$speed > q3_speed , "yes","no")
pokemon$best = as.factor(pokemon$best)
summary(pokemon$best)

#La fonction is.na()
#Filtrer les données dans un objet nommé requete avec les pokemons ayant des valeurs manquantes sur la colonne weight_kg.
requete = subset(pokemon, is.na(weight_kg))
View(requete)

#Filtrer les données dans un objet nommé requete avec les pokemons n'ayant pas des valeurs manquantes sur la colonne weight_kg.
requete = subset(pokemon, !is.na(weight_kg))
View(requete)

#creer des nouvelles variables mais en remplacant les valeurs manquantes par leurs valeurs médianne
med_weight_kg = median(pokemon$weight_kg, na.rm = TRUE)
pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg) , 
                             med_weight_kg ,
                             pokemon$weight_kg)

med_height_m = median(pokemon$height_m, na.rm = TRUE)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m) , 
                            med_height_m ,
                            pokemon$height_m)

#La fonctions cut()
#Créer une nouvelle variable nommée weight_group en regroupant en 3 tranches avec les labels léger / moyen / lourd.
pokemon$weight_group = cut(pokemon$weight_kg, breaks = 3, labels = c("Leger", "moyen", "lourd"))

#Créer une nouvelle variable nommée height_m_group en regroupant en 4 tranches telles que : ]0,1] / ]1,2] / ]2,3] / ]3,max]
pokemon$height_m_group = cut(pokemon$height_m, breaks = c(0,1,2,3, 
                                                          max (pokemon$height_m, 
                                                               na.rm = TRUE)))
# Créer une nouvelle variable nommée defense_group en regroupant en 5 tranches avec les min, max et quartiles telle que : [min,Q1] / (Q1,Q2] / (Q2,Q3] / (Q3,max]
pokemon$defense_group = cut(pokemon$defense ,
                            breaks =  quantile(pokemon$defense, ra.rm=TRUE), 
                            include.lowest = TRUE)
summary(pokemon$defense_group)

#Exercice 3 - Agregation
#La fonction aggregate()
#Calculer la moyenne d'attack par type.
aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))

#Calculer la mediane d'attack par generation et type
aggregate(x = attack ~ generation + type,
          data = pokemon, 
           FUN = function(x) median(x))

#Calculer l'effectif par type.
aggregate(x = pokedex_number ~ type,
          data = pokemon,
          FUN =  function(x) length(x))
#Calculer la moyenne et la mediane de la statistique speed pour chaque generation et type. Afficher également les effectifs de chaque paire.
aggregate(x = speed ~ generation + type,
          data = pokemon,
          FUN = function(x) c(med = median(x),
                            moy = mean(x),
                            eff = length(x)))
