#Import du jeu de données FAO
df = df<-read.csv("C:/Users/Nicolas/Desktop/COURS BUT/R/TP2/fao.csv", sep=";", dec=",", header = TRUE)
View(df)

#Combien de pays sont présents dans ce dataset ?
nrow(df)

#Affichez un résumé des données avec la fonction adaptée.
summary(df)

#Exercice 2 - Statistiques descriptives
#Quelle est la disponibilité alimentaire moyenne mondiale en Kcal/personne/jour ?
mean(df$Dispo_alim, na.rm = TRUE)

#Quel est le nombre d’habitant dans le monde 
mean(df$Population, na.rm = TRUE)

#Quel est l’écart-type du volume des exportations de viande ? Et des importations de viande ?
sd(df$Export_viande, na.rm = TRUE)
sd(df$Import_viande, na.rm = TRUE)

#Quelle est la médiane du volume de production de viande ?
median(df$Prod_viande, na.rm = TRUE)

#Calculez les quartiles du nombre de Kcal de disponibilité alimentaire.
quantile(df$Dispo_alim)

#Calculez les centiles du volume d’importation de viande.
quantile(df$Import_viande, seq(0,1, 0.01 )) # vecteur de 0 a 100% avec un pas de 1%

#Exercice 3 - Tris et filtres
#Construire une requête pour extraire les lignes du dataset avec les 5 pays les moins peuplés.
rang = order(df$Population)
resultat = head(df [rang , ], n = 5)
View(resultat)

#Construire une requête pour extraire les lignes du dataset avec les 5 pays les plus peuplés.
rang = order(df$Population, decreasing = TRUE)
resultat = head(df [rang , ], n = 5)
View(resultat)

#Construire une requête pour extraire les lignes du dataset avec les 5 pays qui produisent le plus de viande.
rang = order (df$Prod_viande, decreasing = TRUE)
resultat = head(df [rang , ], n= 5)
View(resultat)

#Construire une requête pour extraire les lignes du dataset avec les 5 pays qui importent le plus de viande.
rang = order(df$Import_viande , decreasing = TRUE)
resultat = head(df [rang , ], n = 5)
View(resultat)

#Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire supérieure ou égale à 2300 kcal. 
resultat = subset(df , Dispo_alim >= 2300 )
View(resultat) #163 pays

#Construire une requête pour extraire les lignes du dataset avec les pays qui ont une disponibilité alimentaire strictement supérieure à 3500 kcal et qui importe un volume de viande supérieure ou égale à 1 000 000 tonnes par an.
resultat = subset(df , Dispo_alim > 3500 & Import_viande > 1000)
View(resultat) # 5 pays

#Construire une requête pour extraire les lignes du dataset avec la France et la Belgique.
resultat = subset(df , Nom %in%  c("France","Belgique"))
View(resultat)

#Exercice 4 - Modifier le dataframe
#Ajouter une colonne nommée part_export qui correspond à la part des exportations de viande par rapport à la production de viande.
df$part_export = df$Export_viande/df$Prod_viande

#Ajouter une colonne nommée dispo_alim_pays qui correspond à la disponibilité total du pays en Kcal/jour.
df$Dispo_alim_pays = df$Dispo_alim*df$Population

#Exporter le nouveau dataframe dans un fichier csv nommé ExportTp2.csv avec la fonction write.table().
write.table(x = df, file = "ExportTp2.csv")

#Calculer la somme de la disponibilité alimentaire mondiale.
dispo_alim_mondiale = sum(df$Dispo_alim_pays , na.rm = TRUE)
dispo_alim_mondiale

#Combien d’adulte pourrait-on nourrir avec la disponibilité alimentaire mondiale avec plus de 2300kcal  ?
dispo_alim_mondiale/2300  #9858300378 


#Exercice 5 - Corrélation
#Représenter graphiquement dans un nuage de points le lien entre Prod_viande et Export_viande. Commenter le lien entre ces deux variables ?
plot(x = df$Prod_viande, 
     y = df$Export_viande, main = "Pays : Rapport Prod viande / Export viande")
#Elles ont un lien très proche l'une de l'autre. L'une impacte fortement l'autre

#Calculer le coefficient de corrélation de cette relation avec la fonction cor().
cor(x = df$Prod_viande , y = df$Export_viande )

#Construire la matrice des corrélations des variables quantitatives avec la fonction cor(). Afficher cette matrice dans une vue et arrondisser les valeurs avec deux décimales uniquements.
matriceCor = cor(df[ , - 1] , use = "complete.obs") #Création d'un tableau de corrélation en supprimant la colonne 1 (Caractere)
matriceCor = round(matriceCor , 2) #Arrondie a deux decimales
View(matriceCor)

#Installation package corrplot
#commande à executer qu'une seule fois
install.packages("corrplot")

#Construire une Corrélogramme avec la fonction corrplot()
library(corrplot)
corrplot(matriceCor, method = "circle")
