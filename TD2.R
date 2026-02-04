getwd()

#Changement de répertoire (fichier CSV)
setwd(dir = "C:/Users/Nicolas/Desktop/COURS BUT/R/TD2/Dataset")
getwd()

#Importer les nouveaux datasets
bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")

#Affichage de la dimension (ligne et colonnes)
dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)

#Exercice 2 
#Pour chaque dataset, effectuer un résumé des données avec la fonction adaptée.
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)

#Représenter graphiquement dans un nuage de points le lien entre les statistiques des drivers sur Weight et Acceleration
plot(x = drivers$Weight, y = drivers$Acceleration, main = "Drivers : Weight/Acceleration")

# Calculer le coefficient de corrélation (tjr entre -1 et 1)de cette relation avec la fonction cor().
cor(x = drivers$Weight, y = drivers$Acceleration)

#Vérifier ce résultat en calculant vous même le coéfficient de corrélation.
covXY = cov(x = drivers$Weight,
            y = drivers$Acceleration)
sX = sd(drivers$Weight)
sY = sd(drivers$Acceleration)
print(covXY / (sX*sY))

#Calculer le coefficient de détermination de cette même relation.
coefCorr = cor(x = drivers$Weight,
               y = drivers$Acceleration)
coefDeter = coefCorr^2
print(coefDeter)

#Construire la matrice des corrélations des variables quantitatives de statistiques des drivers avec la fonction cor().
matriceCor = cor(drivers [, -1])
matriceCor = round(matriceCor, 2)
View(matriceCor)

#Installation du package
install.packages("corrplot")

#Construire une Corrélogramme avec la fonction corrplot()
library(corrplot) #je charge mon package pour utiliser les fonctionnalités.
corrplot(matriceCor, method = "circle")

#Construire une Corrélogramme pour les 3 autres datasets
#Tires
matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=60, tl.cex=1, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

#Body_Kart
matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", #upper (partie haute), lower partie basse
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)

#Gliders
matriceCor = round(cor(gliders[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
)
#Attention on ne peut pas le trouvers car ecart type nul

#Exercice 3
#Créer un object resultat avec uniquement le nom du Driver et son Weight.
resultat = drivers [,c("Driver", "Weight")]
View(resultat)

#Créer un object resultat avec uniquement le nom du Driver et son Acceleration sur les 10 premières lignes.
resultat = drivers [1:10 ,c("Driver","Acceleration")]
View(resultat)

#Créer un object resultat sans les colonnes 5, 7 et 9.
resultat = drivers [,c(-5, -7, -9)]
View(resultat)

#Créer un object resultatsans les colonnes Weight et Acceleration.
resultat = drivers[ , -c("Weight","Acceleration")] #marche pas car uniquement pour index numérique (pas les textes)
resultat = drivers[ , -c(2,3)] # <- La solution

#Créer un object resultat avec uniquement les colonnes Driver, Acceleration et Weight dans cet ordre.
resultat = drivers [,c("Driver","Acceleration","Weight")]
View(resultat) # Colonnes conservé par l'odre que l'on définit

#Créer un object resultat avec uniquement les Driver 3 , 12 et 32 dans cet ordre.
resultat = drivers [c(3,12,32), "Driver" ]
View(resultat) 

#Créer un object resultat avec uniquement les Driver 32 , 3 , 12 dans cet ordre.
resultat = drivers [c(32,3,12), "Driver"]
View(resultat) #Ordre des lignes défini conservé

#Créer un object resultat avec uniquement les colonnes Driver et Weight en triant les conducteurs du plus léger au plus lourd avec la fonction order().
rang = order(drivers$Weight) #Tri fait sur le weight
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)                   

#Créer un object resultat avec uniquement les colonnes Driver et Acceleration en triant les conducteurs du plus rapide au moins rapide.
rang = order(drivers$Acceleration, decreasing = TRUE)# True pour mettre en Decroissant
resultat = drivers [rang , c("Driver","Acceleration")]
View(resultat)

#Créer un object resultat avec les colonnes Driver, Weight et Acceleration en triant les conducteurs du plus rapide au moins rapide puis du plus léger au plus lourd.
rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))#True pour "acceleration" et False pour "Weight"
resultat = drivers [rang, c("Driver", "Acceleration","Weight")]
View(resultat)

#Exercice 4 - GOAT
#Créer un object topDriver avec les colonnes Driver et Acceleration avec le ou les conducteurs avec la plus grande Acceleration.
help(subset)              #On choisi les meilleures acceleration 
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))
View(topDriver)

#Créer un object topGlider, topTires et topBody avec la même logique de conserver uniquement les meilleurs statistiques d'Acceleration.
topGlider = subset(x = gliders, subset = Acceleration == max(Acceleration), select = c("Glider", "Acceleration"))
View(topGlider)

topTires = subset(x = tires, subset = Acceleration == max(Acceleration), select = c("Tire","Acceleration"))
View(topTires)

topBody = subset(x = bodies_karts, subset = Acceleration ==max(Acceleration) , select = c("Body","Acceleration"))
View(topBody)
