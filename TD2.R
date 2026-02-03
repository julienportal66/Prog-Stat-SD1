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
         type="upper", order="hclust", 
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