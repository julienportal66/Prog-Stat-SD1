iris
class(iris)

# Affichage du jeu de données
View(iris)

# Affiche les lignes de la fonction

nrow(iris)

#Afficher les colonnes 
ncol(iris)

# Afficher le nom des colonnes 
colnames(iris)

#Afficher le resumé du datframe
summary(iris)

# Afficher uniquement les colonnes Sepal.Length et Species.
iris[ ,c("Sepal.Length","Species")]

#Afficher uniquement la ligne 100,103 et 105.
iris [c(100,103,105),]

#Afficher uniquement les lignes 50 à 100.
iris [c(50 : 100),]

#Calculer la moyenne de la variable Sepal.Length.

mean(iris$Sepal.Length)

# Calculer la médiane de la variable Sepal.Width
median(iris$Sepal.Width)

#Calculer l'écart-type de la variable Petal.Length.
sd(iris$Sepal.Length)

# Calculer les déciles de la variable Petal.Width.
quantile(iris$Sepal.Width, probs = seq(from =0.1, to=0.9,by =0.1))

#Exercice 2 

#Import des fichiers CSV
#Attention : les "/" doivent etre dans ce sens. Et pour importer,
#On va copier le "chemin d'accès.
dfManga <- read.csv("C:/Users/Nicolas/Desktop/COURS BUT/R/manga.csv", header = TRUE, sep = ",", dec = ".")
class(dfManga)
dfAnime <- read.csv("C:/Users/Nicolas/Desktop/COURS BUT/R/anime.csv", header = TRUE, sep = ",", dec = ".")
class(dfAnime)

#Afficher les jeux de données dans des vues pour les visualiser
View(dfAnime)
View(dfManga)

#Afficher le nombre de lignes et colonnes avec la fonction dim()
dim(dfAnime)
dim(dfManga)

# Calculer la moyenne de la variable Score pour les deux dataframe. 
mean (dfAnime$Score)
mean (dfManga$Score)

#Calculer le nombre total de votes de la variable Vote pour les deux dataframe.
sum(dfAnime$Vote)
sum(dfManga$Vote)

#Calculer l'écart-type des notes de la variable Score pour les deux dataframe.
sd(dfAnime$Score)
sd(dfManga$Score)

#Calculer les déciles des notes de la variable Score pour les deux dataframe. 
quantile(dfAnime$Score, progs = seq(from =0.1, to=0.9,by =0.1))
quantile(dfManga$Score, progs = seq(from =0.1, to=0.9,by =0.1))

#Les fonctions subset(), table() et prop.table()
#Pour chaque extraction, créer un nouvel objet et calculer le nombre de ligne filtrées.
#Combien de Manga ont une note strictement supérieure à 9/10 ?
notesuppa9 = subset(dfManga, Score > 9 )
nrow(notesuppa9)

#Combien de Manga ont 200000 votes ou plus ?
votes200000 = subset(dfManga, Vote >= 200000 )
nrow(votes200000)

#Combien de Manga ont strictement plus de 200000 votes et plus de 8/10 ?
Extraction3 = subset(dfManga, Vote >= 200000 & Score >= 8 )
nrow((Extraction3))

#Combien de Manga ont une note comprise entre 7/10 et 8/10 
Extraction4 = subset(dfManga, Score >= 7 & Score <= 8)
nrow(Extraction4)

# Calculer les effectifs de la variable Rating()
effectifRating <- table(dfAnime$Rating) #Regroupe les differents élem dans des tables
print(effectifRating) 
length(effectifRating) #Taille de la nouvelle variable
prop.table(effectifRating) #Proportion -> pourcentage

#Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity
Extraction5 = subset(dfAnime, Rating== "R - 17+ (violence & profanity)")
nrow(Extraction5)

#Combien d'Anime sont concernés par le Rating : R - 17+ (violence & profanity) et ont une note supérieur à 8/10 ?
Extraction6 = subset(dfAnime, Rating== "R - 17+ (violence & profanity)" & Score > 8)
nrow(Extraction6)

# Combien d'Anime ne correspondent PAS au Rating : R - 17+ (violence & profanity) ?
Extraction7 <- subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
nrow(Extraction7)

#Combien d'Anime correspondent au Rating : PG - Children et G - All Ages ?
Extraction8 = subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
nrow(Extraction8)

#Combien d'Anime ne correspondent pas au Rating : PG - Children et G - All Ages 
Extraction9 = subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages"))
nrow(Extraction9)

#Combien d'Anime ont une note supérieure à 9/10 ou ont plus de 400000 votes
Extraction10 = subset(dfAnime, Score > 9 | Vote > 400000)
nrow(Extraction10)

#Dans ces questions, nous allons conserver uniquement certaines colonnes pour pouvoir fusionner les deux dataframe ensemble. Puis nous exporterons le résultat.
#Modifier les deux dataframe en ne conservant que les variables : Title,Score,Vote,Ranked.
dfAnime = dfAnime [, c("Title","Score","Vote","Ranked")]
dfManga = dfManga [, c("Title","Score","Vote","Ranked")]

# Pour chaque dataframe créer une colonne Type avec pour valeur Anime ou Manga selon l'objet.
dfAnime$Type = "Anime"
dfManga$Type = "Manga"

#Compiler les deux dataframe avec la fonction rbind() dans un objet appelé dfConcat. Vérifier le résultat avec dans une vue.
dfConcat = rbind(dfManga,dfAnime)
View(dfConcat)

#Exporter le dataframe dans un fichier csv nommée ExportTp1.csv avec la fonction write.table.
write.table(dfConcat, file = "ExportTp1.csv", sep = "," , row.names = FALSE)

