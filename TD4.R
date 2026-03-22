#Import des données : 
df = read.csv(file = "C:/Users/Nicolas/Desktop/COURS BUT/R/TD4/velov.csv",
              header = TRUE,
              sep = ";", 
              dec = "," )

#Résumé, on voit des variables sont mal typées
summary(df)
class(df$status)
class(df$CodePostal)

#Passer les deux variables en type factor
df$status = as.factor(df$status)
df$CodePostal = as.factor(df$CodePostal)


#On souhaite vérifier s'il y a des bornes indisponibles sur les stations. Il suffit de vérifier si le nombre de vélos et places disponibles est égal à la capacity de la station. Créer une colonne nommée bornes avec la valeur OKou KO s'il y a un ou plusieurs bornes indisponibles sur une station. Combien y a-t-il de stations avec au moins une bornes HS ?

df$bornes = ifelse(df$capacity != (df$bikes + df$stands), "KO" , "OK")
table(df$bornes)

#La fonction hist()
#Construire un histogramme de la distribution des capacity à l'aide de la fonction hist(). N'oublier pas de mettre un titre.
hist(x= df$capacity, main = "Distribution de \n capacité des stations", col = "blue")

#Construire le même graphique mais avec 6 classes
hist(x= df$capacity, main = "Distribution de \n capacité des stations",breaks = 6, col = "blue")

#Construire le même graphique mais en rouge.
hist(x= df$capacity, main = "Distribution de \n capacité des stations",breaks = 6, col = "red")

#Renommer l'axe des abscisses par Capacity.
hist(x= df$capacity, main = "Distribution de \n capacité des stations",breaks = 6, col = "red", xlab = "Capacity")


#La fonction abline()
#Sur le dernier graphique, ajouter une ligne horizontale bleue qui à pour ordonné la valeur 100, à l'aide de la fonction abline()
abline(h = 100 , col = "blue", lty = 2)

# Les fonctions hist(), lines() et density()

#Construire le même graphique mais avec la densité plutôt que les effectifs. Supprimer l'argument break pour rétablir les classes par défaut.
hist(x= df$capacity, main = "Distribution de \n capacité des stations",probability = TRUE, col = "red", xlab = "Capacity")

#Ajouter la courbe densité de cette distribution à l'aide des fonctions lines() et density(). On peut mettre cette courbe en bleu en changeant la taille de la courbe avec l'argument lwd.
lines(density(df$capacity), 
      lty = 2,
      col = "blue",
      lwd = 4)

#Pour voir la courbe density en entier, modifier les bornes de l'axe des ordonnées de l'histogramme avec l'argument ylim. Relancer l'ensemble des commandes pour tracer à nouveau le graphique.

hist(x= df$capacity, main = "Distribution de \n capacité des stations",probability = TRUE, col = "red", xlab = "Capacity", ylim = c(0,0.08))
lines(density(df$capacity), 
      lty = 2,
      col = "blue",
      lwd = 2)


#Le boxplot
#La fonction boxplot()
#Construire une boîte à moustache de la distribution des capacity à l'aide de la fonction boxplot(). N'oublier pas de mettre un titre.
boxplot(x = df$capacity,
        main = "Distribution de la capacité")

#Le meme graphique horizontalement 
boxplot(x = df$capacity,
        main = "Distribution de la capacité",
        horizontal = TRUE)

#Construire le même graphique en le remettant à la verticale et en n'affichant pas les valeurs atypiques.
boxplot(x = df$capacity,
        main = "Distribution de la capacité",
        outline = FALSE,
        horizontal = FALSE)

#Ajouter un point supplémentaire qui correspond à la moyenne de la série avec la fonction points(). On souhaite que ce point soit un gros carré rouge
moy = mean(df$capacity, na.rm = TRUE) #On définit la moyenne
points(moy, col = "red", pch = 15, cex = 2)


#La fonction par()
# On souhaite comparer les vélos disponibles sur le 7ème et le 8ème arrondissement .Diviser la fenêtre graphique en deux puis constuire un boxplot pour ces deux arrondissement. Que peut-on dire ?
par(mfrow=c(1,2))  #On fait une fenetre sur 1 et 2 colonnes
df7 = subset(df, CodePostal == "69007")
boxplot(x = df7$bikes, 
        main = "Boxplot nb vélos 69007",
        ylim = c(0,40))

df8 = subset(df, CodePostal == "69008")
boxplot(x = df8$bikes, 
        main = "Boxplot nb vélos 69008",
        ylim = c(0,40))

#Sur le même graphique, on souhaite analyser le nombre de vélos disponibles en fonction de la variable bonus
par(mfrow = c(1,1))
boxplot(formula = bikes ~ bonus,
        data = df, 
        main = "Dispo vélos vs Stations Bonus")


#Les fonctions points() et tapply().
#Ajouter les moyennes de chaque groupes sur le graphique à l'aide de la fonction tapply() et points().

means <- tapply(X = df$bikes, 
                INDEX = df$bonus, 
                FUN = function(X) mean(X)) #Calculer moyenne de chaque grps
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)


#Les DIAGRAMMES
#Les fonctions barplot() et table().
#Créer un diagramme en barre de la réparition du nombre de station bonus à l'aide de la fonction barplot().
effectif = table(df$bonus)
barplot(height = effectif,
        main = "Répartition du nb de station bonus")

#Meme graphique mais horizontal
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus",
        horiz = TRUE)

#Les fonctions barplot() ,prop.table() et legend().
frequence = prop.table(effectif)
barplot(height = frequence,
        main = "Répartition en % du nombre de station bonus",
        horiz = TRUE)

#Construire un diagramme bivariés avec la répartition du nombre de station bonus en fonction du nombre de station avec un terminal de paiement. Les deux variables ayant les mêmes modalités TRUE / FALSE, il est important de définir le nom de l'axe des abscisses. Que remarque t-on ?
effectif = table(df$banking, df$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")  #Attention n'ont pas de légendes

#Afficher une legend pour pouvoir distinguer les couleurs associées aux modalités avec vert pour TRUE et rouge pour FALSE. On peut vérifier si le graphique est cohérent en vérifiant avec l'objet frequence.
#Calcul des pourcentages
frequence = prop.table(x = effectif)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)

#Meme question pourcentage en colonne 
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)



#Même question mais avec un diagramme bivarié non empilé à l'aide de l'argument beside.
#Calcul des pourcentages colonnes
frequence = prop.table(x = effectif, margin = 2)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence)