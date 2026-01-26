# EXO 1 
a <- 10
b <- 5

resultat <- a * b
print(resultat)

A <- 7.2 
B <- 10.1
# en R la majuscule/minuscule est important. 

# On peut voir que la variable resultat change
resultat <- A + B

rm(a,b,B,A,resultat)

# Exo 2 
vecteur = c(1,2,3,4,5)
vecteur [3]

v1 = 1:5

v2 = v1 + 3 

v3 = 1:6

v4 = v3^2

v5 = v4/2 

vecteur_semaine = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
class(vecteur_semaine)
vecteur_semaine[c(2,7)]

vecteur_bool = c(TRUE,FALSE,TRUE,FALSE)
class(vecteur_bool)

vecteur_decimaux = c(1.2, 3.4, 5.6, 7.8, 9.10)
class(vecteur_decimaux)
# On décide ne pas imprimer le 3 
print(vecteur_decimaux[-3])

#affichage des 3 premiers élements de la liste
vecteur_mois = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
class(vecteur_mois)
vecteur_mois[c(1,2,3)]

#Affichage de la place dans la liste. Ici le dernier et le premier
vecteur_negatif = c(-1,-6,-8,-15)
class(vecteur_negatif)
vecteur_negatif[c(4,1)]

#Affichage sauf les positions demandés
vecteur_fruit = c("Banane","Orange","Pomme","Poire")
class(vecteur_fruit)
vecteur_fruit[c(-1,-2)]    

vecteur_manquante = c(1,2,NA,3,4)
class(vecteur_manquante)

#Les fonctions 
sequence1 = seq(from = 1, to = 10 )
length(sequence1)

sequence_paire = seq(from = 2, to =20, by =2)
length(sequence_paire)

sequence_decroissante = seq(from= 0, to = -5)
length(sequence_decroissante)

sequence_nombre = seq(from = 5, to= 50, by = 5)
length(sequence_nombre)

sequence_pas = seq(from =0, to = 1, by = 0.1)
length(sequence_pas)

sequence_neg = seq(from = 5, to = -5, by = -1)
length(sequence_neg)

sequence10 = seq(from= 1, to=10, by =3)
length(sequence10)

#Les fonctions c(), rep()
replique = rep(3, times= 5 )

replique2 = rep(c("A","B","C"), times = 3)
