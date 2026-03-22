#Fonctions / Boucles / Algo 
#La commande function ()

# Créer une fonction salaire_net_cadre() qui prend en entrée le salaire mensuel brut d'un cadre et qui retourne le salaire net avant impôt. Ne pas tenir compte du temps de travail. Tester la fonction pour vérifier.
salaire_net_cadre = function(salaire_brut) {
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }
#Test 
salaire_net_cadre(salaire_brut = 3000)

#Modifier la fonction pour que ce salaire mensuel brut avant impôt soit de 2500€ si l'utilisateur ne renseigne pas ce paramètre.
salaire_net_cadre = function(salaire_brut = 2500) { #Ici on met le parametre par defaut 2500
  salaire_net_avant_impot = salaire_brut * 0.75
  return(salaire_net_avant_impot) }
#Test 
salaire_net_cadre()

#Modifier la fonction salaire_net_cadre() en ajoutant le paramètre temps de travail avec comme valeur par défaut 100%. Ne pas tenir compte du taux de prélèvement à la source. Tester la fonction pour vérifier.
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
#Test 
salaire_net_cadre(salaire_brut = 3000,
                  temps_travail = 0.8)


#La commande if() {}
#Modifier la fonction salaire_net_cadre() pour que la fonction retourne une erreur si le salaire mensuel brut en entrée n'est pas une valeur numerique. 
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
#Test 
salaire_net_cadre(salaire_brut = "2000€")
salaire_net_cadre(salaire_brut = 2000)


#Modifier la fonction salaire_net_cadre() pour que la fonction retourne une erreur si le temps de travail n'est pas numérique et n'est pas compris entre 0 et 1. 
salaire_net_cadre = function(salaire_brut = 2500,temps_travail = 1) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  salaire_net_avant_impot = salaire_brut * 0.75 * temps_travail
  return(salaire_net_avant_impot) 
}
#Test de la fonction
salaire_net_cadre(salaire_brut = 2000, temps_travail = "100%")
salaire_net_cadre(salaire_brut = 2000, temps_travail = 0.8)
salaire_net_cadre(salaire_brut = 2000, temps_travail = 100)


#La commande else() {}
#Créer une nouvelle fonction salaire_net() identique à la précédente mais avec un paramètre en plus qui est le statut cadre/non cadre du salarié. La fonction retourne également une erreur si le statut n'est pas cadre ou non cadre. 
salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur :  le statut doit être cadre ou non cadre")
  }
  
  if (statut == "cadre") {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {  #Attention le else doit bien etre sur la meme ligne 
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  return(salaire_net_avant_impot) 
}
#Test 
salaire_net(salaire_brut = 2000, statut = "cadre")
salaire_net(salaire_brut = 2000, statut = "non cadre")
salaire_net(salaire_brut = 2000, statut = "technicien")



#La commande else if() {}
#Modifier la fonction précédente pour calculer le salaire net mensuel après prélèvement à la source. 
salaire_net = function(salaire_brut = 2500,temps_travail = 1, statut) {
  
  if (!is.numeric(salaire_brut)) {
    return("Erreur :  le salaire brut doit être une valeur numérique")
  }
  
  if (!is.numeric(temps_travail)) {
    return("Erreur :  le temps de travail doit doit être une valeur numérique")
  }
  
  if ( (temps_travail > 1) | (temps_travail < 0)) {
    return("Erreur :  le temps de travail doit être une valeur numérique entre 0 et 1")
  }
  
  if (!statut %in% c("cadre","non cadre")) {
    return("Erreur :  le statut doit être cadre ou non cadre")
  }
  
  if (statut == "cadre") {  #On determine le statut cadre, a 0,75. Si pas cadre, = 0,78
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.75
  } else {
    salaire_net_avant_impot = salaire_brut * temps_travail * 0.78
  }
  
  #Nouveau test logique avec la variable crée au dessus.
  if (salaire_net_avant_impot <= 1591) {
    salaire_net_apres_impot = salaire_net_avant_impot
  } else if (salaire_net_avant_impot <= 2006) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.029)
  } else if (salaire_net_avant_impot <= 3476) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.099)
  } else if (salaire_net_avant_impot <= 8557) {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.20)
  } else {
    salaire_net_apres_impot = salaire_net_avant_impot * (1 - 0.43)
  }
  
  return(salaire_net_apres_impot) 
}

salaire_net(salaire_brut = 5000, temps_travail = 1, "cadre")
salaire_net(salaire_brut = 2500, temps_travail = 1, "non cadre")


#réer une fonction shifumi() qui demande à l'utilsateur de saisir une valeur dans la console entre pierre , papier ou ciseaux. 
#La fonction simule également un de ces trois choix à l'aide de la fonction sample() puis retourne le résultat.
shifumi <- function() {
  # Demander à l'utilisateur de saisir une valeur
  choix_utilisateur = readline(prompt = "Choisissez entre pierre, papier ou ciseaux : ")  #readline pour saisie user
  
  # Vérifier si l'utilisateur a saisi une valeur valide
  if (choix_utilisateur %in% c("pierre", "papier", "ciseaux")) {
    # Simuler un choix aléatoire pour l'ordinateur
    choix_ordi = sample(c("pierre", "papier", "ciseaux"), 1)
    
    # Afficher les choix de l'utilisateur et de l'ordinateur
    cat("Votre choix :", choix_utilisateur, "\n")  #cat pour concat et print les elements fournis
    cat("Choix de l'ordinateur :", choix_ordi, "\n")
    
    # Retourner le résultat du jeu
    if (choix_utilisateur == choix_ordi) {
      return("Égalité !")
    } else if ((choix_utilisateur == "pierre" & choix_ordi == "ciseaux") |
               (choix_utilisateur == "papier" & choix_ordi == "pierre") |
               (choix_utilisateur == "ciseaux" & choix_ordi == "papier")) {
      return("Vous avez gagné !")
    } else {
      return("L'ordinateur a gagné !")
    }
  } else {
    return("Valeur invalide. Veuillez choisir entre pierre, papier ou ciseaux.")
  }
}

#Test 
shifumi()


#Création des boucles
#Boucle for ()
#Somme cummulée : Créer une boucle for() qui parcourt les éléments du vecteur c(1,2,3,4,5) un par un. À chaque itération de la boucle, ajouter l'élément en cours au résultat précédent et afficher le résultat
resultat = 0
for (element in c(1,2,3,4,5)) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
}

#Parcourir toutes les colonnes du dataframe iris à l'aide d'une boucle for(). Pour chaque itération afficher la type de la colonne.
for (colonne in colnames(iris)) {
  type_colonne = class(iris[ , colonne])
  print(paste("la colonne ", colonne, " est de type : ", type_colonne))
}


#Boucle while ()
#Somme cummulée : Créer une boucle while() qui calcule la somme cumulative des nombres entiers à partir de 1 jusqu'à ce que la somme dépasse 50, en affichant le résultat à chaque étape ainsi que la valeur actuelle de l'élément à laquelle la boucle s'est arrêtée.
element = 1
resultat = 0
while (resultat <= 50) {
  resultat = resultat +  element
  print(paste("le resultat est : ",resultat))
  print(paste("le programme s'est arrêté à la valeur : ", element))
  element = element + 1
}

# #Parcourir toutes les colonnes du dataframe iris à l'aide d'une boucle while (). Pour chaque itération afficher la type de la colonne.
# Initialisation de l'indice de colonne
indice_colonne <- 1

# Tant qu'il reste des colonnes à parcourir dans iris
while (indice_colonne <= ncol(iris)) {
  # Récupération du nom de la colonne
  nom_colonne = colnames(iris)[indice_colonne]
  
  # Récupération du type de données de la colonne
  type_colonne = class(iris[, nom_colonne])
  
  # Affichage du résultat
  print(paste("la colonne ", nom_colonne, " est de type : ", type_colonne))
  
  # Passage à la colonne suivante
  indice_colonne <- indice_colonne + 1
}






