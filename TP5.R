#Les jointures
#Exercice 1 - Importer les données
#Placer tous les fichiers dans le même dossier nommée nba puis changer le répertoire courant par défaut de votre session RStudio avec la fonction setwd() pour pointer sur ce dossier.
#La commande setwd()
setwd("C:/Users/Nicolas/Desktop/COURS BUT/R/TP5/nba")
getwd()

#La commande list.files()
#Lister tous les fichiers du dossier nba.
fichiers <- list.files(path = getwd(),
                       pattern = ".csv$",
                       full.names = TRUE)

#Les commandes basename() et file_path_sans_ext()
#Tester la fonction basename() puis file_path_sans_ext() sur le premier fichier de la liste des fichiers afin de retrouver uniquement le nom du fichier sans l'extension .csv.
library(tools)
print(fichiers[1]) #position du fichier dans le dossier
nom_fichier = basename(path = fichiers[1])
nom_fichier_sans_extension = file_path_sans_ext(x = nom_fichier)
print(nom_fichier_sans_extension)

#La commande assign()
#Tester la fonction assign() pour importer le jeu de données avec la fonction read.csv(). Qu'est ce qu'on observe dans la fenêtre Environment ?
# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
assign(x = nom_fichier_sans_extension, 
       value = read.csv(fichiers[1],
                        sep = ",",
                        dec = "."))
#un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.

#Utiliser ce même procéder dans une boucle for pour importer toutes la liste de fichiers.
# Boucle pour lire chaque fichier CSV
for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet = file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time = Sys.time()
  assign(nom_objet, read.csv(fichier, 
                             sep = ",",
                             dec = "."))
  end_time = Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}


#Exercice 2 : Les jointures
#Combien de match se sont dérouler à Los Angeles depuis la création de la NBA ?
df_x1 = subset(team, city == "Los Angeles", select = c("id","city"))
df_y2 = subset(game, select = c("game_id","team_id_home"))
dfJoin1 = merge(x = df_x1, y = df_y2,
               by.x = "id",
               by.y = "team_id_home",
               all.x = TRUE)
#by.x et by.y quand on a pas de clefs identique -> Left join 
nrow(dfJoin1)
View(dfJoin1)

#Quelle est l'affluence moyenne de spectacteur durant ces matchs joués à Los Angeles.
df_x2 = dfJoin1
df_y2 = subset(game_info, select = c("game_id", "attendance"))
dfJoin2 = merge(x = df_x2, y = df_y2, 
               by = "game_id",
               all.x = TRUE) #Montre toutes les colonnes de la table x
mean(dfJoin2$attendance, na.rm = TRUE)
View(dfJoin2)

#Combien d'arbitres différents ont officié durant la saison 2020.
df_x3 = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin3 = merge(x = df_x3, y = officials, 
               by = "game_id", # by quand on a une clef de jointure identique 
               all.x = TRUE)
length(unique(dfJoin3$official_id)) #unique enlève les doublons 
View(dfJoin3)

#Combien de matchs à officié Dick Bavetta par saison ?
df_x4 = subset(game_summary,
              select = c("game_id", "season"))
df_y4 = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfJoin4 = merge(x = df_x, y = df_y, 
               by = "game_id",  # by quand on a une clef de jointure identique 
               all.y = TRUE)
View(dfJoin4)
table(dfJoin4$season)

