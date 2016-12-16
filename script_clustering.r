#Ne pas oublier de set le working directory avant de run le script
setwd('/home/samy/workspaces/projets_mlds/ClusteringElectricite/')
#Lecture des données du fichier electricite.txt
electricite=read.table('electricite.txt')

#Comme les données sont bien lisses et normalisées on passe directement aux algorithmes de classification

########### CAH ###########
#Calcul de la matrices de distances (Distances euclidiennes)
matDistEuc = dist(electricite)
#Ce calcule est relativement long, normal au vu de la dimension des données

#Execution de l'algorithme utlisant la critère d'aggregation de Ward
h_ward = hclust(matDistEuc, method="ward.D2")

#Visualisation du dendrogramme
plot(h_ward)
#En utilisant la méthode de coupure du plus grand saut, on séparre deux clusters

#Obtention de nos classes
classes_CAH <- cutree(h_ward, k=2)

#Interprétation des résultats

##Création d'une matrice de centres de classes 
centers_CAH_2 = matrix(data = 0, nrow = 2 , ncol = 168) #Matrice contenant les deux centres des deux clusters
nb_c1 = 0 #Nombre d'éléments du cluster 1
nb_c2 = 0 #Nombre d'éléments du cluster 2
#Boucle pour sommer les valeures de toute les variables pour chacune des classes
for (i in 1:2914) {
  if (classes_CAH[i] == 1) {
    centers_CAH_2[1,]= centers_CAH_2[1,] + as.matrix(electricite)[i,]
    nb_c1 = nb_c1 + 1
  }
  else {
    centers_CAH_2[2,]= centers_CAH_2[2,] + as.matrix(electricite)[i,]
    nb_c2 = nb_c2 + 1
  }
}
#Les centres calculés sont les moyennes algébirques des élémentes les composant
centers_CAH_2[1,] = centers_CAH_2[1,] / nb_c1
centers_CAH_2[2,] = centers_CAH_2[2,] / nb_c2

#Dessin des courbes pour les deux centres
plot(x = 1:168, y = centers_CAH_2[2,] ,type = 'l' , col = 2)
lines(x = 1:168, y = centers_CAH_2[1,] ,type = 'l' , col = 1)

############## K-means #########
#N'ayant pas de critère à priori pour le nombre de clusters, on test de k dans [2,5]
# K = 2
KM_2 = kmeans(electricite,2)
plot(x = 1:168, y=as.matrix(KM_2$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_2$centers)[2,],type = 'l',col = 2)
KM_2$tot.withinss
# K = 3 
KM_3 = kmeans(electricite,3)
plot(x = 1:168, y=as.matrix(KM_3$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_3$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_3$centers)[3,],type = 'l',col = 3)
KM_3$tot.withinss
# K = 4
KM_4 = kmeans(electricite,4)
plot(x = 1:168, y=as.matrix(KM_4$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_4$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_4$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_4$centers)[2,],type = 'l',col = 2)
KM_4$tot.withinss
# K = 5
KM_5 = kmeans(electricite,5)
plot(x = 1:168, y=as.matrix(KM_5$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_5$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_5$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_5$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_5$centers)[5,],type = 'l',col = 5)
KM_5$tot.withinss

############### SOM ###############
#On va prendre une grille de 10x10
library(class)
library(MASS)
library(kohonen) #Besoin des deux autres librairies pour utiliser celle-ci
SOM_1010 = som(as.matrix(electricite),grid = somgrid(10,10,'rectangular'))
plot(SOM_1010,type = "quality")
plot(SOM_1010, type = "dist.neighbours")

############## Jours ouvrables uniquement ################
#Suffit d'enlever les dernières 48 heures représetant le samedi 06/11/2010 et le dimanche 07/11/2010
electricite_JO = electricite[,1:120] 

############# CAH Jours Ouvrables ##############
#Calcul de la matrices de distances (Distances euclidiennes)
matDistEuc_JO = dist(electricite_JO)
#Ce calcule est relativement long, normal au vu de la dimension des données

#Execution de l'algorithme utlisant la critère d'aggregation de Ward
h_ward_JO = hclust(matDistEuc_JO, method="ward.D2")

#Visualisation du dendrogramme
plot(h_ward_JO)
#Ici, en utilisaint la coupure au plus grand saut, on peut hésiter entre 2 et 3

#Pour deux classes
#Obtention de nos classes
classes_CAH2_JO <- cutree(h_ward, k=2)

##Création d'une matrice de centres de classes 
centers_CAH_2_JO = matrix(data = 0, nrow = 2 , ncol = 168) #Matrice contenant les deux centres des deux clusters
nb_c1_JO = 0 #Nombre d'éléments du cluster 1
nb_c2_JO = 0 #Nombre d'éléments du cluster 2
#Boucle pour sommer les valeures de toute les variables pour chacune des classes
for (i in 1:2914) {
  if (classes_CAH2_JO[i] == 1) {
    centers_CAH_2_JO[1,]= centers_CAH_2_JO[1,] + as.matrix(electricite_JO)[i,]
    nb_c1_JO = nb_c1_JO + 1
  }
  else {
    centers_CAH_2_JO[2,]= centers_CAH_2_JO[2,] + as.matrix(electricite_JO)[i,]
    nb_c2_JO = nb_c2_JO + 1
  }
}
#Les centres calculés sont les moyennes algébirques des élémentes les composant
centers_CAH_2_JO[1,] = centers_CAH_2_JO[1,] / nb_c1_JO
centers_CAH_2_JO[2,] = centers_CAH_2_JO[2,] / nb_c2_JO

#Dessin des courbes pour les deux centres
plot(x = 1:168, y = centers_CAH_2_JO[2,] ,type = 'l' , col = 2)
lines(x = 1:168, y = centers_CAH_2_JO[1,] ,type = 'l' , col = 1)

#Pour 3 clusters
classes_CAH3_JO <- cutree(h_ward, k=2)

##Création d'une matrice de centres de classes 
centers_CAH_3_JO = matrix(data = 0, nrow = 3 , ncol = 168) #Matrice contenant les deux centres des deux clusters
nb3_c1_JO = 0 #Nombre d'éléments du cluster 1
nb3_c2_JO = 0 #Nombre d'éléments du cluster 2
nb3_c3_JO = 0 #Nombre d'éléments du cluster 3
#Boucle pour sommer les valeures de toute les variables pour chacune des classes
for (i in 1:2914) {
  if (classes_CAH3_JO[i] == 1) {
    centers_CAH_3_JO[1,]= centers_CAH_3_JO[1,] + as.matrix(electricite_JO)[i,]
    nb3_c1_JO = nb3_c1_JO + 1
  }
  else if(classes_CAH3_JO[i] == 2){
    centers_CAH_3_JO[2,]= centers_CAH_3_JO[2,] + as.matrix(electricite_JO)[i,]
    nb3_c2_JO = nb3_c2_JO + 1
  }
  else {
    centers_CAH_3_JO[3,]= centers_CAH_3_JO[3,] + as.matrix(electricite_JO)[i,]
    nb3_c3_JO = nb3_c3_JO + 1
  }
}
#Les centres calculés sont les moyennes algébirques des élémentes les composant
centers_CAH_3_JO[1,] = centers_CAH_3_JO[1,] / nb3_c1_JO
centers_CAH_3_JO[2,] = centers_CAH_3_JO[2,] / nb3_c2_JO
centers_CAH_3_JO[3,] = centers_CAH_3_JO[3,] / nb3_c3_JO

#Dessin des courbes pour les trois centres
plot(x = 1:168, y = centers_CAH_2_JO[2,] ,type = 'l' , col = 2)
lines(x = 1:168, y = centers_CAH_2_JO[1,] ,type = 'l' , col = 1)
lines(x = 1:168, y = centers_CAH_2_JO[3,] ,type = 'l' , col = 3)


############ K-means Jours Ouvrables ################
#N'ayant pas de critère à priori pour le nombre de clusters, on test de k dans [2,5]
# K = 2
KM_2_JO = kmeans(electricite_JO,2)
plot(x = 1:168, y=as.matrix(KM_2_JO$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_2_JO$centers)[2,],type = 'l',col = 2)
KM_2_JO$tot.withinss
# K = 3 
KM_3_JO = kmeans(electricite_JO,3)
plot(x = 1:168, y=as.matrix(KM_3_JO$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_3_JO$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_3_JO$centers)[3,],type = 'l',col = 3)
KM_3_JO$tot.withinss
# K = 4
KM_4_JO = kmeans(electricite_JO,4)
plot(x = 1:168, y=as.matrix(KM_4_JO$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_4_JO$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_4_JO$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_4_JO$centers)[2,],type = 'l',col = 2)
KM_4_JO$tot.withinss
# K = 5
KM_5_JO = kmeans(electricite_JO,5)
plot(x = 1:168, y=as.matrix(KM_5_JO$centers)[3,],type = 'l',col = 3)
lines(x=1:168, y =as.matrix(KM_5_JO$centers)[1,],type = 'l',col = 1)
lines(x=1:168, y =as.matrix(KM_5_JO$centers)[4,],type = 'l',col = 4)
lines(x=1:168, y =as.matrix(KM_5_JO$centers)[2,],type = 'l',col = 2)
lines(x=1:168, y =as.matrix(KM_5_JO$centers)[5,],type = 'l',col = 5)
KM_5_JO$tot.withinss

############### SOM Jours Ouvrables #####################
SOM_1010_JO = som(as.matrix(electricite_JO),grid = somgrid(10,10,'rectangular'))
plot(SOM_1010_JO,type = "quality")
plot(SOM_1010_JO, type = "dist.neighbours")


