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
#En utilisant la méthode de coupure du plus grand saut on séparre deux clusters

#Obtention de nos classes
classes_CAH <- cutree(h_ward, k=2)

#Interprétation des résultats

############## K-means #########

