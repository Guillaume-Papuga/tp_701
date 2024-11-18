#####################################################
# HAV701B Ecologie : concepts, outils et applications 
#####################################################

############################################################################################
# Ateliers de terrain - Diversité des communautés floristiques du nord de Montpellier
# Enseignants : Guillaume Papuga & Bastien Mérigot
# TP novembre 2021
############################################################################################

#####
# 1. Préparation des données
#####

# A. Packages
require(ade4)
require(BiodiversityR)
require(dplyr)
require(here)
require(ggplot2)
require(vegan)


install.packages("stringi")
install.packages("htmlTable")
install.packages("Hmisc")
install.packages("RcmdrMisc")
install.packages("BiodiversityR")


# B. Charger les donnees de communautés floristiques
data_flore = read.csv(here::here("data", "com_2024.csv"), # upload data
                   sep = ";", header = T, dec = ",")


#	C. Vérification des données chargées
edit(data_flore) # si besoin de modifications manuelles : data_flore = edit(data_flore)
### refermer la fenetre avant de pouvoir utiliser de nouveau la console

dim(data_flore) # nombre de lignes et colonnes

names(data_flore) # noms des colonnes

attach(data_flore) # attache les variables

head(data_flore) # entête

str(data_flore) # type de varibles

summary(data_flore) # résumé des données

table(versant) # crée une table par zone et/ou autre modalités qualitatives

# D. Tableau simplifié avec uniquement les especes
data_especes = data_flore[,11:ncol(data_flore)]

dim(data_especes)

# Creer deux dataset séparés pour chaque versant avec la fonction subset()
sud = subset(data_flore, versant=="S")
nord = subset(data_flore, versant=="N")

dim(sud)
dim(nord)

# Tableau uniquement avec especes
data_especes_sud = sud[,20:ncol(sud)]
data_especes_nord = nord[,20:ncol(nord)]

dim(data_especes_sud)
dim(data_especes_nord)

# E. Carte des données
plot(longitude, latitude)

identify(longitude, latitude) # sous Rstudio faire echap pour afficher les données après les avoir sélectionné sur le graphique

# Néttoyer le jeu de données
data_map = data_flore
plot(data_map$longitude, data_map$latitude)
points_to_remove = identify(data_map$longitude, data_map$latitude, n = 10)
# echap
data_map = data_map %>%
  filter(!row_number() %in% points_to_remove)
plot(data_map$longitude, data_map$latitude)

# Améliorer le plot
#### A faire

# F. Charger les données de trait
data_trait = read.csv(here::here("data", "traits_2024.csv"), # upload data
                      sep = ";", header = T, dec = ",")

# Verifications...

# G. Charger le referentiel espèces
data_ref = read.csv(here::here("data", "liste_sp_2024.csv"), # upload data
                    sep = ";", header = T, dec = ",")

# Verifications...

#####
# 2. Visualisation des données
#####

# A. Representation graphique du tableau de données 
table.value(data_especes, clegend=0 ) 

# B. Représentation ordonnée par rapport à une variable facteur
table.value(data_especes[order(versant),],  row.labels =versant[order(versant)], clegend=0 ) 

# C. Représentation par sous tableau de données "versant"
table.value(data_especes_sud, clegend=0 ) 
table.value(data_especes_nord, clegend=0 ) 


#####
# 3. Richesse spécifique
#####

###	3.A. Richesse par plot
plot_n = rowSums(data_especes > 0)

# Plot
plot(sort(plot_n, decreasing = T), 
     main = "Richesse des plots")

###	3.B. Richesse totale echantillonnée
dim(data_especes)

sum(colSums(data_especes) > 0)

###	3.C. Exhaustivité de l’échantillonnage / richesse estimée
# Réaliser une courbe pour la zone totale
# Méthode sans remise (nombre de permutations doit etre égal au nombre d'échantillons)
n_plot = nrow(data_especes)

sp = specaccum(data_especes, "random", permutations= n_plot)

sp

plot(sp, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

# Richesse totale (gamma)
round(sp$richness[n_plot],2) # richesse 
round(sp$sd[n_plot],2) # ecart type

# Réaliser 1 courbe pour par versant
## Nord
sp_n  =  specaccum(data_especes_nord, "random", permutations = nrow(data_especes_nord))
plot(sp_n, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

## Sud
sp_s  =  specaccum(data_especes_sud, "random", permutations = nrow(data_especes_sud))
plot(sp_s, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

# Richesse rarifiée à n=30 quadras
round(sp_n$richness[30],2)
round(sp_n$sd[30],2)

round(sp_s$richness[30],2)
round(sp_s$sd[30],2)

# Graphique général
par(mfrow=c(1,3))
plot(sp, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp_n, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp_s, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
par(mfrow = c(1, 1))  # Une seule fenêtre graphique

# Est ce que l’échantillonnage a été suffisamment exhaustif, pour la méthode utilisée ?



### 3.D. Richesse estimée du bois de Montmaur
# Plusieurs fonctions permettent d'estimer la richesse en espèces extrapolée
# dans un pool d'espèces, ou le nombre d'espèces non observées. 
# La fonction specpool est basée sur les incidences dans les sites d'échantillonnage 
# et donne une estimation unique pour une collection de sites d'échantillonnage (matrice). 
# La fonction estimateR est basée sur les abondances (comptages) sur un seul site d'échantillonnage.

# Calculer la richesse estimée globale
specpool(round(data_especes, 0))  # Estime la richesse totale via pool d'espèces

# Calcul de la courbe de rarefaction
rarecurve(round(data_especes, 0), step = 1, sample = 40, col = "blue", label = TRUE)

# Calculer la richesse estimée par plot
estimateR(round(data_especes, 0))

### 3.E. Modèle d'abondance
# Diagramme rang/fréquence avec rankabundance

data_flore$versant = as.factor(data_flore$versant)

rankabuncomp(data_especes, y = data_flore, factor = "versant",  scale = "proportion", legend=T) # y présente la proportion/abondance totale
rankabuncomp(data_especes, y = data_flore, factor = "versant",  scale = "abundance", legend=T) # y présente l'abondance
rankabuncomp(data_especes, y = data_flore, factor = "versant",  scale = "logabun", legend=T) # log base 10

# Modèles ajustant les données avec radfit()

ndata_especes_sud = apply(data_especes_sud,2,sum)
ndata_especes_nord = apply(data_especes_nord,2,sum)

radfit(ndata_especes_sud)
plot(radfit(ndata_especes_sud))

radfit(ndata_especes_nord)
plot(radfit(ndata_especes_nordd))

#####
# 4. Diversité alpha
#####

### 4.A. Calcul des indices

# Richesse spécifique S
sr = specnumber(data_especes, MARGIN=1)

# Couverture totale N par quadra
N = apply(data_especes, 1, sum)

# Abondance de l'espèce dominante Nmax par quadrat
Nmax = apply(data_especes, 1, max)

# Indice de Berger-Parker
bg = Nmax/N

# proportion p of each species per sample
p = as.matrix(data_especes/N)

# Indice hétérogène (RS+equitabilité) de Simpson simp : 1-D
simp  =  diversity(data_especes, "simpson")

# Indice hétérogène (RS+equitabilité) de Simpson (non biaisé) PIE : (n/n-1) x 1-D
simpc  =  (N/(N-1))*diversity(data_especes, "simpson")

# Equitabilité Simpson 1-D / (1- 1/S)
esimp = diversity(data_especes, "simpson")/(1-(1/sr))

#####
# 5. Diversité foctionnelle
#####

# Charger les données de traits fonctionnels

trait = read.table(file.choose(), sep= , dec=, header=, row.names= )

# moyenner la valeur du trait quantitatif pour les 3 individus mesurés sur une espèce (utiliser les noms corrects de variables)
trait1=aggregate(trait$chlo ~ espece, data = trait, mean)

# Indice CWM (Community Weighted Mean)
require(weimea)
icwm = cwm(data_especes,trait1)

# Distances fonctionnelles dij entre espèces mesurées à partir des deux traits
require(FD)
d = gowdis(trait)

#Classification fonctionnelle des espèces (complémentarité-redondance) avec CAH (lien moyen) sur Gower
plot(hclust(d, "average"), hang=-1) 

# Entropie Quadratique (extension de 1-D avec dij)
require(adiv)
qe = QE(data_especes,d)

# Disposer dans une meme matrice les 6 indices

# Realiser un Draftsman plot pour étudier leur relation (ie redondance empirique ou non)




#3.2) Effet de variables explicatives

# Creer 4 sous figures (ie 4 indices, ou 6 avec indices fonctionnels) pour les 3 effets suivants. Pour les tests récupérer scripts des UEs DESINF et EVA) :

# 3.2.1) Effet du versant
# boxplot() versant + t.test() (vérifier hypothèses et orienter plus précisément le test)

par(mfrow=c(2,2))
boxplot(sr~versant)
boxplot(bg~versant)
boxplot(simpc~versant)
boxplot(esimp~versant)

# Données moyennes et sd pour chaque modalité d'une variable qualitative
round(tapply(sr,versant, mean),2)
round(tapply(sr,versant, sd), 2)

round(tapply(bg,versant, mean),2)
round(tapply(bg,versant, sd), 2)

round(tapply(simpc,versant, mean),2)
round(tapply(simpc,versant, sd), 2)

round(tapply(esimp,versant, mean),2)
round(tapply(esimp,versant, sd), 2)

# Test statistique de comparaison de 2 moyennes (cf script UEs DESINF et/ou EVA)


# 3.2.2) Effet du recouvrement arboré
#plot()  vs recouvrement arboré + lm et correlation si lien linéaire

plot(sr ~ rec_espece_dominante_arboree)
scatter.smooth(sr ~ rec_espece_dominante_arboree, span = 2/3, degree = 2)

# A faire également que pour recouvrement chene vert et pin d'alep


# 3.2.3) Effet du recouvrement comparé en versant sud et nord

#indices vs recouvrement en fonction du versant -> ANCOVA
#etc.

# 3.2.4) Effet recouvrement arbres dominants

#boxplot S vs arbres dominants + anova() (vérifier hypothèses et orienter plus précisément le test)
# etc.



#Comment se structure la diversité alpha en fonction des variables explicatives ?


#####
# 7. Composition des communautés & Diversité béta
#####

### 7.A. Diversité béta taxonomique

#3.2.1.1) Globale
#Décomposition Gamma en beta et alpha moyen pour en déduire la béta, pour la zone, par versant, entre chacun des 3 réplicats pour un groupe donné.
#Est ce que la composition en espèces globalement au sein de la zone d’étude, par versant

# Realiser le calcul avec le modele multiplicatif


#3.2.1.2) Inter-quadra
#Données présence-absence : Jaccard

# Jaccard-PCoA (MDS)
require(vegan)
jac = vegdist(data_especes,"jaccard")

library(plotly)
gom = as.matrix(jac)
gom
plot_ly(x=colnames(gom), y=rownames(gom), data_especes = gom, type = "heatmap")
plot_ly(x=colnames(gom), y=rownames(gom), data_especes = gom, type = "heatmap", colorscale= "Earth")

#PCoA
pcbc = dudi.pco(jac)

# axes percentages
pourc = round((pcbc$eig/sum(pcbc$eig))*100,2)
pourc
cumsum(pourc)

# Projections of samples
s.label(pcbc$li, sub="Jaccard")

# Projections of samples according to factorial variables
par(mfrow = c(1,2))
s.class(pcbc$li, versant, col=c(1:4))
s.class(pcbc$li, zone, col=c(1:3))

# A posteriori projection of variables contribution (species correlations to axes)
require(ape)
pcobc=pcoa(jac)
biplot(pcobc, data_especes)

# Données de recouvrement : idem ci-dessus avec Bray-Curtis method="bray"


### 7.A. Diversité béta taxonomique

#3.2.2.1) Globale

# Entropie quadratique 

require(adiv)
discomQE(data_especes, d, structrures=versant) 

# voir aussi
require(picante)
raoD(data_especes, d) 

# 4) Biais d’échantillonnage
# boxplot RS sur 3 relevés pour différentes zones en fonction du groupe matin /aprem

