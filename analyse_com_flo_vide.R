#####################################################
# HAV701B Ecologie : concepts, outils et applications 
#####################################################

############################################################################################
# Ateliers de terrain - Diversité des communautés floristiques du nord de Montpellier
# Enseignants : Guillaume Papuga & Bastien Mérigot
# TP novembre 2024
############################################################################################

#####
# 1. Préparation des données
#####

# A. Packages
require(ade4)
require(adiv)
require(BiodiversityR)
require(dplyr)
require(here)
require(ggplot2)
require(vegan)
require(FD)
require(reshape2)

# B. Charger les donnees de communautés floristiques

data_flore = read.csv(here::here("data", "com_2024.csv"), # upload data
                      sep = ";", header = T, dec = ",")


#	C. Vérification des données chargées
 # si besoin de modifications manuelles : data_flore = edit(data_flore)
### refermer la fenetre avant de pouvoir utiliser de nouveau la console

dim (data_flore)# nombre de lignes et colonnes

colnames (data_flore) # noms des colonnes

attach (data_flore)# attache les variables

head (data_flore) # entête

str (data_flore) # type de varibles

summary (data_flore)# résumé des données

# crée une table par zone et/ou autre modalités qualitatives

# D. Tableau simplifié avec uniquement les especes
data_especes = data_flore[,c(11:ncol(data_flore))]


# Creer deux dataset séparés pour chaque versant avec la fonction subset()
sud = data_flore[data_flore$versant == "S",]
nord = data_flore[data_flore$versant == "N",]

dim(sud)
dim(nord)

# Tableau uniquement avec especes
data_especes_sud = sud [,c(11:ncol(sud))]
data_especes_nord = nord [,c(11:ncol(nord))]

dim(data_especes_sud)
dim(data_especes_nord)

# E. Carte des données
plot(longitude, latitude)

identify(longitude, latitude) # sous Rstudio faire echap pour afficher les données après les avoir sélectionné sur le graphique

# Néttoyer le jeu de données

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
table.value() 

# B. Représentation ordonnée par rapport à une variable facteur
table.value( ) 

# C. Représentation par sous tableau de données "versant"
table.value( ) 
table.value() 


#####
# 3. Richesse spécifique
#####

###	3.A. Richesse par plot
plot_n = rowSums(data_especes>0)

# Plot
plot(plot_n)

plot(sort(plot_n, # utilise sort pour trier les données
          decreasing = T), # met dans l'ordre décroissant
     main = "Richesse spécifique") # c'est le titre

###	3.B. Richesse totale echantillonnée
dim()


###	3.C. Exhaustivité de l’échantillonnage / richesse estimée
# Réaliser une courbe pour la zone totale
# Méthode sans remise (nombre de permutations doit etre égal au nombre d'échantillons)
n_plot = nrow(data_especes)

sp = specaccum(data_especes, 
               "random", 
               permutations= n_plot)

sp

plot(sp)

plot(sp, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")


# Richesse totale (gamma)
round(sp$richness[n_plot],2) # richesse 
round(sp$sd[n_plot],2) # ecart type

# Réaliser 1 courbe pour par versant
## Nord
sp_n  =  
plot()

## Sud
sp_s  =  
plot()

# Richesse rarifiée à n=30 quadras



# Graphique général
par(mfrow=c(1,3))


par(mfrow = c(1, 1))  # Une seule fenêtre graphique

# Est ce que l’échantillonnage a été suffisamment exhaustif, pour la méthode utilisée ?


### 3.D. Richesse estimée du bois de Montmaur
# Plusieurs fonctions permettent d'estimer la richesse en espèces extrapolée
# dans un pool d'espèces, ou le nombre d'espèces non observées. 
# La fonction specpool est basée sur les incidences dans les sites d'échantillonnage 
# et donne une estimation unique pour une collection de sites d'échantillonnage (matrice). 
# La fonction estimateR est basée sur les abondances (comptages) sur un seul site d'échantillonnage.

# Calculer la richesse estimée globale
  # Estime la richesse totale via pool d'espèces

# Calcul de la courbe de rarefaction


# Calculer la richesse estimée par plot


### 3.E. Modèle d'abondance
# Diagramme rang/fréquence avec rankabundance



# Modèles ajustant les données avec radfit()


#####
# 4. Diversité alpha
#####

### 4.A. Calcul des indices

# Richesse spécifique sr
sr = 

# Couverture totale N par quadra
N = 

# Abondance de l'espèce dominante Nmax par quadrat
Nmax = 

# Indice de Berger-Parker
bg = 

# proportion p of each species per sample
p = 

# Indice hétérogène (RS+equitabilité) de Simpson simp : 1-D
simp  =  
  
# Indice hétérogène (RS+equitabilité) de Simpson (non biaisé) PIE : (n/n-1) x 1-D
simpc  =  

# Equitabilité Simpson 1-D / (1- 1/S)
esimp = 

#####
# 5. Diversité foctionnelle
#####

# Visualiser les données de traits fonctionnels
data_trait

# Ajouter les traits des autres années

### 5.A. Trait quantitatif
# Trait moyen (moyenner la valeur du trait quantitatif pour les 3 individus mesurés sur une espèce)
trait_moy_chlo = 

# Utiliser intersect() pour obtenir les colonnes communes
codes_sp_communes = 

# selectionner les colonnes qui ont des traits dispos
mt =  # selectionne les communes
mt_ordo = # trier les données
abondance_plot =  # vecteur pour calcul de pourcentage

# trait des espèces communes
trait_reduit = # selectionne les communes
trait_ordo =  # trier les données
#row.names(trait_ordo) = trait_ordo$code_espece

# calcul la matrice de trait
cwm = 

### 5.B. Trait qualitatif
# table des types biologiques

  
### 5.C. Synthèse quantitatif - qualitatif
# Distances fonctionnelles dij entre espèces mesurées à partir des deux traits
dist_matrix = 

# Classification fonctionnelle des espèces (complémentarité-redondance) avec CAH (lien moyen) sur Gower
hc <- hclust()
plot()

# Code la variable type biologique
type_bio = 

points()


# Entropie Quadratique (extension de 1-D avec dij)
# Tab 
jdd = t(mt_ordo)

# Tab
tab_traits_entro = trait_ordo %>%
  left_join(type_raunkiaer, by = c("code_espece" = "code_espece"))
row.names(tab_traits_entro) = tab_traits_entro$code_espece
tab_traits_entro = tab_traits_entro %>% select (-code_espece)

# Dist traits
d_entro = gowdis(tab_traits_entro)

# Calcul de l'EQ
qe = QE(mt_ordo, d_entro, formula="QE")


#####
# 6. Effet de variables explicatives
#####
# 6.A. Correlation entre variables réponses
# Disposer dans une meme matrice les 6 indices
mat_synth = cbind ()

# Realiser un Draftsman plot pour étudier leur relation (ie redondance empirique ou non)
pairs(mat_synth)

# Alterate
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(mat_synth, lower.panel = panel.smooth, upper.panel = panel.cor,
      gap=0, row1attop=FALSE)

# 6.B. Effet du versant
# boxplot() versant + t.test() (vérifier hypothèses et orienter plus précisément le test)

par(mfrow=c(2,2))



par(mfrow=c(1,1))


# Données moyennes et sd pour chaque modalité d'une variable qualitative
# SR

# BG

# Simpc

# Esimp

# Test statistique de comparaison de 2 moyennes (cf script UEs DESINF et/ou EVA)
# SR

# BG

# Simpc

# Esimp


# 6.C. Effet du recouvrement arboré
# plot() vs recouvrement arboré + lm et correlation si lien linéaire

# SR
## plot basique

## Analyse ANCOVA



#####
# 7. Composition des communautés & Diversité béta
#####

### 7.A. Diversité béta taxonomique

# Décomposition Gamma en beta et alpha moyen pour en déduire la béta, pour la zone, par versant, entre chacun des 3 réplicats pour un groupe donné.
# Est ce que la composition en espèces globalement au sein de la zone d’étude, par versant
# Realiser le calcul avec le modele multiplicatif


# 7.A.1. Données présence-absence : Jaccard
# Jaccard-PCoA (MDS)
jac = vegdist()

# PCoA
pcjac = dudi.pco()
pcjac

# axes percentages
pourc = round((pcjac$eig/sum(pcjac$eig))*100,2)
pourc
cumsum(pourc)

# Projections of samples
s.label()

# Projections of samples according to factorial variables
par(mfrow = c(1,2))

par(mfrow = c(1,1))

# Clustering


# A posteriori projection of variables contribution (species correlations to axes)
require(ape)
pcojac = pcoa(jac)
biplot(pcojac, data_especes)

# 7.A.2. Données d'abondance
# Données de recouvrement : idem ci-dessus avec Bray-Curtis method="bray"


