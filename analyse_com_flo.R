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

# Visualiser les données de traits fonctionnels
data_trait

# Ajouter les traits des autres années

### 5.A. Trait quantitatif
# Trait moyen (moyenner la valeur du trait quantitatif pour les 3 individus mesurés sur une espèce)
trait_moy_chlo = aggregate(data_trait$chlo ~ code_espece, data = data_trait, mean)
colnames (trait_moy_chlo) [2] = "chloro"

# Utiliser intersect() pour obtenir les colonnes communes
codes_sp_communes = intersect(colnames(p), trait_moy_chlo$code_espece)

# selectionner les colonnes qui ont des traits dispos
mt = data_especes[, (colnames(data_especes) %in% colonnes_communes)] # selectionne les communes
mt_ordo = mt[, sort(colnames(mt))] # trier les données
abondance_plot = rowSums(p1_ordo) # vecteur pour calcul de pourcentage

# trait des espèces communes
trait_reduit = trait_moy_chlo[trait_moy_chlo$code_espece %in% colonnes_communes,] # selectionne les communes
trait_ordo = trait_reduit[order(trait_reduit$code_espece), ] # trier les données
#row.names(trait_ordo) = trait_ordo$code_espece

# calcul la matrice de trait
cwm = (rowSums(mt * t(trait_ordo$chloro)))/abondance_plot

### 5.B. Trait qualitatif
# table des types biologiques
type_raunkiaer = unique (data_trait[, c("code_espece", "type_biologique")]) %>%
  arrange(code_espece) %>%
  filter (code_espece %in% codes_sp_communes)

# transformer la matrice en binaire
mt_bin = mt_ordo 
mt_bin[mt_bin>0]=1

# Mettre les relevés en format long
releves_long = mt_bin %>%
  mutate (site = data_flore$code_rel) %>%
  melt() %>%
  filter (value >0) %>%
  left_join(type_raunkiaer, by = c("variable" = "code_espece")) %>%
  group_by(site, type_biologique) %>%
  mutate (nsp = sum(value)) %>%
  arrange (site)

table(releves_long$site, releves_long$type_biologique)


### 5.C. Synthèse quantitatif - qualitatif
# Distances fonctionnelles dij entre espèces mesurées à partir des deux traits
dist_matrix = gowdis(data_trait[, c("chlorophylle", "type_biologique")])

# Classification fonctionnelle des espèces (complémentarité-redondance) avec CAH (lien moyen) sur Gower
hc <- hclust(dist_matrix)
plot(hc, hang = -1)

# Code la variable type biologique
type_bio = factor(data_trait$type_biologique)
couleurs <- c("red", "blue", "green", "purple", "yellow")[type_bio]

points(x = 1:length(hc$order), y = rep(-0.1, length(hc$order)), 
       col = couleurs[hc$order], pch = 19, cex = 1.5)


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
mat_synth = cbind (sr, N, Nmax, bg, simp, simpc, esimp)

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
boxplot(sr~versant, main = "SR")
boxplot(bg~versant, main = "BG")
boxplot(simpc~versant, main = "SIMPC")
boxplot(esimp~versant, main = "ESIMP")
par(mfrow=c(1,1))


# Données moyennes et sd pour chaque modalité d'une variable qualitative
# SR
round(tapply(sr,versant, mean),2)
round(tapply(sr,versant, sd), 2)

# BG
round(tapply(bg,versant, mean),2)
round(tapply(bg,versant, sd), 2)

# Simpc
round(tapply(simpc,versant, mean),2)
round(tapply(simpc,versant, sd), 2)

# Esimp
round(tapply(esimp,versant, mean),2)
round(tapply(esimp,versant, sd), 2)

# Test statistique de comparaison de 2 moyennes (cf script UEs DESINF et/ou EVA)
# SR
t.test(sr~versant)

# BG

# Simpc

# Esimp


# 6.C. Effet du recouvrement arboré
# plot() vs recouvrement arboré + lm et correlation si lien linéaire

# SR
## plot basique
plot(sr ~ rec_espece_dominante_arboree)
scatter.smooth(sr ~ rec_espece_dominante_arboree, span = 2/3, degree = 2)

## Analyse ANCOVA
db_sr = cbind(data_flore, sr) %>%
  filter(espece_arbre_dominante %in% c("QUEILE", "PINHAL"))

mod = lm(sr~ rec_espece_dominante_arboree + versant, data = db_sr)
summary(mod)


#####
# 7. Composition des communautés & Diversité béta
#####

### 7.A. Diversité béta taxonomique

# Décomposition Gamma en beta et alpha moyen pour en déduire la béta, pour la zone, par versant, entre chacun des 3 réplicats pour un groupe donné.
# Est ce que la composition en espèces globalement au sein de la zone d’étude, par versant
# Realiser le calcul avec le modele multiplicatif


# 7.A.1. Données présence-absence : Jaccard
# Jaccard-PCoA (MDS)
jac = vegdist(data_especes,"jaccard")

# PCoA
pcjac = dudi.pco(jac, nf = 2)
pcjac

# axes percentages
pourc = round((pcjac$eig/sum(pcjac$eig))*100,2)
pourc
cumsum(pourc)

# Projections of samples
s.label(pcjac$li, sub="Jaccard")

# Projections of samples according to factorial variables
par(mfrow = c(1,2))
s.class(pcjac$li, factor(versant), col=c(1:4))
s.class(pcjac$li, factor(zone), col=c(1:3))
par(mfrow = c(1,1))

# Clustering


# A posteriori projection of variables contribution (species correlations to axes)
require(ape)
pcojac = pcoa(jac)
biplot(pcojac, data_especes)

# 7.A.2. Données d'abondance
# Données de recouvrement : idem ci-dessus avec Bray-Curtis method="bray"


