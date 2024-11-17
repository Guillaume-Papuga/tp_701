#####################################################
# HAV701B Ecologie : concepts, outils et applications 
#####################################################

############################################################################################
# Ateliers de terrain - Diversité des communautés floristiques du nord de Montpellier
# Enseignants : Guillaume Papuga & Bastien Mérigot  
# TP novembre 2021
############################################################################################

# Charger les données communautés avec la fonction read.table()

x<-read.table(file.choose(), sep=";" , dec=",", header=T)

#	1.	Vérification des données chargées 
edit(x) # si besoin de modifications manuelles : x<-edit(x)

### refermer la fenetre avant de pouvoir utiliser de nouveau la console

dim(x)

names(x)
attach(x)
head(x)
structure(x)

summary(x)

table(versant) #par zone et/ou autre modalités qualitatives
plot(longitude,latitude)
identify(longitude,latitude)
# sous Rstudio faire echap pour afficher les données après les avoir sélectionné sur le graphique

# Tableau uniquement avec especes
z<-x[,20:ncol(x)]
dim(z)

# Creer deux tableaux séparement pour chaque versant avec la fonction subset()
sud<-subset(x, versant=="S")
nord<-subset(x, versant=="N")

dim(sud)
dim(nord)

# Tableau uniquement avec especes
zsud<-sud[,20:ncol(sud)]
znord<-nord[,20:ncol(nord)]

dim(zsud)
dim(znord)


require(ade4)

# Representation graphique du tableau de données 
table.value(z, clegend=0 ) 
# Idem ordonné par rapport à une variable facteur
table.value(z[order(versant),],  row.labels =versant[order(versant)], clegend=0 ) 

#idem par sous tableau de données versant
table.value(zsud, clegend=0 ) 
table.value(znord, clegend=0 ) 


#	2.	Exhaustivité de l’échantillonnage 
# Réaliser 1 courbe pour la zone totale + 1 courbe par versant ; méthode sans remise (nombre de permutations doit etre égal au nombre d'échantillons)
require(vegan)
sp2 <- specaccum(z, "random", permutations=72)
plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

# Richesse totale (gamma)
round(sp2$richness[72],2)
round(sp2$sd[72],2)

rsud<-sud[,20:ncol(sud)]
sp3 <- specaccum(rsud, "random", permutations=42)
plot(sp3, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

# Richesse rarifiée à n=30 quadra
round(sp3$richness[30],2)
round(sp3$sd[30],2)

rnord<-nord[,20:ncol(nord)]
sp4 <- specaccum(rnord, "random", permutations=30)
plot(sp4, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")

round(sp4$richness[30],2)
round(sp4$sd[30],2)

par(mfrow=c(1,3))
plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp3, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
plot(sp4, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")


# Est ce que l’échantillonnage a été suffisamment exhaustif, pour la méthode utilisée ?


# Diagramme rang fréquence

require(BiodiversityR)
x$versant<-as.factor(x$versant)

rankabuncomp(z, y=x, factor="versant",  scale="proportion", legend=T)
rankabuncomp(z, y=x, factor="versant",  scale="abundance", legend=T)
rankabuncomp(z, y=x, factor="versant",  scale="logabun", legend=T)

# Modèles ajustant les données avec radfit()

nzsud<-apply(zsud,2,sum)
nznord<-apply(znord,2,sum)

radfit(nzsud)
plot(radfit(nzsud))

radfit(nznord)
plot(radfit(nznordd))


#3. Indices de diversité  
# 3.1) Calcul des indices (alpha)

#Richesse spécifique S
sr<-specnumber(z,MARGIN=1)

# Nombre d'individus total N, et abondance de l'espèce dominante Nmax, par quadra
N<-apply(z,1,sum)
Nmax<-apply(z,1, max)

# Indice de Berger-Parker
bg<-Nmax/N

# proportion p of each species per sample
p<-z/N
p<-as.matrix(p)

# Indice hétérogène (RS+equitabilité) de Simpson (non biaisé) PIE : (n/n-1) x 1-D
simpc <- (N/(N-1))*diversity(z, "simpson")

# Equitabilité Simpson 1-D / (1- 1/S)
esimp<-diversity(z, "simpson")/(1-(1/sr))

# Diverstié fonctionnelle

# Charger les données de traits fonctionnels

trait<-read.table(file.choose(), sep= , dec=, header=, row.names= )

# moyenner la valeur du trait quantitatif pour les 3 individus mesurés sur une espèce (utiliser les noms corrects de variables)
trait1=aggregate(trait$chlo ~ espece, data = trait, mean)

# Indice CWM (Community Weighted Mean)
require(weimea)
icwm<-cwm(z,trait1)

# Distances fonctionnelles dij entre espèces mesurées à partir des deux traits
require(FD)
d<-gowdis(trait)

#Classification fonctionnelle des espèces (complémentarité-redondance) avec CAH (lien moyen) sur Gower
plot(hclust(d, "average"), hang=-1) 

# Entropie Quadratique (extension de 1-D avec dij)
require(adiv)
qe<-QE(z,d)

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



#3.2) Diversité béta

#3.2.1) Div. béta taxonomique

#3.2.1.1) Globale
#Décomposition Gamma en beta et alpha moyen pour en déduire la béta, pour la zone, par versant, entre chacun des 3 réplicats pour un groupe donné.
#Est ce que la composition en espèces globalement au sein de la zone d’étude, par versant

# Realiser le calcul avec le modele multiplicatif


#3.2.1.2) Inter-quadra
#Données présence-absence : Jaccard

# Jaccard-PCoA (MDS)
require(vegan)
jac<-vegdist(z,"jaccard")

library(plotly)
gom<-as.matrix(jac)
gom
plot_ly(x=colnames(gom), y=rownames(gom), z = gom, type = "heatmap")
plot_ly(x=colnames(gom), y=rownames(gom), z = gom, type = "heatmap", colorscale= "Earth")

#PCoA
pcbc<-dudi.pco(jac)

# axes percentages
pourc<-round((pcbc$eig/sum(pcbc$eig))*100,2)
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
biplot(pcobc, z)

# Données de recouvrement : idem ci-dessus avec Bray-Curtis method="bray"


#3.2.2) Div. béta fonctionnelle

#3.2.2.1) Globale

# Entropie quadratique 

require(adiv)
discomQE(z, d, structrures=versant) 

# voir aussi
require(picante)
raoD(z, d) 

# 4) Biais d’échantillonnage
# boxplot RS sur 3 relevés pour différentes zones en fonction du groupe matin /aprem

