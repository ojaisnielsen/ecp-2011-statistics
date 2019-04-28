# TP3 : Méthodes de Classifications
# 14 Octobre 2010 - ECP
# Hichem HADJOUDJ - Olivier JAIS-NIELSEN

###########################
#  Analyse Discriminante  #
###########################

library(ade4)

## 1 - Analyse graphique de la discrimination sepales et petales

par(mfrow=c(1,2));
s.class(iris[,1:2],iris[,5]);
s.class(iris[,3:4],iris[,5]);

# Les axes 3 et 4 (pétales) permettent de mieux classifier les individus et en particulier les setosas. Les axes 1 et 2 sont bien moins discriminants.

## 2 - Analyse en composantes principales

iris.pca=dudi.pca(iris[,1:4],scan=F, nf=2)
s.class(iris.pca$li,iris[,5]) ;
x11()
s.corcircle(iris.pca$co); 

# La largeure des sépales semble indépendante des 3 autres caractéristiques, par rapport au premier plan factoriel.

## 3 - Analyse discriminante descriptive

iris.d=discrimin(iris.pca,fac=iris[,5],scannf=F);

# Le champ "li" fournit les projections des données sur le premier plan factoriel (le premier axe maximisant la variance inter-classes).
# Le champ "eig" fournit les valeurs propres correspondant à chaque axe factoriel.
# Le champ "va" fournit des grandeurs permettant d'apprécier l'information représentée par chaque axe de manière canonique (pourcentage, contrairement à la simple étude des valeurs propres) : pour chaque axe, il s'agit du rapport de la somme des distances quadratiques inter-groupes par la somme totale des distances quadratiques à la moyenne (rapport de corrélation).

# Visualisation
x11()
s.class(iris.d$li,iris[,5]);

# Analyse : Cette desciption discrimine les 3 classes.


# Cercle des correlations
x11()
par(mfrow=c(1,2));
s.corcircle(iris.pca$co);
title(sprintf('Avant ADD'));
s.corcircle(iris.d$va);
title(sprintf('Après ADD'));
# Analyse : On garde des correlations similaires au modèle ACP tout en discriminant les 3 classes.

## 4 - Analyse discriminante predictive
library(MASS)

model=lda(Species~.,data=iris)
plot(model)

# Cette dernière instruction affiche pour chaque individus un point dont les coordonnées sont les valeurs des fonction discriminantes en ce point, associé à l'étiquette de sa classe réelle. On constate une bonne séparation des sétosas des autres espèces par la première fonction discriminante, alors qu'elle sépare moins franchement mais les deux autres espèces entre elles.

# Echantillonage aléatoire
i = sample(nrow(iris), 0.8 * nrow(iris))
train = iris[i,]
test = iris[-i,]

# Analyse discriminante descriptive des données d'apprentissage
model=lda(Species~.,data=train)

# Prédiction sur les données d'apprentissage
prediction = predict(model, train)

# Matrice de confusion
table(prediction$class, train$Species)

# Prédiction sur les données de test
prediction = predict(model, test)

# Matrice de confusion
table(prediction$class, test$Species)

# Analyse: Dans les cas testés, les matrices de confusion sont quasiment diagonales, ce qui pour la prédiction sur les données d'apprentissage, valide la méthode et, pour la prédiction sur les données de test, valide le modèle.

## 5 - Pouvoir discriminant d'une variable

# Création d'un fonction var2 qui est une var non biaisée
var2=function(X){
	var2=var(X)*(length(X)-1)/length(X)
	return(var2);
}

# Définition de la fonction permettant de calculer le pouvoir discriminant d'une variable
PouvoirDiscrimin=function(X,Y,variable){
	
	nb.classe=length(unique(Y))
	dim.total=nrow(X)
	moyenne=mean(X[,variable])
	var.total=var2(X[,variable])
	
	sum=0
	for(j in 1:nb.classe){
		x=c()
		Xclasse=c()
		for(i in 1:length(Y)){
			if(Y[i]==unique(Y)[j]){
				x=cbind(x,i)
			}
		}
		
		Xclasse=X[x,variable]
		dim.classe=length(Xclasse)
		moyenne.classe=mean(Xclasse)
		sum=sum+(dim.classe/dim.total)*((moyenne.classe-moyenne)^2)	
	}
	return(sum/var.total);
}

# Calcul des pouvoirs discriminants des 4 variables
PouvoirDiscrimin(iris[,1:4],iris[,5],1);
PouvoirDiscrimin(iris[,1:4],iris[,5],2);
PouvoirDiscrimin(iris[,1:4],iris[,5],3);
PouvoirDiscrimin(iris[,1:4],iris[,5],4);
# Analyse : on retrouve bien ce que l'on avait constaté sur le premier graphique, à savoir que les axes 3 et 4 sont bien discriminants et que l'axe 2 est le moins discriminant

### Vérification de la formule de décomposition de la variance en définissant une fonction var.inter (c) et var.intra (b) puis comparaison de la somme avec la var non biaisée (c)

## a - Fonction qui permet de calculer la variance inter classe
var.inter=function(X,Y,variable){
	
	nb.classe=length(unique(Y))
	dim.total=nrow(X)
	moyenne=mean(X[,variable])
	
	sum=0
	for(j in 1:nb.classe){
		x=c()
		Xclasse=c()
		for(i in 1:length(Y)){
			if(Y[i]==unique(Y)[j]){
				x=cbind(x,i)
			}
		}
		
		Xclasse=X[x,variable]
		dim.classe=length(Xclasse)
		moyenne.classe=mean(Xclasse)
		sum=sum+(dim.classe/dim.total)*((moyenne.classe-moyenne)^2)	
	}
	return(sum);
}


## b - Fonction qui permet de calculer la variance intra
var.intra=function(X,Y,variable){
	
	nb.classe=length(unique(Y))
	dim.total=nrow(X)
	moyenne=mean(X[,variable])
	var.total=var2(X[,variable])
	
	sum=0
	for(j in 1:nb.classe){
		x=c()
		Xclasse=c()
		for(i in 1:length(Y)){
			if(Y[i]==unique(Y)[j]){
				x=cbind(x,i)
			}
		}
		
		Xclasse=X[x,variable]
		dim.classe=length(Xclasse)
		sum=sum+(dim.classe/dim.total)*var2(Xclasse)	
	}
	return(sum);
}


## c - Vérification de la somme
var2(iris[,1])
var.inter(iris[,1:4],iris[,5],1)+var.intra(iris[,1:4],iris[,5],1);
var2(iris[,2])
var.inter(iris[,1:4],iris[,5],2)+var.intra(iris[,1:4],iris[,5],2);
var2(iris[,3])
var.inter(iris[,1:4],iris[,5],3)+var.intra(iris[,1:4],iris[,5],3);
var2(iris[,4])
var.inter(iris[,1:4],iris[,5],4)+var.intra(iris[,1:4],iris[,5],4);
# Analyse : On retrouve bien l'égalité var = inter + intra

### Après analyse discriminante

# Après analyse
PouvoirDiscrimin(iris.d$li,iris[,5],1)
PouvoirDiscrimin(iris.d$li,iris[,5],2)
# On voit que le pouvoir discriminant est meilleure sur l'axe 1 (comme illustré sur la figure) mais reste faible sur l'axe 2 (comme illustré sur la figure)




###########################
#  Régression logistique  #
###########################

# 1 - Chargement du fichier.

setwd("/Users/hichem/Documents/Écoles/Centrale/Troisième année/Statistiques et Data Mining/TP/TP3")

# Chargement des données
tab=read.table("SAheart.txt", header=TRUE,row.names="row.names", sep=",",dec=".")[,c(1,2,3,5,7,8,9,10)];
tab;

# Interprétation : les 7 premières variables correspondent à des caractéristiques physiques, comportementales et médicales d'une population de 462 individus(matrice X de dimension n=462 x d=7), la variable chd étant l'étiquette indiquant si oui (chd=1) ou non (chd=0) l'individu a eu un incident cardiaque (vecteur Y de dimension n=462 à valeur dans {0,1})

## 2 -scatterplot
x11();
pairs(tab,pch=22,bg=c("red","blue")[unclass(factor(tab[,"chd"]))]);
#Analyse : a priori aucune variable ne permet de classifier de manière simple les données.

## 3 - Régression logistique
tab.glm=glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family=binomial(link="logit"),data=tab);
anova(tab.glm,"Chisq");

## 4 - Prédiction
# Matrice de confusion
table(tab.glm$fitted.values>0.5,tab$chd);

# Calculs des pourcentages de "faux"
Pfauxpositifs=sum((tab.glm$fitted.values>0.5)*(tab$chd==0))/sum(tab$chd==1);
Pfauxnegatifs=sum((tab.glm$fitted.values<0.5)*(tab$chd==1))/sum(tab$chd==0);
Pfauxpositifs;
Pfauxnegatifs;

## 5 - Validation croisée
i=sample(nrow(iris), 0.75 * nrow(iris))
tab.train=tab[i,];
tab.test=tab[-i,];
tab.train.glm=glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family=binomial(link="logit"),data=tab.train);
tab.test.predict=predict.glm(tab.train.glm,newdata=tab.test);
table(tab.test.predict>0.5,tab.test$chd);
# Analyse : une telle approche permet de prédire un risque d'accident cardiaque à partir des caractéristiques d'un individu. Par exemple, on recueille les informations d'un individu et on détecte un comportement ou profil à risque. L'individu sera alors surveillé voire traité préventivement en insistant sur les facteurs de risque (choleterol, hypertension).

## 6 - Variables explicatives
tab.glm$coefficients;
# Analyse : les variables les plus significatives sont l'historique familiale, le cholesterol (ldl) alors que les moins significatives sont de manière surprenante l'alcool et la pression sanguine.
