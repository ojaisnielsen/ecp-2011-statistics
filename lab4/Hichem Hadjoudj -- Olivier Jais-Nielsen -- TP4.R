# TP4 : Méthodes de Régressions
# 4 Novembre 2010 - ECP
# Hichem HADJOUDJ - Olivier JAIS-NIELSEN


# Remarque : avant toute chose, on définit le path :
setwd("D:\\Olivier\\Documents\\Centrale S5 S6\\SDM")

# Liste des documents à inclure dans le dossier source : 
#	- immo.txt
#	- test-immo.txt (à créer avec les valeurs à tester : "surface \n 155 \n 178 \n 200 \n 220 \n 255")
#	- prostate.txt
#	- UsCrime.txt

## ATTENTION : il faut charger l'ensemble de l'exercice à chaque fois, pour éviter les conflits de noms

##############################################################
#	          I - Régression linéaire multiple	             #
##############################################################

### - 1 - Chargement des données
tab=read.table('immo.txt',header=T,sep=';',dec='.')
nrow(tab)
# Analyse : il y a 20 observations.

##############################################################

### - 2 - Interprétation Graphique
plot(tab)
# Analyse : les nuages de points sont concentrés autour d'une direction.
cor(tab)
# Analyse : la matrice de corrélation présente des termes très proche de 1, ce qui confirme l'observation faite sur le graphique.

##############################################################

### - 3 - Modèles de regression lineaire
# Modèle : Y représente le montant de la transaction (3ème colonne) et X les deux autres variables, on modèlise par Y/X suit une loi normale
res=lm('transaction~.',data=tab)

##  - 3.1 - Res
res
# Analyse : on obtient les paramètres de la régression : transaction = 309.7 + 2.6 * surface + 0.045 * valeur
# Le prix du bien augmente de 2634 € si la surface augmente de 1 m2

## - 3.2 - Res détaillé
attributes(res)

## - 3.3 - Graphique
x11()
plot(tab$transaction,res$fit)
abline(a=0,b=1,col='red')
# On constate que la regression suit assez correctement la bissectrice.

## - 3.4 - Modèle réduit
# On peut faire deux régressions à une seule variable car les corrélations initiales étaient très correctes.

res1=lm('transaction~.',data=tab[,c(1,3)])
res2=lm('transaction~.',data=tab[,c(2,3)])

x11()
plot(tab$transaction,res1$fit)
abline(a=0,b=1,col='red')
x11()
plot(tab$transaction,res2$fit)
abline(a=0,b=1,col='red')
# Gain pratique : On peut estimer par rapport à un seul des deux paramètres.

# - 3.4.1 - Prédiction
test=read.table('test-immo.txt',header=T)
prediction=predict.lm(res1,test)
x11()
plot(prediction,test$surface)
title(sprintf('Prédictions'));

##############################################################

### - 4 - Test et intervalles de confiances
summary(res)
res$coef

## - 4.1 - R2
# R2 = Var(Y^) / Var (Y). Plus R2 est proche de 1, plus la regression est bonne.
# Ici R2 vaut 0.8344, la regression est plutôt correcte
# On détermine la significativité par le F-test. La p-value est de 2.3*10-7 ce qui est trËs inférieur ‡ 5%. Si le risque accepté est 5%, l'hypothèse nulle (tous les coefficients sont nuls) est rejettée; le modèle est donc significatif globalement.
res$pvalue

## - 4.2 - 
res$coefficients
# Les codes représentent l'ordre de grandeur des p-values associées aux coefficients. Plus le nombre d'étoiles est grand, plus la p-value est petite est donc la significativité du coefficient élevée.
# On remarque donc que le coefficient associé à la surface est bien plus significatif que celui associé à la valeur initiale d'achat.

confint(res,'surface') 
# L'estimation donne un intervalle de confiance très large donc inutilisable.

## - 4.3 -
x11()
plot(tab$transaction,res$residuals)
# On remarque que la distribution des résidus est bien centrée sur 0. Ce qui est conforme avec le modèle adopté (erreur de moyenne nulle).

x11()
qqnorm(res$residuals)
qqline(res$residuals)
# Le normal QQ plot montre des points assez proche de la ligne théorique, ce qui tend à valider le modèle d'erreur gaussienne de moyenne nulle.


## - 4.4 - 
x11()
par(mfrow=c(2,2));
plot(res)

## - 4.5 - 
library(gplots)
prediction=predict(res1, test, interval="confidence")
x11()
plotCI(test$surface, prediction[,1], ui = prediction[,3], li = prediction[,2])
title(sprintf('Prédiction et intervalle de confiance'));
# On constate que l'intervalle de confiance est proportionnel à la variable explicative (la surface); cela signifie que la précision est approximativement constante.


##############################################################

### - 5 - Validation croisée

crossValidation<-function(trainIndices) 
{
	train = tab[trainIndices,]
	test = tab[-trainIndices,]	
	model=lm('transaction~.',data=train)
	prediction=predict.lm(model,test)	
	err = norm(matrix(test[,3] - prediction), type='F') / length(prediction)
	return(err)
}

mean = 0
for (n in 1:10)
{
	i = sample(nrow(tab), 0.75 * nrow(tab))	
	mean = mean + crossValidation(i)	
}
mean = mean / 10

mean * length(tab[,3])/ sum(tab[,3])

# On obtient une erreur moyenne d'environ 2% de la valeur moyenne réelle moyenne.

##############################################################
#  F I N   D E   L A   P A R T I E   1   ( M U L T I P L E ) #
##############################################################



##############################################################
# 	   II - Régression multiple et sélection de modèles      #
##############################################################


### - 0 - Chargement des données
tab=read.table("UsCrime.txt", header=TRUE, sep="",dec=".");
tab;
Y=tab[,1]
X=tab[,2:14]

##############################################################

### - 1 - Analyse des données
# Il y a 47 observations, une par état américain étudié.
plot(tab)
# Analyse : on remarque une forte corrélation entre Ex0 et Ex1 (dépenses concernant la police pour 1959 et 1960) et une légère corrélation entre U1 et U2 (taux de chômage pour les 14-24 ans et les 35-39 ans) ainsi qu'entre W et X (relation inversée) (valeur médiane de biens et revenus transférables et nombre de famille dont les revenus sont inférieurs à la moitié du revenus moyen).
cor(tab)
# La matrice des corrélations confirme ces corrélations, notamment entre U1 et U2 (0.9936), entre U1 et U2 (0.7459), entre W et X (-0.8840) mais font également apparaître de nombreuses autres corrélations.

##############################################################

### - 2 - Modèle de régression multiple
# Modèle explicite : Y = A0 + A.X + Eps où A est une matrice n*p et A0 et Eps sont des vecteurs n.

regp=lm("R~.",data=tab)
summary(regp)

## - a - Analyse rapide du modèle : on remarque que R2 = Var(Y^) / Var (Y) vaut 0.7692, la regression est plutot correcte.

## - b - Significance globale : La p-value est ici de 3.7*10-7 ce qui est très inférieur à 5%. Si le risque accepté est 5%, l'hypothèse "tous les coefficients sont nuls" est rejetée et la régression est significative globalement.

## - c - Significance des coefficients : pour cela on utilise le code en étoiles. On remarque que les coefficients les plus significatifs sont Ed (éducation) et X (famille en dessous de la moitié du revenu médian) (**) suivis de Age et U2 (chômage des 35-39) (*)


##############################################################

### - 3 - Sélection de modèles

## - a -
# Fonction "step" : 
reg=lm(R~.,data=tab); summary(reg);
s0=step(reg); #defaut backward et AIC 
reg0=lm(formula(s0),data=tab); summary(reg0)
# Analyse : Cette sélection a permis une réduction du nombre de variables de 14 à 6. R2 vaut 0.7478 (contre 0.7692) ce qui montre que la régression reste de qualité correcte, la p-value est en 10-10 ce qui montre que le modèle est très significtif globalement et l'ensemble des coefficients sont très significatifs individuellement. Néanmoins il y a une perte d'informations puisque on réduit le nombre de variables du modèle.

## - b -
xnames=names(tab)[2:ncol(tab)];
for (k in 1:length(xnames)) {
	print(sprintf(’---> %s’,xnames[k]));
	fk=paste("R ~", paste(xnames[-k], collapse= "+"));
	regk=lm(as.formula(fk),data=tab);
	print(summary(regk));
	mes=sprintf("%5s --> %5.2f",xnames[k],AIC(regk));
	print(mes)
	}

##############################################################
#      F I N   D E   L A   P A R T I E   2   ( A I C )       #
##############################################################



##############################################################
#  					III - Régression ridge		             #
##############################################################

library(MASS)
library(ade4)

### - 0 - Chargement des données
tab=read.table("UsCrime.txt", header=TRUE, sep="",dec=".");
Y=tab[,1]
X=tab[,2:14]


##############################################################

### - 1 - Analyse des données et régression ridge
# La régression de type Ridge consiste à déterminer une régression linéaire Y = X * Beta minimisant la quantité, Norm2(Y - X*Beta) + lambda * Norm2(Beta)
# Cette méthode pénalisante peut être également vue comme une régression multiple à coefficients bornés : Y = X * Beta avec Norm2(Beta)<c

# Analyse des données : Ce framedata contient 47 observations de 14 variables dont une variable que l'on souhaite expliquer : le taux de crime et 13 variables démographiques et économiques. (Il s'agit d'une étude statistique sur le nombre de crimes dans 47 états américains en 1960.

##############################################################

### - 2 - Régression ridge
ridge=lm.ridge(R~.,data=tab,lambda=c(0,100))
coef(ridge)
# L'objet ridge$coef renvoit les coefs sur une différente échelle

resmultiple=lm(R~.,data=tab)
coef(resmultiple)
# On retrouve bien la régression multiple simple (avec lambda=0)

##############################################################

### - 3 - Optimisation de la régression
resridge=lm.ridge(R~.,data=tab,lambda=seq(0,100,0.01))
x11()
plot(resridge$GCV)
x11()
plot(resridge)
# On constate qu'il existe une valeur de lambda pour laquelle la validation croisée est minimale.
# Il faut retenir le modèle pour lequel lambda minimise le risque quadratique, on peut le déterminer soit à partir du tracé des coefficients ou plus facilement à partir du tracé de la validation croisée.
lambda=as.double(names(which.min(resridge$GCV)))
Coefridge=coef(lm.ridge(R~.,data=tab,lambda=lambda))

##############################################################

### - 4 - Erreur quadratique moyenne
Yridge=as.matrix(cbind(matrix(1,47,1),X))%*%as.vector(Coefridge)

Errridge=norm(as.matrix(Y) - Yridge, type='F')^2 / length(Yridge)
Errridge
# L'erreur quadratique moyenne est assez importante.


##############################################################
#     F I N   D E   L A   P A R T I E   3   ( R I D G E )    # 
##############################################################


##############################################################
#  					IV - Régression Lasso    	             #
##############################################################

### - 1 - Définition
## - 1.1 - La régression de type Lasso consiste à déterminer une régression linéaire Y = X * Beta minimisant la quantité, Norm2(Y - X*Beta) - lambda * Norm1(Beta)
library(lars)

## - 1.2 - Chargement des données
prostate=read.table("prostate.data",header=T,dec='.', row.names = 1)
train = subset(prostate, train==TRUE)
test = subset(prostate, train==FALSE)

Xtrain = data.matrix(train[,1:8])
Ytrain = train[,9]

Xtest = data.matrix(test[,1:8])
Ytest = test[,9]

##############################################################

### - 2 - Régréssion de type Lasso
## - 2.1 - Régression
reslasso=lars(Xtrain,Ytrain,type="lasso")

## - 2.2
plot(reslasso)

## - 2.3
x11()
plot(reslasso$lambda)

##############################################################

### - 3 - Coefficients lambda
coef=predict.lars(reslasso,Xtrain,type="coefficients",mode="lambda",s=0)
x11()
barplot(coef$coefficients)

##############################################################

### - 4 - Affichage en fonction de lambda
for (lambda in c(1,2,4))
{
	coef=predict.lars(reslasso,Xtrain,type="coefficients",mode="lambda",s=lambda)
	x11()
	barplot(coef$coefficients)
}

##############################################################

### - 5 - Erreur quadratique
pY=predict.lars(reslasso,Xtest,type="fit",mode="lambda",s=2)
Errlasso = norm(data.matrix(pY$fit - Ytest), type='F')^2 / length(Ytest)
Errlasso


##############################################################
#     F I N   D E   L A   P A R T I E   4   ( L A S S O )    #
##############################################################


##############################################################
#  				  V - Comparaison des méthodes	             #
##############################################################

### - 1 - Partition des données
tab=read.table("UsCrime.txt", header=TRUE, sep="",dec=".");
i=sample(nrow(tab),0.7*nrow(tab))
train=tab[i,]
test=tab[-i,]

##############################################################

## - a - Méthode de régression multiple

model=lm('R~.',data=train)

predictionlm.train=predict.lm(model,train)	
Errlm.train = norm(matrix(train[,1] - predictionlm.train), type='F')^2 / length(predictionlm.train)

predictionlm.test=predict.lm(model,test)	
Errlm.test = norm(matrix(test[,1] - predictionlm.test), type='F')^2 / length(predictionlm.test)

##############################################################

## - b - Méthode de régression multiple et sélection AIC

reg.train=lm(R~.,data=train);
s0=step(reg.train);
reg0.train=lm(formula(s0),data=train);
predictionAIC.train=predict.lm(reg0.train,train)
ErrAIC.train=norm(matrix(train[,1] - predictionAIC.train), type='F')^2 / length(predictionAIC.train)

predictionAIC.test=predict.lm(reg0.train,test)
ErrAIC.test=norm(matrix(test[,1] - predictionAIC.test), type='F')^2 / length(predictionAIC.test)

##############################################################

## - c - Méthode de régression ridge

resridge.train=lm.ridge(R~.,data=train,lambda=seq(0,100,0.01))
lambda.train=as.double(names(which.min(resridge.train$GCV)))
Coefridge.train=coef(lm.ridge(R~.,data=tab,lambda=lambda.train))
Yridge.train=as.matrix(cbind(matrix(1,32,1),train[,2:14]))%*%as.vector(Coefridge.train)
Errridge.train = norm(as.matrix(train[,1]) - Yridge.train, type='F')^2 / length(prediction)

Yridge.test=as.matrix(cbind(matrix(1,15,1),test[,2:14]))%*%as.vector(Coefridge.train)
Errridge.test = norm(matrix(test[,1] - Yridge.test), type='F')^2 / length(prediction)



##############################################################

## - d - Méthode de régression lasso
library(lars)
reslasso.train=lars(data.matrix(train[,2:14]),train[,1],type="lasso")

lambdas.train = seq(0, 100, 0.01)
errlasso.train = array(0, length(lambdas.train))
for (i in 1:nrow(err.train))
{
	pY.train=predict.lars(reslasso.train,train[,2:14],type="fit",mode="lambda",s=lambdas.train[i])
	errlasso.train[i] = norm(data.matrix(pY.train$fit - train[,1]), type='F')^2 / length(test[,1])
}
lambda.train = lambdas.train[which.min(err.train)]
Errlasso.train=min(errlasso.train)

pY.test=predict.lars(reslasso.train,test[,2:14],type="fit",mode="lambda",s=lambda.train)
Errlasso.test = norm(data.matrix(pY.test$fit - test[,1]), type='F')^2 / length(test[,1])

##############################################################

### - 3 - Comparaison des modèles et conclusions
# Nous traçon finalement les erreurs quadratiques moyennes pour chacun des modèles pour la base d'apprentissage et pour la base de test
x11()
barplot(c(Errlm.train,Errlm.test,ErrAIC.train,ErrAIC.test,Errridge.train,Errridge.test,Errlasso.train,Errlasso.train),col=c("orchid","orchid4","orange","orange4","slateblue","slateblue4","palegreen","palegreen4"),space=c(0,0.5,1.5,0.5,1.5,0.5,1.5,0.5),legend=c("Lm Train","Lm Test","AIC Train","AIC Test","Ridge Train","Ridge Test","Lasso Train","Lasso Test"))
title(sprintf('Erreur quadratique moyenne par modèle'));


##############################################################
#        				F I N   D U   T P  	  	             #
##############################################################