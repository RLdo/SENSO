# Cartographie des préférences

##  Objectif de la méthode

L’objectif de la cartographie des préférences est d’identifier, au sein de l’espace produit sensoriel, les zones associées à une forte ou une faible appréciation. 
Il s’agit de croiser deux sources d’information : les profils sensoriels mesurés par un panel d’experts, et les notes hédoniques attribuées par des consommateurs naïfs. 
Cette approche permet de localiser, dans l’espace produit testé, les régions où se situeraient les produits idéaux.

##  Description des données

Pour réaliser une cartographie des préférences, il nous faut :

Un recueil de **données sensorielles par un panel d’expert**, les données sont des données purement descriptive et son structuré de la même manière qu’un jeu de données issus d’une QDA (voir onglet QDA)

Un recueil de **données hédonique par un panel de consommateurs naïfs**, en très grand nombre. 
Ce jeu de données est structuré avec en ligne les fiches de dégustation (un produit par un juge) et en colonne le juge, le produit et la note hédonique. 

Les juges experts évaluent chaque produit selon un ensemble descripteurs sensoriels quantitatifs (ex. sucré, ferme, juteux), tandis que les consommateurs attribuent une note d’appréciation globale sur une échelle hédonique (souvent de 0 à 10).

###  Exemple : 
Pour illustrer nos notes, nous utilisons le jeu de données cocktail, inclut dans le package SensoMineR.
Les données utilisées ici concernent 16 cocktails.

Il existe 3 fichiers correspondant : à la composition des cocktails, à leur description sensorielle, et aux scores hédoniques.
Pour la composition des cocktails : les concentrations de mangue, banane, orange et citron sont connues ;
Pour la description sensorielle : chaque cocktail a été évalué par 12 panélistes selon 13 descripteurs sensoriels (seules les moyennes par cocktail sont fournies) ;
Pour les données hédoniques : chaque cocktail a été évalué par 100 consommateurs sur une échelle structurée de 0 à 10, selon leur niveau de déplaisir (0) ou de plaisir (10).

Nous ne nous intéresserons pas à la composition des cocktails auourd'hui.

```{r}
library(SensoMineR)
library(kableExtra)
library(tidyverse)
data(cocktail)
tabc1  <-head(compo.cocktail)
tabc2 <-head(hedo.cocktail)
```


Voici une rapide visualisation de nos jeux de données : 


 # CHANGER STYLE TABLEAU
senso.cocktail : un data frame de 16 lignes et 13 colonnes : chaque cocktail a été évalué par 12 panélistes selon 13 descripteurs sensoriels ;

```{r}
kable(tabc2 , digits = 2, align = "c", caption = "6 premières lignes du jeu de hedo.cocktail", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = T, background = "#ff7f00") %>%
  row_spec(1:nrow(tabc1), background="#FFF0F5")%>%
  column_spec(2, bold = T, color="#df6d14")
```

hedo.cocktail : un data frame de 16 lignes et 100 colonnes : chaque cocktail a été évalué sur une échelle structurée de 0 à 10 par 100 consommateurs, en fonction de leur déplaisir (0) ou plaisir (10).


```{r}
kable(tabc3 , digits = 2, align = "c", caption = "6 premières lignes du jeu de senso.cocktail", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = T, background = "#ff7f00") %>%
  row_spec(1:nrow(tabc1), background="#FFF0F5")%>%
  column_spec(2, bold = T, color="#df6d14")
```

## Construction de l’espace sensoriel (ACP)

Afin de représenter les produits dans un espace sensoriel réduit, nous appliquons une Analyse en Composantes Principales (ACP) sur le tableau des profils sensoriels. 
Les notes des descripteurs sont préalablement ajustées selon le modèle suivant :

# ÉCRIRE MODELE

 : effet fixe du produit

 : effet juge

Les moyennes ajustées par produit sont extraites et servent de base pour l’ACP. 
Cette méthode permet de projeter les produits dans un repère de dimension réduite, tout en conservant un maximum de variance sensorielle. 
Les données sont centrées et réduites afin de neutraliser les différences d’échelle entre descripteurs.
Les deux premières composantes principales (Dim 1 et Dim 2) sont retenues pour la cartographie : elles concentrent l’essentiel de la variance sensorielle totale de du jeu de données. 
Chaque produit est donc positionné dans ce plan selon ses coordonnées (Dim1, Dim2), issues de la projection.

Dans notre exemple, nous réalison l'ACP sur le jeu de données senso.cocktail, en ajoutant le jeu de données compo.cocktail en illustratfi. 
La composition des coktail ne va donc pas influer la construction des axes de l'espace produit, mais nous donneras des informations supplémentaire pour notre analyse.


```{r}
res.acp <- PCA(senso.cocktail)
```

# Interprétation

## Modélisation du liking (préférences)


# MODELE



## Cartographie des zones de préférences

Nous souhaitons ensuite relier la position des produits dans la représentation sensorielle à l’appréciation des consommateurs. 
Pour cela, un modèle de régression linéaire est ajusté pour chaque consommateurs, prenant en entrée les coordonnées ACP des produits qu’il a notés :

# Modèle

Une fois tous les modèles construits, nous allons discrétisé l'espace sensoriel en une grille fine (comme un plateau de jeu de go). 

IMAGE GRILLE FINE

Pour chaque point de cette grille, nous utilisons tous les modèles des consommateurs pour prédire leur note de liking pour ce point.

Chaque juge naif est ainsi modélisé par un plan de régression qui lui est propre, permettant d’estimer sa note pour n’importe quelle position dans l’espace sensoriel. 
Ce choix repose sur l’hypothèse émise que deux produits proches dans l’espace sensoriel devraient être appréciés de manière similaire.

Selon les hypothèses psychophysiques retenues, une version quadratique peut être testée (pour tenir compte d’effets de saturation), si on estime que liking est en constante croissance, notre modèle ne prendre pas en compte les effets quadratique, à contrario, si nous estiment qu'à partir d'un certain seuil, sur un attribut sensoriel, le liking diminuerai, il faut inclure des effets quadratiques dans le modèle 


Un seuil de liking est fixé (par exemple, on peut considérer qu’un produit ayant reçu une note supérieure ou égale à  6/10 est un produit apprécié). 
Pour chaque point de la grille, on détermine le pourcentage de consommateurs dont la note prédite dépasse ce seuil. Ce pourcentage permet de définir des zones de fort liking et des zones de disliking.


Il n’y a plus qu'à sortir les crayons de couleurs : la zone sera coloriée en rouge si c’est le pourcentage est fort, et en bleu si le pourcentage est bas. 

On obtient ainsi une carte où chaque zone reflète l’intensité prédite d’appréciation. 
L’objectif est d’identifier les zones où un produit hypothétique aurait le plus de chances de plaire à une large part de consommateurs.

Voici la cartographie des préférences pour notre exemple avec les cocktails:


```{r}
res.carto <- carto(res.acp$ind$coord[,1:2], hedo.cocktail)
```

# INTERPRETATION



# Retour au sensoriel : profil du produit idéal dans l’espace produit
Une fois la zone de fort liking identifiée, il est possible d’en extraire les coordonnées (Dim1, Dim2) correspondantes. 
Pour revenir à une interprétation sensorielle, on utilise la projection inverse de l’ACP, ce qui nous permet ainsi d’obtenir un profil sensoriel théorique du produit idéal.
## Limites : 
L’ACP ne conserve qu’une partie de la variance totale : la projection dans un plan 2D entraîne une perte d’information. 
Il convient de vérifier que Dim1 et Dim2 expliquent une part suffisamment importante de la variabilité sensorielle (% d’inertie).  
*L’inertie représente la variance totale expliquée par chaque axe principal. Elle mesure la part d’information du jeu de données capturée par chaque dimension.*


# CONCLUSION
La cartographie des préférences est très apprécié en marketing et permet de cibler franchement les attributs sensoriels attendus d'un produit idéal. 
Toutefois, cette méthode présentes de certaines limites. Un coût très élevé (Panel d'expert, très grand nombre de consommateur, plusieurs produits à tester...) et reste cantonnée à l'espace produit sensoriel. 
Et si le VRAI produit idéal se trouvait à l'extérieur de cet espace? 





