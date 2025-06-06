# SENSO
# Données JAR ("Just About Right")

L’analyse JAR (“Just About Right” ) permet de relier des perceptions spécifiques (trop/pas assez) à l’appréciation globale d’un produit. 
Elle répond à la question : quels attributs sensoriels repoussent ou séduisent les consommateurs ? Cela permet d’identifier des drivers of liking (à renforcer) et des drivers of disliking (à corriger).  
Un driver of disliking est un défaut perçu associé à une baisse de note d’appréciation et un driver of liking est une caractéristique valorisée associée à une hausse de cette note.
Les résultats d'une analyses JAR peuvent se traduirent directement par des axes d’ajustement précis (si c’est “trop” il faudra réduire, si c’est “pas assez” il faudra augmenter).

# Mise en place du test

Lors d’un test JAR, les consommateurs évaluent un produit à partir d’une liste préétablie de descripteurs sensoriels (ex. sucré, croustillant, onctueux), définie en amont. 
Pour chaque descripteur, ils indiquent si l’intensité perçue leur semble insuffisante (pas assez), excessive (trop) ou optimale  (“Just About Right”). 
Cette évaluation repose sur une échelle verbale, généralement en 3 ou 5 points (trop, très, JAR, peu, pas assez), centrée autour de l’optimum perçu (JAR). 
En complément, chaque consommateur donne une note d’appréciation globale, souvent sur une échelle de type hédonique (par exemple de 0 = "je déteste" à 10 = "j’adore").

# Receuil de données
Un recueil de données JAR se présente donc sous la forme d'un jeu de données avec une ligne par évaluation individuelle, une colonne pour la note de liking, une colonne pour chaque descripteur sensoriel codé en modalité verbale (JAR, Trop, Pas assez). 

```{r}
orange <- read.csv2("orange.csv")
summary(orange, maxsum = 8)

for (j in c(1:2,4:9)) orange[,j] <- as.factor(orange[,j])

```


Voici une visualisation des données avec lesquelles nous allons jouer pour illustrer la méthode JAR.
```{r}
ggplot(tab, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("Modalities") +
  ylab("Number of occurrences") +
  ggtitle("Distribution of modalities for the attribute Io") +
  theme_minimal()
```
Pour cet exemple, nous avons à faire à 8 jus d'orange évalué par ?? consommateurs sur la base de ?? attributs sensoriels. 


Voici à quoi ressemble un jeu de données JAR


```{r}
for (j in 4:9) levels(orange[,j]) <- c("ne","ne","JAR","tm","tm")

taborange  <-head(orange)
kable(taborange , digits = 2, align = "c", caption = "6 premières lignes de orange", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = T, background = "#ff7f00") %>%
  row_spec(1:nrow(df), background="#FFF0F5")%>%
  column_spec(2, bold = T, color="#df6d14")
```

# Modèle statistique 
L’analyse JAR se concentre sur les perceptions extrêmes des consommateurs, en excluant volontairement les mentions “Just About Right” qui correspondent à un niveau optimal perçu. 
Le modèle va donc uniquement prendre en compte les modalités de “défauts”, c'est-à-dire “Trop” et “Pas assez”, pour ainsi cibler les écarts à l’idéal sensoriel.

La construction d’un tableau disjonctif permet de passer d’un ressenti verbal ("trop sucré", "pas assez croustillant") à une base de données exploitable par les outils statistiques. 
Concrètement, on va créer pour chaque modalité extrême de chaque descripteur, une indicatrice binaire (0 = non perçu ; 1 = perçu). Chaque colonne correspond donc à un défaut sensoriel clairement identifié, intégré comme effet fixe explicite.
Cette opération permet d’intégrer les modalités comme effets fixes explicites dans le modèle linéaire.
Il faut exclure volontairement les mentions “Just About Right”, car elles ne représentent pas un défaut perçu. Ainsi, on centre l’analyse sur la déviation par rapport à l’optimum perçu, nous permettant de quantifier l’impact d’un défaut perçu sur la note globale de liking Ce qui nous permet donc de prioriser les défauts les plus pénalisants à corriger dans une prochaine version du produit.


```{r}
# install.packages("ade4")
library(ade4)
orange.dummy <- acm.disjonctif(orange[,4:9])
orange.dummy <- cbind(orange[,1:3], orange.dummy)
orange.dummy[1:5,]

```


Le modèle s’écrit sous la forme :
<div class="box-girly"> $$ Y_{ijkl} = \mu + \sum_{a} \beta_a \cdot X_{a,ijkl} + \gamma_j + \lambda_k + \varepsilon_{ijkl} $$ </div>
Y<sub>ijkl</sub> : score d’appréciation hédonique (quantitatif, ex. 0–10)
X<sub>a</sub> : indicatrices des modalités extrêmes des descripteurs sensoriels
β<sub>a</sub> : effets marginaux de ces défauts
γ<sub>j</sub> : effet aléatoire du juge
λ<sub>k</sub> : effet fixe du produit
ε<sub>ijkl</sub> : erreur résiduelle (supposée iid, normale centrée)

Les juges sont modélisés ici comme un effet aléatoire, dans un modèle linéaire mixte. Car on suppose qu’ils représentent un échantillon aléatoire d’une population plus large de consommateurs naïfs. 
Chaque modalité de défaut devient une variable. On ne dit plus seulement “les gens trouvent ça trop (attribut sensoriel)”, on mesure que “ce défaut fait perdre x point en liking moyen”.

```{r}
library(SensoMineR)
res.jar <- JAR(orange, col.p = 2, col.j = 1, col.pref = 3, jarlevel="JAR")
res.jar$penalty2
```
Les coefficients issus du modèle indiquent donc combien de points en appréciation globale sont perdus lorsqu’un défaut sensoriel est perçu (trop / pas assez). 
Pour chaque modalité extrême testée, l’hypothèse nulle est H₀ : β = 0 (le défaut n’a aucun effet sur la note de liking). Une p-value inférieure à 0,05 permet de rejeter cette hypothèse et conclure à un impact significatif.




```{r}
# Penalties can also be estimated for each product.

orange.dummy.2JPR <- orange.dummy[orange.dummy$Juice == "2JPR",]

# Use the AovSum() function to estimate your model for which the liking is explained by the presence or absence of the defects.
res.penalty.one <- AovSum(Liking ~ Nc.ne + Nc.tm + Io.ne + Io.tm + Su.ne + Su.tm +
                            Ac.ne + Ac.tm + Bt.ne + Bt.tm + Pu.ne + Pu.tm,
                          data = orange.dummy.2JPR)
res.penalty.one$Ttest
```
# INTERPRETATION

# CONCLUSIONS À TIRER 

Pour guider les décisions produit, il est utile de croiser l’impact (estimate) et la fréquence d’apparition de chaque défaut. Un défaut perçu par 50 % des consommateurs et faisant perdre 1 point d’appréciation mérite une attention immédiate. 
À l’inverse, un défaut très rare mais très pénalisant peut être secondaire. 
Les attributs avec fort impact et forte fréquence de défaut doivent être priorisés dans la reformulation.
