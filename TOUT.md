---
title: "Notes de cours - Sensométrie - 2025"
author: "Pauline CAMARD, Erell DOYEN, Riwal Le Moan Delalande"
output: 
  html_document: 
    toc: true
    number_sections: true
    toc_depth: 2
    toc_float: 
      collapsed: false
      smooth_scroll: true
editor_options: 
  markdown: 
    wrap: sentence

---

```{r, include=FALSE}
library(tidyverse)
library(pivottabler)
library(FactoMineR)
library(kableExtra)
library(fmsb)
library(writexl)
library(SensoMineR)
```

```{=html}
<style>
@import url('https://fonts.googleapis.com/css2?family=Lora&family=Raleway:wght@400;500;600;800&display=swap');

body {
  font-family: 'Lora', serif;
  font-size: 16px;
  line-height: 1.6;
  font-weight: 400;
  color: #5E4443;
  margin: 0;
  padding: 0;
}

h1 {
  font-family: 'Raleway', sans-serif;
  font-weight: 800;
  font-size: 3em;
  color: #CB2C30; /* rouge fraise */
  letter-spacing: 1px;
  margin-top: 0.5em;
}

h2 {
  font-family: 'Raleway', sans-serif;
  font-weight: 600;
  font-size: 1.8em;
  color: #E78B90; /* rose pulpe */
  letter-spacing: 0.5px;
  margin-top: 1.5em;
}

h3 {
  font-family: 'Raleway', sans-serif;
  font-weight: 500;
  font-size: 1.4em;
  color: #5E4443; 
  margin-top: 1em;
}

h4, h5 {
  counter-reset: none;
}

h4 > .header-section-number,
h5 > .header-section-number {
  display: none;
}

footer {
  background: linear-gradient(to right, #E78B90, #FFF1F2 50%, #5e9732);
  color: #5E4443; /* texte brun lisible sur fond clair */
  padding: 2em;
  text-align: center;
}

.box {
  background-color: #e3f4df; 
  border: 2px dashed #ff69b4; /* rose bonbon */
  border-radius: 15px;
  padding: 20px;
  margin: 15px 0;
  box-shadow: 0 0 10px #ffc0cb;
}

.sparkle {
  background: linear-gradient(135deg, #FFFFFF 0%, #fbd3d7 100%);
  animation: glitter 2s infinite alternate;
  padding: 10px;
  border-radius: 8px;
  text-align: left;
}

#TOC {
  font-family: 'Raleway', sans-serif;
  color: #5e9732;
  background-color: #ffffffcc;
  border: 1px solid #5e9732;
  padding: 1em;
  border-radius: 10px;
  box-shadow: 0 0 10px #dcdcdc;
}

#TOC a {
  color: #5e9732;
  text-decoration: none;
}

#TOC a:hover {
  text-decoration: underline;
  color: #CB2C30;
}

@keyframes glitter {
  0% { box-shadow: 0 0 10px #ff99cc; }
  100% { box-shadow: 0 0 20px #ff66cc; }
}


</style>



```
# Données QDA (Quantitative Descriptive Analysis)

## Contexte

Variétés de fraises?
<br> Comment ça variétés de fraises?
<br> Elle a quoi ma fraise?
<br> Une fraise, c’est une fraise non?
<br>

On compte plus de 600 variétés de fraises, mais perçoit-on réellement une différence d'une variété à l'autre ?
<br>

Quand un producteur affirme que la variété qu’il a sélectionnée avec soin et cultivée avec amour est la plus sucrée, est-ce vrai ?

Ou est-ce seulement une astuce marketing ?
<br> Le goût ou la texture de toutes les variétés de fraises sont-ils perçus de la même manière ?
<br> Y’a t-il des fraises significativement plus rouges que d'autres ?
<br>

Pour répondre à ces questions, nous avons mené notre enquête.
Ou plutôt notre analyse (sensorielle évidemment).

## Cadre de l’analyse

4 variétés de fraises, toutes cultivées par une petite productrice près de Rennes, ont été évaluées par 12 juges naïfs (étudiants à l'Institut Agro de Rennes) lors de deux sessions successives selon 10 descripteurs sensoriels.
Une fraise par variété et par session a été dégustée.

Les descripteurs sensoriels étaient les suivants : Taille, Couleur, Odeur, Ferme, Juteux, Fondant, Sucrée, Acide, Arôme et Fraise des bois

La réponse était obligatoire.
Les notes pouvaient prendre des valeurs de 0 (très faiblement perçu) à 10 (énormément perçu).

### Jeu de données

Les données ont été recueillies grâce au logiciel Fizz®, spécialisé dans la mise en place de tests sensoriels.

Comme tout bon statisticien le sait, un jeu de données, bien structuré et propre, est la condition préalable à toute analyse fiable et pertinente.

Notre premier jeu de données, intitulé fraise1, se présente sous la forme d’un dataframe de 96 lignes et 14 colonnes.

Chaque ligne correspond à une fiche de dégustation individuelle.
Nous avons 12 juges × 2 sessions × 4 variétés = 96 fiches de dégustation.
Chaque colonne contient soit : l’une des 10 notes attribuées aux descripteurs sensoriels, soit une variable contextuelle liée à la dégustation : le numéro du juge, la variété de la fraise dégustée, le numéro de la session.

```{r, include=TRUE, echo= FALSE}

load("fraise.Rdata")
df <-head(fraise1)

kable(df, digits = 2, align = "c", caption = "6 premières lignes de fraise1", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = T, background = "#FADADD") %>%
  row_spec(1:nrow(df), background="#FFF0F5")%>%
  column_spec(1, bold = T, color="black")
```

### Etude des données

Nous nous intéressons en particulier aux variétés de fraises.
Le graphique ci-dessous permet de visualiser les ressemblances et différences entre les 4 variétés de fraises testées.

```{r, include=TRUE, echo= FALSE, error=FALSE}
df_moyennes <- fraise1 %>%
  group_by(ProductName) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

attributs <- names(df_moyennes)[-1]
max_vals <- rep(10, length(attributs))
min_vals <- rep(0, length(attributs))


df_radar <- df_moyennes %>%
  select(-ProductName) %>%
  as.data.frame()
rownames(df_radar) <- df_moyennes$ProductName

df_radar_fmsb <- rbind(max_vals, min_vals, df_radar)
colnames(df_radar_fmsb) <- attributs

colors_border <- c("#C94C4C", "#7CBA59", "#3FBAC2", "#A678B6")
colors_in <- adjustcolor(colors_border, alpha.f = 0.2)


radarchart(df_radar_fmsb,
           axistype = 1,
           pcol = colors_border,
           plwd = 2,
           plty = 1,
           cglcol = "grey",
           cglty = 1,
           axislabcol = "grey20",
           caxislabels = seq(0, 10, 2),
           cglwd = 0.8,
           vlcex = 0.9,
           title = "Profil sensoriel moyen par variété"
)

legend("topright", legend = rownames(df_radar), 
       col = colors_border, 
       lty = 1, lwd = 2, bty = "n")

```

Nous avons donc observé des différences dans la perception des 4 variétés de fraises testées.

Pour creuser et affiner notre observation, nous adoptons une approche en trois temps : 1.
Analyse unidimensionnelle : observer la différence variété par variété, descripteur par descripteur.
2.
Analyse multidimensionnelle : représenter les produits dans un espace sensoriel global (ACP).
3.
Validation par répétition : estimer la variabilité des résultats grâce à des techniques de bootstrap.

## Analyse UNIDIMENSIONNELLE

### Modélisation (ANOVA)

Pour analyser les différences entre les variétés de fraises, nous utilisons un modèle d’analyse de la variance (ANOVA) qui prend en compte trois effets principaux :

l’effet de la variété (α<sub>i</sub>) l’effet du juge (γ<sub>j</sub>) l’effet de la session (λ<sub>k</sub>)

L’objectif de l’ANOVA est de tester si les moyennes des notes attribuées varient significativement d’une variété à l’autre, en tenant compte des différences liées aux juges ou aux conditions de test.

Le modèle est le suivant :

::: box
$$ Y_{ijk} = \mu + \alpha_i + \gamma_j + \lambda_k + \epsilon_{ijk} $$
:::

Où :

Y<sub>ijk</sub> : note attribuée à un descripteur μ : moyenne globale α<sub>i</sub> : effet de la variété i  γ<sub>j</sub> : effet du juge j  λ<sub>k</sub> : effet de la session k  ε<sub>ijk</sub> : erreur aléatoire (sous contrainte : ∑α<sub>i</sub> = 0)

Les variétés étant précisément définies et d’intérêt principal, elles sont considérées comme un effet fixe.
L’effet du juge et de la session sont considérés comme aléatoire car nous souhaitons généraliser nos conclusions à d’autres panels de juges similaires.
Ce choix permet de modéliser la variabilité inter-juges et inter-session comme un bruit aléatoire plutôt que comme une constante à estimer pour chaque juge ou pour chaque session.

Par curiosité, nous avons comparé avec un modèle plus simple :

::: box
$$ Y_{i} = \mu + \alpha_i +\epsilon_i $$
:::

Nous constatons que :

Les coefficients α<sub>i</sub> restent inchangés : les effets mesurés pour chaque variété sont les mêmes.
En revanche, les tests statistiques évoluent (la statistique de test t et sa p-value).

Dans un plan équilibré (même nombre de juges pour chaque produit), les moyennes ajustées coïncident avec les moyennes simples.
Le choix du modèle influence alors uniquement la variance résiduelle, donc la significativité, mais pas l’estimation des effets.
Autrement dit : l’effet variété reste le même, mais le "bruit" augmente, ce qui impacte la fiabilité statistique des résultats.\
Notre plan à nous était bien évidemment équilibré, avec le même nombre d’observation par combinaisons.

Ainsi, nous observons belles est bien des différences, parfois significatives, dans la perception des 4 variétés de fraises.


Nous utilisons la fonction `decat` du package SensoMineR.
`decat` permet d’identifier les attributs sensoriels qui distinguent réellement les produits.
Pour un producteur ou un responsable qualité, cela signifie savoir sur quels critères sensoriels miser pour différencier une variété.
Concrètement, `decat` fait tourner une ANOVA pour chaque descripteur sensoriel afin de déterminer : les descripteurs les plus discriminants (effet variété globalement significatif) ceux qui caractérisent une variété en particulier (test t).

En sortie, on obtient : un tableau couleurs : bleu = descripteur \> moyenne rouge = descripteur \< moyenne

```{r, include=TRUE}

fraise1 <- as.data.frame(fraise1)

res_fraise<- decat(fraise1, formul="~ProductName + CJ + NR +ProductName:CJ + ProductName:NR", firstvar = 4, lastvar = 13)

# Paramètres :

# Données : fraise1
# Modèle : Note ~ ProductName + CJ + NR
# Position du 1er descripteur : 4 ("Taille")
# Seuil proba : 0.05 par défaut

```

```{r, include=TRUE}
res_fraise$tabT
```

On obtient également plusieurs objets, parmi lesquels resT qui donne la carte d'identité sensorielle de chaque variété.

<br>

#### Interprétation 

Le résultat de la fonction decat est une liste contenant plusieurs objets.
Le premier objet analysé est le resT, les résultats du T test.
Pour chaque variété de fraises, les descripteurs significatifs sont affichés.
La colonnes coeff donne le coefficient d’écart à la moyenne générale (alpha).
C'est-à- dire que pour cette variété, la moyenne ajustée de l’attribut est égale à la moyenne générale + le coefficient.
Si il est positif c’est donc que la fraise a des valeurs plus élevées que la moyenne pour ce descripteur et si il est négatif des valeurs plus basses que la moyenne.
La colonne p value indique si le coefficient est significatif.
La fonction decat affiche uniquement les descripteurs significatifs pour chaque fraise.

Si on prend la fraise A comme exemple, on peut dire qu’elle est plus rouge et plus ferme que les autres fraises.
En effet pour ces deux descripteurs elle a des coefficient d’écart à la moyenne général positif.
Par contre elle est moins grande, moins fondante et moins juteuse que les autres.
Car ces coefficient sont négatifs.

Le deuxième objet que l’on regarde est le tableau des moyennes ajusté.
On peut le retrouver dans l’objet adjmean, mais la fonction affiche directement ce tableau dans sa sortie.
La moyenne ajustée = la moyenne générale + le coefficient d’écart à la moyenne générale.

Le tableau prend en ligne les variétés de fraises et en colonnes les différents descripteurs.
Dans les cases, il indique la moyenne ajustée que prend la fraise pour ce descripteur.
C'est-à- dire la note moyenne que la fraise reçoit pour le descripteur en question.

Les cases en rouge signifie que la moyenne ajustée est significativement plus petite que la moyenne globale.
Et les cases en bleu que la moyenne ajustée est significativement plus grande que la moyenne globale.

Les fraises sont également rangées dans un ordre précis.
Si deux fraises sont proches dans le tableau c’est qu'elles prennent des valeurs similaires sur les différents descripteurs.

Sur le tableau on voit deux groupes de fraises.
La A et la D qui ont des fortes valeurs par rapport à la moyenne pour couleur et fermeté mais des plus faibles pour la taille, le juteux et le fondant.
Les fraise B et C qui prennent des fortes valeurs là où le A et D n’en prennent pas.

On peut se poser la question de qu’est ce qu’il se passe si on change le modèle utilisé dans la fonction decat.
Les données étant équilibrées grâce à un plan d’expérience, les estimateurs de la moyenne ne vont pas changer.
Ce qui va changer c’est la significativité de l’effet produit sur les descripteurs, c'est-à- dire la façon dont les cases s'allument en bleu ou en rouge.

L’analyse unidimensionnelle permet d’identifier les descripteurs sensoriels qui différencient le plus les variétés entre elles.
Ces descripteurs peuvent devenir des leviers de communication ou de sélection : une variété particulièrement fondante ou riche en arôme de fraise des bois peut être positionnée comme « gourmande », tandis qu’une variété perçue comme plus ferme ou plus acide conviendra peut-être plus à la transformation.

### Études du panel 

Une fois avoir analysé nos données sensorielles, on peut se questionner sur la performance de nos juges.
Pour celà il faut se pencher sur les intéractions.
Une interaction c’est quand l’influence d’une variable explicative sur la variable réponses dépend d’une autre variable explicative.

Dans notre cas d’étude sensoriel il y a 3 intéractions qui existent : Juge : session Session : produit Juge : produit

Sachant que l’on se questionne sur notre produit, il convient d’écarter l'interaction qui n'inclut pas le produit.
Il faut donc se concentrer sur les intéractions Juge:Produit et <Session:Produit>.

L’interaction <Session:Produit> nous informe sur la répétabilité : est-ce que les produits ont été évalués de manière similaire d’une session à l’autre ?
L’interaction Juge:Produit, quant à elle, concerne la reproductibilité : est-ce que les juges évaluent les produits de manière cohérente ?
Autrement dit, obtiendrait-on des résultats comparables si l’on changeait de panel de juges ?


## Analyse multidimensionnelle (ACP)

Nous appliquons une Analyse en Composantes Principales (ACP) sur le tableau des moyennes ajustées extrait de decat.

L’objectif est de projeter les produits dans un espace sensoriel global et de visualiser leurs proximités et différences.
Lors d’une ACP, les données sont centrées (soustraction de la moyenne de chaque variable) et réduites (division par l’écart-type), afin de ne pas donner plus de poids aux descripteurs les plus dispersés.

Les deux premières composantes principales retenues pour la représentation graphique sont celles qui expliquent la plus grande part de la variabilité totale, et qui offrent une lecture sensorielle contrastée entre les produits, c'est à dire la projection dns l'espace qui différencient le mieux nos variétés de fraises).


L’ACP permet de visualiser les variétés dans un espace sensoriel simplifié.
Cette représentation aide à segmenter les produits selon des profils perceptifs clairs.


#### Interpretation

La première dimension explique 49.51% de la variabilité et la deuxième 33.57%.

Sur le graphique des individus on voit que les fraises sont assez séparées.
La Fraise A est particulièrement éloignée de toutes les autres sur la première dimension.
Sur la deuxième dimension les Fraise A et D sont presque au même niveau (d'où la proximité dans les tableau des moyennes ajustées présenté par decat).

Il faut utiliser le graphique des variables pour pouvoir interpréter la position des fraises.
Sur la première dimension plus une fraise est à gauche plus elle est rouge et ferme, ce qui correspond bien à la description de la fraise A faite précédemment.
Plus une fraise est à droite sur la dimension 1 plus elle est grande, sucrée, juteuse, et fondante.\
Plus une fraise est haute sur la dimension deux, plus elle a un arôme prononcé et un goût de fraise des bois.
On peut donc dire que la Fraise C qui est en haut est une fraise très aromatique alors que la Fraise B est une fraise avec un profil aromatique plus faible.


## Validation par Bootstrap

L’ACP est faite sur des moyennes observées.
Mais que se passerait-il si nous avions interrogé d’autres juges ?

Plutôt que de faire appel à un nouveau panel pour déguster nos 4 variétés de fraises, nous utilisons la méthode du Bootstrap, une technique de rééchantillonage.

Cette méthode consiste à générer un grand nombre de jeux de données simulés en tirant avec remise des juges présents dans l’échantillon initial.
Pour chaque jeu "bootstrapé", on recalcule les moyennes ajustées par variété, puis on projette ces nouvelles moyennes dans l’espace sensoriel défini par l’ACP.
Ce processus est répété un très grans nombre de fois.

Le bootstrap ne nous donne pas la distribution exacte des perceptions, mais une approximation fondée sur les juges présents.
Pour utiliser cette technique, il faut émmettre l'hypothèse que le panel réel est représentatif d’un ensemble plus large de consommateurs.

### Ellipses de confiance

L’ensemble des projections issues des nouveaux jeux de données "bootstrapés" forme un nuage de points autour de chaque variété, dont on extrait une ellipse de confiance.
Cette ellipse représente la variabilité potentielle de la perception moyenne de la variété si l’on recommençait l’étude avec un autre panel.

Plus l’ellipse est petite, plus la perception est stable d’un panel à l’autre.
Si deux ellipses se confondent ou se chevauchent, cela signifie que les différences entre les variétés n'ont pas ou peu été perçues.
À contrario, si les ellipses sont bien distinctes, alors les produits ont été perçus de manière significativement différente, indépendamment du choix des juges.

Pour automatiser cette procédure, nous utilisons la fonction `panellipse` du package SensoMineR, qui réalise à la fois le bootstrap, le recalcul des ACP et la génération des ellipses.

Contrairement à decat, la fonction panellipse sélectionne uniquement les descripteurs sensoriels significatifs (au seuil de 5 %) pour la construction de l’ACP, ce qui renforce la lisibilité de la représentation.

Deux graphiques sont produits :

Graphique des individus : chaque produit est représenté avec une ellipse de confiance.
Si deux ellipses se chevauchent, cela signifie que les différences entre les variétés ne sont pas statistiquement robustes.
Si elles sont distinctes, les produits sont perçus différemment, indépendamment du panel.
Graphique des variables : les petits points autour des flèches indiquent la variabilité de chaque descripteur dans les jeux bootstrap.
Plus ces points sont concentrés, plus le descripteur est stablement représenté, donc discriminant dans l’espace sensoriel.

```{r, include=TRUE}

elispsefraise <- panellipse(fraise1, col.p = 3, col.j = 2, firstvar = 4, lastvar = 13)


#Paramètres : 
#le jeu de données (ici fraise1), 
#la variable Juge (ici NR), 
#la variable Produit (ici CJ) 
#la position du premier descripteur sensoriel (ici la variable “Taille”, en position 4).

```

#### Interprétation

Sur le graphique des individus, on voit qu’aucune des ellipses de confiance ne se chevauche.
Celà veut dire que nos produits sont bien différenciable.

Sur le graphique, on observe que la variable taille est très stable, car les points qui l'entourent sont très rapprochés.
Les variables acide, ferme, couleur, juteux et fondant sont également stables, bien que les points qui les entourent soient un peu plus dispersés que pour taille.
En revanche, les variables fraise des bois et odeur sont les moins bien représentées, car les points qui les entourent sont très dispersés.

L’analyse par bootstrap valide la robustesse des différences perçues.
Si une variété est bien différenciée mais son ellipse de confiance est large ou chevauche celle d’un concurrent, cela signifie que cette différenciation est instable : un autre panel pourrait percevoir autrement.


## BONUS : rappel sur l'écart-type

L’intervalle de confiance d’une moyenne est généralement calculé à partir de :

l’écart-type des observations individuelles (σ),
et du nombre d’individus (n),
selon la formule :

<div class="box">

$$
\frac{\sigma}{\sqrt{n}}
$$
</div>

Cela nous donne une idée de la variabilité attendue autour d’une moyenne si l’on recommençait l’expérience plusieurs fois.



# Données JAR 

L’analyse JAR (“Just About Right” ) permet de relier des perceptions spécifiques (trop/pas assez) à l’appréciation globale d’un produit. 
Elle répond à la question : quels attributs sensoriels repoussent ou séduisent les consommateurs ? Cela permet d’identifier des drivers of liking (à renforcer) et des drivers of disliking (à corriger).  
Un driver of disliking est un défaut perçu associé à une baisse de note d’appréciation et un driver of liking est une caractéristique valorisée associée à une hausse de cette note.
Les résultats d'une analyses JAR peuvent se traduirent directement par des axes d’ajustement précis (si c’est “trop” il faudra réduire, si c’est “pas assez” il faudra augmenter).

## Mise en place du test

Lors d’un test JAR, les consommateurs évaluent un produit à partir d’une liste préétablie de descripteurs sensoriels (ex. sucré, croustillant, onctueux), définie en amont. 
Pour chaque descripteur, ils indiquent si l’intensité perçue leur semble insuffisante (pas assez), excessive (trop) ou optimale  (“Just About Right”). 
Cette évaluation repose sur une échelle verbale, généralement en 3 ou 5 points (trop, très, JAR, peu, pas assez), centrée autour de l’optimum perçu (JAR). 
En complément, chaque consommateur donne une note d’appréciation globale, souvent sur une échelle de type hédonique (par exemple de 0 = "je déteste" à 10 = "j’adore").

### Receuil de données
Un recueil de données JAR se présente donc sous la forme d'un jeu de données avec une ligne par évaluation individuelle, une colonne pour la note de liking, une colonne pour chaque descripteur sensoriel codé en modalité verbale (JAR, Trop, Pas assez). 

```{r, echo=FALSE}
load("orange.RData")
```


Voici une visualisation des données avec lesquelles nous allons jouer pour illustrer la méthode JAR.

Ces données conserne des jus d'oranges : 


 8 jus d’orange ont été sélectionnés selon 3 facteurs expérimentaux :
-  Marque : Jafaden, Tropicana
-  Présence de pulpe : avec ou sans pulpe
-  Réfrigération : oui ou non

106 consommateurs ont évalué les 8 jus selon : <br>
- un score hédonique <br>
- 6 attributs sensoriels mesurés sur une échelle JAR :<br>
  - Nuance de la couleur<br>
  - Intensité de l’odeur<br>
  - Goût sucré <br>
  - Acidité <br>
  - Amertume <br>
  - Caractère pulpeux<br>
<br>


```{r, echo=FALSE}
tab <- as.data.frame(table(orange[,5]))
ggplot(tab, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "orange") +
  xlab("Modalities") +
  ylab("Number of occurrences") +
  ggtitle("Distribution des modalités pour l'attribut sensoriel Io") +
  theme_minimal()
```

Voici donc à quoi ressemble un jeu de données JAR


```{r, echo=FALSE}
for (j in 4:9) levels(orange[,j]) <- c("ne","ne","JAR","tm","tm")

taborange  <-head(orange)

kable(taborange, digits = 2, align = "c", caption = "6 premières lignes de orange", format = "html") %>%
  kable_styling(full_width = FALSE,
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = TRUE, background = "#ff7f00") %>%
  row_spec(1:nrow(taborange), background = "#FFF0F5") %>%
  column_spec(2, bold = TRUE, color = "#df6d14")
```

## Modèle statistique associé
L’analyse JAR se concentre sur les perceptions extrêmes des consommateurs, en excluant volontairement les mentions “Just About Right” qui correspondent à un niveau optimal perçu. 
Le modèle va donc uniquement prendre en compte les modalités de “défauts”, c'est-à-dire “Trop” et “Pas assez”, pour ainsi cibler les écarts à l’idéal sensoriel.

La construction d’un tableau disjonctif permet de passer d’un ressenti verbal ("trop sucré", "pas assez croustillant") à une base de données exploitable par les outils statistiques. 
Concrètement, on va créer pour chaque modalité extrême de chaque descripteur, une indicatrice binaire (0 = non perçu ; 1 = perçu). Chaque colonne correspond donc à un défaut sensoriel clairement identifié, intégré comme effet fixe explicite.
Cette opération permet d’intégrer les modalités comme effets fixes explicites dans le modèle linéaire.
Il faut exclure volontairement les mentions “Just About Right”, car elles ne représentent pas un défaut perçu. Ainsi, on centre l’analyse sur la déviation par rapport à l’optimum perçu, nous permettant de quantifier l’impact d’un défaut perçu sur la note globale de liking Ce qui nous permet donc de prioriser les défauts les plus pénalisants à corriger dans une prochaine version du produit.


```{r, echo=FALSE, include=FALSE}
library(ade4)
orange.dummy <- acm.disjonctif(orange[,4:9])
orange.dummy <- cbind(orange[,1:3], orange.dummy)
orange.dummy[1:5,]

```
```{r, echo=FALSE}
orange.dummy |>
  head(10) |>
  kbl(caption = "Tableau de contingence disjonctif des modalités (10 premières lignes)", 
      align = "c", 
      booktabs = TRUE) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, 
                position = "center") |>
  row_spec(0, background = "#FFA500", color = "white", bold = TRUE)
```


Le modèle s’écrit sous la forme :
<div class="box"> $$ Y_{ijkl} = \mu + \sum_{a} \beta_a \cdot X_{a,ijkl} + \gamma_j + \lambda_k + \varepsilon_{ijkl} $$ </div>
Y<sub>ijkl</sub> : score d’appréciation hédonique (quantitatif, ex. 0–10)
X<sub>a</sub> : indicatrices des modalités extrêmes des descripteurs sensoriels
β<sub>a</sub> : effets marginaux de ces défauts
γ<sub>j</sub> : effet aléatoire du juge
λ<sub>k</sub> : effet fixe du produit
ε<sub>ijkl</sub> : erreur résiduelle (supposée iid, normale centrée)

Les juges sont modélisés ici comme un effet aléatoire, dans un modèle linéaire mixte. Car on suppose qu’ils représentent un échantillon aléatoire d’une population plus large de consommateurs naïfs. 
Chaque modalité de défaut devient une variable. On ne dit plus seulement “les gens trouvent ça trop (attribut sensoriel)”, on mesure que “ce défaut fait perdre x point en liking moyen”.

```{r, echo=FALSE}
res.jar <- JAR(orange, col.p = 2, col.j = 1, col.pref = 3, jarlevel="JAR")
res.jar$penalty2
```
Les coefficients issus du modèle indiquent donc combien de points en appréciation globale sont perdus lorsqu’un défaut sensoriel est perçu (trop / pas assez). 
Pour chaque modalité extrême testée, l’hypothèse nulle est H₀ : β = 0 (le défaut n’a aucun effet sur la note de liking). Une p-value inférieure à 0,05 permet de rejeter cette hypothèse et conclure à un impact significatif.

# INTERPRETATION


```{r, echo=FALSE}
# Penalties can also be estimated for each product.

orange.dummy.2JPR <- orange.dummy[orange.dummy$Juice == "2JPR",]

# Use the AovSum() function to estimate your model for which the liking is explained by the presence or absence of the defects.
res.penalty.one <- AovSum(Liking ~ Nc.ne + Nc.tm + Io.ne + Io.tm + Su.ne + Su.tm +
                            Ac.ne + Ac.tm + Bt.ne + Bt.tm + Pu.ne + Pu.tm,
                          data = orange.dummy.2JPR)
res.penalty.one$Ttest
```
# INTERPRETATION

### CONCLUSION

Pour guider les décisions produit, il est utile de croiser l’impact (estimate) et la fréquence d’apparition de chaque défaut. Un défaut perçu par 50 % des consommateurs et faisant perdre 1 point d’appréciation mérite une attention immédiate. 
À l’inverse, un défaut très rare mais très pénalisant peut être secondaire. 
Les attributs avec fort impact et forte fréquence de défaut doivent être priorisés dans la reformulation.





# Cartographie des préférences

##  Objectif de la méthode

L’objectif de la cartographie des préférences est d’identifier, au sein de l’espace produit sensoriel, les zones associées à une forte ou une faible appréciation. 
Il s’agit de croiser deux sources d’information : les profils sensoriels mesurés par un panel d’experts, et les notes hédoniques attribuées par des consommateurs naïfs. 
Cette approche permet de localiser, dans l’espace produit testé, les régions où se situeraient les produits idéaux, les produits qui seraient les plus appréciés.

##  Description des données

Pour réaliser une cartographie des préférences, il nous faut :

Un recueil de **données sensorielles par un panel d’expert**, les données sont des données purement descriptive et son structuré de la même manière qu’un jeu de données issus d’une QDA (voir onglet QDA)

Un recueil de **données hédonique par un panel de consommateurs naïfs**, en très grand nombre. 
Ce jeu de données est structuré avec en ligne les fiches de dégustation (un produit par un juge) et en colonne le juge, le produit et la note hédonique. 

Les juges experts évaluent chaque produit selon un ensemble descripteurs sensoriels quantitatifs (ex. sucré, ferme, juteux), tandis que les consommateurs attribuent une note d’appréciation globale sur une échelle hédonique (souvent de 0 à 10).

###  Exemple : 

Dans notre exemple, nous utilisons les données cocktail du package SensoMineR. Le fichier senso.cocktail contient les profils sensoriels (16 produits × 13 descripteurs). Le fichier hedo.cocktail regroupe les notes hédoniques (16 produits évalués par 100 consommateurs).


```{r, include=FALSE}

data(cocktail)
tabc3  <-head(senso.cocktail)
tabc2 <-head(hedo.cocktail)
```


Voici une rapide visualisation de nos jeux de données : 



senso.cocktail : un data frame de 16 lignes et 13 colonnes : chaque cocktail a été évalué par 12 panélistes selon 13 descripteurs sensoriels ;

```{r, echo=FALSE}
kable(tabc2 , digits = 2, align = "c", caption = "6 premières lignes du jeu de hedo.cocktail", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = T, background = "#3eecac") %>%
  row_spec(1:nrow(tabc2), background="#cca3ff")
```

hedo.cocktail : un data frame de 16 lignes et 100 colonnes : chaque cocktail a été évalué sur une échelle structurée de 0 à 10 par 100 consommateurs, en fonction de leur déplaisir (0) ou plaisir (10).


```{r, echo=FALSE}
kable(tabc3 , digits = 2, align = "c", caption = "6 premières lignes du jeu de senso.cocktail", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) %>%
  row_spec(0, bold = T, background = "#ff7f00") %>%
  row_spec(1:nrow(tabc3), background="#FFF0F5")%>%
  column_spec(2, bold = T, color="#df6d14")
```


## Construction de l’espace sensoriel 

Afin de représenter les produits dans un espace sensoriel réduit, nous appliquons une Analyse en Composantes Principales (ACP) sur le tableau des profils sensoriels. 
Les notes des descripteurs sont préalablement ajustées selon le modèle suivant :

::: box
$$ Y_{ijk} = \mu + \alpha_i + \gamma_j + \epsilon_{ijk} $$
:::

Où :

Y<sub>ijk</sub> : note attribuée à un descripteur μ : moyenne globale α<sub>i</sub> : effet fixe du produit i  γ<sub>j</sub> : effet aléatoire du juge j  ε<sub>ijk</sub> : erreur aléatoire (sous contrainte : ∑α<sub>i</sub> = 0)


Les moyennes ajustées par produit sont extraites et servent de base pour l’ACP. 
Cette méthode permet de projeter les produits dans un repère de dimension réduite, tout en conservant un maximum de variance sensorielle. 
Les données sont centrées et réduites afin de neutraliser les différences d’échelle entre descripteurs.
Chaque produit est donc positionné dans ce plan selon ses coordonnées (Dim1, Dim2), issues de la projection.
L’ACP permet de projeter les produits dans un plan factoriel (Dim1, Dim2), en capturant la variance maximale. 
Les deux premières composantes sont retenues pour la cartographie : elles concentrent l’essentiel de la variance totale.

La proportion de variance expliquée par chaque dimension s’appelle l’inertie. Elle correspond à la part d’information du jeu de données capturée par l’axe. Ce % nous indique si notre plan résume bien notre jeu de données au non. Dans l'idéal, nous souhaitons que notre plan explique au minimum 60% de la variabilité totale (% inertie Dim1 + % inertie Dim2).

```{r}
res.acp <- PCA(senso.cocktail)
```

# Interprétation

## Prédire du liking 

Nous souhaitons ensuite relier la position des produits dans la représentation sensorielle à l’appréciation des consommateurs. 
Pour cela, un modèle de régression linéaire est ajusté pour chaque consommateurs, prenant en entrée les coordonnées ACP des produits qu’il a notés :

<div class='box'>$$
Y = \\beta_0 + \\beta_1 \\cdot x + \\beta_2 \\cdot y + \\varepsilon
$$</div>

avec :  
- \\( Y_{ij} \\) : **note de liking** attribuée par le consommateur *i* au produit *j* ;  
- \\( \\beta_{0i} \\) : niveau de satisfaction moyen ;  
- \\( x_{1j} \\) : **coordonnée du produit *j* sur la dimension 1** de l'ACP ;  
- \\( x_{2j} \\) : **coordonnée du produit *j* sur la dimension 2** de l'ACP ;  
- \\( \\beta_{1i}, \\beta_{2i} \\) : **effets directionnels** des préférences du consommateur *i* dans l’espace sensoriel ;  
- \\( \\varepsilon_{ij} \\) : erreur aléatoire


Chaque consommateur est ainsi modélisé par un plan de régression qui lui est propre, permettant d’estimer sa note pour n’importe quelle position dans l’espace sensoriel. 
Ce choix repose sur l’hypothèse émise que deux produits proches dans l’espace sensoriel devraient être appréciés de manière similaire.

Selon les hypothèses psychophysiques retenues, une version quadratique peut être testée (pour tenir compte d’effets de saturation), si on estime que liking est en constante croissance, notre modèle ne prendre pas en compte les effets quadratique, à contrario, si nous estiment qu'à partir d'un certain seuil, sur un attribut sensoriel, le liking diminuerai, il faut inclure des effets quadratiques dans le modèle. 

Voici le modèle où les effets quadratiques sont pris en compte: 

<div class='box'>$$
Y = \\beta_0 + \\beta_1 \\cdot x + \\beta_2 \\cdot y + \\beta_3 \\cdot x^2 + \\beta_4 \\cdot y^2 + \\varepsilon
$$</div>

avec :  
- \( Y \) : note de liking prédite pour une position donnée dans l’espace sensoriel (x, y) ;  
- \( x \), \( y \) : coordonnées du point dans le plan ACP (Dim1, Dim2) ;  
- \( \beta_0 \) : constante (niveau moyen de liking) ;  
- \( \beta_1 \), \( \beta_2 \) : effets directionnels linéaires ;  
- \( \beta_3 \), \( \beta_4 \) : effets de courbure (prise en compte d'un seuil de saturation) ;  
- \( \varepsilon \) : erreur aléatoire.



## Cartographier des zones de préférences

L’espace sensoriel obtenu suite à l'ACP est discrétisé en une grille fine. Pour chaque point de cette grille, la note de liking est prédite à partir des modèles individuels. On fixe ensuite un seuil d’acceptabilité (ex. 6/10).

Pour chaque point de la grille, on calcule le pourcentage de consommateurs dont la note prédite dépasse ce seuil. OCe pourcentage permet de définir des zones de fort liking et des zones de disliking.
Il n’y a plus qu'à sortir les crayons de couleurs : la zone sera coloriée en rouge si c’est le pourcentage est fort, et en bleu si le pourcentage est bas. 

On obtient ainsi une carte où chaque zone reflète l’intensité prédite d’appréciation. 
L’objectif est d’identifier les zones où un produit hypothétique aurait le plus de chances de plaire à une large part de consommateurs.

Voici la cartographie des préférences pour notre exemple avec les cocktails:


```{r}
res.carto <- carto(res.acp$ind$coord[,1:2], hedo.cocktail)
```

# INTERPRETATION



## Retour au sensoriel : profil du produit idéal dans l’espace produit

Une fois la zone de fort liking identifiée, il est possible d’en extraire les coordonnées (Dim1, Dim2) correspondantes. 
Pour revenir à une interprétation sensorielle, on utilise la projection inverse de l’ACP, ce qui nous permet ainsi d’obtenir un profil sensoriel théorique du produit idéal.

### Limites : 
L’ACP ne conserve qu’une partie de la variance totale : la projection dans un plan 2D entraîne une perte d’information. 
Il convient de vérifier que Dim1 et Dim2 expliquent une part suffisamment importante de la variabilité sensorielle (% d’inertie).  
*L’inertie représente la variance totale expliquée par chaque axe principal. Elle mesure la part d’information du jeu de données capturée par chaque dimension.*


## CONCLUSION
La cartographie des préférences est très apprécié en marketing et permet de cibler franchement les attributs sensoriels attendus d'un produit idéal. 
Toutefois, cette méthode présentes de certaines limites. Un coût très élevé (Panel d'expert, très grand nombre de consommateur, plusieurs produits à tester...) et reste cantonnée à l'espace produit sensoriel. 
Et si le VRAI produit idéal se trouvait à l'extérieur de cet espace? 



# Conclusion générale


