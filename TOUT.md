---
title: "Notes de cours - Sensométrie - 2025"
author: "Pauline CAMARD, Erell DOYEN, Riwal LE MOAN DELALANDE"
output:
  html_document:
    toc: true
    number_sections: true
    toc_depth: 2
    toc_float:
      collapsed: false
      smooth_scroll: true
  pdf_document:
    toc: true
    toc_depth: '2'
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
@import url('https://fonts.googleapis.com/css2?family=Space+Mono:ital,wght@0,400;0,700;1,400;1,700&display=swap');

body {
  font-family: "Space Mono", monospace;
  font-weight: 400;
  font-style: normal;
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


.box {
  background-color: #ffc0db; 
  border: 2px dashed #ff69b4; /* rose bonbon */
  border-radius: 15px;
  padding: 20px;
  margin: 15px 0;
  box-shadow: 0 0 10px #ffc0cb;
}

#TOC a {
  color: #CB2C30; /* rouge fraise */
  text-decoration: none;
  font-weight: bold;
}

#TOC a:hover {
  text-decoration: underline;
  color: #5e9732; /* vert feuille fraise */
}

#TOC .tocify-header {
  color: #CB2C30;
  font-weight: 700;
  margin-top: 1em;      /* espace AVANT le titre */
  margin-bottom: 0.2em; /* petit espace APRÈS */
}

#TOC .tocify-subheader {
  color: #E78B90;
  font-weight: 500;
  margin-top: 0.1em;
  margin-bottom: 0.1em;
  margin-left: 1.2em;
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

Quand un producteur affirme que la variété qu’il a sélectionnée avec soin et cultivée avec amour est la plus sucrée, est-ce vrai ? Ou est-ce seulement une stratégie marketing ?
<br> Le goût ou la texture de toutes les variétés de fraises sont-ils perçus de la même manière ?
<br> Y’a t-il des fraises significativement plus rouges que d'autres ?
<br>

Pour répondre à ces questions, nous avons mené notre enquête.
Ou plutôt notre analyse (sensorielle évidemment).

## Cadre de l’analyse

4 variétés de fraises, toutes cultivées par une petite productrice près de Rennes, ont été évaluées par 12 juges (étudiants à l'Institut Agro de Rennes) lors de deux sessions successives selon 10 descripteurs sensoriels.
Une fraise par variété et par session a été dégustée.

Les descripteurs sensoriels étaient les suivants : Taille, Couleur, Odeur, Ferme, Juteux, Fondant, Sucrée, Acide, Arôme et Fraise des bois

La réponse était obligatoire.
Les notes pouvaient prendre des valeurs de 0 (très faiblement perçue) à 10 (énormément perçue).

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

```{r, include=TRUE, echo= FALSE, error=TRUE}
df_moyennes <- fraise1 %>%
  group_by(ProductName) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))

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

Nous avons donc observé des différences dans la perception de ces 4 variétés.

Pour creuser et affiner notre observation, nous adoptons une approche en trois temps : <br>
1.Analyse unidimensionnelle : observer la différence variété par variété, descripteur par descripteur.
<br>
2.Analyse multidimensionnelle : représenter les produits dans un espace sensoriel global (ACP).
<br>
3.Validation par répétition : estimer la variabilité des résultats grâce à des techniques de bootstrap.

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

Y<sub>ijk</sub> : note attribuée à un descripteur <br> μ : moyenne globale α<sub>i</sub> : effet de la variété i <br> γ<sub>j</sub> : effet du juge j <br> λ<sub>k</sub> : effet de la session k <br> ε<sub>ijk</sub> : erreur aléatoire (sous contrainte : ∑α<sub>i</sub> = 0)

Les variétés étant précisément définies et d’intérêt principal, elles sont considérées comme un effet fixe.
L’effet du juge et de la session sont considérés comme aléatoire car nous souhaitons généraliser nos conclusions à d’autres panels de juges similaires.
Ce choix permet de modéliser la variabilité inter-juges et inter-session comme un bruit aléatoire plutôt que comme une constante à estimer pour chaque juge ou pour chaque session.

Par curiosité, nous avons comparé avec un modèle plus simple :

::: box
$$ Y_{i} = \mu + \alpha_i +\epsilon_i $$
:::

Nous constatons que :

Les coefficients α<sub>i</sub> restent inchangés : les effets mesurés pour chaque variété sont les mêmes.
<br> En revanche, les tests statistiques évoluent (la statistique de test t et sa p-value).

Dans un plan équilibré (même nombre de juges pour chaque produit), les moyennes ajustées coïncident avec les moyennes simples.
Le choix du modèle influence alors uniquement la variance résiduelle, donc la significativité, mais pas l’estimation des effets.
<br> Autrement dit, l’effet de la variété est toujours présent, mais l’incertitude autour de cet effet peut augmenter, ce qui affecte la significativité statistique.\
Notre plan à nous était bien évidemment équilibré, avec le même nombre d’observation par combinaisons.

Nous pouvons donc en conclure que dans un plan équilibré, les estimations des effets α<sub>i</sub> sont identiques d’un modèle à l’autre. En revanche, les tests statistiques (valeurs de t et p-values) peuvent différer, car la variance résiduelle change selon les effets inclus dans le modèle.
<br>

Ainsi, nous observons bel et bien des différences, parfois significatives, dans la perception des 4 variétés de fraises.

Nous utilisons la fonction `decat` du package SensoMineR.
<br> `decat` permet d’identifier les attributs sensoriels qui distinguent réellement les produits.
<br> Pour un producteur ou un responsable qualité, cela signifie savoir sur quels critères sensoriels miser pour différencier une variété.
Concrètement, `decat` fait tourner une ANOVA pour chaque descripteur sensoriel afin de déterminer les descripteurs les plus discriminants (effet variété globalement significatif) et ceux qui caractérisent une variété en particulier (test t).
<br>

En sortie, on obtient un tableau coloré avec : <br> bleu = descripteur \> moyenne <br> rouge = descripteur \< moyenne<br>

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

Le résultat de la fonction decat est une liste contenant plusieurs objets. <br>
Le premier objet analysé est le resT, les résultats du T test.<br>
Pour chaque variété de fraises, les descripteurs significatifs sont affichés.
La colonnes coeff donne le coefficient d’écart à la moyenne générale α<sub>.
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
Les fraise B et C qui prennent des fortes valeurs là où les fraises A et D n’en prennent pas.

Mais que se passerait-il si l'on changait le modèle en argument de 'decat'?
Comme mentionné plus haut, nos données sont équilibrées, donc les estimateurs de la moyenne ne vont pas changer.
Ce qui va changer c’est la significativité de l’effet variété sur les descripteurs, c'est-à- dire la façon dont les cases s'allument en bleu ou en rouge.

<br>

L’analyse unidimensionnelle permet d’identifier les descripteurs sensoriels qui différencient le plus les variétés entre elles.
Ces descripteurs peuvent devenir des leviers de communication ou de sélection : une variété particulièrement fondante ou riche en arôme de fraise des bois peut être positionnée comme « gourmande », tandis qu’une variété perçue comme plus ferme ou plus acide conviendra peut-être plus à la transformation.

### Études du panel

Une fois les données sensorielles analysées, il est pertinent d’évaluer la performance de notre panel de juges.
Pour celà il faut se pencher sur les intéractions.
Une interaction c’est quand l’influence d’une variable explicative sur la variable réponses dépend d’une autre variable explicative.

Dans notre cas d’étude sensoriel il y a 3 intéractions qui existent : 
Juge : Session <br>
Session : Produit <br>
Juge : Produit <br>

Sachant que l’on se questionne sur notre produit, il convient d’écarter l'interaction qui n'inclut pas le produit.
Il faut donc se concentrer sur les interactions Juge:Produit et Session:Produit.

L’interaction <Session:Produit> nous informe sur la **répétabilité** : est-ce que les produits ont été évalués de manière similaire d’une session à l’autre ?
L’interaction <Juge:Produit>, quant à elle, concerne la **reproductibilité**: est-ce que les juges évaluent les produits de manière cohérente ?
Autrement dit, obtiendrait-on des résultats comparables si l’on changeait de panel de juges ?

## Analyse multidimensionnelle (ACP)

Nous effectuons une Analyse en Composantes Principales (ACP) sur le tableau des moyennes ajustées extrait de decat.
<br>

L’objectif est de projeter les produits dans un espace sensoriel global et de visualiser leurs proximités et différences.
Lors d’une ACP, les données sont centrées (soustraction de la moyenne de chaque variable) et réduites (division par l’écart-type), afin de ne pas donner plus de poids aux descripteurs les plus dispersés.
<br>

Les deux premières composantes principales, retenues pour la représentation graphique, sont celles qui expliquent la plus grande part de variabilité totale. Elles permettent une lecture contrastée des profils sensoriels entre les variétés, en les projetant dans un espace où leurs différences sont les plus marquées.

L’ACP permet de visualiser les variétés dans un espace sensoriel simplifié.
<br> Cette représentation aide à segmenter les produits selon des profils perceptifs clairs.

#### Interpretation

La première dimension explique 49.51% de la variabilité et la deuxième 33.57%.

Sur le graphique des individus, les fraises apparaissent bien dispersées, ce qui suggère des profils sensoriels différenciés.
La Fraise A est particulièrement éloignée de toutes les autres sur la première dimension.
Sur la deuxième dimension les Fraise A et D sont presque au même niveau (d'où la proximité dans les tableau des moyennes ajustées présenté par decat).

L’interprétation de la position des fraises repose sur le graphique des variables, qui indique les contributions de chaque descripteur sensoriel aux axes factoriels. <br>
Sur la première dimension plus une fraise est à gauche plus elle est rouge et ferme, ce qui correspond bien à la description de la fraise A faite précédemment.  
Plus une fraise est située à droite sur la dimension 1, plus elle est perçue comme grande, sucrée, juteuse et fondante.  
Une position élevée sur la dimension 2 reflète une intensité aromatique plus marquée, notamment en arôme de fraise des bois.  
La Fraise C, située en haut du plan, présente un profil aromatique intense, tandis que la Fraise B, plus basse, est perçue comme moins aromatique.  

## Validation par Bootstrap

L’ACP se nourrit des moyennes ajustées observées.
Mais que se passerait-il si nous avions interrogé d'autres juges ?

Plutôt que de faire appel à un nouveau panel pour déguster nos 4 variétés de fraises (ce qui serait coûteux et très chronophage), nous utilisons la méthode du Bootstrap, une technique de rééchantillonage.

Cette méthode consiste à générer un grand nombre de jeux de données simulés en tirant avec remise des juges présents dans l’échantillon initial.
Pour chaque jeu "bootstrapé", on recalcule les moyennes ajustées par variété, puis on projette ces nouvelles moyennes dans l’espace sensoriel défini par l’ACP.
Ce processus est répété un très grand nombre de fois.  

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

Sur le graphique des individus, aucune ellipse de confiance ne se chevauche. Cela signifie que les produits sont perçus comme significativement différents, indépendamment du panel interrogé.  

Sur le graphique, on observe que la variable taille est très stable, car les points qui l'entourent sont très rapprochés.
Les variables acide, ferme, couleur, juteux et fondant sont également stables, bien que les points qui les entourent soient un peu plus dispersés que pour taille.
En revanche, les descripteurs "fraise des bois" et "odeur" présentent une forte variabilité, comme en témoigne la dispersion importante des points qui les entourent.  Ces descripteurs ont sûrement été moins bien compris par les juges. 

L’analyse par bootstrap valide la robustesse des différences perçues.
Si une variété est bien différenciée mais son ellipse de confiance est large ou chevauche celle d’un concurrent, cela signifie que cette différenciation est instable : un autre panel pourrait percevoir autrement.

## BONUS : rappel sur l'écart-type

L’intervalle de confiance d’une moyenne est généralement calculé à partir de :

l’écart-type des observations individuelles (σ), et du nombre d’individus (n), selon la formule :

::: box
$$
\frac{\sigma}{\sqrt{n}}
$$
:::

Cela nous donne une idée de la variabilité attendue autour d’une moyenne si l’on recommençait l’expérience plusieurs fois.

# Données JAR

L’analyse JAR (“Just About Right” ) permet de relier des perceptions spécifiques (trop/pas assez) à l’appréciation globale d’un produit.
Elle répond à la question : quels attributs sensoriels repoussent ou séduisent les consommateurs ?
Cela permet d’identifier des drivers of liking (caractéristiques à valoriser) et des drivers of disliking (défauts à corriger). Un driver of disliking est une caractéristique sensorielle perçue négativement, associée à une baisse de la note d’appréciation ; à l’inverse, un driver of liking est associé à une hausse de cette note.
Les résultats d'une analyse JAR peuvent déboucher sur des axes d’ajustement clairs : si un attribut est jugé "trop" présent, il conviendra de le réduire ; s’il est perçu comme "pas assez", il faudra l’intensifier).

## Mise en place du test

Lors d’un test JAR, les consommateurs évaluent un produit à partir d’une liste préétablie de descripteurs sensoriels (ex. sucré, croustillant, onctueux), définie en amont.
Pour chaque descripteur, ils indiquent si l’intensité perçue leur semble insuffisante (pas assez), excessive (trop) ou optimale (“Just About Right”).
Cette évaluation repose sur une échelle verbale, généralement en 3 ou 5 points (trop, très, JAR, peu, pas assez), centrée autour de l’optimum perçu (JAR).
En complément, chaque consommateur donne une note d’appréciation globale, souvent sur une échelle de type hédonique (par exemple de 0 = "je déteste" à 10 = "j’adore").

### Receuil de données

Un jeu de données JAR se structure avec une ligne par évaluation individuelle. Chaque ligne contient une note de liking globale, ainsi qu’une colonne par descripteur sensoriel, codé selon la modalité perçue par le consommateur ("JAR", "Trop", "Pas assez").

```{r, echo=FALSE}
load("orange.RData")
```

Voici une visualisation des données avec lesquelles nous allons jouer pour illustrer la méthode JAR.

Ces données conserne des jus d'oranges :

8 jus d’orange ont été sélectionnés selon 3 facteurs expérimentaux : - Marque : Jafaden, Tropicana - Présence de pulpe : avec ou sans pulpe - Réfrigération : oui ou non

106 consommateurs ont évalué les 8 jus selon : <br> 
- un score hédonique <br> 
- 6 attributs sensoriels mesurés sur une échelle JAR :

  <br> - Nuance de la couleur
  <br> - Intensité de l’odeur
  <br> - Goût sucré 
  <br> - Acidité 
  <br> - Amertume 
  <br> - Caractère pulpeux
  
  <br> <br>

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
                font_size = 14)
```

## Modèle statistique associé

L’analyse JAR se concentre sur les perceptions extrêmes des consommateurs, en excluant volontairement les mentions “Just About Right” qui correspondent à un niveau optimal perçu.
Le modèle va donc uniquement prendre en compte les modalités de “défauts”, c'est-à-dire “Trop” et “Pas assez”, pour ainsi cibler les écarts à l’idéal sensoriel.

La construction d’un tableau disjonctif permet de passer d’un ressenti verbal ("trop sucré", "pas assez croustillant") à une base de données exploitable par les outils statistiques.
Concrètement, on va créer pour chaque modalité extrême de chaque descripteur, une indicatrice binaire (0 = non perçu ; 1 = perçu).
Chaque colonne correspond donc à un défaut sensoriel clairement identifié, intégré comme effet fixe explicite.
Cette opération permet d’introduire les défauts perçus comme variables explicites (effets fixes) dans un modèle linéaire, afin d’évaluer leur impact sur la note de liking.  

Il faut exclure volontairement les mentions “Just About Right”, car elles ne représentent pas un défaut perçu.
Ainsi, on centre l’analyse sur la déviation par rapport à l’optimum perçu, nous permettant de quantifier l’impact d’un défaut perçu sur la note globale de liking Ce qui nous permet donc de prioriser les défauts les plus pénalisants à corriger dans une prochaine version du produit.

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
                position = "center") 
```

Le modèle s’écrit sous la forme :

::: box
$$ Y_{ijkl} = \mu + \sum_{a} \beta_a \cdot X_{a,ijkl} + \gamma_j + \lambda_k + \varepsilon_{ijkl} $$
:::

Y<sub>ijkl</sub> : score d’appréciation hédonique (quantitatif, ex. 0–10) <br>
X<sub>a</sub> : indicatrices des modalités extrêmes des descripteurs sensoriels <br>
β<sub>a</sub> : effets marginaux de ces défauts <br> γ<sub>j</sub> : effet aléatoire du juge <br>
λ<sub>k</sub> : effet fixe du produit <br>
ε<sub>ijkl</sub> : erreur résiduelle (supposée iid, normale centrée)<br>

<br>

Les juges sont modélisés ici comme un effet aléatoire car on suppose qu’ils représentent un échantillon aléatoire d’une population plus large de consommateurs naïfs.
Chaque modalité de défaut devient une variable.
On ne se contente plus de constater qu’un défaut est perçu : on quantifie précisément son impact sur la note d’appréciation, en estimant combien de points il fait perdre en moyenne (“ce défaut fait perdre x point en liking moyen”).



```{r, echo=FALSE}
res.jar <- JAR(orange, col.p = 2, col.j = 1, col.pref = 3, jarlevel="JAR")
res.jar$penalty2
```

Les coefficients issus du modèle indiquent donc combien de points en appréciation globale sont perdus lorsqu’un défaut sensoriel est perçu (trop / pas assez).
Pour chaque modalité extrême testée, l’hypothèse nulle est H₀ : β = 0 (le défaut n’a aucun effet sur la note de liking).
Une p-value inférieure à 0,05 permet de rejeter cette hypothèse et conclure à un impact significatif.

#### Interpretation

```{r, echo=FALSE}

orange.dummy.2JPR <- orange.dummy[orange.dummy$Juice == "2JPR",]

res.penalty.one <- AovSum(Liking ~ Nc.ne + Nc.tm + Io.ne + Io.tm + Su.ne + Su.tm +
                            Ac.ne + Ac.tm + Bt.ne + Bt.tm + Pu.ne + Pu.tm,
                          data = orange.dummy.2JPR)
res.penalty.one$Ttest
```

#### Interprétaion

Ce tableau présente, pour chaque défaut sensoriel potentiel, son impact estimé sur la note d’appréciation (« liking ») pour 1 seuil jus d'orange (codé sous le nom 2JPR).
La colonne p-value indique si cet effet est statistiquement significatif, tandis que la ligne Estimate (coefficient) reflète l’intensité de la baisse de la note en présence du défaut.
Les trois défauts ayant le plus fort impact négatif sur la note de liking du jus 2JPR sont les suivants : <br>– Pas assez sucré : baisse moyenne de 1,95 points <br>– Trop amer : baisse de 1,91 points <br>– Trop sucré : baisse de 1,35 points

Ces résultats mettent en évidence que l’équilibre en sucre est un critère central dans l’appréciation d’un jus d’orange : qu’il soit trop sucré ou pas assez, un déséquilibre entraîne une baisse significative de la note de liking.
De plus, une amertume trop marquée constitue également un facteur fortement pénalisant pour l’évaluation globale du produit.

## Conclusion de la méthode JAR

Pour guider les décisions produit, il est utile de croiser l’impact (estimate) et la fréquence d’apparition de chaque défaut.
Un défaut perçu par 50 % des consommateurs et faisant perdre 1 point d’appréciation mérite une attention immédiate.
À l’inverse, un défaut très rare mais très pénalisant peut être secondaire.
Les attributs avec fort impact et forte fréquence de défaut doivent être priorisés dans la reformulation.

# Cartographie des préférences

## Objectif de la méthode

L’objectif de la cartographie des préférences est d’identifier, au sein de l’espace produit sensoriel, les zones associées à une forte ou une faible appréciation.
Il s’agit de croiser deux sources d’information : les profils sensoriels mesurés par un panel d’experts, et les notes hédoniques attribuées par des consommateurs naïfs.
Cette approche permet de localiser, dans l’espace produit testé, les régions où se situeraient les produits idéaux, les produits qui seraient les plus appréciés.

## Description des données

Pour réaliser une cartographie des préférences, il nous faut :

Un recueil de données sensorielles issues d’un panel d’experts, de nature descriptive, structuré comme un jeu de données QDA (voir section correspondante).  

Un recueil de **données hédonique par un panel de consommateurs naïfs**, en très grand nombre.
Ce jeu de données est structuré avec en ligne les fiches de dégustation (un produit par un juge) et en colonne le juge, le produit et la note hédonique.

Les juges experts évaluent chaque produit selon un ensemble descripteurs sensoriels quantitatifs (ex. sucré, ferme, juteux), tandis que les consommateurs attribuent une note d’appréciation globale sur une échelle hédonique (souvent de 0 à 10).

### Exemple :

Dans notre exemple, nous utilisons les données cocktail du package SensoMineR.
Le fichier senso.cocktail contient les profils sensoriels évalués par le panel d’experts, tandis que hedo.cocktail regroupe les notes d’appréciation (liking) attribuées par les consommateurs.

```{r, include=FALSE}

data(cocktail)
tabc3  <-head(senso.cocktail)
tabc2 <-head(hedo.cocktail)
```

Voici une rapide visualisation de nos jeux de données :

senso.cocktail (16x13), chaque cocktail a été évalué par 12 panélistes selon 13 descripteurs sensoriels ;

```{r, echo=FALSE}
kable(tabc2 , digits = 2, align = "c", caption = "6 premières lignes du jeu de hedo.cocktail", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) 
```

hedo.cocktail (16 x 100) chaque cocktail a été évalué sur une échelle structurée de 0 à 10 par 100 consommateurs, en fonction de leur déplaisir (0) ou plaisir (10).

```{r, echo=FALSE}
kable(tabc3 , digits = 2, align = "c", caption = "6 premières lignes du jeu de senso.cocktail", format = "html") %>%
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover"),
                position = "center",
                font_size = 14) 
```

## Construction de l’espace sensoriel

Afin de représenter les produits dans un espace sensoriel réduit, nous appliquons une Analyse en Composantes Principales (ACP) sur le tableau des profils sensoriels.
Les notes des descripteurs sont préalablement ajustées selon le modèle suivant :

::: box
$$ Y_{ijk} = \mu + \alpha_i + \gamma_j + \epsilon_{ijk} $$
:::

Où :

Y<sub>ijk</sub> : note attribuée à un descripteur <br>
μ : moyenne globale <br>
α<sub>i</sub> : effet fixe du produit i <br>
γ<sub>j</sub> : effet aléatoire du juge j <br>
ε<sub>ijk</sub> : erreur aléatoire <br>

(sous contrainte : ∑α<sub>i</sub> = 0)

Les moyennes ajustées par produit sont extraites et servent de base pour l’ACP.
Cette méthode permet de projeter les produits dans un repère de dimension réduite, tout en conservant un maximum de variance sensorielle.
Les données sont centrées et réduites afin de neutraliser les différences d’échelle entre descripteurs.
Chaque produit est donc positionné dans ce plan selon ses coordonnées (Dim1, Dim2), issues de la projection.
L’ACP permet de projeter les produits dans un plan factoriel (Dim1, Dim2) en maximisant la variance représentée. Les deux premières composantes, qui concentrent l’essentiel de l’information sensorielle, sont retenues pour construire la cartographie.

La proportion de variance expliquée par chaque dimension s’appelle l’inertie.
Elle correspond à la part d’information du jeu de données capturée par l’axe.
Ce pourcentage indique dans quelle mesure notre plan factoriel résume efficacement la variabilité du jeu de données.
Dans l'idéal, nous souhaitons que notre plan explique au minimum 60% de la variabilité totale (% inertie Dim1 + % inertie Dim2).

```{r}
res.acp <- PCA(senso.cocktail)
```

#### Interprétation

La première dimension explique 53,25 % de la variabilité totale et la deuxième 24,13 %.
Ensemble, ces deux axes capturent donc une part importante de l'information.

Le graphique des variables permet d’interpréter les positions des cocktails sur le plan factoriel (graphique des individus).
Plus un cocktail est situé du côté d’un descripteur, plus il a obtenu une valeur élevée pour cet attribut.

Sur la dimension 1, plus un cocktail est situé à droite, plus il est perçu comme fort, acide, amer, avec des odeurs d’agrumes (citron et orange).
À l’inverse, plus il est situé à gauche, plus il est perçu comme sucré, épais, avec une odeur de banane.

Maintenant que les axes ont été interprétés, on peut analyser la distribution des cocktails sur le graphique des individus.
Les cocktails situés à gauche s’opposent sensoriellement à ceux situés à droite.
Par exemple, le cocktail 4, positionné à gauche, est jugé sucré, épais, pulpeux et présente une forte intensité olfactive.
À l’inverse, le cocktail 15, positionné à droite sur l’axe 1, est perçu comme plus fort, plus acide, et moins sucré.

## Prédire du liking

Nous souhaitons ensuite relier la position des produits dans la représentation sensorielle à l’appréciation des consommateurs.
Pour cela, un modèle de régression linéaire est ajusté pour chaque consommateurs, prenant en entrée les coordonnées ACP des produits qu’il a notés :

::: box
$$
Y = \beta_0 + \beta_1 \cdot x + \beta_2 \cdot y + \varepsilon
$$
:::

avec :\
- \\( Y\_{ij} \\) : note de liking attribuée par le consommateur i au produit j ; <br> - \\( \\beta\_{0i} \\) : niveau de satisfaction moyen ; <br> - \\( x\_{1j} \\) : coordonnée du produit j sur la dimension 1 de l'ACP ; <br> - \\( x\_{2j} \\) : coordonnée du produit j sur la dimension 2 de l'ACP ; <br> - \\( \\beta\_{1i}, \\beta\_{2i} \\) : effets directionnels des préférences du consommateur i dans l’espace sensoriel ; <br> - \\( \\varepsilon\_{ij} \\) : erreur aléatoire<br>

Chaque consommateur est ainsi modélisé par un plan de régression qui lui est propre, permettant d’estimer sa note pour n’importe quelle position dans l’espace sensoriel.
Ce choix repose sur l’hypothèse émise que deux produits proches dans l’espace sensoriel devraient être appréciés de manière similaire.

Selon les hypothèses psychophysiques retenues, il est possible d’inclure des effets quadratiques dans le modèle pour prendre en compte des phénomènes de saturation.
Si l’on considère que le liking augmente de manière linéaire avec l’intensité d’un attribut, un modèle linéaire suffit. En revanche, si l’on pense qu’au-delà d’un certain seuil, le liking décroît (ex. : trop sucré), il est nécessaire d’ajouter des termes quadratiques.

Voici le modèle où les effets quadratiques sont pris en compte:

::: box
$$
Y = \beta_0 + \beta_1 \cdot x + \beta_2 \cdot y + \beta_3 \cdot x^2 + \beta_4 \cdot y^2 + \varepsilon
$$
:::

avec :\
- $Y$ : note de liking prédite pour une position donnée dans l’espace sensoriel (x, y) ; <br> - $x$, $y$ : coordonnées du point dans le plan ACP (Dim1, Dim2) ; <br> - $\beta_0$ : constante (niveau moyen de liking) ; <br> - $\beta_1$, $\beta_2$ : effets directionnels linéaires ; <br> - $\beta_3$, $\beta_4$ : effets de courbure (prise en compte d'un seuil de saturation) ; <br> - $\varepsilon$ : erreur aléatoire.<br>

## Cartographier des zones de préférences

L’espace sensoriel obtenu suite à l'ACP est discrétisé en une grille fine.
Pour chaque point de cette grille, la note de liking est prédite à partir des modèles individuels.
On fixe ensuite un seuil d’acceptabilité (ex. 6/10).

Pour chaque point de la grille, on calcule le pourcentage de consommateurs dont la note prédite dépasse ce seuil.
Ce pourcentage permet de définir des zones de forte ou faible appréciation, en identifiant les régions où les produits sont susceptibles d’être aimés ou rejetés par une majorité de consommateurs.
Il n’y a plus qu'à sortir les crayons de couleurs : la zone sera coloriée en rouge si c’est le pourcentage est fort, et en bleu si le pourcentage est bas.

On obtient ainsi une carte où chaque zone reflète l’intensité prédite d’appréciation.
L’objectif est d’identifier les zones où un produit hypothétique aurait le plus de chances de plaire à une large part de consommateurs.

Voici la cartographie des préférences pour notre exemple avec les cocktails:

```{r}
res.carto <- carto(res.acp$ind$coord[,1:2], hedo.cocktail)
```

#### Interprétation

Sur la carte obtenue, deux zones distinctes apparaissent clairement : À droite, une zone en bleu, correspondant à des niveaux de liking faibles (inférieurs à 20 %) À gauche, une zone en rouge, où les pourcentages de liking sont élevés (jusqu’à 90 %).

Les cocktails 15 et 13 se situent dans la zone bleue.
On peut donc conclure que ces produits sont globalement peu appréciés par les consommateurs.
Grâce à l’ACP utilisé pour créer cette carte, il est possible de caractériser ces cocktails.
Ces cocktails sont caractérisés par une forte amertume, une acidité marquée, et des odeurs d’agrumes (citron, orange).

À l’inverse, le cocktail 3 est situé dans la zone rouge, avec un taux de liking de 90 %, ce qui indique qu’il est largement préféré.
D’après l’ACP, il est perçu comme sucré, épais, avec une intensité olfactive élevée, notamment marquée par des arômes de banane.
Cela suggère que les consommateurs privilégient des profils sensoriels, sucrés et fruités.

## Retour au sensoriel : profil du produit idéal dans l’espace produit

Une fois la zone de fort liking identifiée sur la cartographie, on peut en extraire les coordonnées dans le plan ACP (Dim1, Dim2).
Pour revenir à une interprétation sensorielle, on utilise la projection inverse de l’ACP, ce qui nous permet ainsi d’obtenir un profil sensoriel théorique du produit idéal.

## Limites

L’ACP ne conserve qu’une partie de la variance totale : la projection dans un plan 2D entraîne une perte d’information.
Il convient de vérifier que Dim1 et Dim2 expliquent une part suffisamment importante de la variabilité sensorielle (% d’inertie).\
*L’inertie représente la variance totale expliquée par chaque axe principal. Elle mesure la part d’information du jeu de données capturée par chaque dimension.*

## Conclusion

La cartographie des préférences est très apprécié en marketing et permet de cibler franchement les attributs sensoriels attendus d'un produit idéal.
Toutefois, cette méthode présentes de certaines limites.
Un coût très élevé (Panel d'expert, très grand nombre de consommateur, plusieurs produits à tester...) et reste cantonnée à l'espace produit sensoriel.
Et si le VRAI produit idéal se trouvait à l'extérieur de cet espace?

# Conclusion générale

La méthode QDA est utilisée avec un panel entraîné pour décrire le profil sensoriel de plusieurs produits. 
Grâce à l’analyse de la variance, il est possible d’identifier les descripteurs sensoriels pour lesquels le produit exerce un effet significatif.
Il devient alors possible de caractériser précisément chacun des produits testés. L’Analyse en Composantes Principales (ACP) permet ensuite de représenter l’espace produit, de visualiser les distances entre produits, et de tracer des ellipses de confiance. Cette représentation affine la caractérisation sensorielle des produits et permet de vérifier qu’ils sont perçus comme significativement différents les uns des autres et d’identifier les descripteurs les plus discriminants, c’est-à-dire ceux pour lesquels les produits ont des notes très distinctes. L’ACP permet également de repérer les corrélations entre les attributs sensoriels.

La méthode JAR repose sur un panel de consommateurs. Elle permet d’identifier les drivers of liking et de disliking, autrement dit les attributs sensoriels qui augmentent ou diminuent significativement l’appréciation globale du produit. En intégrant une dimension hédonique à la description produit (via les mentions “Trop”, “Pas assez” et “Juste comme il faut”), il devient possible de réaliser une analyse des pénalités et d’identifier les défauts sensoriels les plus critiques. Méthode très opérationnelle, elle oriente clairement les décisions de reformulation en identifiant les attributs à ajuster en priorité.

Le preference mapping est une méthode permettant de relier la perception sensorielle à l’appréciation. Elle commence par la construction d’un espace produit via une QDA, nécessitant un panel d’experts. Ensuite, un panel naïf évalue les produits selon leur niveau de liking, ce qui permet d’estimer l’appréciation sur l’ensemble de l’espace sensoriel. Bien que cette méthode soit coûteuse à mettre en œuvre (double panel, nombreux produits à tester), elle permet une modélisation fine et individualisée des préférences consommateurs et de prédire les notes de liking sur une grille fine. En fixant un seuil de liking (par exemple 6/10), on peut cartographier les zones de forte et faible appréciation, où un produit aurait respectivement plus ou moins de chances de plaire à une majorité de consommateurs, offrant ainsi une visualisation claire des préférences dans l’espace sensoriel.
