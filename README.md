# SENSO
# Données QDA (Quantitative Descriptive Analysis)
## Contexte 

Variétés de fraises? <br>
Comment ça variétés de fraises? <br>
Elle a quoi ma fraise? <br>
Une fraise, c’est une fraise non? <br>

On compte plus de 600 variétés de fraises, mais perçoit-on réellement une différence d'une variété à l'autre ? <br>

Quand un producteur affirme que la variété qu’il a sélectionnée avec soin et cultivée avec amour est la plus sucrée, est-ce vrai ?

Ou est-ce seulement une astuce marketing ? <br>
Le goût ou la texture de toutes les variétés de fraises sont-ils perçus de la même manière ? <br>
Y’a t-il des fraises significativement plus rouges que d'autres ? <br>

Pour répondre à ces questions, nous avons mené notre enquête.
Ou plutôt notre analyse (sensorielle évidemment).


## Cadre de l’analyse

4 variétés de fraises, toutes cultivées par une petite productrice près de Rennes, ont été évaluées par 12 juges naïfs (étudiants à l'Institut Agro de Rennes) lors de deux sessions successives selon 10 descripteurs sensoriels. Une fraise par variété et par session a été dégustée.

Les descripteurs sensoriels étaient les suivants : Taille, Couleur, Odeur, Ferme, Juteux, Fondant, Sucrée, Acide, Arôme et Fraise des bois

La réponse était obligatoire. Les notes pouvaient prendre des valeurs de 0 (très faiblement perçu) à 10 (énormément perçu).

## Jeu de données

Les données ont été recueillies grâce au logiciel Fizz®, spécialisé dans la mise en place de tests sensoriels.

Comme tout bon statisticien le sait, un jeu de données, bien structuré et propre, est la condition préalable à toute analyse fiable et pertinente.

Notre premier jeu de données, intitulé fraise1, se présente sous la forme d’un dataframe de 96 lignes et 14 colonnes.

Chaque ligne correspond à une fiche de dégustation individuelle.
Nous avons 12 juges × 2 sessions × 4 variétés = 96 fiches de dégustation.
Chaque colonne contient soit :
l’une des 10 notes attribuées aux descripteurs sensoriels,
soit une variable contextuelle liée à la dégustation :
le numéro du juge,
la variété de la fraise dégustée,
le numéro de la session.

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

dta1<-read.csv("2025_resultats_2R.csv", header = TRUE, sep=";", stringsAsFactors = TRUE)
dta2<-read.csv("2025_resultats_2Rco.csv", header = TRUE, sep=";", stringsAsFactors = TRUE)

dta1 <- dta1[,c(-1, -2, -5)]
dta2<-dta2[,c(-1, -2, -5)]

dta1$NR <-as.factor(dta1$NR)
dta1$ProductName <-as.factor(dta1$ProductName)
dta1$CJ <-as.factor(dta1$CJ)
dta1$AttributeName <-as.factor(dta1$AttributeName)

dta2$NR <-as.factor(dta2$NR)

dta1p<-pivot_wider(dta1, names_from = AttributeName, values_from = Note)
head(dta1p)
summary(dta1p)

#Création 1 seul dataset
fraise1 <- left_join(dta1p, dta2, by=c("CJ", "ProductName", "NR" ))
summary(fraise1)

```

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

## Etude des données

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

# Ajouter une légende
legend("topright", legend = rownames(df_radar), 
       col = colors_border, 
       lty = 1, lwd = 2, bty = "n")

```



Nous avons donc observé des différences dans la perception des 4 variétés de fraises testées.

Pour creuser et affiner notre observation, nous adoptons une approche en trois temps :
1. Analyse unidimensionnelle : observer la différence variété par variété, descripteur par descripteur.
2. Analyse multidimensionnelle : représenter les produits dans un espace sensoriel global (ACP).
3. Validation par répétition : estimer la variabilité des résultats grâce à des techniques de bootstrap.

## Analyse UNIDIMENSIONNELLE
### Modélisation (ANOVA)

Pour analyser les différences entre les variétés de fraises, nous utilisons un modèle d’analyse de la variance (ANOVA) qui prend en compte trois effets principaux :

l’effet de la variété (α<sub>i</sub>)
l’effet du juge (γ<sub>j</sub>)
l’effet de la session (λ<sub>k</sub>)

L’objectif de l’ANOVA est de tester si les moyennes des notes attribuées varient significativement d’une variété à l’autre, en tenant compte des différences liées aux juges ou aux conditions de test.

Le modèle est le suivant :

<div class="box-girly"> $$ Y_{ijk} = \mu + \alpha_i + \gamma_j + \lambda_k + \epsilon_{ijk} $$ </div>
Où :

Y<sub>ijk</sub> : note attribuée à un descripteur
μ : moyenne globale
α<sub>i</sub> : effet de la variété
γ<sub>j</sub> : effet du juge
λ<sub>k</sub> : effet de la session
ε<sub>ijk</sub> : erreur aléatoire
(sous contrainte : ∑α<sub>i</sub> = 0)

Les variétés étant précisément définies et d’intérêt principal, elles sont considérées comme un effet fixe.
L’effet du juge et de la session sont considérés comme aléatoire car nous souhaitons généraliser nos conclusions à d’autres panels de juges similaires. Ce choix permet de modéliser la variabilité inter-juges et inter-session comme un bruit aléatoire plutôt que comme une constante à estimer pour chaque juge ou pour chaque session.

Par curiosité, nous avons comparé avec un modèle plus simple :

<div class="box-girly"> $$ Y_{i} = \mu + \alpha_i +\epsilon_i $$ </div>


Nous constatons que :  

Les coefficients α<sub>i</sub> restent inchangés : les effets mesurés pour chaque variété sont les mêmes.
En revanche, les tests statistiques évoluent (la statistique de test t et sa p-value).  

Dans un plan équilibré (même nombre de juges pour chaque produit), les moyennes ajustées coïncident avec les moyennes simples. Le choix du modèle influence alors uniquement la variance résiduelle, donc la significativité, mais pas l’estimation des effets. Autrement dit : l’effet variété reste le même, mais le "bruit" augmente, ce qui impacte la fiabilité statistique des résultats.  
Notre plan à nous était bien évidemment équilibré, avec le même nombre d’observation par combinaisons. 

Ainsi, nous observons belles est bien des différences, parfois significatives, dans la perception des 4 variétés de fraises.  


#### Fonction decat
Nous utilisons la fonction ``decat`` du package SensoMineR. 
``decat`` permet d’identifier les attributs sensoriels qui distinguent réellement les produits. Pour un producteur ou un responsable qualité, cela signifie savoir sur quels critères sensoriels miser pour différencier une variété.
Concrètement, ``decat`` fait tourner une ANOVA pour chaque descripteur sensoriel afin de déterminer :
les descripteurs les plus discriminants (effet variété globalement significatif)
ceux qui caractérisent une variété en particulier (test t).

En sortie, on obtient :
un tableau couleurs :
bleu = descripteur > moyenne
rouge = descripteur < moyenne


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
On obtient également plusieurs objets, parmi lesquels  resT qui donne la carte d'identité sensorielle de chaque variété.



# INTERPRETATION POUR LES FRAISES

L’analyse unidimensionnelle permet d’identifier les descripteurs sensoriels qui différencient le plus les variétés entre elles. Ces descripteurs peuvent devenir des leviers de communication ou de sélection : une variété particulièrement fondante ou riche en arôme de fraise des bois peut être positionnée comme « gourmande », tandis qu’une variété perçue comme plus ferme ou plus acide conviendra peut-être plus à la transformation.



# EXPLIQUER INTERRACTION 


### Analyse multidimensionnelle (ACP)

Nous appliquons une Analyse en Composantes Principales (ACP) sur le tableau des moyennes ajustées extrait de decat. 

L’objectif est de projeter les produits dans un espace sensoriel global et de visualiser leurs proximités et différences.
Lors d’une ACP, les données sont centrées (soustraction de la moyenne de chaque variable) et réduites (division par l’écart-type), afin de ne pas donner plus de poids aux descripteurs les plus dispersés. 

Les deux premières composantes principales retenues pour la représentation graphique sont celles qui expliquent la plus grande part de la variabilité totale, et qui offrent une lecture sensorielle contrastée entre les produits, c'est à dire la projection dns l'espace qui différencient le mieux nos variétés de fraises). 


# INTERPRETATION






Les deux premiers axes expliquent XX % de la variabilité totale (préciser)
La Fraise A est plus acide, ferme, colorée (gauche du graphe)….BLABLA


L’ACP permet de visualiser les variétés dans un espace sensoriel simplifié. Cette représentation aide à segmenter les produits selon des profils perceptifs clairs.



### Validation par Bootstrap 

L’ACP est faite sur des moyennes observées. Mais que se passerait-il si nous avions interrogé d’autres juges ? 

Plutôt que de faire appel à un nouveau panel pour déguster nos 4 variétés de fraises, nous utilisons la méthode du Bootstrap, une technique de rééchantillonage.

Cette méthode consiste à générer un grand nombre de jeux de données simulés en tirant avec remise des juges présents dans l’échantillon initial. Pour chaque jeu "bootstrapé", on recalcule les moyennes ajustées par variété, puis on projette ces nouvelles moyennes dans l’espace sensoriel défini par l’ACP. Ce processus est répété un très grans nombre de fois.

Le bootstrap ne nous donne pas la distribution exacte des perceptions, mais une approximation fondée sur les juges présents. Pour utiliser cette technique, il faut émmettre l'hypothèse que le panel réel est représentatif d’un ensemble plus large de consommateurs.


### Ellipses de confiance

L’ensemble des projections issues des nouveaux jeux de données "bootstrapés" forme un nuage de points autour de chaque variété, dont on extrait une ellipse de confiance. Cette ellipse représente la variabilité potentielle de la perception moyenne de la variété si l’on recommençait l’étude avec un autre panel.

Plus l’ellipse est petite, plus la perception est stable d’un panel à l’autre.
Si deux ellipses se confondent ou se chevauchent, cela signifie que les différences entre les variétés n'ont pas ou peu été perçues.
À contrario, si les ellipses sont bien distinctes, alors les produits ont été perçus de manière significativement différente, indépendamment du choix des juges.


Pour automatiser cette procédure, nous utilisons la fonction ``panellipse`` du package SensoMineR, qui réalise à la fois le bootstrap, le recalcul des ACP et la génération des ellipses.


Contrairement à decat, la fonction panellipse sélectionne uniquement les descripteurs sensoriels significatifs (au seuil de 5 %) pour la construction de l’ACP, ce qui renforce la lisibilité de la représentation.

Deux graphiques sont produits :

Graphique des individus : chaque produit est représenté avec une ellipse de confiance. Si deux ellipses se chevauchent, cela signifie que les différences entre les variétés ne sont pas statistiquement robustes. Si elles sont distinctes, les produits sont perçus différemment, indépendamment du panel.
Graphique des variables : les petits points autour des flèches indiquent la variabilité de chaque descripteur dans les jeux bootstrap. Plus ces points sont concentrés, plus le descripteur est stablement représenté, donc discriminant dans l’espace sensoriel.

```{r, include=TRUE}

elispsefraise <- panellipse(fraise1, col.p = 3, col.j = 2, firstvar = 4, lastvar = 13)


#Paramètres : 
#le jeu de données (ici fraise1), 
#la variable Juge (ici NR), 
#la variable Produit (ici CJ) 
#la position du premier descripteur sensoriel (ici la variable “Taille”, en position 4).

```




# INTERPRETATION


L’analyse par bootstrap valide la robustesse des différences perçues. Si une variété est bien différenciée mais son ellipse de confiance est large ou chevauche celle d’un concurrent, cela signifie que cette différenciation est instable : un autre panel pourrait percevoir autrement. 


