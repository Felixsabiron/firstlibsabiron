# Générer un rapport d’analyses

## Introduction

Ce document montre comment utiliser les fonctions summary_commune,
summary_departement, et generer_rapport du package **firstlibsabiron**
pour générer des rapports d’analyses.

- summary_commune(): pour obtenir un résumé des élus d’une commune.

- summary_departement(): pour analyser les élus d’un département.

- generer_rapport(): pour générer un rapport HTML complet avec Quarto.

Le package firstlibsabiron permet d’analyser les données des élus
français à travers ces fonctions.

## 1. Utilisation de summary_commune

La fonction summary_commune permet de résumer les données pour une
commune donnée.

Elle affiche :

- Le nom de la commune
- Le nombre total d’élus
- La repartition professionnelle

#### Execution de summary_commune()

``` r
library(firstlibsabiron)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

``` r
# Exemple pour la commune de Nantes (44109)
commune_data <- elus_conseillers_municipaux_cm %>%
  filter(Code.de.la.commune == "44109") %>%
  creer_commune()

summary_commune(commune_data)
#> Resume de la commune :
#> Nom de la commune : Nantes 
#> Nombre total d'elus : 66 
#> 
#> Repartition professionnelle :
#> # A tibble: 25 × 2
#>    Code.de.la.categorie.socio.professionnelle     n
#>                                         <dbl> <int>
#>  1                                         11     1
#>  2                                         13     1
#>  3                                         21     1
#>  4                                         22     2
#>  5                                         23     2
#>  6                                         31     6
#>  7                                         33     8
#>  8                                         34     4
#>  9                                         35     5
#> 10                                         37    11
#> # ℹ 15 more rows
```

**Interprétation des résultats :**

- le nom de la communne est “Nantes”
- Il y a un total de 66 elus dans la commune de Nantes

## 2. Utilisation de summary_departement

La fonction summary_departement() offre une vue d’ensemble des élus d’un
département.

Elle affiche le resume du departement, soit :

- Le nom du département
- Le nombre total de communes
- Le nombre total d’elus

#### Exécution de summary_departement()

``` r
# Exemple pour le département de la Loire-Atlantique (44)
departement_data <- elus_conseillers_municipaux_cm %>%
  filter(Code.du.departement == "44") %>%
  creer_departement()

summary_departement(departement_data)
#> Resume du departement :
#> Nom du departement : Loire-Atlantique 
#> Nombre total de communes : 207 
#> Nombre total d'elus : 4791
```

**Interprétation :**

- Le département étudié est Loire-Atlantique.
- Il contient 207 communes différentes.
- Il y a 4791 elus dans ce department

La fonction summary_departement() est un outil permettant une analyse
rapide sur le nombre de communes représentées et la structure
démographique des élus.

## Génération d’un Rapport Quarto

### Fonction generer_rapport(commune, departement, output)

La fonction generer_rapport() permet de générer un rapport interactif en
HTML avec Quarto.

Le rapport contiendra :

- Un résumé des élus de la commune et du département.
- Des statistiques détaillées sur les élus.
- Des graphiques pour illustrer les résultats.

Le fichier rapport_commune.html sera généré et contiendra toutes les
analyses.

### 3. Utilisation de generer_rapport

Pour générer un rapport HTML avec Quarto : Exemple avec la commune 44109
et le departement 44: **generer_rapport(commune = “44109”, departement =
“44”, output = “rapport_nantes_final.html”)**
