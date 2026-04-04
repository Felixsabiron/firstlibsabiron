# Visualiser la repartition des elus par code professionnel

La fonction compte le nombre d'elus pour chaque code professionnel
(`Code.de.la.categorie.socio.professionnelle`), filtre les codes n'ayant
aucun elu, puis produit un graphique en barres classe par ordre
decroissant.

## Usage

``` r
plot_code_professions(data)
```

## Arguments

- data:

  Un dataframe contenant au moins la colonne suivante :

  - `Code.de.la.categorie.socio.professionnelle` (character) : Code
    professionnel des elus.

## Value

Un graphique `ggplot2` representant la repartition des elus par code
professionnel.

## Details

Cette fonction genere un graphique en barres horizontales representant
le nombre d'elus par code professionnel.

- Les valeurs a `n = 0` sont filtrees avant l'affichage du graphique.

- Le graphique est trie par nombre d'elus de maniere decroissante.

- Les barres sont colorees en bleu fonce pour une meilleure lisibilite.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
library(dplyr)

# Exemple de dataframe
data <- data.frame(
  Code.de.la.categorie.socio.professionnelle = c("A1", "B2", "A1", "C3", "B2", "A1")
)

# Generer le graphique
plot_code_professions(data)
} # }
```
