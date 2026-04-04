# Calculer la distribution des ages des elus

Cette fonction calcule la distribution des ages a partir d'une colonne
`Date.de.naissance` en format **Date**.

## Usage

``` r
calcul_distribution_age(data)
```

## Arguments

- data:

  Un dataframe contenant au moins la colonne suivante :

  - `Date.de.naissance` (Date) : La date de naissance des elus, qui doit
    etre deja convertie en format `Date`.

## Value

Un vecteur numerique representant les ages des elus en annees.

## Details

- La fonction verifie la presence de la colonne `Date.de.naissance`.

- Elle verifie que la colonne est bien de type **Date**, sinon elle
  renvoie une erreur.

- Elle calcule l'age en annees en utilisant la difference entre la date
  du jour ([`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)) et la
  `Date.de.naissance`.

## Examples

``` r
if (FALSE) { # \dontrun{
library(dplyr)
library(lubridate)

# Exemple de dataframe avec la colonne en format Date
data <- data.frame(
  Date.de.naissance = as.Date(c("1950-05-12", "1965-09-23", "1948-11-07"))
)

# Calculer la distribution des ages
calcul_distribution_age(data)
} # }

```
