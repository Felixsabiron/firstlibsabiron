# Creer un objet de type "departement"

Cette fonction transforme un dataframe representant un departement en un
objet de classe "departement".

## Usage

``` r
creer_departement(df)
```

## Arguments

- df:

  Un dataframe contenant au moins la colonne `Code.du.departement`.

## Value

Un dataframe de classe `"departement"`, qui peut etre utilise avec des
fonctions specifiques aux departements.

## Details

- La fonction verifie que la colonne `Code.du.departement` est presente
  dans `df`.

- Elle s'assure que le dataframe represente un seul departement, sinon
  elle genere une erreur.

- La classe `"departement"` est ajoutee a l'objet pour permettre un
  traitement specifique.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  Code.du.departement = rep("75", 10),
  Nom.du.departement = rep("Paris", 10),
  Population = c(100, 200, 150, 180, 220, 250, 300, 270, 260, 280)
)
departement <- creer_departement(data)
class(departement)  # Verifier la classe
} # }
```
