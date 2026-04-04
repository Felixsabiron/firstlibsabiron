# Validation du schema du dataframe

Cette fonction verifie que le dataframe contient les colonnes
necessaires pour etre considere comme un objet de type "commune" ou
"departement".

## Usage

``` r
validate_schema(df)
```

## Arguments

- df:

  Un dataframe a valider.

## Value

Aucun retour si la validation est reussie. Stoppe avec une erreur sinon.

## Details

La fonction verifie la presence des colonnes essentielles :

- Pour un objet "commune" : `Code.de.la.commune`,
  `Libelle.de.la.commune`

- Pour un objet "departement" : `Code.du.departement`,
  `Libelle.du.departement`

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(
  Code.de.la.commune = "75056",
  Libelle.de.la.commune = "Paris"
)
validate_schema(data)
} # }
```
