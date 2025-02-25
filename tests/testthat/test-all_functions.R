library(testthat)
library(dplyr)
library(firstlibsabiron)

# Vérifie que les données sont bien chargées
skip_if_not_installed("firstlibsabiron")

# Charger les donnees du package
data("elus_conseillers_municipaux_cm", package = "firstlibsabiron")


#### 1️⃣ TESTS POUR creer_commune() ####
test_that("creer_commune fonctionne avec une seule commune", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Libelle.de.la.commune == "Paris")
 result <- creer_commune(df_test)
 expect_true(inherits(result, "commune"))
})

test_that("creer_commune renvoie une erreur si plusieurs communes", {
 expect_error(creer_commune(elus_conseillers_municipaux_cm),
              "Le dataframe contient plusieurs communes. Fournissez les donnees d'une seule commune.")
})

#### 2️⃣ TESTS POUR summary.commune() ####
test_that("summary.commune fonctionne correctement", {
 df_test <- elus_conseillers_municipaux_cm %>%
  dplyr::filter(Libelle.de.la.commune == "Paris") %>%
  creer_commune()

 result <- summary(df_test)
 expect_true(is.list(result))
 expect_s3_class(result, "summary.commune")
})


#### 5️⃣ TESTS POUR plot.commune() ####
test_that("plot.commune genere un ggplot valide", {
 df_test <- elus_conseillers_municipaux_cm %>%
  filter(Libelle.de.la.commune == "Paris") %>%
  creer_commune()

 p <- plot(df_test)
 expect_s3_class(p, "ggplot")
})

#### 6️⃣ TESTS POUR plot.departement() ####
test_that("plot.departement genere un ggplot valide", {
 df_test <- elus_conseillers_municipaux_cm %>%
  filter(Code.du.departement == "75") %>%
  creer_departement()

 p <- plot(df_test)
 expect_s3_class(p, "ggplot")
})

#### 7️⃣ TESTS POUR summary.commune() ####
test_that("summary.commune fonctionne correctement", {
 df_test <- elus_conseillers_municipaux_cm %>%
  filter(Libelle.de.la.commune == "Paris") %>%
  creer_commune()

 result <- summary(df_test)
 expect_true(is.list(result))
 expect_s3_class(result, "summary.commune")
})

#### 8️⃣ TESTS POUR summary.departement() ####
test_that("summary.departement fonctionne correctement", {
 df_test <- elus_conseillers_municipaux_cm %>%
  filter(Code.du.departement == "75") %>%
  creer_departement()

 result <- summary(df_test)
 expect_true(is.list(result))
 expect_s3_class(result, "summary.departement")
})

