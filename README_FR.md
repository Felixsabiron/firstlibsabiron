# firstlibsabiron – Package R pour l’analyse des élus municipaux français

🇬🇧 [English version](README.md)

Ce projet consiste en le développement d’un **package R dédié à l’analyse des données sur les élus municipaux français**.

Son objectif est de fournir des outils simples et reproductibles permettant d’analyser les élus à l’échelle de la **commune** et du **département**, tout en permettant la génération automatique de **rapports HTML avec Quarto**.

Le package comprend trois fonctions principales :

- `summary_commune()` : résume les informations relatives aux élus d’une commune donnée
- `summary_departement()` : fournit une vue d’ensemble à l’échelle d’un département
- `generer_rapport()` : génère automatiquement un rapport d’analyse complet au format HTML

---

## 📑 Présentation du projet

Le package a été conçu afin de simplifier les workflows d’analyse de données territoriales et publiques sous R.

Il permet notamment de :

- résumer rapidement les données relatives aux élus
- étudier le nombre de communes et de représentants dans un département
- analyser la répartition des catégories socioprofessionnelles
- générer des rapports HTML reproductibles comprenant des statistiques descriptives et des visualisations

Ce projet combine :

- **développement de package R**
- **manipulation de données avec dplyr**
- **génération automatisée de rapports avec Quarto**
- **analyse de données publiques**

---

## 🌐 Site de documentation

Un site de documentation complet présentant le fonctionnement du package, le rôle de chaque fonction et des exemples pratiques est disponible ici.

🔗 **Accéder au site de documentation**  
https://felixsabiron.github.io/firstlibsabiron/

---

## 📌 Objectifs du projet

- 📊 Analyser les données relatives aux élus municipaux français
- 📈 Fournir des fonctions réutilisables de synthèse à l’échelle des communes et des départements
- 📝 Automatiser la génération de rapports avec Quarto
- 🔍 Faciliter des workflows reproductibles d’analyse de données publiques

---

## 📂 Structure du projet

```bash
.
├── R/
├── man/
├── vignettes/
├── data/
├── tests/
├── README.md
├── README_FR.md
└── DESCRIPTION
```
