# firstlibsabiron – R Package for French Elected Officials Analysis

This project consists of the development of an **R package dedicated to the analysis of French municipal elected officials data**.

Its objective is to provide simple and reproducible tools to analyze elected representatives at both the **commune** and **department** levels, while also allowing the automatic generation of **HTML reports with Quarto**.

The package includes three main functions:

- `summary_commune()`: summarizes elected officials for a specific commune
- `summary_departement()`: provides an overview at the department level
- `generer_rapport()`: generates a complete HTML analysis report

---

## 📑 Project Overview
The package was designed to simplify territorial and public data analysis workflows in R.

It allows users to:

- quickly summarize elected officials data
- study the number of communes and representatives in a department
- analyze professional category distributions
- generate reproducible HTML reports with descriptive statistics and visualizations

This project combines:

- **R package development**
- **data manipulation with dplyr**
- **automated reporting with Quarto**
- **public data analysis**

---

## 🌐 Documentation Website
A full documentation website presenting the package workflow, the purpose of each function, and practical examples is available here.

Please note that the documentation website is written in **French**.

🔗 **Access the French documentation website**  
https://felixsabiron.github.io/firstlibsabiron/articles/generer_un_rapport_d_analyses.html

---

## 📌 Project Objectives
- 📊 Analyze French municipal elected officials data
- 📈 Provide reusable summary functions for communes and departments
- 📝 Automate report generation with Quarto
- 🔍 Facilitate reproducible public sector data analysis workflows

---

## 📂 Project Structure
```bash
.
├── R/
├── man/
├── vignettes/
├── data/
├── tests/
├── README.md
└── DESCRIPTION
