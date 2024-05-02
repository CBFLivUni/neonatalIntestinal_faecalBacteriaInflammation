## Investigation of neonatal inflammation in relation to key intestinal microflora

Scripts for the analysis performed in the publication:

*Features of the intestinal microbiome formation and the level of intestinal inflammatory response in newborns*
(Citation & Link to published location)

Repository contains:

`02_scripts/`: Contains the scripts (mix of `Rmds` and `.R` files) as well as the R package dependencies for the analyses. 
* `01_eda.Rmd`: Code for performing initial exploratory and univaraite analyses on the dataset.
* `02_fixed_effects_linear_modelling.Rmd`: Code for performing simple fixed-effects multiple regression models on data.
* `03_random_effects_modelling.Rmd`: Code to perform mixed-effects modelling of patients over time.
* `extra_functions.R`: Additional functions used in other code.
* `get_libraries.R`: Functions for reading in and installing relevant R librares in:
* `package_dependencies/`: `.json` files of R packages to install with github, BiocManager or default R installation.
