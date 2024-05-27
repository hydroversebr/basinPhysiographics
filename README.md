[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) 
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)

<img align='right' src='https://github.com/hydroversebr/hydrobr/blob/main/man/figures/logo.png' width="100">

<p align="center">
  <span>English</span> |
  <a href="https://github.com/hydroversebr/hydrobr/tree/main/lang/portugues">Português</a>

# hydrobr 


## Description

The basinDelimitationBR package was developed to help users to obtain digital elevation subproducts and delimitate watershed basins.

This is a voluntary initiative from a few Brazilian hydrologists and part of the <a href="https://github.com/hydroversebr/">hydroversebr</a>. The package improvement is open to enthusiasts. Contact us if you want to be part of the team and help us develop this project.

## Installing this package

You can download and install the most up-to-date version directly from this repository. The procedure is
1. Install the package "devtools" (you only have to do this once. Note that this will also install several dependancies)
2. Load the devtools library
3. Install the package.

The commands are:
``` R
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("hydroversebr/basinDelimitaionBR", build_vignettes = TRUE)
```
To read the vignettes and examples of how to use the package:

``` R
vignette(package = 'hydrobr', topic = 'intro_to_hydrobr')
```

## Contact

<div> 
  <a href = "mailto:hydroversebr@gmail.com; tcalegario@gmail.com; daniel_althoff@hotmail.com;"><img src="https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=gmail&logoColor=white" target="_blank"></a>

  ![](https://komarev.com/ghpvc/?username=hydrobr)



