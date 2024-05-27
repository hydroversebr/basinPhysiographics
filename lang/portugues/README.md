
<img align='right' src='https://github.com/hydroversebr/hydrobr/blob/main/man/figures/logo.png' width="100">

<p align="center">
  <span>Português</span> |
  <a href="https://github.com/hydroversebr/hydrobr">English</a>

# basinDelimitationBR

## Descrição

O pacote basinDelimitationBR foi desenvolvido para ajudar os usuários a obter subprodutos de modelos digitais de elevação e delimitar bacia hidrográficas. Por favor, certifique-se de que os processamentos estejam corretos em seu estudo de caso por meio de uma verificação cruzada e nos informe.

Este é uma iniciativa voluntária de alguns hidrólogos brasileiros e faz parte do <a href="https://github.com/hydroversebr/">hydroversebr</a>. O aprimoramento do pacote é aberto à entusiastas. Contate-nos se você quer fazer parte do time e ajude-nos a desenvolver esse projeto.

## Instalando o pacote

Você pode baixar e instalar a versão mais atualizada do pacote a partir desse diretório. O procedimento é:
1. Instalar o pacote "devtools" (Você só precisa fazer isso uma vez. Note que serão instaladas várias dependências.)
2. Carregar o pacote "devtools"
3. Instalar o pacote hydrobr

Os comandos são:
``` R
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("hydroversebr/hydrobr", build_vignettes = TRUE)
```

Para ler os vignettes e examplos de como usar:
``` R
vignette(package = 'hydrobr', topic = 'intro_to_hydrobr')
```


## Contato

<div> 
  <a href = "mailto:hydroversebr@gmail.com; tcalegario@gmail.com; daniel_althoff@hotmail.com;"><img src="https://img.shields.io/badge/Gmail-D14836?style=for-the-badge&logo=gmail&logoColor=white" target="_blank"></a>

![](https://komarev.com/ghpvc/?username=hydrobr)

