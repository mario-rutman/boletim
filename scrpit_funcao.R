library(tidyverse)
# Primeiro importar o arquivo do boletim com as notas, vou chamar de boletim.

library(readr)
boletim <- read_csv2("boletim_formato_modelo.csv")
View(boletim)
saveRDS(boletim, "boletim.rds")


nota_segundo_trimestre <- seq(10, 0, -1)
nota_terceiro_trimestre <- round((21 - ) - nota_segundo_trimestre, 0)
comb_nota_2_e_3 <- tibble(nota_segundo_trimestre, nota_terceiro_trimestre) %>% 
  filter(nota_segundo_trimestre <= 10 & nota_terceiro_trimestre <= 10)  
  


# O objetivo aqui é criar uma função que fará a combinação de notas
# tanto depois da primeira prova quanto da segunda.
# Os argumentos serão: trimestre(1 ou 2) e notas(nota_1 e nota_2)


# trim_1 dá a combinação de notas depois da prova_1.
trim_1 <- function(nota_1){
  prova_2 <- seq(10, 0, -1)
  prova_3 <- round((21 - nota_1) - prova_2, 1)
  comb_nota_2_e_3 <- tibble(prova_2, prova_3) %>% filter(prova_2 <= 10 & prova_3 <= 10)
  return(comb_nota_2_e_3)
}


# trim_2 dá a combinação de notas depois da prova_2
trim_2 <- function(nota_1, nota_2){
  prova_3 <- seq(10, 0, -1)
  vg <- round(10 - (nota_1 + nota_2 + prova_3)/3, 1)
  comb_nota_3_e_vg <- tibble(prova_3, vg) %>% 
    mutate(vg = ifelse(vg <= 3, "Aprov. Dir.", vg))
  return(comb_nota_3_e_vg)
}

trim_3 <- function(nota_1, nota_2, nota_3){
  if(mean(c(nota_1, nota_2, nota_3)) >= 70)
  {
    paste("A P R O V A D A com média",round(mean(c(nota_1, nota_2, nota_3))))
  } else {
    paste("Nota mínima necessária para aprovação:",100 - floor(mean(c(nota_1, nota_2, nota_3))))  
  } 
}



# comb_notas dá a combinação de notas a partir do trimestre e das notas.

comb_notas <- function(trimestre, nota_1, nota_2){
  if (trimestre == "primeiro") {
    trim_1(nota_1)
  } else {
    trim_2(nota_1, nota_2)
  }
}

# Por exemplo: primeiro trimestre e nota 7
comb_notas('primeiro', 7) 

# primeiro trimestre com nota 9
comb_notas("primeiro", 9)

# segundo trimestre com notas 8 e 3.7
comb_notas("segundo", 8, 3.7)

# notas 8 e 8.5 (se teve duas notas é óbvio que é segundo trimestre)
comb_notas("segundo", 8, 8.5)

# Mas o que quero mesmo é uma função que crie o grob a partir da tibble comb_notas.

grob_02 <- function(disciplina, trimestre){
  
  # Primeiro localiza a nota.
  nota_1 <- ghj %>% filter(nome == params$nome & trimestre == "primeiro" & materia == disciplina) %>% 
    group_by(materia) %>% summarise(result = sum(nota)) %>% ungroup() %>% select(result) %>% as.numeric()
  
  nota_2 <- ghj %>% filter(nome == params$nome & trimestre == "segundo" & materia == disciplina) %>% 
    group_by(materia) %>% summarise(result = sum(nota)) %>% ungroup() %>% select(result) %>% as.numeric()
  
  # Depois aplica-se a nota à função combinação de notas.
  comb <- comb_notas(trimestre, nota_1, nota_2)
  
  # Isto resulta num data frame que tem que ser transformado em grob.
  
  cvm <- tableGrob(comb, rows = NULL)
  
  return(cvm)
  
}
