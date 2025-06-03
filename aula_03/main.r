project_root_path <- getwd()
source(paste0(project_root_path, "/definitions.R"))
source(paste0(project_root_path, "/install_load_packages.R"), encoding = encoding)

library(readxl)
veiculos <- read_excel("aula_03/veiculos.xls")
View(veiculos)

veiculos <- veiculos %>%
  dplyr::mutate(
    veiculo_adj = factor(veiculo),
    proc_adj = factor(proc)
  )

head(veiculos)
str(veiculos)
summary(veiculos)

# AED de duas variaveis quantitativas
## grafico de dispersao
plot(
  x = veiculos$preco,
  y = veiculos$motor,
  xlab = "Preço",
  ylab = "Potência do motor",
  main = "Gráfico de dispersão preço potência"
)

## correlacao
cor(x = veiculos$preco, y = veiculos$motor, method = "spearman")

# AED de uma variavel quant e uma quali
## grafico boxplot
boxplot(veiculos$motor ~ veiculos$proc_adj,
  col = "lightblue",
  border = "gray",
  xlab = "Procedencia",
  ylab = "Potencia do motor",
  main = "Boxplot potencia por procedencia"
)

plot(
  x = veiculos$comp,
  y = veiculos$preco,
  pch = as.numeric(veiculos$proc_adj),
  col = veiculos$proc_adj,
  ylab = "Preco",
  xlab = "Comp",
  main = "Dispersao entre preco e comp por procedencia"
)
legend("topleft",
  legend = c("Importado", "Nacional"),
  pch = c(1, 2),
  col = c("black", "red"),
  bty = "n"
)

## Grafico particao
ggplot2::ggplot(data = veiculos, aes(x = motor, y = comp)) +
  geom_point() +
  facet_wrap(~proc)
tapply(veiculos$comp, veiculos$proc, summary)
tapply(veiculos$motor, veiculos$proc, summary)

tabela_correlacao <- round(cor(veiculos[, c("preco", "comp", "motor")]), 1)
tabela_correlacao
ntil155e

# SONO

qualidade_sono <- qualidade_sono %>%
  dplyr::mutate(
    sleep_disorder_adj = factor(Sleep.Disorder),
    occupation_adj = factor(Occupation)
  )


plot(
  x = qualidade_sono$Sleep.Duration,
  y = qualidade_sono$Quality.of.Sleep,
  xlab = "Duracao do sono",
  ylab = "Qualidade do sono",
  main = "Gráfico de dispersão Duracao e Qualidade do sono"
)

boxplot(qualidade_sono$Physical.Activity.Level ~ qualidade_sono$sleep_disorder_adj,
  col = "lightblue",
  border = "gray",
  xlab = "Desordem  de sono",
  ylab = "Nivel de atividade fisica",
  main = "Boxplot desordem de sono do sono por nivel de atividade fisica"
)
