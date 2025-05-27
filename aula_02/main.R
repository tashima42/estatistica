###
# Definição caminho, definições e pacotes
###

# Diretório do projeto, de onde serão salvos e lidos os arquivos
project_root_path <- getwd()

###
# Variáveis e definições
###
source(paste0(project_root_path, "/definitions.R"))

###
# Instalando e carregando pacotes 
###
source(paste0(project_root_path, "/install_load_packages.R"), encoding = encoding)

###
# Análise exploratória de dados para uma variável
###

# Dados de exemplo: diamonds (fonte: pacote ggplot)
# Referência 1 para aprender mais R: livro R para data science
# Referência 2 para aprender mais R: cusro R (https://livro.curso-r.com/)
# Referência 3 para aprender mais R: https://rpubs.com/EstatBasica

## Passo 1: obtenção dos dados
dados <- diamonds # tibble = data frames com ajustes que as deixam mais amigáves (https://livro.curso-r.com/7-1-tibbles.html)
dados
View(dados)
?diamonds # Informações sobre conjunto de dados -> Preços e outas características de quase 54.000 diamantes de corte redondo 
dim(dados)
str(dados)
## Observe:
## 1. formato dos dados
## 2. tipos de variáveis


## Passo 2: descrição dos dados
summary(dados)

###################
### 2.1 Análise exploratória de uma variável qualitativa
### corte: qualidade do corte (Regular, Bom, Muito Bom, Premium, Ideal)
###################
freq_abs <- table(dados$cut)
freq_rel <- round(prop.table(freq_abs)*100, 1)
gbarras1 <- barplot(freq_abs) #, plot = F
gbarras2 <- barplot(freq_abs, 
                    xlab = 'Qualidade do corte',
                    ylab = 'Frequência absoluta',
                    main = "Qualidade do corte (n)",
                    col = 'blue',
                    ylim = c(0,23500))
text(x = gbarras2, y = freq_abs+1000, labels = as.character(freq_abs))

gbarras3 <- barplot(freq_rel, 
                    xlab = 'Qualidade do corte',
                    ylab = 'Frequência absoluta',
                    main = "Qualidade do corte (%)",
                    col = 'blue',
                    ylim = c(0,42))
text(x = gbarras3, y = freq_rel+1, labels = as.character(freq_rel))

gpizza1 <- pie(freq_abs)
gpizza2 <- pie(freq_abs,
               labels = paste0(freq_rel,"%"),
               border = "white",
               col = rainbow(length(freq_abs)),
               main = 'Qualidade do corte')
legend("bottomright", levels(dados$cut), fill  =  rainbow(length(freq_abs)), cex = 0.7)

###################
### 2.2 Análise exploratória de uma variável quantitativa
### preço: preço, em dólares
###################
media <- mean(dados$price)
mediana <- median(dados$price)
quantis_default <- quantile(dados$price)
quantis_outros <- quantile(dados$price, probs = c(0.10, 0.20, 0.30, 0.40, 0.50))
variancia <- var(dados$price)
desvio_padrao <- sd(dados$price)
ghist1 <- hist(dados$price)
ghist2 <- hist(dados$price,
               main = "Histograma preço",
               ylab = "Frequência",
               xlab = "Preço",
               col = "lightgreen",
               border = "white",
               breaks = seq(0, 19000, by = 500))
gboxplot1 <- boxplot(dados$price)
gboxplot2 <- boxplot(dados$price,
                     pch = "*",  # tipo de marcador dos outliers
                     col = "lightblue", # cor do preenchimento do box plot
                     border = "darkgrey", # cor da linha do box plot
                     boxwex = 0.3 # Tamanho da caixa
)
title("Boxplot preço dos diamantes")

gboxplot3 <- boxplot(dados$price,
                     outline = FALSE, # exclui os outliers
                     col = "lightblue",
                     border = "darkgrey",
                     boxwex = 0.3
)
title("Boxplot preço dos diamantes, desconsiderando outliers")


