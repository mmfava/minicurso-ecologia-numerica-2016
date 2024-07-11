# Semana Acadêmica de Biologia
# Minicurso de Ecologia Numérica
# Marília Melo Favalesso e Thaís Maylin Sobjak

# Medidas de Diversidade

# Como calcular o índice de Shannon
abund <- c(13, 23, 29, 29, 30, 48, 72, 76, 83, 84, 86, 97, 102, 229, 343)
tot <- sum(abund) 
tot
pi <- abund / tot  # n/N
pi
log.pi <- log(pi)
log.pi
pi.log.pi <- pi * log.pi
pi.log.pi
H <- -sum(pi.log.pi)
H

# Pode ser realizado com uma única fórmula
-sum(abund / sum(abund) * log(abund / sum(abund)))

# Além disso, alguns pacotes permitem calcular os índices de forma ainda mais direta

library(vegan)
library(Hotelling)

# Para começar, vamos abrir um conjunto de dados pré-construído chamado "dune"
data(dune)
dune

# Verificando os nomes das colunas
names(dune)

# O número de colunas, que neste exemplo é o número de espécies
ncol(dune)

# O número de linhas, que neste exemplo é a quantidade de amostras
nrow(dune)

# Depois de verificarmos nossos dados, podemos começar a calcular os índices de diversidade

# Cálculo de Shannon usando a função diversidade
shannon <- diversity(dune)
shannon

# Cálculo do índice de Simpson
simpson <- diversity(dune, "simpson")
simpson

# Histograma dos dois índices
par(mfrow = c(1, 2))  # para gerar um painel com 2 gráficos
hist(shannon, main = "Índice de Shannon", ylab = "Frequência", xlab = "")
hist(simpson, main = "Índice de Simpson", ylab = "Frequência", xlab = "")

# Podemos calcular a rarefação
sp.abund <- rowSums(dune)  # número de indivíduos em cada amostra
raremax <- min(rowSums(dune))  # a rarefação usa o menor número de observações por amostra para extrapolar o número esperado
raremax

Srare <- rarefy(dune, raremax)
par(mfrow = c(1, 2))
plot(sp.abund, Srare, xlab = "Número de spp observadas", ylab = "Número de spp esperadas")
rarecurve(dune, col = "blue")
