## XXV SEMANA ACADÊMICA DE CIÊNCIAS BIOLÓGICAS
## UNIVERSIDADE ESTADUAL DO OESTE DO PARANÁ
## PRÁTICA 2 - ANÁLISE DE COMPONENTES PRINCIPAIS

## Marília Melo Favalesso

## A Análise de Componentes Principais (PCA) é uma técnica de simplificação de uma base de dados multivariada.
## Esta análise auxilia na representação de uma variável estatística reduzida, baseada na variação de um banco de dados complexo.

## O banco de dados deve conter apenas variáveis quantitativas. Se a base de dados contiver variáveis qualitativas (não-numéricas), você deve excluí-las.

## A função que usaremos é a 'princomp', a qual calculará os desvios-padrões dos componentes principais.
## Além disso, iremos explorar cargas fatoriais e escores padronizados.

## Passo 0: Diretório
# setwd("minicurso-ecologia-numerica-2016/dia1")

## Passo 1: Carregando os pacotes necessários para a execução da PCA
library(vegan)
library(ggfortify)
library(psych)
library(MVN)

## Passo 2: Carregando o banco de dados

## Abrir a base de dados
dados <- read.csv2("trabalho-PCA.csv", sep = ",", dec = ".", header = TRUE)
dados
names(dados)

## Removendo a coluna nominal
dados1 <- dados[, -1]
dados1
attach(dados1)

## Pressupostos para a aplicação da Análise de Componentes Principais

## a. Todas as variáveis são numéricas?
dados1

## b. Existe associação entre as variáveis?

## Correlação linear de Pearson
correlacao <- cor(dados1)
correlacao
pairs(cbind(dados1), pch = "+", col = "blue")

## Teste de Esfericidade de Bartlett
## Teste de esfericidade de Bartlett entre a matriz de correlação e o tamanho da amostra (n).
## Ele apresenta a significância da associação entre pelo menos algumas das variáveis amostradas (P < 0,05).
bartlett.test(dados1)
# Resultado: X² =  , P =

## c. Critério de Kayser-Meyer-Olkin (KMO)
## Adequação amostral segundo a medida KMO.
## > 0,9       Excelente
## (0,8; 0,9]  Meritória
## (0,7; 0,8]  Intermediária
## (0,6; 0,7]  Medíocre
## (0,5; 0,6]  Mísera
## < 0,5       Inaceitável
KMO(correlacao)

## Ajustando os dados
dados <- dados[, -7]
dados
names(dados)
attach(dados)

dados1 <- dados[, -1]
dados1

correlacao <- cor(dados1)
correlacao

pairs(cbind(dados1), pch = "+", col = "blue")

bartlett.test(dados1)

KMO(correlacao)

## d. Normalidade multivariada
MVN::mvn(dados1)

## e. Análise descritiva para observar o comportamento de cada variável entre os pontos
par(mfrow = c(2, 4))
boxplot(Temperatura ~ dados$ponto, ylab = "Temperatura")
boxplot(pH ~ dados$ponto, ylab = "pH")
boxplot(Potêncial.de.oxirredução ~ dados$ponto, ylab = "Potêncial de oxirredução")
boxplot(Turbidez ~ dados$ponto, ylab = "Turbidez")
boxplot(Oxigênio.dissolvido ~ dados$ponto, ylab = "Oxigênio dissolvido")
boxplot(Espécie.B ~ dados$ponto, ylab = "Espécie B")
dev.off()

## f. Análise de Componentes Principais - PCA
pca.dados <- princomp(dados1, cor = TRUE)
pca.dados

## Resultados da análise:
## Visualizando a proporção da variância total explicada de cada componente principal.
summary(pca.dados)
## Standard deviation = Autovalor
## Proportion of Variance = o quanto cada componente explica a variação dos dados
## Cumulative Proportion = % acumulada de explicabilidade de todos os fatores

## Cargas fatoriais: coeficientes das combinações lineares das variáveis contínuas
pca.dados$loadings

## Escores padronizados
pca.dados$scores

## Método Plot:
## Este plot representa as variâncias (y) associadas com os componentes principais (x).
## É um método interessante para decidir quantos componentes principais serão utilizados na análise.
plot(pca.dados)
## Critério de Broken-stick: Autovalor > 1

## Diagrama de ordenação: representação gráfica da PCA
attach(dados)
autoplot(pca.dados, data = dados, colour = "ponto") # O comando colour coloriu os pontos amostrados

autoplot(pca.dados, data = dados, colour = "ponto", label = TRUE) # O comando 'label = TRUE' dá nomes numéricos na ordem da tabela.

autoplot(pca.dados, data = dados, colour = "ponto", loadings = TRUE) # Inclui os auto-vetores

autoplot(pca.dados,
    data = dados, colour = "ponto",
    loadings = TRUE, loadings.colour = "blue",
    loadings.label = TRUE, loadings.label.size = 3
) # Agora nós colocamos os nomes dos auto-vetores.
