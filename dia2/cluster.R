# Mini-curso em Ecologia Numérica
# XXV Semana Acadêmica de BIOLOGIA - UNIOESTE Cascavel

# Marília Melo Favalesso

# Prática 1: Análise de Agrupamento - CLUSTER
#            Métodos Hierárquicos Aglomerativos

# O cluster hierárquico (função hclust) está disponível no R por padrão,
# não sendo necessário realizar o download de qualquer pacote adicional.
# Para a construção de clusters no R, é necessária a criação de uma matriz de distância.
# O R possui o comando padrão "dist" para esta finalidade, porém, este comando não contém
# todos os índices ecológicos que podem ser utilizados como medidas de distância.
# Portanto, é comum os pesquisadores utilizarem o comando "vegdist" disponível no pacote vegan.

# Índices ecológicos são úteis por apresentarem valor máximo de 1 para absoluta diferença entre 
# locais (ou seja, '1' significa 'não compartilha espécies'). Em contraste, a distância euclidiana,
# que é uma das mais utilizadas, não possui limite superior e varia com a soma das abundâncias totais
# dos locais quando não há espécies compartilhadas, utilizando o quadrado das diferenças das abundâncias.

# Nesta atividade, utilizaremos a distância euclidiana, mas vocês podem experimentar outras medidas
# em casa com os dados utilizados neste minicurso.

# Carregando os pacotes necessários
library(vegan)

# A Atividade:

# Cinco locais foram amostrados e o pesquisador deseja saber, com base na frequência absoluta
# das espécies amostradas, quais locais são mais parecidos entre si.

## --> Passo 1: Inserindo os dados amostrados.

# Criando a matriz com os dados (comando 'matrix')
P1 = matrix(c(0,0,10,8,0,0,0,12,9,0,0,0,13,5,10,2,3,0,4,12,5,10,0,0,16,15,20,0,0,0), nrow=6, byrow=5)
P1

# Nomeando as colunas e linhas da matriz
colnames(P1) = c("A", "B", "C", "D", "E")
rownames(P1) = c("Local 1", "Local 2", "Local 3", "Local 4", "Local 5", "Local 6")
P1 # versão final da nossa matriz/tabela

## --> Passo 2: Criar uma matriz de distâncias

# Criando a matriz de similaridade/dissimilaridade - qual distância utilizar?
# Existem dois comandos do pacote vegan bem utilizados - "dist" e "vegdist"

P1 # Matriz original

eu = vegdist(P1, method = "euclidean", binary = FALSE) # Matriz com distância euclidiana - comando "vegdist"
eu

# --> Passo 3: O Cluster

# 1. Método do Vizinho Mais Próximo - method = "single"

# Comando de criação de hierarquia cluster via método do vizinho mais próximo
vizinhop = hclust(eu, method = "single")

# Visualização gráfica
plot(vizinhop) # O cluster

plot(vizinhop, main = "Método do Vizinho Mais Próximo", sub = "",
     ylab = "Distância Euclidiana", xlab = "Locais", hang = -1) # Deixando mais palpável

rect.hclust(vizinhop, k = 3, border = "red") # Divisão de grupos

cutree(vizinhop, 3) # Visualização dos indivíduos nos grupos

# 2. Método da Ligação Completa ou do Vizinho Mais Distante ("Complete Linkage")
#    method = "complete"

# Comando de criação de hierarquia cluster via método do vizinho mais distante
vizinhod = hclust(eu, method = "complete")

# Visualização gráfica
plot(vizinhod) # O cluster

plot(vizinhod, main = "Método do Vizinho Mais Distante", sub = "",
     ylab = "Distância Euclidiana", xlab = "Locais", hang = -1) # Deixando mais palpável

rect.hclust(vizinhod, k = 3, border = "red") # Divisão de grupos

cutree(vizinhod, 3) # Visualização dos indivíduos nos grupos

# 3. Método da Ligação Média (Average Linkage)

# Comando de criação de hierarquia cluster via método da ligação média
media = hclust(eu, method = "average")

# Visualização gráfica
plot(media) # O cluster

plot(media, main = "Método da Ligação Média", sub = "",
     ylab = "Distância Euclidiana", xlab = "Locais", hang = -1) # Deixando mais palpável

rect.hclust(media, k = 3, border = "red") # Divisão de grupos

cutree(media, 3) # Visualização dos indivíduos nos grupos

# --> Passo 4: Qual Cluster? Avaliação!

# Avaliação dos métodos de clusterização usando coeficiente cofenético
cor(eu, cophenetic(vizinhop)) # 0,8985643
cor.test(eu, cophenetic(vizinhop)) # t = 7,3827; df = 13, P < 0,001
plot(eu, cophenetic(vizinhop), main = "Coeficiente Cofenético = 0,90",
     xlab = "Valores da Matriz", ylab = "Valores do Cluster")
abline(0, 1)

cor(eu, cophenetic(vizinhod)) # 0,9070452
cor.test(eu, cophenetic(vizinhod)) # t = 7,7676; df = 13, P < 0,001
plot(eu, cophenetic(vizinhod), main = "Coeficiente Cofenético = 0,91",
     xlab = "Valores da Matriz", ylab = "Valores do Cluster")
abline(0, 1)

cor(eu, cophenetic(media)) # 0,9236304
cor.test(eu, cophenetic(media)) # t = 8,6886; df = 13, P < 0,001
plot(eu, cophenetic(media), main = "Coeficiente Cofenético = 0,92",
     xlab = "Valores da Matriz", ylab = "Valores do Cluster")
abline(0, 1)
