# ---------------------------------------------------------------
# Implementar um script em linguagem R baseado no conjunto de 
# dados USArrests, que deve calcular as médias das prisões por 
# assaltos, assassinatos e estupros para as 5 maiores e as 5 
# menores populações urbanas. Utilizar o conhecimento adquirido 
# até o momento. Por exemplo, evitar funções de filtro e média. 
# Identificar manualmente a quantidade de registros ou 
# observações do conjunto de dados e realizar os demais cálculos 
# via script.
# ---------------------------------------------------------------

data("USArrests")

qtd_observacoes <- 50

indices_decrescente <- order(USArrests$UrbanPop, decreasing = TRUE)

indices_crescente <- order(USArrests$UrbanPop, decreasing = FALSE)

indices_5_maiores <- indices_decrescente[1:5]
indices_5_menores <- indices_crescente[1:5]

estados_5_maiores <- USArrests[indices_5_maiores, ]
estados_5_menores <- USArrests[indices_5_menores, ]

estados_5_maiores
estados_5_menores

qtd_estados <- 5

soma_assault_maiores  <- sum(estados_5_maiores$Assault)
soma_murder_maiores   <- sum(estados_5_maiores$Murder)
soma_rape_maiores     <- sum(estados_5_maiores$Rape)

media_assault_maiores <- soma_assault_maiores / qtd_estados
media_murder_maiores  <- soma_murder_maiores / qtd_estados
media_rape_maiores    <- soma_rape_maiores / qtd_estados

soma_assault_menores  <- sum(estados_5_menores$Assault)
soma_murder_menores   <- sum(estados_5_menores$Murder)
soma_rape_menores     <- sum(estados_5_menores$Rape)

media_assault_menores <- soma_assault_menores / qtd_estados
media_murder_menores  <- soma_murder_menores / qtd_estados
media_rape_menores    <- soma_rape_menores / qtd_estados


# Resultados
cat("Número de observações do dataset:", qtd_observacoes, "\n\n")

cat("Médias - 5 estados com MAIOR população urbana:\n")
cat("Assault:", media_assault_maiores, "\n")
cat("Murder :", media_murder_maiores, "\n")
cat("Rape   :", media_rape_maiores, "\n\n")

cat("Médias - 5 estados com MENOR população urbana:\n")
cat("Assault:", media_assault_menores, "\n")
cat("Murder :", media_murder_menores, "\n")
cat("Rape   :", media_rape_menores, "\n")