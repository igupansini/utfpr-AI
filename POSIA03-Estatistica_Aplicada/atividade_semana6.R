library(MASS)

n_bolas <- 6
n_vermelhas <- 2
n_brancas <- n_bolas - n_vermelhas

cat("== Espaço amostral ==\n")
espaco <- combn(n_bolas, n_vermelhas)
X <- espaco[2, ]
resultados <- data.frame(
  vermelha_1 = espaco[1, ],
  vermelha_2 = espaco[2, ],
  X = X
)
print(resultados)
cat("Total de resultados equiprováveis:", ncol(espaco), "=", "choose(6, 2) =", choose(n_bolas, n_vermelhas), "\n\n")

cat("== Distribuição de X: contagem no espaço amostral x probabilidade condicional x fórmula ==\n")
valores <- 1:n_bolas
p_contagem <- as.numeric(table(factor(X, levels = valores))) / ncol(espaco)

p_condicional <- sapply(valores, function(k) {
  if (k < n_vermelhas) return(0)
  dhyper(n_vermelhas - 1, n_vermelhas, n_brancas, k - 1) * 1 / (n_bolas - (k - 1))
})

em_quinze_avos <- function(p) paste0(round(p * 15), "/15")
tabela <- data.frame(
  X = valores,
  contagem = em_quinze_avos(p_contagem),
  condicional = em_quinze_avos(p_condicional),
  formula = em_quinze_avos(ifelse(valores >= 2, (valores - 1) / 15, 0))
)
print(tabela)
cat("Soma das probabilidades:", sum(p_contagem), "\n")
suporte <- valores[p_contagem > 0]
cat("Valores possíveis de X:", suporte, "\n\n")

cat("== Conferência das alternativas ==\n")
alternativas <- list(
  a = list(f = function(x) (x - 1) / 15, dominio = 1:6),
  b = list(f = function(x) (x - 1) / 30, dominio = 1:6),
  c = list(f = function(x) (x - 1) / 15, dominio = 2:6),
  d = list(f = function(x) x / 15, dominio = 1:6)
)

conferencia <- do.call(rbind, lapply(names(alternativas), function(letra) {
  alt <- alternativas[[letra]]
  px <- alt$f(alt$dominio)
  data.frame(
    alternativa = letra,
    soma = as.character(fractions(sum(px))),
    soma_igual_1 = isTRUE(all.equal(sum(px), 1)),
    dominio_igual_espaco = identical(as.integer(alt$dominio), as.integer(suporte)),
    probabilidades_conferem = isTRUE(all.equal(px, p_contagem[alt$dominio]))
  )
}))
print(conferencia, row.names = FALSE)

correta <- conferencia$alternativa[conferencia$soma_igual_1 & conferencia$dominio_igual_espaco & conferencia$probabilidades_conferem]
cat("\nAlternativa correta:", correta, "\n")
