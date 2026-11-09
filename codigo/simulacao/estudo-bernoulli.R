
# Descrição da primeira etapa ---------------------------------------------

# O objetivo dessa primeira etapa é utilizar a função `rbinom()` para fazer um estudo de simulação
# referente à distribuição Bernoulli. Para isso, serem definidas amostras de diferentes tamanhos `n`,
# analisadas em diferentes probabilidades.
# Nessa primeira etapa, será observado quais intervalos são definidos para cada tamanho de amostra e probabilidade,
# definindo seus limites inferiores e superiores.


# Tabela 1 - Houve problema? ----------------------------------------------

# Gerando números pseudo-aleatórios ---------------------------------------
set.seed(123)
 
  # Estudo: o que ela faz - gerador de números pseudo-aleatóriios (RNG) e fixa um ponto
  # de partida determinístico para a sequência de números gerados. 

  # Por que usar em simulações?
  # Garante a reprodutibilidade: faz com que a mesma sequência de números seja reproduzida sempre que o código for executado
  # Permite replicação por outros pesquisadores: qualquer pessoa que rodar o código com o mesmo set.seed() terá os mesmo resultados
  # Controla o ponto inicial



# Definindo o tamanho da amostra e as probabilidades ----------------------

tamanho_n <- c(10, 30, 50, 100, 500, 1000, 5000)
valores_p <- c(0.02, 0.05, 0.10, 0.50, 0.70, 0.95, 0.98)


# Criando a tabela  -------------------------------------------------------

tabela_1_problemas <- data.frame(
  n = integer(),
  p_teorico = numeric(),
  p_estimado = numeric(),
  li_bruto = numeric(),
  ls_bruto = numeric(),
  houve_problema = character(),
  cobre_p = logical()
)










