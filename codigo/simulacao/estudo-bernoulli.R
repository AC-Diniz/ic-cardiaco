
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

# O que foi feito: foi criada um data frame vazio e definida as colunas específicas,
# bem como o tipo de caractere que iria em cada coluna





# Simulação com for -------------------------------------------------------

# Criando o laço d repetição para percorrer cada valor (combinações)
for (n in tamanho_n) {
  for (p in valores_p) {
    
    # Amostra de Bernoulli
    y <- rbinom( n = n, size = 1, prob = p)
    
    # Estimativa pontual (calcula a proporção amostral p^)
    p_pont <- mean(y)
    
    # Erro padrão da proporção (raiz da variância amostral)
    ep <- sqrt((p_pont * (1-p_pont))/ n)
    
    # Limites inferior e superior (intervalo de confiaça com 1-a = 0.95 -> Wald)
    li_bruto <- p_pont - 1.96 * ep
    ls_bruto <- p_pont + 1.96 * ep
    
    # Condição de classificação de problemas
    if (li_bruto < 0 & ls_bruto > 1) {
      status_problema <- "Sim (abaixo de 0 e acima de 1"
    } 
    else if ( li_bruto < 0) {
      status_problema <- "Sim (abaixo de 0)"
    }
    else if (ls_bruto > 1) {
      status_problema <- "Sim (acima de 1)"
    } else {
      status_problema <- "Não"
    }
    
    # Limitando o intervalo de 0 a 1 (Bernoulli) -> corrige os limites para eles ficarem dentro do intervalo (0, 1)
    li_trunc <- max(0, li_bruto)
    ls_trunc <- min(1, ls_bruto)
    
    # Verificando se o parâmetro real (p) está dentro do intervalo
    cobre <- (p >= li_trunc) & (p <= ls_trunc)
    
    tabela_1_problemas <- rbind(tabela_1_problemas, data.frame(
      n = n,
      p_teorico = p,
      p_estimado = round(p_pont, 3),
      li_bruto = round(li_bruto, 3),
      ls_bruto = round(ls_bruto, 3),
      houve_problema = status_problema,
      cobre_p = cobre
    ))
    
  }
}

View(tabela_1_problemas)







