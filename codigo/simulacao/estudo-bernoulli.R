
# Descrição da primeira etapa ---------------------------------------------

# O objetivo dessa primeira etapa é utilizar a função `rbinom()` para fazer um estudo de simulação
# referente à distribuição Bernoulli. Para isso, serem definidas amostras de diferentes tamanhos `n`,
# analisadas em diferentes probabilidades.
# Nessa primeira etapa, será observado quais intervalos são definidos para cada tamanho de amostra e probabilidade,
# definindo seus limites inferiores e superiores.


# TABELA 1: HOUVE PROBLEMA? ----------------------------------------------

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



# TABELA  2: PROPORÇÃO DE IC PROBLEMÁTICOS ÁRA 500 RÉPLICAS --------------

# Definindo B para usar no intervalo (número de réplicas)
num_rep <- 500


# Criando a tabela --------------------------------------------------------

tabela_2_proporcao <- data.frame(
  n = integer(),
  p_teorico = numeric(),
  prop_abaixo_0 = numeric(),
  prop_acima_1 = numeric(),
  prop_problema_qualquer = numeric()
)


# Simulação for -----------------------------------------------------------

for (n in tamanho_n) {
  for (p in valores_p) {
    
    # Vetores para guardar as réplicas
    prob_li <- logical(num_rep)
    prob_ls <- logical(num_rep)

    # Loop
    for (j in 1:num_rep) {
      
      y <- rbinom(n = n, size = 1, prob = p)
      p_pont <- mean(y)
      
      # Erro padrão e limites de Wald (usando as amostras)
      ep <- sqrt((p_pont * (1-p_pont))/ n)
      li_bruto <- p_pont - 1.96 * ep
      ls_bruto <- p_pont + 1.96 * ep
      
      # Verificando se ultrapassa os limites
      prob_li[j] <- (li_bruto < 0)
      prob_ls[j] <- (ls_bruto > 1)
      
    }
    
    # Proporção das réplicas
    p_abaixo_0 <- mean(prob_li)
    p_acima_1 <- mean(prob_ls)
    p_problema_qualquer <- mean(prob_li | prob_ls)
    
    tabela_2_proporcao <- rbind(tabela_2_proporcao, data.frame(
      n = n,
      p_teorico = p,
      prop_abaixo_0 = round(p_abaixo_0, 3),
      prop_acima_1 = round(p_acima_1, 3),
      prop_problema_qualquer = round(p_problema_qualquer, 3)
    ))
        
  }
}

View(tabela_2_proporcao)




# TABELA 3: TAXA DE COBERTURA PARA 500 RÉPLICAS ---------------------------

# Criando a tabela --------------------------------------------------------
tabela_3_cobertura <- data.frame(
  n = integer(),
  p_teorico = numeric(),
  taxa_cobertura = numeric(),
  diferenca_nomial = numeric()
)


# Simulação for -----------------------------------------------------------

for (n in tamanho_n) {
  for (p in valores_p) {
    
    coberturas <- logical(num_rep)
    
    for (r in 1:num_rep) {
      
      # Gerando a Bernoulli
      y <- rbinom(n = n, size = 1, prob = p)
      p_pont <- mean(y)
      
      # Erro padrão e limites de Wald
      ep <- sqrt((p_pont*(1-p_pont))/n)
      li <- max(0, p_pont - 1.96 * ep)
      ls <- min(1, p_pont + 1.96 * ep)
      
      # Verificando se o intervalo contém o verdadeiro valor de p
      coberturas[r] <- (p >= li) & (p <= ls)
      
    }
    
    # Taxa de cobertura empírica (média dos TRUE/ FALSE nas 500 réplicas)
    taxa_empirica <- mean(coberturas)
    
    tabela_3_cobertura <- rbind(tabela_3_cobertura, data.frame(
      n = n,
      p_teorico = p,
      taxa_cobertura = round(taxa_empirica, 3),
      # Quanto desvio de 95%
      diferenca_nomial = round(taxa_empirica - 0.95, 3)
    ))
    
    
  }
}

View(tabela_3_cobertura)
