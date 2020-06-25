# --------------------------------------------------
# Obtenção dos dados obtidos em
# https://brasil.io/dataset/covid19/caso 

# -------------------------------------------------- 
# Coleta de dados (Web scraping) ÓBITOS

df_obitos  = read.csv2('https://brasil.io/dataset/covid19/obito_cartorio/?format=csv', sep = ',')

# -------------------------------------------------- 
# Preparação dos dados (data wrangling) ÓBITOS

# organizar o banco de dados de modo 
# que mostre todas as datas de um estado
# específico de uma vez (por ordem alfabética)
# e deixar o estado como a primeira variável 
# do banco de dados:

df_obitos <- df_obitos[order(df_obitos$state, df_obitos$date),]
df_obitos <- df_obitos[c('state', names(df_obitos)[!names(df_obitos) %in% 'state'])]


# transformando em data:
df_obitos$date <- as.Date(df_obitos$date, format = "%Y-%m-%d")


# -------------------------------------------------- 
# Coleta de dados CASOS CONFIRMADOS

df_casos  = read.csv2('https://brasil.io/dataset/covid19/caso_full/?format=csv', sep = ',')

# -------------------------------------------------- 
# Preparação dos dados (data wrangling)  CASOS CONFIRMADOS
# mesmo esquema para o banco de casos
# neste caso, deixarei estado, cidade e data como as primeiras variáveis:

df_casos <- df_casos[order(df_casos$state, df_casos$city, df_casos$date),]
org_casos <- c('state', 'city', 'date')
df_casos <- df_casos[c(org_casos, names(df_casos)[!names(df_casos) %in% org_casos])]

# transformando em data:
df_casos$date <- as.Date(df_casos$date, format = "%Y-%m-%d")

# -------------------------------------------------- 
# Salva dados em um arquivo RDS

saveRDS(df_casos,
        file = here::here("dados","casos_covid19_br_mun.rds"))

saveRDS(df_obitos,
        file = here::here("dados","obitos_br_uf.rds"))
