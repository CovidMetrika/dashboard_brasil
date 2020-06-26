# --------------------------------------------------
# Obtenção dos dados obtidos em
# https://brasil.io/dataset/covid19/caso 

# -------------------------------------------------- 
# Coleta de dados (Web scraping) ÓBITOS

df_obitos <- NULL

path <- "https://brasil.io/dataset/covid19/obito_cartorio/?format=csv"
request <- try(GET(url = path))

if(class(request) == "try-error") {
  df_obitos <- read_csv("dados/obitos_cartorio_reserva.csv", guess_max = 10000)
} else if(request$status_code == 404){
  df_obitos <- read_csv("dados/obitos_cartorio_reserva.csv", guess_max = 10000)
} else {
  df_obitos <- read_csv("https://brasil.io/dataset/covid19/obito_cartorio/?format=csv", guess_max = 10000)
  write_csv(df_obitos,"dados/obitos_cartorio_reserva.csv")
}

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
# lendo dados do brasil.io

df_casos <- NULL

path <- "https://brasil.io/dataset/covid19/caso_full/?format=csv"
request <- try(GET(url = path))

if(class(request) == "try-error") {
  df_casos <- read_csv("dados/brasil.io_reserva.csv")
} else if(request$status_code == 404) {
  df_casos <- read_csv("dados/brasil.io_reserva.csv")
} else {
  df_casos <- read_csv("https://brasil.io/dataset/covid19/caso_full/?format=csv")
  write_csv(df_casos,"dados/brasil.io_reserva.csv")
}


# -------------------------------------------------- 
# Preparação dos dados (data wrangling)  CASOS CONFIRMADOS
# mesmo esquema para o banco de casos
# neste caso, deixarei estado, cidade e data como as primeiras variáveis:

df_casos <- df_casos[order(df_casos$state, df_casos$city, df_casos$date),]
org_casos <- c('state', 'city', 'date')
df_casos <- df_casos[c(org_casos, names(df_casos)[!names(df_casos) %in% org_casos])]

# transformando em data:
df_casos$date <- as.Date(df_casos$date, format = "%Y-%m-%d")

rm(list=setdiff(ls(),c("df_casos","df_obitos")))

#-----------------------------------
# shapfiles dos estados
mapa_brasil <- sf::st_read("shapefiles/brasil_uf/BRUFE250GC_SIR.shp", quiet = TRUE) %>%
  mutate(NM_ESTADO = str_to_title(NM_ESTADO)) # todas os estados com letra  de título


# shapefile uf_municipios

lendo_shapefile_uf_mun <- function() {
  estados_siglas <- read_excel("dados/estados_siglas.xlsx") %>%
    mutate(estado = str_to_title(Estado), id = as.factor(Sigla)) %>%
    arrange(Codigo)
  
  pasta <- "shapefiles/uf_municipios/"
  arquivos <- str_c(pasta,list.files(pasta, pattern = ".shp"))
  
  dados <- map(arquivos,sf::st_read, quiet = T)
  
  names(dados) <- estados_siglas$id
  
  return(dados)
  
}

dados_shp_uf_mun <- lendo_shapefile_uf_mun()

#transforma em um arquivo que o leaflet consegue ler!!!!
estados_siglas <- read_excel("dados/estados_siglas.xlsx") %>%
  mutate(NM_ESTADO = str_to_title(Estado), id = as.factor(Sigla)) %>%
  select(NM_ESTADO, id)

labels_estado <- estados_siglas$id
names(labels_estado) <- estados_siglas$NM_ESTADO

mapa_brasil <- mapa_brasil %>% 
  merge(estados_siglas, by = 'NM_ESTADO') %>% 
  st_transform("+init=epsg:4326")

#-------------------------------------
# banco de dados de  casos confirmados:
covid <- df_casos %>%
  as.data.frame()

# criando banco_14_dias

banco_14_dias <- covid %>%
  filter(place_type == "state") %>%
  group_by(state) %>%
  mutate(data_inicial = min(date[last_available_deaths >= 10])) %>%
  ungroup()

banco_14_dias <- as.data.frame(banco_14_dias)

aux <- list()

for (i in unique(banco_14_dias$state)) {
  for (j in unique(banco_14_dias[banco_14_dias$state == i, "data_inicial"]):max(banco_14_dias[banco_14_dias$state == i,"date"])) {
    aux[[i]][[as.character(as_date(j))]] <- tibble(confirmados_14_dias = banco_14_dias[banco_14_dias$date == j & banco_14_dias$state == i, "last_available_confirmed"] - banco_14_dias[banco_14_dias$date == j-14 & banco_14_dias$state == i, "last_available_confirmed"],
                                                   obitos_14_dias = banco_14_dias[banco_14_dias$date == j & banco_14_dias$state == i, "last_available_deaths"] - banco_14_dias[banco_14_dias$date == j-14 & banco_14_dias$state == i, "last_available_deaths"],
                                                   confirmados_100k_hab_14_dias = (banco_14_dias[banco_14_dias$date == j & banco_14_dias$state == i, "last_available_confirmed"] - banco_14_dias[banco_14_dias$date == j-14 & banco_14_dias$state == i, "last_available_confirmed"])*100000/banco_14_dias[banco_14_dias$state == i,"estimated_population_2019"][1],
                                                   obitos_100k_hab_14_dias = (banco_14_dias[banco_14_dias$date == j & banco_14_dias$state == i, "last_available_deaths"] - banco_14_dias[banco_14_dias$date == j-14 & banco_14_dias$state == i, "last_available_deaths"])*100000/banco_14_dias[banco_14_dias$state == i,"estimated_population_2019"][1],
                                                   state = i,
                                                   date = as_date(j))
  }
}

aux <- aux %>%
  map(bind_rows) %>%
  bind_rows()

aux2 <- read_excel("dados/estados_siglas.xlsx") %>%
  mutate(estado = str_to_title(Estado), id = as.factor(Sigla)) %>%
  arrange(Codigo) %>%
  select(id,Regiao)

banco_14_dias <- left_join(banco_14_dias,aux, by = c("state","date")) %>%
  left_join(aux2, by = c("state" = "id")) %>%
  filter(last_available_deaths > 10)

rm(aux)

# banco de dados por estado:
data_state <- covid %>%
  select(state, last_available_confirmed, last_available_deaths, last_available_confirmed_per_100k_inhabitants, 
         last_available_death_rate, is_last, place_type, estimated_population_2019) %>%
  filter(is_last == 'TRUE' & place_type == 'state')

# banco de dados obitos cartorio:

obitos_cartorio <- df_obitos %>%
  filter(date <= Sys.Date()) # filtrando a partir do primeiro caso

names(obitos_cartorio) <- c("Estado","Data","Semana_epidemiologica_2020","Acumulado mortes total 2019","Acumulado mortes total 2020","Acumulado mortes COVID-19",
                            "Mortes total 2020","Acumulado mortes indeterminadas 2019","Acumulado mortes indeterminadas 2020",
                            "Acumulado mortes outras 2019","Acumulado mortes outras 2020","Acumulado mortes Pneumonia 2019","Acumulado mortes Pneumonia 2020",
                            "Acumulado mortes por insuficiência respiratória 2019","Acumulado mortes por insuficiência respiratória 2020",
                            "Acumulado mortes srag 2019","Acumulado mortes srag 2020","Acumulado mortes septicemia 2019","Acumulado mortes septicemia 2020",
                            "Mortes COVID-19","Mortes indeterminadas 2020","Mortes outras 2020","Mortes Pneumonia 2020","Mortes por insuficiência respiratória 2020",
                            "Mortes srag 2020","Mortes septicemia 2020")


obitos_cartorio <- obitos_cartorio %>%
  select(c(Estado:Semana_epidemiologica_2020,starts_with("Acumulado"))) %>%
  select(-c("Acumulado mortes total 2019","Acumulado mortes total 2020")) %>%
  group_by(Data,Semana_epidemiologica_2020) %>%
  pivot_longer(
    cols = -c(Estado,Semana_epidemiologica_2020,Data),
    names_to = "disease_type",
    values_to = "acumulado_mortes"
  ) %>%
  mutate(ano = ifelse(disease_type != "Acumulado mortes COVID-19", str_extract(disease_type,"....$"), "2020"))

data_minima <- obitos_cartorio %>%
  group_by(Estado,disease_type) %>%
  filter(!is.na(acumulado_mortes)) %>%
  summarise(data_minima = min(Data, na.rm = T)) %>%
  ungroup() %>%
  add_row(Estado = "RR", disease_type = "Acumulado mortes indeterminadas 2019",data_minima = as_date("2020-12-31"))

obitos_cartorio <- obitos_cartorio %>%
  as.data.frame()

obitos_cartorio$frequencia_mortes <- obitos_cartorio$acumulado_mortes

  
for(i in unique(obitos_cartorio$Estado)) {
  for(j in unique(obitos_cartorio$disease_type)) {
    indices <- which(obitos_cartorio$Estado==i&obitos_cartorio$disease_type==j)
    for(k in indices) {
      if(is.na(obitos_cartorio$frequencia_mortes[k])) {
        if(obitos_cartorio$Data[k] < data_minima$data_minima[data_minima$Estado==i&data_minima$disease_type==j]) {
          obitos_cartorio$frequencia_mortes[k] <- 0
          obitos_cartorio$acumulado_mortes[k] <- 0
        } else {
          obitos_cartorio$frequencia_mortes[k] <- 0
          obitos_cartorio$acumulado_mortes[k] <- obitos_cartorio$acumulado_mortes[indices[which(indices==k)-1]]
        }
      } else {
        if(k == indices[1]) {
          obitos_cartorio$frequencia_mortes[k] <- obitos_cartorio$acumulado_mortes[k]
        } else {
          obitos_cartorio$frequencia_mortes[k] <- obitos_cartorio$acumulado_mortes[k]-obitos_cartorio$acumulado_mortes[indices[which(indices==k)-1]]
        }
      }
    }
  }
}

# arrumando labels

obitos_cartorio$disease_type <- as.character(factor(obitos_cartorio$disease_type,
  levels = unique(obitos_cartorio$disease_type), 
  labels = c("COVID-19","Indeterminada 2019","Indeterminada 2020","Demais Óbitos 2019","Demais Óbitos 2020","Pneumonia 2019",
             "Pneumonia 2020","Insuficiência Respiratória 2019","Insuficiência Respiratória 2020","SRAG 2019","SRAG 2020",
             "Septicemia 2019","Septicemia 2020")))

# banco de dados com total de casos no brasil por dia 2.0
# criei um novo para que não ficasse aqueles números negativos

pop_br = sum(data_state$estimated_population_2019)

ultima_atualizacao <- covid %>%
  select(state, last_available_confirmed, last_available_deaths, date, place_type, is_last) %>%
  filter(place_type == "state") %>%
  group_by(is_last) %>%
  filter(is_last) %>%
  summarise(last_available_confirmed = sum(last_available_confirmed), last_available_deaths = sum(last_available_deaths), conf_per100k = sum(last_available_confirmed)/pop_br*100000,
            letal = sum(last_available_deaths)/sum(last_available_confirmed)) %>%
  select(-is_last)

casos_br <- covid %>%
  select(state, last_available_confirmed, last_available_deaths, date, place_type, is_last) %>%
  filter(place_type == "state") %>%
  group_by(date) %>%
  summarise(last_available_confirmed = sum(last_available_confirmed), last_available_deaths = sum(last_available_deaths), conf_per100k = sum(last_available_confirmed)/pop_br*100000,
            letal = sum(last_available_deaths)/sum(last_available_confirmed)) %>%
  arrange(date)

casos_br[nrow(casos_br),c("last_available_confirmed","last_available_deaths","conf_per100k","letal")] <- ultima_atualizacao  

casos_br <- as.data.frame(casos_br[,c(2:5,1)])

# cor e ooções de seleção da variável a ser vista

fcolor <- c("#dd4b39", "#605ca8", "#f39c12", "#d81b60")
select_choices <- c("Casos Confirmados", "Óbitos", "Casos/100k hab.", "Letalidade")
select_choices2 <- c("last_available_confirmed","last_available_deaths","last_available_confirmed_per_100k_inhabitants","last_available_death_rate")

obts <- df_obitos

temp <- obts %>%
  select(date, epidemiological_week_2020)

temp <- merge(casos_br, temp, by = 'date')
temp <- unique(temp)

casos_br <- casos_br %>%
  mutate(ep_week = temp$epidemiological_week_2020)

rm(temp)

estados_siglas <- read_excel("dados/estados_siglas.xlsx") %>%
  mutate(NM_ESTADO = str_to_title(Estado), id = as.factor(Sigla))

