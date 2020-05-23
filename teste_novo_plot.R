# gráfico 14 dias

aux2 <- read_excel("dados/estados_siglas.xlsx") %>%
  mutate(estado = str_to_title(Estado), id = as.factor(Sigla)) %>%
  arrange(Codigo) %>%
  select(id,Regiao)

aux <- banco_14_dias %>%
  filter(deaths > 10) %>%
  left_join(aux2, by = c("state" = "id"))
  
minx <- min(banco_14_dias$obitos_100k_hab_14_dias) 
miny <- min(banco_14_dias$confirmados_100k_hab_14_dias) 
maxx <- max(banco_14_dias$obitos_100k_hab_14_dias) 
maxy <- min(banco_14_dias$confirmados_100k_hab_14_dias) 

x_range_ <- c(min(aux3$obitos_100k_hab_14_dias, na.rm=T),max(aux3$obitos_100k_hab_14_dias, na.rm=T))  
y_range <- 
  
  
p1 <- ggplot(banco_14_dias) +
  geom_point(aes(x = obitos_100k_hab_14_dias, y = confirmados_100k_hab_14_dias, col = state, label = date)) +
  geom_path(aes(x = obitos_100k_hab_14_dias, y = confirmados_100k_hab_14_dias, col = state, group = state)) +
  geom_abline(slope = 10, size = .3) +
  geom_abline(slope = 100, size = .3) +
  facet_wrap(~Regiao, scales = "free")

ggplotly(p1)

aux3 <- aux %>%
  filter(state %in% c("AM","PA","SP","RJ","CE","PE"))

aux3 <- banco_14_dias %>%
  filter(Regiao == "Sudeste")

p1 <- ggplot(aux3) +
  geom_point(aes(x = obitos_100k_hab_14_dias, y = confirmados_100k_hab_14_dias, col = state, label = date)) +
  geom_path(aes(x = obitos_100k_hab_14_dias, y = confirmados_100k_hab_14_dias, col = state, group = state)) +
  geom_abline(slope = 10, size = .3) +
  geom_abline(slope = 100, size = .3) +
  labs(x = "óbitos por 100 mil habitantes nos últimos 14 dias",
       y = "Casos confirmados por 100 mil habitantes nos últimos 14 dias",
       caption = "Dias consecutivos são conectados pelas linhas")

y_range <- layer_scales(p1)$y$range$range
x_range <- layer_scales(p1)$x$range$range
x_to_y <- (x_range[2] - x_range[1])/(y_range[2] - y_range[1])

#p1 <- p1 + coord_fixed(ratio = x_to_y) + 
#  annotate(
#    "text",
#    x = diff(x_range)*11/12+x_range[1],
#    y = (diff(x_range)*11/12+x_range[1])*10-(1/100*diff(y_range)),
#    angle = atan(10 * x_to_y) * 180/pi,
#    label = "Letalidade 10%"
#  ) +
#  annotate(
#    "text",
#    x = diff(x_range)*1/15+x_range[1]-(1/100*diff(x_range)),
#    y = (diff(x_range)*1/15+x_range[1])*100+(1/100*diff(y_range)),
#    angle = atan(100 * x_to_y) * 180/pi,
#    label = "Letalidade 1%"
#  )



anotacoes <- list(
  x = c(diff(x_range)*11/12+x_range[1],diff(x_range)*1/15+x_range[1]-(1/100*diff(x_range)),1),
  y = c((diff(x_range)*11/12+x_range[1])*10-(1/100*diff(y_range)),(diff(x_range)*1/15+x_range[1])*100+(1/100*diff(y_range)),0.005),
  text = c("Letalidade 10%","Letalidade 1%","Dias consecutivos desde o marco de 10 óbitos são conectados pelas linhas"),
  textangle = c(-(atan(10 * (x_to_y-0.003)) * 180/pi),-atan(100 * (x_to_y-0.003)) * 180/pi,0),
  xref = c("x","x","paper"),
  yref = c("y","y","paper"),
  showarrow = F,
  font = list(size = 13)
)

lyp2 <- ggplotly(p1) %>%
  layout(annotations = anotacoes,
         yaxis = list(scaleanchor = "x", scaleratio = x_to_y))

ggplotly(p1) %>%
  layout(annotations = anotacoes)




