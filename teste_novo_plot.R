input <- "Casos/100k hab."
tipo <- "br"
filtro <- c("SP","RJ","CE","PE","AM","AP","AC","MA","PE","RR")
filtro <- unique(covid$state)

plot_quadradinhos <- function(filtro, input, tipo) {
  
  aux_var <- select_choices2[which(input == select_choices)]
  
  if(tipo == "br") {
    aux_var_2 <- "state"
  } else {
    aux_var_2 <- "city"
  }
  
  var <- rlang::sym(aux_var)
  
  var_2 <- rlang::sym(aux_var_2)
  
  if (aux_var == "confirmed") {
    paleta <- "Reds"
  } else if (aux_var == "deaths") {
    paleta <- "Purples"
  } else if (aux_var == "confirmed_per_100k_inhabitants") {
    paleta <- "Oranges"
  } else {
    paleta <- "PuRd"
  }
  
  if(tipo == "br") {
    aux <- covid %>%
      filter(place_type == "state") %>%
      filter(!!var_2 %in% filtro) %>%
      arrange(date)
  } else {
    aux <- covid %>%
      filter(state == "city") %>%
      filter(!!var_2 %in% filtro) %>%
      arrange(date)
  }
  
  aux <- as.data.frame(aux)
  
  p <- ggplot(aux, aes(x = date, y = !!var_2, fill = !!var)) +
    geom_tile() +
    scale_fill_gradientn(colours = brewer.pal(9,paleta)) +
    theme_tufte(base_family="Helvetica")
  
  ggplotly(p)
  
  if(tipo == "Diário") {
    
    ordem <- as.character(format(aux$date, "%d-%m"))
    
    if(aux_var %in% select_choices2[c(1,2)]) {
      
      aux$novos <- c(aux[1,aux_var],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- aux[i,aux_var]-aux[i-1,aux_var]
      }
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = date, y = !!var), color = col_sel) + 
        geom_col(aes(x = date, y = novos), fill = col_sel) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    } else {
      
      aux$date <- as.character(format(aux$date, "%d-%m"))
      
      p <- ggplot(aux) +
        geom_line(aes(x = date, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = date, y = !!var), color = col_sel) + 
        scale_x_discrete(limits = ordem) +
        labs(x = "Dia", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
      if(aux_var == "death_rate") {
        p <- p +
          scale_y_continuous(labels=percent)
      }
      
    }
    
    
    
  } else {
    
    temp2 <- obts %>%
      select(date, epidemiological_week_2020) %>%
      filter(date %in% aux$date) %>%
      unique()
    
    aux <- left_join(aux,temp2, by = "date") %>%
      group_by(epidemiological_week_2020) %>%
      filter(date == max(date)) %>%
      ungroup()
    
    ordem <- as.character(aux$epidemiological_week_2020)
    
    
    
    if(aux_var %in% select_choices2[c(1,2)]) {
      
      aux$novos <- c(as.data.frame(aux)[1,aux_var],rep(NA,nrow(aux)-1))
      for(i in 2:nrow(aux)) {
        aux$novos[i] <- as.data.frame(aux)[i,aux_var]-as.data.frame(aux)[i-1,aux_var]
      }
      
      aux$epidemiological_week_2020 <- as.character(aux$epidemiological_week_2020)
      
      p <- ggplot(aux) +
        geom_line(aes(x = epidemiological_week_2020, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = epidemiological_week_2020, y = !!var), color = col_sel) + 
        geom_col(aes(x = epidemiological_week_2020, y = novos), fill = col_sel) +
        scale_x_discrete(limits = ordem) +
        labs(x = "Semana Epidemiológica", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
    } else {
      
      aux$epidemiological_week_2020 <- as.character(aux$epidemiological_week_2020)
      
      p <- ggplot(aux) +
        geom_line(aes(x = epidemiological_week_2020, y = !!var, group = 1), color = col_sel, linetype = 'dotted') +
        geom_point(aes(x = epidemiological_week_2020, y = !!var), color = col_sel) + 
        scale_x_discrete(limits = ordem) +
        labs(x = "Semana Epidemiológica", y = input) +
        theme(axis.text.x = element_text(angle=45,size=8, vjust = 0.5)) +
        theme(plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              panel.grid.major = element_blank())
      
      if(aux_var == "death_rate") {
        p <- p +
          scale_y_continuous(labels=percent)
      }
      
    }
    
  }
  
  ggplotly(p) 
  
}
