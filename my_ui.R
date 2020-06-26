# My UI

theme_set(theme_gray())

options(OutDec= ",")

ui <- dashboardPage(
  #-------------------------------------
  # Titulo do dashboard:
  
  dashboardHeader(
    title = shinyDashboardLogoDIY(
      boldText = "COVID"
      ,mainText = "-19"
      ,textSize = 16
      ,badgeText = "CovidMetrika"
      ,badgeTextColor = "white"
      ,badgeTextSize = 2
      ,badgeBackColor = "#39cccc"
      ,badgeBorderRadius = 3)
  ),
  
  #-------------------------------------
  # Menu do dashboard:
  
  dashboardSidebar(
    sidebarMenu(
      selectInput(inputId = 'typevar', label = "Selecione a variável:", 
                  choices = select_choices, selected = select_choices[1]),
      
      menuItem("Covid-19", icon = icon("chart-area"), tabName = "dashbr", 
               badgeLabel = "BR", badgeColor = "teal"),
      menuItem("Covid-19", icon = icon("chart-bar"), tabName = "dashuf", 
               badgeLabel = "UF", badgeColor = "teal"),
      menuItem("Total de Óbitos", icon = icon("chart-line"), tabName = "cart", badgeLabel = "BR", badgeColor = "teal"),
      menuItem("Fonte de Dados", icon = icon("file-download"), tabName = "dados", badgeColor = "teal"),
      menuItem("CovidMetrika", icon = icon("users"), tabName = "us", badgeColor = "teal")
    )
  ),
  
  #-------------------------------------
  # 'corpo' do dashboard: 
  dashboardBody(
    shinyDashboardThemes(
      theme = "grey_light"
    ),
    tabItems(
      tabItem("dashbr", 
              
              # incluindo o script do google analytics para acompanhamento de dados
              
              tags$head(includeHTML(("google_analytics.html"))),
              
              # funções extras
              
              useShinyjs(),
              
              # para a mensagem de popup
              
              useShinyalert(),
              
              fluidRow(
                #-------------------------------------
                # as três 'caixas' com informações resumo: 
                
                valueBoxOutput("casosBox", width = 3),
                valueBoxOutput("obitosBox", width = 3),
                valueBoxOutput("taxaBox", width = 3),
                valueBoxOutput("letalBox", width = 3),
                
                #  h6(em(data_hora_atual)),
                
                #-------------------------------------
                # plot geral - primeiro plot
                box(leafletOutput("mapaPlot", height = 350L), width = 6L, height = 390L),
                box(plotlyOutput("barPlot", height = 350L), width = 6L, height = 390L),   
                column(width = 12,
                       tabsetPanel(type = "tabs",
                                   tabPanel("Diário", 
                                            box(width = NULL, 
                                                plotlyOutput("confPlot", height = 400L))
                                   ), #tabpanel 1
                                   
                                   tabPanel("Semana Epidemiológica", 
                                            box(width = NULL, 
                                                plotlyOutput("conf2Plot", height = 400L))
                                   ) #tabpanel 2
                                   
                       ) #tabset
                       
                ), # coluna
                column(
                  width = 12,
                  uiOutput("ui_filtro_quadradinhos_br")
                ),
                column(
                  width = 12,
                  box(
                    width = 12,
                    selectInput(
                      "filtro_estados",
                      label = paste0("Selecione os estados de interesse(por default estão selecionados os 5 primeiros a atingirem 10 óbitos por COVID-19"),
                      choices = unique(banco_14_dias$state),
                      selected = c("SP","RJ","CE","PE","AM"),
                      multiple = T
                    ),
                    plotlyOutput("plot_14_dias_br", height = 650L)
                  )
                )
                
              ) #fluidrow
      ), # final da parte dos dados nacionais
      
      tabItem("dashuf",
              
              selectInput(
                "estado",
                label = "Escolha um estado",
                choices = labels_estado,
                selected = "RS"
              ),
              
              fluidRow(
                valueBoxOutput("casos_uf", width = 3),
                valueBoxOutput("obitos_uf", width = 3),
                valueBoxOutput("taxa_uf", width = 3),
                valueBoxOutput("letal_uf", width = 3),
                #  h6(em(data_hora_atual)),
              ),
              fluidRow(
                box(
                  leafletOutput("mapa_uf", height = 390L),
                  width = 9,
                  height = 410L
                ),
                box(
                  dataTableOutput("table_uf", height = "390px"),
                  width = 3,
                  height = 410L
                ),
                column(
                  width = 12,
                  tabBox(id = "tab_uf",
                         width = 12,
                         title = NULL,
                         tabPanel("Diário",
                                  plotlyOutput("uf_dia_plot", height = 400)
                         ),
                         tabPanel("Semana Epidemiológica",
                                  plotlyOutput("uf_sem_plot", height = 400)
                         )
                         
                  )
                ),
                column(
                  width = 12,
                  uiOutput("ui_filtro_quadradinhos_uf"),
                  box(
                    width = 12,
                    plotlyOutput("plot_14_dias_uf", height = 650L)
                  )
                )
              )
              
      ),
      tabItem("cart",
              
              # pegando de alguma forma htmlzada a versão mais recente dos ícones font awesome
              
              tags$script(src = "https://kit.fontawesome.com/ab15947b75.js", crossorigin="anonymous"), 
              
              # reescrevendo as cores default para as que eu quero nas boxes de óbitos cartório
              
              tags$style(".small-box.bg-navy { background-color: #1F78B4 !important; color: #FFFFFF !important; }"),
              tags$style(".small-box.bg-olive { background-color: #33A02C !important; color: #FFFFFF !important; }"),
              tags$style(".small-box.bg-lime { background-color: #FB9A99 !important; color: #FFFFFF !important; }"),
              
              
              
              fluidRow(
                
                column(
                  width = 12,
                  actionButton("sidebar_toggle", "Mostre/Minimize os filtros")
                ),
                div(
                  id = "Sidebar",
                  sidebarPanel(
                    h3("Filtros"),
                    selectizeInput("estado_cart",
                                   label = "Digite os estados para os gráficos",
                                   choices = unique(obitos_cartorio$Estado),
                                   multiple = T,
                                   options = list(maxItems = 35, placeholder = 'Escolha os estados'),
                                   selected = unique(obitos_cartorio$Estado)),
                    selectizeInput("tipo_morte_cart",
                                   label = "Digite as causas de óbito de interesse para os gráficos",
                                   choices = unique(obitos_cartorio$disease_type),
                                   multiple = T,
                                   options = list(maxItems = 20, placeholder = 'Escolha as causas'),
                                   selected = c("COVID-19",unique(obitos_cartorio$disease_type)[str_detect(unique(obitos_cartorio$disease_type),"20$")])),
                    width = 12
                  )
                ),
                
                # valueBoxOutput("box_pneumonia", width = 4),
                # valueBoxOutput("box_falha", width = 4),
                # valueBoxOutput("box_covid", width = 4),
                #   h6(em(data_hora_atual)),
                column(
                  width = 12,
                  tabBox(id = "tab_cart",
                         width = 12,
                         title = "Número de óbitos novos registrados em cartório",
                         tabPanel("Diário",
                                  plotlyOutput("cart_dia_plot", height = 600)
                         ),
                         tabPanel("Semana Epidemiológica",
                                  plotlyOutput("cart_sem_plot", height = 600)
                         )
                         
                  ) # tabBox 
                  
                ) # coluna
              ) # fluid
              
      ),
      tabItem("dados", 
              
              fluidRow(
                infoBox(tags$p("Brasil.io", style = "font-size: 200%;"), 
                        subtitle = 'Fonte: Secretarias de Saúde das Unidades Federativas, 
                        dados tratados por Álvaro Justen e colaboradores',
                        icon = icon("at"), color = "teal", width = 12, 
                        href = "https://brasil.io/dataset/covid19/caso", fill = TRUE),
                
                valueBox('Sobre:', subtitle = 'Informações dos dados', icon = icon("info"), 
                         color = 'teal', width = 3, 
                         href = 'https://github.com/turicas/covid19-br/blob/master/api.md#casos'),
                
                valueBox('Dados:', subtitle = 'Download', icon = icon("download"), 
                         color = 'teal', width = 3,
                         href = 'https://data.brasil.io/dataset/covid19/_meta/list.html'),
                
                valueBox('F.A.Q.:', subtitle = 'Perguntas Frequentes', icon = icon("question-circle"), 
                         color = 'teal', width = 3,
                         href = 'https://github.com/turicas/covid19-br/blob/master/faq.md'),
                
                valueBox('Licença:', subtitle = '(CC BY-SA 4.0)', icon = icon("creative-commons"), 
                         color = 'teal', width = 3,
                         href = 'https://creativecommons.org/licenses/by-sa/4.0/deed.en')
                
              ) #fluidRow
              
              
              
      ), 
      tabItem("us",
              
              fluidRow(
                column(
                  width = 6,
                  valueBoxOutput("covidMetrika",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("git_covidMetrika", width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dashboard_poa",width = 12)
                ),
                column(
                  width = 6,
                  valueBoxOutput("dashboard_rs", width = 12)
                ),
                
                widgetUserBox(
                  title = tags$b("Franciele Lobo Pallaoro"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "franciele.jpg",
                  color = "teal",
                  footer = "Contato: franpallaoro@gmail.com"
                ),
                
                widgetUserBox(
                  title = tags$b("Gabriel Holmer Saul"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "gabriel.jpg",
                  color = "teal",
                  footer = "Contato: gabrielholmersaul@gmail.com"
                )
                ,
                
                widgetUserBox(
                  title = tags$b("Gustavo Machado Utpott"),
                  subtitle = "Estudante de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "gustavo.png",
                  color = "teal",
                  footer = "Contato: gustavo.utpott@gmail.com"
                ),
                
                widgetUserBox(
                  title = tags$b("Juliana Sena de Souza"),
                  subtitle = "Estudante de Pós-Graduação em Epidemiologia da UFRGS",
                  type = 2,
                  width = 4,
                  src =  "juliana.jpeg",
                  color = "teal",
                  footer = "Contato: julianass.estatistica@gmail.com"
                ),
                
                
                widgetUserBox(
                  title = tags$b("Márcia Helena Barbian"),
                  subtitle = "Professora do departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "marcia.png",
                  color = "teal",
                  footer =  "Contato: mhbarbian@ufrgs.com"
                ), 
                
                widgetUserBox(
                  title = tags$b("Rodrigo Citton P. dos Reis"),
                  subtitle = "Professor do departamento de Estatística da UFRGS",
                  type = 2,
                  width = 4,
                  src = "rodrigo.jpg",
                  color = "teal",
                  footer =  "Contato: citton.padilha@ufrgs.br"
                ), 
                
                tags$img(src = "logos.png", 
                         height = "150", width = "1000")
                
              )
              
      ) 
    )
    
  ) # dashboard body
) # final de ui
#-------------------------------------
