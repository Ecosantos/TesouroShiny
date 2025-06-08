library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(janitor)
library(plotly)

# Link padrão do Tesouro
url <- "https://www.tesourotransparente.gov.br/ckan/dataset/df56aa42-484a-4a59-8184-7676580c81e3/resource/796d2059-14e9-44e3-80c9-2d9e30b405c1/download/precotaxatesourodireto.csv"

ui <- fluidPage(
    # Cabeçalho personalizado
    tags$div(
        style = "background-color: #f8f9fa; padding: 15px; border-bottom: 1px solid #dee2e6; margin-bottom: 10px;",
        tags$p(
            style = "margin: 0; font-style: italic; text-align: center;",
            "Desenvolvido com carinho num domingo preguiçoso por ",
            tags$a(href = "https://ecosantos.netlify.app/", target = "_blank", "Gabriel Santos")
        ),
        tags$div(
            style = "margin-top: 5px; display: flex; align-items: center; justify-content: center;",
            tags$span("Dúvidas, sugestões e contribuições são bem-vindas."),
            tags$a(href = "https://github.com/Ecosantos/TesouroShiny", target = "_blank",
                   tags$img(src = "https://cdn.jsdelivr.net/gh/simple-icons/simple-icons/icons/github.svg",
                            height = "24px", width = "24px", style = "margin-left: 10px;"))
        )
    ),
    
    titlePanel("📊 Análise Interativa - Tesouro Direto"),
    
    sidebarLayout(
        sidebarPanel(
            actionButton("atualizar", "🔄 Atualizar valores"),
            helpText("Caso o botão de atualizar não funcione, pegue o link do site do tesouro e cole no espaço abaixo. Isso provavelmente ocorre pq o link é atualizado todos os dias."),
            
            uiOutput("link_botoes"),  # botões dinâmicos
            
            br(),
            textInput("url_custom", "Link alternativo:", 
                      placeholder = "Cole aqui o novo link CSV do Tesouro Direto"),
            hr(),
            uiOutput("controle_titulo"),
            uiOutput("controle_data")
        ),
        
        mainPanel(
            plotlyOutput("grafico")
        )
    )
)

server <- function(input, output, session) {
    dados <- reactiveVal(NULL)
    
    observeEvent(input$atualizar, {
        showNotification("Carregando dados do Tesouro Direto...", type = "message")
        
        # Usa link customizado se fornecido
        link_final <- ifelse(nzchar(input$url_custom), input$url_custom, url)
        
        df <- read_csv2(link_final, locale = locale(encoding = "Latin1")) %>%
            clean_names()
        
        df$data_base <- as.Date(df$data_base, format = "%d/%m/%Y")
        df$data_vencimento <- as.Date(df$data_vencimento, format = "%d/%m/%Y")
        
        df$ano_vencimento <- format(df$data_vencimento, "%Y")
        df$tipo_ano <- paste(df$tipo_titulo, df$ano_vencimento)
        
        # Filtro: somente títulos com vencimento igual ou posterior ao ano atual, e sem "Selic"
        ano_atual <- as.numeric(format(Sys.Date(), "%Y"))
        df <- df %>%
            filter(ano_vencimento >= ano_atual) %>%
            filter(!grepl("(?i)selic", tipo_titulo))  # remove qualquer Selic, case-insensitive
        
        dados(df)
        
        showNotification("✅ Dados carregados com sucesso!", type = "message")
    })
    
    output$controle_titulo <- renderUI({
        req(dados())
        selectInput("titulo", "Selecione o título:",
                    choices = unique(dados()$tipo_ano))
    })
    
    output$controle_data <- renderUI({
        req(dados())
        dateRangeInput("datas", "Período:",
                       start = min(dados()$data_base),
                       end = max(dados()$data_base))
    })
    
    output$grafico <- renderPlotly({
        req(dados(), input$titulo, input$datas)
        
        df_filtrado <- dados() %>%
            filter(tipo_ano == input$titulo,
                   data_base >= input$datas[1],
                   data_base <= input$datas[2])
        
        df_filtrado %>%
            arrange(data_base) %>%
            plot_ly(x = ~data_base, y = ~taxa_compra_manha, 
                    type = "scatter",
                    mode = 'lines+markers',
                    line = list(color = '#B9B9B9'),
                    marker = list(color = 'black', size = 5)) %>%
            layout(
                title = paste("Rendimento %a.a. -", input$titulo),
                xaxis = list(title = "Data"),
                yaxis = list(title = "Taxa (%)"),
                hovermode = "x unified"
            )
    })
    
    output$link_botoes <- renderUI({
        link_final <- ifelse(nzchar(input$url_custom), input$url_custom,
                             "https://www.tesourotransparente.gov.br/ckan/dataset/taxas-dos-titulos-ofertados-pelo-tesouro-direto/resource/796d2059-14e9-44e3-80c9-2d9e30b405c1")
        
        fluidRow(
            column(6,
                   tags$a(href = "https://www.tesourotransparente.gov.br/ckan/dataset/taxas-dos-titulos-ofertados-pelo-tesouro-direto/resource/796d2059-14e9-44e3-80c9-2d9e30b405c1?_gl=1*139vbas*_gcl_au*NTA1NTkyMjk3LjE3NDY5MDEwNzQ.*_ga*NDMyMzU0OTY5LjE3NDY5MDEwNzU.*_ga_95FH8RQ7M0*czE3NDk0MDY2NTEkbzIkZzEkdDE3NDk0MDc2NjAkajYwJGwwJGgw", 
                          class = "btn btn-primary btn-block", 
                          target = "_blank", "📂 Pegar o link")
            ),
            column(6,
                   tags$a(href = "https://raw.githubusercontent.com/Ecosantos/TesouroShiny/refs/heads/master/Instru%C3%A7%C3%B5es.png", 
                          class = "btn btn-secondary btn-block", 
                          target = "_blank", "❓ Instruções")
            )
        )
    })
}

shinyApp(ui = ui, server = server)
