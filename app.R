library(shiny)
library(readr)
library(dplyr)
library(ggplot2)  # ainda pode ser usado para theme ou outros plots
library(janitor)
library(plotly)

url <- "https://www.tesourotransparente.gov.br/ckan/dataset/df56aa42-484a-4a59-8184-7676580c81e3/resource/796d2059-14e9-44e3-80c9-2d9e30b405c1/download/precotaxatesourodireto.csv"

ui <- fluidPage(
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
            helpText("Caso o botão de atualizar não funcione, copie e cole o link no espaço abaixo. Isso provavelmente ocorre pq o link é atualizado todos os dias."),
            fluidRow(
                column(6,
                       tags$a(href = "https://www.tesourotransparente.gov.br/ckan/dataset/taxas-dos-titulos-ofertados-pelo-tesouro-direto/resource/796d2059-14e9-44e3-80c9-2d9e30b405c1?_gl=1*ahrcph*_gcl_au*NTA1NTkyMjk3LjE3NDY5MDEwNzQ.*_ga*NDMyMzU0OTY5LjE3NDY5MDEwNzU.*_ga_95FH8RQ7M0*czE3NDk0MTIzMjUkbzMkZzEkdDE3NDk0MTQ5MzEkajI3JGwwJGgw", 
                              class = "btn btn-primary btn-block", target = "_blank", "📂 Acesse o histórico")
                ),
                column(6,
                       tags$a(href = "#",  # Substitua o # pelo link real depois
                              class = "btn btn-secondary btn-block", target = "_blank", "❓ Instruções")
                )
            ),
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
        
        df <- read_csv2(url, locale = locale(encoding = "Latin1")) %>%
            janitor::clean_names()
        
        df$data_base <- as.Date(df$data_base, format = "%d/%m/%Y")
        df$data_vencimento <- as.Date(df$data_vencimento, format = "%d/%m/%Y")
        
        ano_atual <- format(Sys.Date(), "%Y")  # Pega o ano atual como string
        
        df$ano_vencimento <- format(df$data_vencimento, "%Y")
        df <- df %>%
            filter(!grepl("SELIC", tipo_titulo, ignore.case = TRUE),
                   ano_vencimento >= ano_atual) %>%
            mutate(tipo_ano = paste(tipo_titulo, ano_vencimento))
        
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

        
        df_filtrado%>%
            arrange(data_base)%>%
            plot_ly(., x = ~data_base, y = ~taxa_compra_manha, 
                    type="scatter",
                    mode = 'lines+markers',
                    line = list(color = '#B9B9B9'),
                    marker = list(color = 'black',size=5)) %>%
            layout(
                title = paste("Rendimento %a.a. -"),
                xaxis = list(title = "Data"),
                yaxis = list(title = "Taxa (%)"),
                hovermode = "x unified"
            )
    })
}



shinyApp(ui = ui, server = server)
