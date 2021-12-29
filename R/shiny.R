styling <- function() {
    shiny::tags$head(shiny::tags$style(
        HTML(".hr {margin-top: 0px;margin-bottom: 0px;}"),
        HTML(".bttn-simple.bttn-default {color: #000;}"),
        HTML(".bttn-simple.bttn-default {
                position: relative;
                background-color: #fff;
                border: 1px black;
                padding: 5px;
                margin: 5px;
                width: 34px;
                height: 34px;
                font-size: 18px;
                display: flex;
                justify-content: center;
                align-items: center;
                flex-direction: column;
                box-shadow: 0 10px 10px rgba(0, 0, 0, 0.1);
                cursor: pointer;
                transition: all 0.2s cubic-bezier(0.68, -0.55, 0.265, 1.55);
            }
            .bttn-simple.bttn-default:hover {
                right: -5px;
                opacity: 1;
                visibility: visible;
            }
            .bootstrap-select.form-control {
                box-shadow: 0 10px 10px rgba(0, 0, 0, 0.1);
            }
        ")
    ))
}

uiGraph <- function() {
    shiny::fillPage(
        padding = 0, title = 'movierecommendations',
        shiny::fluidRow(
            style = 'background-color: #e2e2e2;',
            column(width = 3, style='padding: 30px 20px 5px 30px;', shinyWidgets::pickerInput(
                inputId = "search",
                choices = sort(tidygraph::as_tibble(tidygraph::activate(co_occurrences(), nodes))$name),
                selected = '12 Years a Slave (2013)',
                options = list(`live-search` = TRUE, liveSearchStyle = 'contains', liveSearchNormalize = TRUE, liveSearchPlaceholder = 'Type smth...'),
                width = '100%'
            )),
            column(width = 3, style='padding: 25px 20px 5px 0px;', shinyWidgets::actionBttn(
                "search_trigger", 
                label = "", 
                icon = shiny::icon("search", lib = "font-awesome"), 
                style = "simple", 
                size = 'sm', 
                color = 'default'
            ))
        ),
        shiny::fillRow(
            style = 'background-color: #e2e2e2;',
            visNetwork::visNetworkOutput('net', width = "100%", height = "100vh")
        ),
        styling()
    )
}

serverGraph <- function(input, output, session) {
    
    co_occurrences <- co_occurrences()
    vals <- reactiveValues(graph = NULL, selected = NULL, query = NULL)

    # Initiate graph
    observeEvent(input$search_trigger, {
        vals$graph <- recommendationNetwork(graph = co_occurrences, query = input$search)
    })

    # Increment graph
    observeEvent(input$net_selected, {
        req(input$net_selected)
        vals$selected <- input$net_selected
        vals$query <- tidygraph::as_tibble(co_occurrences)$name[tidygraph::as_tibble(co_occurrences)$movieID == input$net_selected]
        vals$graph <- addMovie(sub = vals$graph, graph = co_occurrences, query = vals$query)
    })
    
    output$selected <- shiny::renderPrint ({ vals$selected })
    output$query <- shiny::renderPrint ({ vals$query })
    output$value <- shiny::renderPrint ({ input$search })
    output$net <- visNetwork::renderVisNetwork ({ showRecommendations(vals$graph) })
    output$nn <- shiny::renderPrint ({ nrow(tidygraph::as_tibble(vals$graph)) })

}

movieExplorer <- function() {shiny::shinyApp(ui = uiGraph(), server = serverGraph)}
