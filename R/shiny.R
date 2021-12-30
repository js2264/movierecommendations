css <- function() {
    shiny::tags$head(shiny::tags$style(
        shiny::HTML("
            #page {width: 100%; padding: 0px;}
            #toprow {
                padding: 10px 5px 10px 25px;
                background-color: #e2e2e2;
            }
            #maindivnet {height: 100%;}
            .pushbar.from_left {
                width: 30%;
            }
            .pushbar_overlay {
                background: #6a6a6a;
            }
            hr {margin-top: 0px;margin-bottom: 0px;}
            .bttn-gradient, .bttn-simple {
                color: #000;
                background-color: #fff;
                border-width: 1px;
                border-color: #ccc;
                box-shadow: 0 10px 10px rgba(0, 0, 0, 0.1);
                cursor: pointer;
            }
            .bttn-simple.bttn-default:hover {
                opacity: 1;
                visibility: visible;
                background-color: #e2e2e2;
            }
            .bootstrap-select.form-control {
                box-shadow: 0 10px 10px rgba(0, 0, 0, 0.1);
            }
        ")
    ))
}

uiGraph <- function() {
    shiny::fluidPage(
        id = 'page', title = 'movierecommendations',
        shiny::fluidRow(
            shiny::column(width = 12, id = "toprow", shiny::actionButton("setup", label = NULL, icon = shiny::icon("bars", lib = "font-awesome")))
        ),
        pushbar::pushbar(
            id = "filters", 
            shiny::fluidRow(
                shiny::column(width = 12, 
                    shiny::h2("Movie recommendations"),
                    shiny::hr(), 
                    shiny::br(), 
                    shinyWidgets::pickerInput(
                        inputId = "search",
                        label = 'Pick your favourite movie',
                        choices = sort(tidygraph::as_tibble(tidygraph::activate(co_occurrences(), nodes))$name),
                        selected = '12 Years a Slave (2013)',
                        options = list(`live-search` = TRUE, liveSearchStyle = 'contains', liveSearchNormalize = TRUE, liveSearchPlaceholder = 'Type smth...'),
                        width = '100%'
                    ),
                    shinyWidgets::noUiSliderInput(
                        inputId = "n_recommendations", 
                        label = "How many recommendations do you want?",
                        min = 3, max = 30, value = 10,
                        step = 1, format = shinyWidgets::wNumbFormat(decimals = 0)
                    ),
                    shiny::br(), 
                    shiny::br(), 
                    shiny::h4("Additional filters"),
                    shiny::hr(), 
                    shiny::br(), 
                    shinyWidgets::noUiSliderInput(
                        inputId = "runtime", 
                        label = "Max. runtime (minutes)",
                        min = 15, max = 180, value = 150,
                        step = 5, format = shinyWidgets::wNumbFormat(decimals = 0)
                    ),
                    shinyWidgets::noUiSliderInput(
                        inputId = "year_range", 
                        label = "Year of production",
                        min = 1950, max = 2028, value = c(1980, 2024),
                        step = 1, format = shinyWidgets::wNumbFormat(decimals = 0)
                    ),
                    shinyWidgets::pickerInput(
                        inputId = "lang", 
                        label = "Language",
                        choices = c("en", "fr", "ja", "de", "it", "es", "cn", "ru", "hi", "zh",
                            "ko", "sv", "da", "nl", "pt", "tr", "ta", "no", "th", "pl", "fa",
                            "sr", "cs", "id", "et", "fi", "he", "hu", "sk", "te", "tl", "ar",
                            "ro", "xx", "be", "bn", "bs", "ca", "el", "km", "nb", "sh", "sl"
                        ), 
                        selected = c("en", "fr", "ja", "de", "it", "es", "cn", "ru", "hi", "zh",
                            "ko", "sv", "da", "nl", "pt", "tr", "ta", "no", "th", "pl", "fa",
                            "sr", "cs", "id", "et", "fi", "he", "hu", "sk", "te", "tl", "ar",
                            "ro", "xx", "be", "bn", "bs", "ca", "el", "km", "nb", "sh", "sl"
                        ), 
                        options = list(`actions-box` = TRUE), 
                        multiple = TRUE
                    ),
                    shiny::hr(), 
                    shiny::br(), 
                    shiny::br(), 
                    shiny::actionButton("search_trigger", label = "Search", icon = shiny::icon("search", lib = "font-awesome"))
                )
            )
        ),
        shiny::column(width = 12, style = 'background-color: #e2e2e2;',
            shiny::fluidRow(
                visNetwork::visNetworkOutput('net', width = "100%", height = "100vh")
            )
        ),
        waiter::useWaiter(),
        pushbar::pushbar_deps(),
        css()
    )
}

serverGraph <- function(input, output, session) {
    
    pushbar::setup_pushbar()
    co_occurrences <- co_occurrences()
    vals <- shiny::reactiveValues(graph = NULL, selected = NULL, query = NULL, filters = filters())

    # Pushbar 
    observeEvent(input$setup, {
        pushbar::pushbar_open(id = "filters")
    })  

    # Initiate graph
    shiny::observeEvent(input$search_trigger, {
        w <- waiter::Waiter$new(
            html = waiter::spin_folding_cube(), 
            color = rgb(0, 0, 0, 0.25)
        )
        w$show()
        vals$filters <- filters(
            n_recommendations = input$n_recommendations,
            th_filter_lang = input$lang,
            th_runtime = input$runtime,
            th_year_max = input$year_range[2],
            th_year_min = input$year_range[1]
        )
        vals$graph <- recommendationNetwork(graph = co_occurrences, query = input$search, filters = vals$filters)
        w$hide() 
        pushbar::pushbar_close()
    })

    # Increment graph
    shiny::observeEvent(input$net_selected, {
        shiny::req(input$net_selected)
        vals$selected <- input$net_selected
        w <- waiter::Waiter$new(
            html = waiter::spin_folding_cube(), 
            color = rgb(0, 0, 0, 0.25)
        )
        w$show()
        vals$query <- tidygraph::as_tibble(co_occurrences)$name[tidygraph::as_tibble(co_occurrences)$movieID == input$net_selected]
        vals$graph <- addMovie(sub = vals$graph, graph = co_occurrences, query = vals$query, filters = vals$filters)
        w$hide() 
    })
    
    output$selected <- shiny::renderPrint ({ vals$selected })
    output$query <- shiny::renderPrint ({ vals$query })
    output$value <- shiny::renderPrint ({ input$search })
    output$net <- visNetwork::renderVisNetwork ({ showRecommendations(vals$graph) })
    output$nn <- shiny::renderPrint ({ nrow(tidygraph::as_tibble(vals$graph)) })

}

movieExplorer <- function() {shiny::shinyApp(ui = uiGraph(), server = serverGraph)}
