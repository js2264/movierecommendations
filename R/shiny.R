css <- function() {
    shiny::tags$head(shiny::tags$style(
        shiny::HTML("
            #page {width: 100%; padding: 0px;}
            #topleft {
                padding: 10px 5px 10px 25px;
                background-color: #e2e2e2;
                height: 54px;
            }
            #topcenter {
                padding: 10px 5px 10px 25px;
                background-color: #e2e2e2;
                height: 54px;
            }
            #topright {
                padding: 10px 25px 10px 5px;
                background-color: #e2e2e2;
                height: 54px;
                text-align: right;
            }
            #maindivnet {height: 100%;}
            .pushbar.from_left {
                width: 30%;
            }
            .pushbar.from_right {
                width: 40%;
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
            shiny::column(width = 1, id = "topleft", shiny::actionButton("setup", label = NULL, icon = shiny::icon("bars", lib = "font-awesome"))),
            shiny::column(width = 10, id = "topcenter", ),
            shiny::column(width = 1, id = "topright", shiny::actionButton("movieInfos", label = NULL, icon = shiny::icon("chevron-left", lib = "font-awesome"))),
        ),
        pushbar::pushbar(
            id = "filters", 
            shiny::fluidRow(
                shiny::column(width = 12, 
                    shiny::h2("Movie recommendations"),
                    shiny::hr(), 
                    shiny::br(), 
                    shiny::htmlOutput('picker'),
                    # shinyWidgets::pickerInput(
                    #     inputId = "search",
                    #     label = 'Pick your favourite movie',
                    #     choices = choices,
                    #     selected = '12 Years a Slave (2013)',
                    #     options = list(`live-search` = TRUE, liveSearchStyle = 'contains', liveSearchNormalize = TRUE, liveSearchPlaceholder = 'Type smth...'),
                    #     width = '100%'
                    # ),
                    shinyWidgets::noUiSliderInput(
                        inputId = "n_recommendations", 
                        label = "How many recommendations do you want?",
                        min = 3, max = 20, value = 10,
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
                        min = 30, max = 180, value = 150,
                        step = 5, format = shinyWidgets::wNumbFormat(decimals = 0)
                    ),
                    shinyWidgets::noUiSliderInput(
                        inputId = "year_range", 
                        label = "Year of production",
                        min = 1980, max = 2028, value = c(1980, 2024),
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
        pushbar::pushbar(
            id = "infos", 
            from = 'right',
            shiny::fluidRow(
                shiny::column(width = 12, 
                    shiny::htmlOutput('movie_info')
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
    
    # Initiate session
    {
        w <- waiter::Waiter$new(
            html = shiny::tagList(
                waiter::spin_folding_cube(),
                shiny::h3("Initiating session..."),
                shiny::h5("(Awesome recommendations are worth the wait!)"),
            ),
            color = '#0665b37e'
        )
        w$show()
        tmdb_movies <- tmdb_movies()
        choices <- tmdb_movies$unique_title[tmdb_movies$year > 1980 & tmdb_movies$runtime > 30 & tmdb_movies$budget > 0]
        output$picker <- shiny::renderUI({ 
            shinyWidgets::pickerInput(
                inputId = "search",
                label = 'Pick your favourite movie',
                choices = choices,
                options = list(`live-search` = TRUE, liveSearchStyle = 'contains', liveSearchNormalize = TRUE, liveSearchPlaceholder = 'Type smth...')
            )
        })

        w$hide() 
        pushbar::setup_pushbar()
        vals <- shiny::reactiveValues(
            graph = NULL, selected = NULL, query = NULL, filters = filters(), 
            movie_info = list()
        )
    }

    # Left pushbar (settings)
    observeEvent(input$setup, {
        pushbar::pushbar_open(id = "filters")
    })

    # Right pushbar (infos)
    observeEvent(input$movieInfos, {
        pushbar::pushbar_open(id = "infos")
    })

    # Initiate graph
    shiny::observeEvent(input$search_trigger, {
        w <- waiter::Waiter$new(
            html = shiny::tagList(
                waiter::spin_folding_cube(),
                shiny::h3("Getting some recommendations for you..."),
                shiny::h5("(Use this time wisely and go fetch some popcorn!)"),
            ),
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
        vals$graph <- recommendationNetwork(query = input$search, movies = tmdb_movies, filters = vals$filters)

        # Get movie infos
        infos <- as.list(searchMovie(x = input$search, movies = tmdb_movies, field = 'unique_title'))
        infos$TMDB_url <- ''
        infos$TMDB_picture <- ''
        vals$movie_info <- infos

        w$hide() 
        pushbar::pushbar_close()
    })

    # Increment graph
    shiny::observeEvent(input$net_selected, {
        
        shiny::req(input$net_selected)
        vals$selected <- input$net_selected
        w <- waiter::Waiter$new(
            html = shiny::tagList(
                waiter::spin_folding_cube(),
                shiny::h3("Fetching more movies for your evening...")
            ),
            color = rgb(0, 0, 0, 0.25)
        )
        w$show()

        # Add new movie to graph
        vals$query <- tmdb_movies$unique_title[tmdb_movies$id == input$net_selected]
        vals$graph <- addMovie(sub = vals$graph, movies = tmdb_movies, query = vals$query, filters = vals$filters)

        # Get movie infos
        infos <- as.list(searchMovie(x = input$net_selected, movies = tmdb_movies, field = 'id'))
        infos$TMDB_url <- ''
        infos$TMDB_picture <- ''
        vals$movie_info <- infos

        w$hide() 
    })
    
    # Update output values
    output$selected <- shiny::renderPrint ({ vals$selected })
    output$query <- shiny::renderPrint ({ vals$query })
    output$value <- shiny::renderPrint ({ input$search })
    output$net <- visNetwork::renderVisNetwork ({ showRecommendations(vals$graph) })
    output$nn <- shiny::renderPrint ({ nrow(tidygraph::as_tibble(vals$graph)) })
    output$movie_info <- shiny::renderUI({
        fluidRow(
            column(width = 12, { 
                shiny::HTML(paste0(
                    shiny::h2("Movie informations"),
                    shiny::hr(),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Original title:")), shiny::column(width = 9, shiny::p(vals$movie_info$original_title, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("English title:")), shiny::column(width = 9, shiny::p(vals$movie_info$title, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Original language:")), shiny::column(width = 9, shiny::p(vals$movie_info$original_language, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Release date:")), shiny::column(width = 9, shiny::p(vals$movie_info$release_date, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Runtime:")), shiny::column(width = 9, shiny::p(vals$movie_info$runtime, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Budget:")), shiny::column(width = 9, shiny::p(vals$movie_info$budget, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Revenue:")), shiny::column(width = 9, shiny::p(vals$movie_info$revenue, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Vote average:")), shiny::column(width = 9, shiny::p(vals$movie_info$vote_average, style = 'margin: 10px 0px;'))),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 3, shiny::h4("Plot:")), shiny::column(width = 9, shiny::p(vals$movie_info$overview, style = 'margin: 10px 0px;')))
                ))
            })
        )
    })
}

movieExplorer <- function() {shiny::shinyApp(ui = uiGraph(), server = serverGraph)}
