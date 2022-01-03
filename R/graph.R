filters <- function(
    th_year_min = NULL, #e.g. '2010'
    th_year_max = NULL, #e.g. '2020'
    th_runtime = 300,
    th_budget = 5000000000,
    th_filter_lang = NULL, #e.g. '"en"'
    th_filter_hubs_nlists = 500,
    th_filter_hubs_degree = 2000,
    n_recommendations = 10
) {
    list(
        year = ifelse(!is.null(th_year_min) & !is.null(th_year_max), glue::glue('year >= {th_year_min} & year <= {th_year_max}'), 'TRUE'),
        runtime = ifelse(!is.null(th_runtime), glue::glue('runtime <= {th_runtime}'), 'TRUE'),
        budget = ifelse(!is.null(th_budget), glue::glue('budget <= {th_budget}'), 'TRUE'),
        filter_lang = ifelse(!is.null(th_filter_lang), paste(glue::glue("original_language == '{th_filter_lang}'"), collapse = ' | '), 'TRUE'),
        filter_hubs = ifelse(!is.null(th_filter_hubs_nlists) & !is.null(th_filter_hubs_degree), glue::glue('n_lists <= {th_filter_hubs_nlists} & degree <= {th_filter_hubs_degree}'), 'TRUE'),
        n_recommendations = n_recommendations
    )
}

recommendationNetwork <- function(query, movies = movierecommendations::tmdb_movies(), filters = movierecommendations::filters()) {

    movie <- searchMovie(query, movies, field = 'unique_title')
    edges <- get_TMDB_recommendation(movie_id = movie$id, n_recommendations = filters[['n_recommendations']]) %>%
        tibble::tibble(
            from = movie$id,
            to = .
        ) %>% 
        tidyr::drop_na() %>% 
        dplyr::mutate(from = id2title(from, movies), to = id2title(to, movies))
    nodes <- movies[movies$unique_title %in% edges$to,]
    
    # Create graph from recommendations
    graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "unique_title")
    
    # Remove self edges
    graph <- tidygraph::activate(graph, edges) |> dplyr::filter(from != to)

    # Remove duplicate edges
    graph <- tidygraph::activate(graph, edges) |> dplyr::distinct()

    # Filter out movies according to criteria
    graph <- tidygraph::activate(graph, nodes) |> 
        dplyr::filter(eval(rlang::parse_expr(filters[['year']])) | {unique_title %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['runtime']])) | {unique_title %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['budget']])) | {unique_title %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['filter_lang']])) | {unique_title %in% query}) |>
        dplyr::filter(!tidygraph::node_is_isolated() | {unique_title %in% query})

    # Label queries
    graph <- tidygraph::activate(graph, nodes) |> dplyr::mutate(isQuery = unique_title %in% query)

    return(graph)
}

addMovie <- function(sub = NULL, query, movies = movierecommendations::tmdb_movies(), filters = movierecommendations::filters()) {
    
    if (is.null(sub)) stop("Please initiate a recommendation network with `recommendationNetwork()`.")
    if (is.null(query)) return(sub)

    # New recommendations
    graph <- recommendationNetwork(query, movies, filters)
        
    # Join networks
    joined_sub <- tidygraph::graph_join(
        tidygraph::select(sub, -isQuery), tidygraph::select(graph, -isQuery),
        by = c("id", "unique_title", "title", "original_title", "original_language", "release_date", "year", "runtime", "budget", "revenue", "vote_average", "popularity", "adult")
    )

    # Remove duplicate edges
    joined_sub <- tidygraph::activate(joined_sub, edges) %>% dplyr::distinct()
    
    # Re-label queries
    queries <- c(query, sub %>% tibble::as_tibble() %>% dplyr::filter(isQuery) %>% dplyr::pull(unique_title))
    joined_sub <- tidygraph::activate(joined_sub, nodes) |> dplyr::mutate(isQuery = unique_title %in% queries)
    
    return(joined_sub)
}