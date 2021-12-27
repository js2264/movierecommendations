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
        filter_lang = ifelse(!is.null(th_filter_lang), glue::glue('original_language == {th_filter_lang}'), 'TRUE'),
        filter_hubs = ifelse(!is.null(th_filter_hubs_nlists) & !is.null(th_filter_hubs_degree), glue::glue('n_lists <= {th_filter_hubs_nlists} & degree <= {th_filter_hubs_degree}'), 'TRUE'),
        weight_trehshold = n_recommendations
    )
}

getRecommendationNetwork <- function(gr = co_occurrencesGraph(co_occurrences(), tmdb()), query, filters = movierecommendations::filters()) {

    sub <- gr

    # Filter out hubs
    sub <- sub |>
        tidygraph::activate(nodes) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['filter_hubs']])) | {name %in% query}) |>
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query})
    
    # Filter out movies according to criteria
    sub <- sub |>
        tidygraph::activate(nodes) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['year']])) | {name %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['runtime']])) | {name %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['budget']])) | {name %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['filter_lang']])) | {name %in% query}) |>
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query})

    # Filter out movies that are far away from query
    q <- which({sub |> activate(nodes) |> pull(name)} %in% query)
    sub <- sub |>
        tidygraph::activate(nodes) |>
        dplyr::mutate(
            dist = distances(
                sub, v = q, to = seq_len(gorder(sub)), mode = "out", weights = NA, algorithm = "automatic"
            ) %>% apply(2, min)
        ) |>
        dplyr::filter(dist <= 1) |> 
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query})
    
    # Only keep edges anchored at a query
    q <- which({sub |> activate(nodes) |> pull(name)} %in% query)
    sub <- sub |>
        tidygraph::activate(edges) |> 
        dplyr::filter(from %in% q | to %in% q) |> 
        tidygraph::activate(nodes) |> 
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query}) 

    # Only keep edges with greatest co-occurrences
    sub <- sub |>
        tidygraph::activate(edges) |> 
        dplyr::mutate(weight2 = weight / {as_tibble(activate(sub, nodes))$degree[as_tibble(activate(sub, edges))$from]}) |> 
        dplyr::arrange(desc(weight2)) |>
        # dplyr::filter(weight2 >= quantile(weight2, filters[['weight_trehshold']])) |> 
        dplyr::filter(weight2 >= sort(weight2, decreasing = TRUE)[filters[['weight_trehshold']]]) |> 
        tidygraph::activate(nodes) |> 
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query}) 

    # Label queries
    sub <- sub %>% 
        tidygraph::activate(nodes) |>
        dplyr::mutate(isQuery = name %in% query)

    return(sub)
}

addMovie <- function(sub, gr = co_occurrencesGraph(co_occurrences(), tmdb()), query, filters = movierecommendations::filters()) {
    
    sub2 <- gr

    # Filter out hubs
    sub2 <- sub2 |>
        tidygraph::activate(nodes) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['filter_hubs']])) | {name %in% query}) |>
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query})
    
    # Filter out movies according to criteria
    sub2 <- sub2 |>
        tidygraph::activate(nodes) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['year']])) | {name %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['runtime']])) | {name %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['budget']])) | {name %in% query}) |>
        dplyr::filter(eval(rlang::parse_expr(filters[['filter_lang']])) | {name %in% query}) |>
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query})

    # Filter out movies that are far away from query
    q <- which({sub2 |> activate(nodes) |> pull(name)} %in% query)
    sub2 <- sub2 |>
        tidygraph::activate(nodes) |>
        dplyr::mutate(
            dist = distances(
                sub2, v = q, to = seq_len(gorder(sub2)), mode = "out", weights = NA, algorithm = "automatic"
            ) %>% apply(2, min)
        ) |>
        dplyr::filter(dist <= 1) |> 
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query})
    
    # Only keep edges anchored at a query
    q <- which({sub2 |> activate(nodes) |> pull(name)} %in% query)
    sub2 <- sub2 |>
        tidygraph::activate(edges) |> 
        dplyr::filter(from %in% q | to %in% q) |> 
        tidygraph::activate(nodes) |> 
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query}) 

    # Only keep edges with greatest co-occurrences
    sub2 <- sub2 |>
        tidygraph::activate(edges) |> 
        dplyr::mutate(weight2 = weight / {as_tibble(activate(sub2, nodes))$degree[as_tibble(activate(sub2, edges))$from]}) |> 
        dplyr::arrange(desc(weight2)) |>
        # dplyr::filter(weight2 >= quantile(weight2, filters[['weight_trehshold']])) |> 
        dplyr::filter(weight2 >= sort(weight2, decreasing = TRUE)[filters[['weight_trehshold']]]) |> 
        tidygraph::activate(nodes) |> 
        dplyr::filter(!tidygraph::node_is_isolated() | {name %in% query}) 

    # Join networks
    joined_sub <- tidygraph::graph_join(
        select(sub, -dist), select(sub2, -dist), 
        by = c("name", "movieID", "original_title", "original_language", "release_date", "year", "runtime", "budget", "revenue", "vote_average", "popularity", "adult", "overview", "n_lists", "degree")
    )
    queries <- c(query, sub %>% as_tibble() %>% filter(isQuery) %>% pull(name))
    joined_sub <- joined_sub %>% 
        tidygraph::activate(nodes) |>
        dplyr::mutate(dist = 1) |>
        dplyr::mutate(isQuery = name %in% queries)

    return(joined_sub)
}