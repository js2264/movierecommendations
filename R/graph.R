recommendationGraph <- function(q, tmdb) {
    nodes <- tibble::tibble(name = unique(c(q$from_title, q$to_title))) |>
        dplyr::left_join(tmdb$movies, by = c('name' = 'title'))
    edges <- tibble::tibble(from = q$from_title, to = q$to_title, weight = q$`co-occurrence`)
    gr <- tidygraph::tbl_graph(edges = edges, nodes = nodes, node_key = 'name', directed = FALSE)
    nrec <- length(unique(edges$from))
    n <- tidygraph::activate(gr, nodes) |>
        tibble::as_tibble() |>
        dplyr::mutate(group = c(
            rep('queries', nrec), 
            rep('results', {tidygraph::activate(gr, nodes) |> tibble::as_tibble() |> nrow()}-nrec)
        )) |>
        dplyr::mutate(size = n_lists/25) |>
        as.data.frame()
    e <- tidygraph::activate(gr, edges) |>
        tibble::as_tibble() |>
        dplyr::mutate(from = from - 1) |>
        dplyr::mutate(to = to - 1) |>
         as.data.frame()
    net <- networkD3::forceNetwork(
        Links = e, Nodes = n, 
        Source = "from", 
        Target = "to",
        NodeID = "name", 
        Group = "group", 
        Nodesize = "size",
        opacity = 0.9, 
        zoom = TRUE, 
        linkColour = 'black', 
        bounded = FALSE, 
        fontSize = 7, fontFamily = 'sans',
        height = 1300, width = 1300, opacityNoHover = 0
    )
    return(net)
}

co_occurrencesGraph <- function(co_occurrences, tmdb) {
    nodes <- tibble::tibble(name = unique(c(co_occurrences$from, co_occurrences$to))) |>
        dplyr::left_join(tmdb$movies, by = c('name' = 'title'))
    edges <- tibble::tibble(from = co_occurrences$from, to = co_occurrences$to, weight = co_occurrences$co_occur) |>
        dplyr::filter(weight > 10)
    gr <- tidygraph::tbl_graph(edges = edges, nodes = nodes, node_key = 'name', directed = FALSE) |>
        dplyr::mutate(degree = tidygraph::centrality_degree()) |>
        dplyr::filter(!tidygraph::node_is_isolated()) |>
        dplyr::mutate(community = )
    return(gr)
}

plotRecommendations <- function(graph) {
    
    # Nodes
    vis.nodes <- as_tibble(tidygraph::activate(graph, nodes))
    vis.nodes$id <- 1:nrow(vis.nodes)
    vis.nodes$shape <- "dot"  
    vis.nodes$shadow <- TRUE 
    vis.nodes$group <- '1' 
    vis.nodes$title <- glue::glue('{vis.nodes$name}:\n{vis.nodes$overview}')
    vis.nodes$label <- vis.nodes$name
    # vis.nodes$size   <- scales::rescale(vis.nodes$degree, to = c(10, 20))
    vis.nodes$borderWidth <- 2
    vis.nodes$color.background <- '#ffffff'
    vis.nodes$color.border <- c("#e0e0e0", '#7e2424')[factor(vis.nodes$isQuery, c(FALSE, TRUE))]
    vis.nodes$color.highlight.background <- "orange"
    vis.nodes$color.highlight.border <- "darkred"
    
    # Edges
    vis.edges <- as_tibble(tidygraph::activate(graph, edges))
    vis.edges$width <- scales::rescale(vis.edges$weight2, to = c(1, 15))
    vis.edges$color <- "#383838"
    vis.edges$smooth <- FALSE
    vis.edges$shadow <- FALSE

    # Graph
    net <- visNetwork::visNetwork(as.data.frame(vis.nodes), as.data.frame(vis.edges), width="100%", height="90vh") %>% 
        visNetwork::visOptions(
            highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE), 
            # nodesIdSelection = list(enabled = TRUE, selected = c('1', '3'), values = c(5:10)), 
            autoResize = TRUE
        ) %>%
        visNetwork::visGroups(groupname = "1", shape = "icon", icon = list(code = "f008", size = 90, color = 'black')) %>%
        visNetwork::addFontAwesome() %>%
        visNetwork::visPhysics(stabilization = FALSE) %>%
        visNetwork::visLayout(randomSeed = 11)
    return(net)
}

