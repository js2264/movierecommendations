#' title2id
#' 
#' @export

title2id <- function(titles, tmdb) {
    tmdb[['movies']] |>
        dplyr::filter(title %in% titles) |>
        dplyr::pull(movieID)
}

#' id2title
#' 
#' @export

id2title <- function(ids, tmdb = tmdb()) {
    tmdb[['movies']] |>
        dplyr::filter(movieID %in% ids) |>
        dplyr::pull(title)
}

#' searchField
#' 
#' @export

searchField <- function(pattern, tmdb = tmdb(), field = 'title', return_first = FALSE) {
    entries <- tmdb[['movies']] |> dplyr::select(field) |> dplyr::pull(field)
    lapply(pattern, \(x) {
        res <- agrep(x, entries, ignore.case = TRUE, value = TRUE)
        res <- res[order(stringdist::stringdist(x, res))]
        if (return_first | res[1] == x) {
            res[1]
        }
        else {
            res
        }
    }) %>% unlist()
}

#' fetchMovieByID
#' 
#' @export

fetchMovieByID <- function(x, tmdb = tmdb()) {
    dplyr::filter(tmdb[['movies']], movieID == x)
}

#' searchMovie
#' 
#' @export

searchMovie <- function(pattern, tmdb = tmdb(), field = 'title') {
    x <- searchField(pattern, tmdb, field = field, return_first = TRUE)
    x <- tmdb[['movies']] |>
        dplyr::filter(eval(rlang::parse_expr(field)) == x) |>
        dplyr::pull(movieID)
    dplyr::filter(tmdb[['movies']], movieID == x)
}

#' getRecommendations
#' 
#' @export

getRecommendations <- function(
    nodes, 
    tmdb, 
    nRecommendations = 20, 
    nlist_threshold = 100,
    nmovie_threshold = 100000
) {
    q <- lapply(seq_along(nodes), function(K) {
        node <- nodes[[K]]
        if (is.character(node)) {
            titles <- searchField(node, tmdb)
            if (length(titles) != 1) {
                stop("Title not found (or several titles found). Use movierecommendations::searchField() to get exact title.")
            }
            node <- title2id(node, tmdb)
        }
        tmdb$movies %>% 
            dplyr::filter(movieID == node) %>% 
            dplyr::left_join(tmdb$lists, by = 'movieID') %>% 
            dplyr::select(listID) %>% 
            dplyr::left_join(tmdb$lists, by = 'listID') %>% 
            dplyr::filter(n_movies <= nmovie_threshold) %>%
            dplyr::select(movieID) %>% 
            dplyr::left_join(tmdb$movies, by = 'movieID') %>% 
            dplyr::filter(n_lists <= nlist_threshold) %>%
            dplyr::select(movieID) %>% 
            dplyr::group_by(movieID) %>% 
            dplyr::tally(name = 'co-occurrence') %>% 
            dplyr::filter(movieID != node) %>%
            dplyr::arrange(desc(`co-occurrence`)) %>% 
            head(n = nRecommendations) %>% 
            dplyr::mutate(node = node) %>%
            dplyr::rename(from = 'node', to = 'movieID') %>% 
            dplyr::select(from, to, `co-occurrence`) %>% 
            enrichRecommendation(tmdb)
    }) %>% dplyr::bind_rows()
    q <- tibble::as_tibble(cbind(dplyr::select(q, -to_info), q$to_info)) %>% 
        dplyr::select(-from, -to, -movieID, -title, -original_title, -original_language, -year) %>% 
        dplyr::relocate(`co-occurrence`, .after = to_title)
    return(q)
}

#' enrichRecommendation
#' 
#' @export

enrichRecommendation <- function(q, tmdb) {
    q %>% 
        dplyr::mutate(from_title = id2title(from, tmdb)) %>% 
        dplyr::mutate(to_title = id2title(to, tmdb)) %>% 
        dplyr::mutate(to_info = lapply(to, fetchMovieByID, tmdb) %>% dplyr::bind_rows())
}

