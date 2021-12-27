############################################
######## SCRAP MOVIE LISTS #################
############################################

n_cores <- parallelly::availableCores() - 1
future::plan(future::multiprocess, workers = n_cores)
requests <- paste0('list/', 1:100000) %>% furrr::future_map(get_TMDB)

lists <- parallel::mclapply(mc.cores = 6, seq_along(requests), function(K) {
    print(K)
    movies <- httr::content(requests[[K]])[['items']] %>% 
        sapply(function(x) {
            x[['id']]
        })
    list <- list(movies)
    names(list) <- httr::content(requests[[K]])[['id']]
    return(list)
}) %>% purrr::flatten()

list_names <- parallel::mclapply(mc.cores = 6, seq_along(requests), function(K) {
    if (K %% 1000 == 0) print(K)
    x <- httr::content(requests[[K]])[['name']]
    if (is.null(x)) {
        NA
    } 
    else {
        x
    }
}) %>% unlist()

TMDB_lists <- tibble::tibble(
    listID = as.integer(names(lists)), 
    listName = list_names,
    movies = lists, 
    n_movies = lengths(lists)
)
saveRDS(TMDB_lists, 'TMDB_lists.rds')

############################################
######## SCRAP MOVIE INFOS #################
############################################

listed_movies <- sort(unique(unlist(TMDB_lists$movies)))
n_cores <- parallelly::availableCores() - 1
future::plan(future::multiprocess, workers = n_cores)
requests <- paste0('movie/', listed_movies) %>% furrr::future_map(get_TMDB)
saveRDS(requests, 'requests_movies.rds')
TMDB_movies <- parallel::mclapply(mc.cores = 18, seq_along(requests), function(K) {
    print(K)
    if (httr::status_code(requests[[K]]) == 200) {
        x <- httr::content(requests[[K]])
        x[sapply(x, is.null)] <- NA
        xx <- tibble::as_tibble(x[c(
            'id', 'title', 'original_title', 'original_language', 
            'release_date', 'runtime', 'budget', 'revenue', 
            'vote_average', 'popularity', 'adult', 'overview'
        )])
        xx$genres <- list(ifelse(is.na(x[['genres']]), NA, sapply(x[['genres']], '[[', 'name')))
        return(xx)
    } 
    else {
        tibble::tibble(
            'id' = K,
            'title' = NA,
            'original_title' = NA,
            'original_language' = NA,
            'release_date' = NA,
            'runtime' = NA,
            'budget' = NA,
            'revenue' = NA,
            'vote_average' = NA, 
            'popularity' = NA,
            'adult' = NA, 
            'overview' = NA,
            'genres' = NA
        )
    }
}) %>% 
    dplyr::bind_rows() %>% 
    dplyr::left_join(
        TMDB_lists %>% 
            dplyr::filter(n_movies > 0) %>% 
            tidyr::unnest(cols = movies) %>% 
            dplyr::select(movies) %>% 
            dplyr::group_by(movies) %>% 
            dplyr::tally(name = 'n_lists') %>% 
            dplyr::rename(id = movies)
    ) %>% 
    dplyr::mutate(
        year = stringr::str_replace(release_date, '-.*', ''), 
        title = glue::glue("{title} ({year})")
    ) %>% 
    dplyr::relocate(year, .after = release_date) %>% 
    tidyr::drop_na(original_title) %>% 
    dplyr::distinct()
duplicated_titles <- unique(TMDB_movies$title[duplicated(TMDB_movies$title)])
highest_ids <- purrr::map(duplicated_titles, \(x) {TMDB_movies[TMDB_movies$title == x, ] %>% dplyr::arrange(desc(n_lists)) %>% dplyr::slice_head(n = 1) %>% dplyr::pull(id)}) %>% unlist()
TMDB_movies <- TMDB_movies[!TMDB_movies$title %in% duplicated_titles | TMDB_movies$id %in% highest_ids, ]
saveRDS(TMDB_movies, 'TMDB_movies.rds')

############################################
######## SCRAP GENRE INFOS #################
############################################

listed_genres <- sort(unique(unlist(TMDB_movies$genres)))
TMDB_genres <- dplyr::filter(TMDB_movies, lengths(genres) > 0) %>% 
    tidyr::unnest(genres) %>% 
    dplyr::select(id, genres)
saveRDS(TMDB_genres, 'TMDB_genres.rds')

############################################
######## MAKE SQL DATABSE ##################
############################################

TMDB <- RSQLite::dbConnect(RSQLite::SQLite(), "inst/extdata/tmdb.sqlite")

dplyr::select(TMDB_movies, -genres) %>% 
    dplyr::rename(movieID = id) %>%
    RSQLite::dbWriteTable(TMDB, "movies", ., overwrite = TRUE)
dplyr::filter(TMDB_lists, lengths(movies) > 0) %>% 
    tidyr::unnest(movies) %>%
    dplyr::rename(movieID = movies) %>%
    RSQLite::dbWriteTable(TMDB, "lists", ., overwrite = TRUE)
dplyr::rename(TMDB_genres, movieID = id) %>% 
    RSQLite::dbWriteTable(TMDB, "genres", ., overwrite = TRUE)

RSQLite::dbDisconnect(TMDB)

############################################
######## MAKE CO-OCCURRENCES TABLE #########
############################################

tmdb <- tmdb()
n_cores <- parallelly::availableCores() - 1
future::plan(future::multiprocess, workers = n_cores)
res <- tmdb$lists |>
    dplyr::select(listID, movieID) %>%
    dplyr::left_join({
        .x <- .
        tibble::tibble(listID = unique(.x$listID), chunk = as.numeric(cut(seq_along(unique(listID)), breaks = 1000, include.lowest = TRUE)))
    }) |>
    dplyr::group_by(chunk) |> dplyr::group_split() |>
    furrr::future_map(\(df) {
        df |>
        dplyr::count(listID, movieID) %>%
        dplyr::inner_join(., ., by = 'listID') |>
        dplyr::filter(movieID.x != movieID.y) |>
        dplyr::group_by(movieID.x, movieID.y) |>
        dplyr::tally(name = 'co_occur')
    }) 
co_occurrences <- dplyr::bind_rows(res) |>
    dplyr::group_by(movieID.x, movieID.y) |>
    dplyr::summarize(co_occur = sum(co_occur)) |>
    dplyr::ungroup() |>
    dplyr::left_join(dplyr::select(tmdb$movies, movieID, title), by = c('movieID.x' = 'movieID')) |>
    dplyr::rename(title_from = title) |> dplyr::select(-movieID.x) |>
    dplyr::left_join(dplyr::select(tmdb$movies, movieID, title), by = c('movieID.y' = 'movieID')) |>
    dplyr::rename(title_to = title) |> dplyr::select(-movieID.y) |> 
    dplyr::relocate(co_occur, .after = title_to) %>% 
    dplyr::filter(co_occur > 2) %>%
    tidyr::drop_na() %>%
    dplyr::arrange(dplyr::desc(co_occur)) %>% 
    dplyr::mutate(from = dplyr::case_when(title_from < title_to ~ title_from, TRUE ~ title_to)) %>% 
    dplyr::mutate(to = dplyr::case_when(title_from < title_to ~ title_to, TRUE ~ title_from)) %>% 
    dplyr::filter(dplyr::select(., from, to) %>% duplicated() %>% `!`) %>% dplyr::select(-title_from, -title_to) %>%
    dplyr::relocate(co_occur, .after = to)

