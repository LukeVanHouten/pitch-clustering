library(RPostgres)
library(tidyverse)
library(ggplotify)
library(patchwork)
library(mclust)

conn <- dbConnect(Postgres(), dbname = "drpstatcast", host = "localhost",
                  port = 5432, user = "postgres", password = "drppassword")

stats_query <- "
SELECT *
FROM statcast
WHERE game_date NOT BETWEEN '2021-03-01' AND '2021-03-31'
   AND game_date NOT BETWEEN '2015-03-28' AND '2015-04-04'
   AND game_date NOT BETWEEN '2016-03-28' AND '2016-04-02'
   AND game_date NOT BETWEEN '2017-03-28' AND '2017-04-01'
   AND game_date NOT BETWEEN '2022-03-28' AND '2022-04-06'
   AND game_date NOT BETWEEN '2023-03-28' AND '2023-03-29'
"

stats_df <- dbGetQuery(conn, stats_query)

dickey_df <- stats_df %>%
    select(pitch_type, game_pk, game_year, game_date, pitcher, release_speed,
           release_spin_rate, pfx_x, pfx_z, at_bat_number, pitch_number, 
           events, type) %>%
    filter(pitcher == 285079, !is.na(release_speed), !is.na(pfx_x), 
           !is.na(pfx_z), !(pitch_type %in% c("IN", "PO", "")))

outcomes_df <- dickey_df %>%
    select(game_pk, game_year, pitcher, at_bat_number, events, type) %>%
    group_by(pitcher, game_year, game_pk, at_bat_number) %>%
    mutate(event = as.numeric(
        events %in% c("strikeout", "field_out", "grounded_into_double_play", 
                      "double_play", "triple_play", "pickoff_1b", "pickoff_3b", 
                      "caught_stealing_2b", "caught_stealing_home", "other_out",
                      "strikeout_double_play")), call = as.numeric(type == "S"), 
        score = event + call, outcome = as.numeric(score >= 1)) %>%
    ungroup() %>%
    select(outcome)

plot_points <- data.frame(
    color = c("", "", "blue", "red", "green", "purple", "orange", "lightblue", 
              "maroon", "darkgreen", "steelblue"),
    shape = c("", "", "circle", "square", "triangle", "plus", "square", "cross", 
              "circle", "star", "triangle")) %>% t() %>%
    `colnames<-`(c("pitch_type", "row_id", 1:9))

mean_good_kn_spin_rate <- round(mean(filter(stats_df, game_year >= 2021, 
                                            pitch_type == "KN", 
                                            release_spin_rate < 
                                            500)$release_spin_rate), 0)
num_bad_pitches <- dickey_df %>%
    group_by(game_year) %>%
    summarise(missing_count = sum(is.na(release_spin_rate))) %>%
    select(missing_count) %>%
    unlist() %>%
    unname()

set.seed(333)

low_spin_rates <- mapply(function(x, n) {
    sort(x[(min(na.omit(stats_df$release_spin_rate)) < x) & 
           (x < 500)][1:num_bad_pitches[[n]]], decreasing = TRUE)
    }, x = lapply(num_bad_pitches, function(y) {
           abs(round(rnorm(round(1.1 * y, 0), mean = mean_good_kn_spin_rate, 
                           sd = (mean_good_kn_spin_rate / 2)), 0))}), n = 1:3, 
                         SIMPLIFY = FALSE)

set.seed(333)

dickey_features <- dickey_df %>%
    select(game_year, release_speed, release_spin_rate, pfx_x, pfx_z, 
           pitch_type) %>%
    group_by(game_year) %>%
    mutate(rank = rank(release_speed, ties.method = "first", na.last = "keep"), 
           jittered_rank = rank + rnorm(n(), mean = 0, sd = 1000)) %>%
    arrange(game_year, desc(jittered_rank)) %>%
    ungroup() %>%
    mutate(release_spin_rate = replace(release_spin_rate,
                                       is.na(release_spin_rate),
                                       unlist(low_spin_rates))) %>%
    select(-rank, -jittered_rank)

set.seed(333)

cluster_df <- dickey_features %>%
    group_by(game_year) %>%
    mutate(id = row_number(), model = map2(id, game_year,
           ~ {if (.x == 1) Mclust(data.frame(release_speed, release_spin_rate,
                                             pfx_x, pfx_z), modelNames = "VVV")
              else NA_character_}), cluster = model[[1]]$classification) %>%
    select(-id) %>%
    ungroup() %>%
    cbind(outcomes_df)

set.seed(333)

plot_clusters <- function(year) {
    mod <- filter(cluster_df, game_year == year)$model[[1]]
    assign("dickey_model", mod, envir = globalenv())
    
    p1 <- as.ggplot(function() plot(mod, dimen = c(2, 1), 
                                    what = "classification")) +
        labs(title = paste("   R.A. Dickey", year, "Pitch Clusters"))
    p2 <- as.ggplot(function() plot(mod, dimen = c(3, 4), 
                                    what = "classification"))
    p1 / p2
}

set.seed(333)

get_cluster_table <- function(year) {
    cluster_names <- cluster_df %>%
        filter(game_year == year) %>%
        select(pitch_type, cluster) %>%
        group_by(pitch_type, cluster) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = cluster, values_from = count, 
                    values_fill = 0) %>%
        select(pitch_type, sort(colnames(.)[2:ncol(.)])) %>%
        mutate(row_id = row_number()) %>%
        arrange(match(row_id, apply(across(-pitch_type), 2, which.max))) %>%
        as.data.frame() %>%
        select(pitch_type, row_id, colnames(select(., -pitch_type, 
                                                   -row_id))) %>%
        rbind(., c("", "", sapply(1:(ncol(.) - 2), function(x) {
            data <- cluster_df %>%
                select(game_year, outcome) %>%
                filter(game_year == year) %>%
                cbind(., model$classification) %>%
                `colnames<-`(c("pitcher", "game_year", "outcome", "class")) %>%
                filter(class == x) %>%
                select(outcome)
            round(sum(data) / nrow(data), 4)})), plot_points[, 1:ncol(.)]) %>%
        `rownames<-`(c(.$pitch_type[1:(nrow(.) - 3)], "Outcomes", "Color", 
                       "Shape")) %>%
        select(-pitch_type, -row_id)
    
    if (ncol(cluster_names) >= 4) {
        cluster_table <- rbind(cluster_names, 
                               c("R.A. Dickey", year, "pitch", "clusters",
                                 rep("", ncol(cluster_names) - 4))) %>%
            `rownames<-`(c(rownames(cluster_names), "Pitcher")) %>%
            suppressWarnings()
        View(cluster_table)
    } else {
        View(cluster_names)
    }
}

get_dickey_clusters <- function(season) {
    print(plot_clusters(year = season))
    get_cluster_table(year = season)
}

get_dickey_clusters(2015)
