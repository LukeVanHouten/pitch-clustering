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
           events, type, strikes, woba_value, 
           estimated_woba_using_speedangle, babip_value) %>%
    filter(pitcher == 285079, !is.na(release_speed), !is.na(pfx_x), 
           !is.na(pfx_z), !(pitch_type %in% c("IN", "PO", "")))

outcomes_df <- dickey_df %>%
    select(game_pk, game_year, at_bat_number, pitch_number, events, type, 
           strikes, woba_value, estimated_woba_using_speedangle,
           babip_value) %>%
    group_by(game_year, game_pk, at_bat_number) %>%
    mutate(event = as.numeric(
        events %in% c("strikeout", "field_out", "grounded_into_double_play", 
                      "double_play", "triple_play", "pickoff_1b", "pickoff_3b", 
                      "caught_stealing_2b", "caught_stealing_home", "other_out",
                      "strikeout_double_play")), call = as.numeric(type == "S"), 
        score = event + call, outcome = as.numeric(score >= 1),
        strike_percentage = as.numeric(type != "B"), pitch_ba = as.numeric(
            events %in% c("single", "double", "triple", "home_run")), 
        hr_percentage = as.numeric(events == "home_run"),
        k_percentage = as.numeric(events == "strikeout"),
        first_pitch = as.numeric(pitch_number == 1),
        two_strikes = as.numeric(strikes == 2), 
        xwoba = estimated_woba_using_speedangle) %>%
    ungroup() %>%
    select(outcome, pitch_ba, strike_percentage, hr_percentage, k_percentage, 
           first_pitch, two_strikes, woba_value, xwoba, babip_value)

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
    
    assign("ari", round(adjustedRandIndex(mod$classification, as.vector(
        cluster_df %>% 
            filter(game_year == year) %>% 
            select(pitch_type))$pitch_type), 4), envir = globalenv())
    
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
        rbind(., plot_points[, 1:ncol(.)], 
              cbind(rep("", 9), rep("", 9), 
                    sapply(1:(ncol(.) - 2), function(x) {
            cluster_df %>%
                select(game_year, outcome, strike_percentage, k_percentage,
                       hr_percentage, first_pitch, two_strikes, woba_value, 
                       xwoba, babip_value, cluster) %>%
                filter(game_year == year, cluster == x) %>%
                select(-game_year, -cluster) %>%
                summarize(outcome = round(sum(outcome) / nrow(.), 4), 
                          strike_percentage = round(sum(strike_percentage) /
                                                        nrow(.), 4),
                          k_percentage = round(sum(k_percentage) / nrow(.), 4),
                          hr_percentage = round(sum(hr_percentage) / nrow(.), 
                                                    4),
                          first_pitch = round(sum(first_pitch) / sum(filter(
                              cluster_df, game_year == year)$first_pitch), 4),
                          two_strikes = round(sum(two_strikes) / sum(filter(
                              cluster_df, game_year == year)$two_strikes), 4),
                          babip = round(sum(na.omit(babip_value)) / 
                                            length(na.omit(babip_value)), 4),
                          woba = round(mean(na.omit(woba_value)), 4),
                          xwoba = round(mean(na.omit(xwoba)), 4)) %>%
                t()}) %>% as.data.frame()) %>%
                  `colnames<-`(c("pitch_type", "row_id", 1:(ncol(.) - 2))), 
              cbind(rep("", 4), rep("", 4), dickey_model$parameters$mean %>% 
                        as.data.frame() %>% 
                        mutate_all(round, 4)) %>% 
                  `colnames<-`(c("pitch_type", "row_id", 1:(ncol(.) - 2)))) %>%
        `rownames<-`(c(.$pitch_type[1:(nrow(.) - 15)], "Color", "Shape", 
                       "Outcomes", "Strike%", "K%", "HR%", "1st Pitch", 
                       "2 Strikes", "BABIP", "wOBA", "xwOBA", "Mean Velocity", 
                       "Mean Spin Rate", "Mean X-Break", "Mean Z-Break")) %>%
        select(-pitch_type, -row_id) %>%
        rbind(., c(year, "pitch", "clusters", rep("", ncol(.) - 3)),
              c("Level", "Stats", rep("", ncol(.) - 2)),
              c("Level", "Stats", rep("", ncol(.) - 2)),
              c("Level", "Stats", rep("", ncol(.) - 2))) %>%
        `rownames<-`(c(rownames(.)[1:(nrow(.) - 4)], "Dickey", "Pitch", "PA", 
                       "Cluster")) %>%
        suppressWarnings()

    cluster_table <- cluster_names %>%
        slice(-which(rownames(cluster_names) %in% c("Pitch", "PA", 
                                                    "Cluster", 
                                                    "Dickey"))) %>%
        add_row(cluster_names["Pitch", , drop = FALSE], 
                .after = which(rownames(cluster_names) == "Shape")) %>%
        add_row(cluster_names["PA", , drop = FALSE], 
                .after = which(rownames(cluster_names) == "1st Pitch")) %>%
        add_row(cluster_names["Cluster", , drop = FALSE], 
                .after = which(rownames(cluster_names) ==
                                   "Mean Spin Rate")) %>%
        add_row(cluster_names["Dickey", , drop = FALSE], 
                .after = which(rownames(cluster_names) == "Shape")) %>%
        `rownames<-`(c(rownames(.)[1:(nrow(.) - 19)], "Color", "Shape", 
                       "Dickey", "Pitch", "Outcomes", "Strike%", "K%", 
                       "HR%", "PA", "1st Pitch", "2 Strikes", "BABIP", 
                       "wOBA", "xwOBA", "Cluster", "Mean Velocity", 
                       "Mean Spin Rate", "Mean X-Break", "Mean Z-Break"))
    View(cluster_table)
}

set.seed(333)

get_dickey_clusters <- function(season) {
    print(plot_clusters(year = season))
    get_cluster_table(year = season)
}

get_dickey_clusters(2015)
