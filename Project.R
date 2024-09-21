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

pitcher_names <- stats_df %>%
    select(game_date, pitcher, game_year) %>%
    group_by(pitcher, game_year) %>%
    summarize(pitch_count = n()) %>%
    filter(pitch_count >= 500) %>%
    mutate(pitcher_year = paste0(pitcher, "_", game_year)) %>%
    ungroup() %>%
    select(-pitcher, -game_year, -pitch_count)

pitcher_df <- stats_df %>%
    select(pitch_type, game_pk, game_year, game_date, pitcher, release_speed,
           release_spin_rate, pfx_x, pfx_z, at_bat_number, pitch_number, 
           events, type, strikes, woba_value, 
           estimated_woba_using_speedangle, babip_value) %>%
    mutate(pitcher_year = paste0(pitcher, "_", game_year)) %>%
    filter(pitcher_year %in% pitcher_names$pitcher_year, 
           !is.na(release_speed), !is.na(release_spin_rate), !is.na(pfx_x), 
           !is.na(pfx_z), !(pitch_type %in% c("IN", "PO", ""))) %>%
    arrange(pitcher, game_date, game_pk, at_bat_number, pitch_number)

outcomes_df <- pitcher_df %>%
    select(game_pk, game_year, pitcher, at_bat_number, pitch_number, events, 
           type, strikes, estimated_woba_using_speedangle) %>%
    group_by(pitcher, game_year, game_pk, at_bat_number) %>%
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
           first_pitch, two_strikes, xwoba)

# set.seed(333)
# This code is bad and takes hours to run
# cluster_df <- pitcher_df %>%
#     select(-game_pk, -game_date, -at_bat_number, -pitch_number, 
#            -pitcher_year) %>%
#     group_by(pitcher, game_year) %>%
#     mutate(id = row_number(), model = map2(id, pitcher, 
#            ~ {if (.x == 1) set.seed(333)
#                            Mclust(data.frame(release_speed, release_spin_rate, 
#                                              pfx_x, pfx_z), 
#                                   modelNames = "VVV") 
#               else if (.x == 2) "second" 
#               else if (.x == 3) "third"
#               else NA_character_}), 
#            model = replace(model, 2, paste(model[[1]]$G, "clusters")), 
#            model = replace(model, 3, paste(
#                "ARI =", round(adjustedRandIndex(pitch_type, 
#                                                 model[[1]]$classification), 4)
#            )), cluster = model[[1]]$classification) %>%
#     ungroup()
# 
# write.csv(cluster_df$cluster, "clusters.csv", row.names = FALSE)
# 
# more_stats_df <- cluster_df %>%
#     group_by(pitcher, game_year) %>%
#     mutate(model = replace(model, 4, paste("BIC =", round(model[[1]]$bic, 4))),
#            model = replace(model, 5, paste("ICL =", round(model[[1]]$icl, 4))),
#            model = replace(model, 6, paste("Model name =", 
#                                            model[[1]]$modelName)),
#            model = replace(model, 7, paste("n =", model[[1]]$n)),
#            model = replace(model, 8, paste("df =", model[[1]]$df)),
#            model = replace(model, 9, paste("Log-likelihood =", 
#                                            round(model[[1]]$loglik, 4)))) %>%
#     ungroup() %>%
#     select(pitcher, game_year, model)
# 
# more_stats_info <- more_stats_df %>%
#     group_by(pitcher, game_year) %>%
#     mutate(model = replace(model, 1, "model")) %>%
#     ungroup() %>%
#     select(model) %>%
#     mutate(model = as.character(model))
# 
# write.csv(more_stats_info, "model_info.csv", row.names = FALSE)

clusters <- read.csv("clusters.csv")
model_info <- read.csv("model_info.csv")

cluster_df <- cbind(pitcher_df, outcomes_df, clusters, model_info) %>%
    select(-game_pk, -game_date, -at_bat_number, -pitch_number, -events, -type, 
           -pitcher_year, -strikes, -estimated_woba_using_speedangle) %>%
    `colnames<-`(c(colnames(.)[1:17], "cluster", "mod"))

cluster_counts <- cluster_df %>%
    select(pitcher, game_year, cluster) %>%
    group_by(pitcher, game_year) %>%
    summarize(num_clusters = max(cluster), .groups = "drop") %>%
    ungroup() %>%
    group_by(game_year) %>%
    summarize(avg_clusters = mean(num_clusters), .groups = "drop")

plot_points <- data.frame(
    color = c("", "", "blue", "red", "green", "purple", "orange", "lightblue", 
              "maroon", "darkgreen", "steelblue"),
    shape = c("", "", "circle", "square", "triangle", "plus", "square", "cross", 
              "circle", "star", "triangle")) %>% t() %>%
    `colnames<-`(c("pitch_type", "row_id", 1:9))
    

set.seed(333)

get_model <- function(player, year_id) {
    set.seed(333)
    
    features_df <- pitcher_df %>%
        select(pitcher, game_year, release_speed, release_spin_rate, pfx_x, 
               pfx_z) %>%
        filter(pitcher == player, game_year == year_id) %>%
        select(-pitcher, -game_year)
    
    local_model <- Mclust(features_df, modelNames = "VVV")
    assign("model", local_model, envir = globalenv())
}

set.seed(333)

plot_clusters <- function(pitcher_id, year) {
    mod <- get_model(player = pitcher_id, year_id = year)
    
    assign("ari", round(adjustedRandIndex(model$classification, as.vector(
        cluster_df %>% 
            filter(pitcher == pitcher_id, game_year == year) %>% 
            select(pitch_type))$pitch_type), 4), envir = globalenv())
    
    p1 <- as.ggplot(function() plot(mod, dimen = c(2, 1), 
                                    what = "classification")) +
        labs(title = paste("   ", pitcher_id, year, "Pitch Clusters"))
    p2 <- as.ggplot(function() plot(mod, dimen = c(3, 4), 
                                    what = "classification"))
    p1 / p2
}

set.seed(333)

get_cluster_table <- function(pitcher_id, year) {
    cluster_names <- cluster_df %>%
        filter(pitcher == pitcher_id, game_year == year) %>%
        select(pitch_type) %>%
        cbind(., model$classification) %>%
        `colnames<-`(c("pitch_type", "cluster")) %>%
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
                select(pitcher, game_year, outcome, strike_percentage, 
                       hr_percentage, k_percentage, first_pitch, 
                       two_strikes, woba_value, xwoba, babip_value) %>%
                filter(pitcher == pitcher_id, game_year == year) %>%
                cbind(., model$classification) %>%
                `colnames<-`(c(colnames(.)[1:11], "class")) %>%
                filter(class == x) %>%
                select(outcome , strike_percentage, k_percentage, hr_percentage,
                       first_pitch, two_strikes, babip_value, woba_value, 
                       xwoba) %>%
                summarize(outcome = round(sum(outcome) / nrow(.), 4), 
                          strike_percentage = round(sum(strike_percentage) /
                                                    nrow(.), 4),
                          k_percentage = round(sum(k_percentage) / nrow(.), 4),
                          hr_percentage = round(sum(hr_percentage) / nrow(.), 
                                                4),
                          first_pitch = round(sum(first_pitch) / nrow(.), 4),
                          two_strikes = round(sum(two_strikes) / nrow(.), 
                                              4),
                          babip = round(sum(na.omit(babip_value)) / 
                                        length(na.omit(babip_value)), 4),
                          woba = round(mean(na.omit(woba_value)), 4),
                          xwoba = round(mean(na.omit(xwoba)), 4)) %>%
                t()}) %>% as.data.frame()) %>%
                  `colnames<-`(c("pitch_type", "row_id", 1:(ncol(.) - 2))), 
              cbind(rep("", 4), rep("", 4), model$parameters$mean %>% 
                        as.data.frame() %>% 
                        mutate_all(round, 4)) %>% 
                  `colnames<-`(c("pitch_type", "row_id", 1:(ncol(.) - 2)))) %>%
        `rownames<-`(c(.$pitch_type[1:(nrow(.) - 15)], "Color", "Shape", 
                       "Outcomes", "Strike%", "K%", "HR%", "1st Pitch", "2 Strikes",
                       "BABIP", "wOBA", "xwOBA", "Mean Velocity", 
                       "Mean Spin Rate", "Mean X-Break", "Mean Z-Break")) %>%
        select(-pitch_type, -row_id)

    if (ncol(cluster_names) >= 4) {
        cluster_table <- rbind(cluster_names, 
                               c(pitcher_id, year, "pitch", "clusters",
                                 rep("", ncol(cluster_names) - 4)),
                               c("Level", "Stats",
                                 rep("", ncol(cluster_names) - 2)),
                               c("Level", "Stats",
                                 rep("", ncol(cluster_names) - 2)),
                               c("Stats", rep("", ncol(cluster_names) - 1))) %>%
            `rownames<-`(c(rownames(cluster_names), "Pitcher", "Pitch", "PA",
                           "Cluster")) %>%
            suppressWarnings()
        cluster_table <- cluster_table %>%
            slice(-which(rownames(cluster_table) %in% c("Pitch", "PA", 
                                                        "Cluster", 
                                                        "Pitcher"))) %>%
            add_row(cluster_table["Pitch", , drop = FALSE], 
                    .after = which(rownames(cluster_table) == "Shape")) %>%
            add_row(cluster_table["PA", , drop = FALSE], 
                    .after = which(rownames(cluster_table) == "BABIP")) %>%
            add_row(cluster_table["Cluster", , drop = FALSE], 
                    .after = which(rownames(cluster_table) ==
                                   "Mean Spin Rate")) %>%
            add_row(cluster_table["Pitcher", , drop = FALSE], 
                    .after = which(rownames(cluster_table) == "Shape")) %>%
            `rownames<-`(c(rownames(.)[1:(nrow(.) - 19)], "Color", "Shape", 
                           "Pitcher", "Pitch", "Outcomes", "Strike%", "K%", 
                           "HR%", "1st Pitch", "2 Strikes", "PA", "BABIP", 
                           "wOBA", "xwOBA", "Cluster", "Mean Velocity", 
                           "Mean Spin Rate", "Mean X-Break", "Mean Z-Break"))
        View(cluster_table)
    } else {
        cluster_table <- rbind(cluster_names, 
                               c("Level", "Stats",
                                 rep("", ncol(cluster_names) - 2)),
                               c("Level", "Stats",
                                 rep("", ncol(cluster_names) - 2)),
                               c("Stats", rep("", ncol(cluster_names) - 1))) %>%
            `rownames<-`(c(rownames(cluster_names), "Pitch", "PA", 
                           "Cluster")) %>%
            suppressWarnings()
        cluster_table <- cluster_table %>%
            slice(-which(rownames(cluster_table) %in% c("Pitch", "PA", 
                                                        "Cluster"))) %>%
            add_row(cluster_table["Pitch", , drop = FALSE], 
                    .after = which(rownames(cluster_table) == "Shape")) %>%
            add_row(cluster_table["PA", , drop = FALSE], 
                    .after = which(rownames(cluster_table) == "BABIP")) %>%
            add_row(cluster_table["Cluster", , drop = FALSE], 
                    .after = which(rownames(cluster_table) ==
                                   "Mean Spin Rate")) %>%
        `rownames<-`(c(rownames(.)[1:(nrow(.) - 18)], "Color", "Shape", 
                       "Pitch", "Outcomes", "Strike%", "K%", "HR%", 
                       "1st Pitch", "2 Strikes", "PA", "BABIP", "wOBA", 
                       "xwOBA", "Cluster", "Mean Velocity", 
                       "Mean Spin Rate", "Mean X-Break", "Mean Z-Break"))
        View(cluster_table)
    }
}

set.seed(333)

get_pitcher_clusters <- function(pitcher_mlbid, season) {
    print(plot_clusters(pitcher_id = pitcher_mlbid, year = season))
    get_cluster_table(pitcher_id = pitcher_mlbid, year = season)
}

set.seed(333)

get_pitcher_clusters(pitcher_mlbid = 669302, season = 2023)
