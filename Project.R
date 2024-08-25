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
           events, type) %>%
    mutate(pitcher_year = paste0(pitcher, "_", game_year)) %>%
    filter(pitcher_year %in% pitcher_names$pitcher_year, 
           !is.na(release_speed), !is.na(release_spin_rate), !is.na(pfx_x), 
           !is.na(pfx_z), !(pitch_type %in% c("IN", "PO", ""))) %>%
    arrange(pitcher, game_date, game_pk, at_bat_number, pitch_number)

outcomes_df <- pitcher_df %>%
    select(game_pk, game_year, pitcher, at_bat_number, events, type) %>%
    group_by(pitcher, game_year, game_pk, at_bat_number) %>%
    mutate(event = as.numeric(
        events %in% c("strikeout", "field_out", "grounded_into_double_play", 
                      "double_play", "triple_play", "pickoff_1b", "pickoff_3b", 
                      "caught_stealing_2b", "caught_stealing_home", "other_out",
                      "strikeout_double_play")), call = as.numeric(type == "S"), 
           score = event + call, outcome = as.numeric(score >= 1)) %>%
    select(-event, -call, -score)

# Class <- pitcher_df$pitch_type
# clp <- clPairs(select(pitcher_df, release_speed, release_spin_rate,
#                       pfx_x, pfx_z), pitcher_df$pitch_type, lower.panel = NULL)
# clPairsLegend(0.1, 0.55, class = clp$class, col = clp$col, pch = clp$pch)
# table(Class)
# 
# set.seed(333)
# 
# cluster_df <- pitcher_df %>%
#     select(-game_pk, -game_date, -at_bat_number, -pitch_number, 
#            -pitcher_year) %>%
#     group_by(pitcher, game_year) %>%
#     mutate(id = row_number(), model = map2(id, pitcher, 
#            ~ {if (.x == 1) Mclust(data.frame(release_speed, release_spin_rate, 
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

test_df_1 <- read.csv("clusters.csv")

more_stats_df <- cluster_df %>%
    group_by(pitcher, game_year) %>%
    mutate(model = replace(model, 4, paste("BIC =", round(model[[1]]$bic, 4))),
           model = replace(model, 5, paste("ICL =", round(model[[1]]$icl, 4))),
           model = replace(model, 6, paste("Model name =", 
                                           model[[1]]$modelName)),
           model = replace(model, 7, paste("n =", model[[1]]$n)),
           model = replace(model, 8, paste("df =", model[[1]]$df)),
           model = replace(model, 9, paste("Log-likelihood =", 
                                           round(model[[1]]$loglik, 4)))) %>%
    ungroup() %>%
    select(pitcher, game_year, model)

more_stats_info <- more_stats_df %>%
    group_by(pitcher, game_year) %>%
    mutate(model = replace(model, 1, "model")) %>%
    ungroup() %>%
    select(model) %>%
    mutate(model = as.character(model))

write.csv(more_stats_info, "model_info.csv", row.names = FALSE)

test_df_2 <- read.csv("model_info.csv")

set.seed(333)

get_model <- function(pitcher_id, year) {
    features_df <- pitcher_df %>%
        select(pitcher, game_year, release_speed, release_spin_rate, pfx_x, 
               pfx_z) %>%
        filter(pitcher == pitcher_id, game_year == year) %>%
        select(-pitcher, -game_year)
    
    local_model <- Mclust(features_df, modelNames = "VVV")
    assign("model", local_model, envir = globalenv())
}

get_model(pitcher_id = 669302, year = 2023)

plot_clusters <- function(pitcher_id, year) {
    plot_df <- cluster_df %>%
        filter(pitcher == pitcher_id, game_year == year)
    mod <- plot_df$model[[1]]
    
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
        select(-pitcher, -game_year, -id, -model) %>%
        group_by(pitch_type, cluster) %>%
        summarise(count = n(), .groups = "drop") %>%
        pivot_wider(names_from = cluster, values_from = count, 
                    values_fill = 0) %>%
        select(pitch_type, sort(colnames(.)[2:ncol(.)])) %>%
        mutate(row_id = row_number()) %>%
        arrange(match(row_id, apply(across(-pitch_type), 2, which.max))) %>%
        as.data.frame() %>%
        `rownames<-`(.$pitch_type) %>%
        select(-pitch_type, -row_id)

    if (ncol(cluster_names) >= 4) {
        rbind(cluster_names, c(pitcher_id, year, "pitch", "clusters",
                               rep("", ncol(cluster_names) - 4))) %>%
            `rownames<-`(c(rownames(cluster_names), "Pitcher")) %>%
            suppressWarnings() %>%
            View()
    } else {
        View(cluster_names)
    }
}

set.seed(333)

get_pitcher_clusters <- function(pitcher_mlbid, season) {
    print(plot_clusters(pitcher_id = pitcher_mlbid, year = season))
    get_cluster_table(pitcher_id = pitcher_mlbid, year = season)
    
    model_info <- more_stats_df %>%
        filter(pitcher == pitcher_mlbid, game_year == season) %>%
        select(model) %>%
        head(9)
    
    assign("chosen_model", model_info, envir = globalenv())
    View(chosen_model)
}

get_pitcher_clusters(pitcher_mlbid = 669302, season = 2023)
