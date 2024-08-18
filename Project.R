library(RPostgres)
library(tidyverse)
library(ggplotify)
library(gridExtra)
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

pitcher_df <- stats_df %>%
    select(pitch_type, game_pk, game_year, game_date, pitcher, release_speed,
           release_spin_rate, pfx_x, pfx_z, at_bat_number, pitch_number) %>%
    filter(pitcher == 543037, game_year == 2023, !is.na(release_spin_rate),
           !(pitch_type %in% c("IN", "PO", ""))) %>%
    arrange(game_date, game_pk, at_bat_number, pitch_number)

features_df <- pitcher_df %>%
    select(release_speed, release_spin_rate, pfx_x, pfx_z)

Class <- pitcher_df$pitch_type
clp <- clPairs(features_df, Class, lower.panel = NULL)
clPairsLegend(0.1, 0.55, class = clp$class, col = clp$col, pch = clp$pch)
table(Class)

set.seed(333)

model <- Mclust(features_df, modelNames = "VVV")
summary(model)
adjustedRandIndex(Class, model$classification)

# plot(model, what = "classification")
grid.arrange(as.ggplot(function() plot(model, dimen = c(2, 1),
                                       what = "classification")) + 
                 labs(title = "   Gerrit Cole 2023 Pitch Clusters"),
             as.ggplot(function() plot(model, dimen = c(3, 4),
                                       what = "classification")), nrow = 2)

clustered_df <- cbind(features_df, Class, 
                      model$classification) %>%
    `colnames<-`(c(colnames(features_df), "pitch_id", "cluster"))
pitcher_df$cluster <- model$classification

cluster_names <- clustered_df %>%
    group_by(pitch_id, cluster) %>%
    summarise(count = n(), .groups = "drop") %>%
    pivot_wider(names_from = cluster, values_from = count, values_fill = 0) %>%
    select(pitch_id, sort(colnames(.)[2:ncol(.)])) %>%
    mutate(row_id = row_number()) %>%
    arrange(match(row_id, apply(across(-pitch_id), 2, which.max))) %>%
    as.data.frame() %>%
    `rownames<-`(.$pitch_id) %>%
    select(-pitch_id, -row_id)
