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
           release_spin_rate, pfx_x, pfx_z, at_bat_number, pitch_number) %>%
    filter(pitcher == 285079, game_year == 2015, !is.na(release_speed),
           !is.na(pfx_x), !is.na(pfx_z), !(pitch_type %in% c("IN", "PO", "")))

set.seed(333)

bad_pitches <- length(which(is.na(dickey_df$release_spin_rate)))
mean_good_kn_spin_rate <- round(mean(filter(stats_df, game_year >= 2021, 
                                            pitch_type == "KN", 
                                            release_spin_rate < 
                                            500)$release_spin_rate), 0)
spin_rates_sample <- abs(round(rnorm(round(1.1 * bad_pitches, 0), 
                                     mean = mean_good_kn_spin_rate, 
                                     sd = mean_good_kn_spin_rate / 2), 0))
low_spin_rates <- spin_rates_sample[((min(na.omit(stats_df$release_spin_rate))) 
                                     < spin_rates_sample) & 
                                    (spin_rates_sample < 
                                     500)][1:bad_pitches] %>%
    sort(decreasing = TRUE)

set.seed(333)

dickey_features <- dickey_df %>%
    select(release_speed, release_spin_rate, pfx_x, pfx_z, pitch_type) %>%
    mutate(rank = rank(release_speed, ties.method = "first", na.last = "keep"), 
           jittered_rank = rank + rnorm(n(), mean = 0, sd = 1000)) %>%
    arrange(desc(jittered_rank)) %>%
    mutate(release_spin_rate = replace(release_spin_rate,
                                       is.na(release_spin_rate),
                                       low_spin_rates)) %>%
    select(-rank, -jittered_rank)

set.seed(333)

dickey_model <- Mclust(select(dickey_features, -pitch_type), modelNames = "VVV")
summary(dickey_model)

dickey_p1 <- as.ggplot(function() plot(dickey_model, dimen = c(2, 1), 
                                       what = "classification")) +
    labs(title = paste("   R.A Dickey 2015 Pitch Clusters"))
dickey_p2 <- as.ggplot(function() plot(dickey_model, dimen = c(3, 4), 
                                       what = "classification"))
dickey_p1 / dickey_p2

cluster_names <- cbind(dickey_features$pitch_type, 
                       dickey_model$classification) %>%
    as.data.frame() %>%
    `colnames<-`(c("pitch_type", "cluster")) %>%
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
    rbind(cluster_names, c("R.A. Dickey", 2015, "pitch", "clusters",
                           rep("", ncol(cluster_names) - 4))) %>%
        `rownames<-`(c(rownames(cluster_names), "Pitcher")) %>%
        suppressWarnings() %>%
        View()
} else {
    View(cluster_names)
}
