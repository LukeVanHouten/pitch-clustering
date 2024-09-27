# pitch-clustering

In this project, I used the R package `mclust` to identify baseball pitch types using model-based clustering based on their physical features, such as velocity, spin rate, and pitch movement. I found that these clusters oftentimes offered a more granular perspective of pitch types than MLB's labels in Statcast did. But these labels are constrained by the reality of pitcher grips defining pitch types; so, I built this R script to not only cluster pitch types, but direclty compare them with MLB's labeled pitch types.

To use this script, first build a Statcast pitch database using this guide:

https://billpetti.github.io/2021-04-02-build-statcast-database-rstats-version-3.0/

Then, run `Project.R` to get a chosen pitcher's pitch clusters in a given season. The pitcher and season can be changed via the `get_pitcher_clusters(pitcher_mlbid, season)` function, with pitcher inputs being their MLB ID (which can be found in the URL of their MLB.com or Baseball Savant page).

A special case of this project can be found in the script `Dickey.R`. MLB pitcher R.A. Dickey pitched for 3 seasons in the Statcast era (2015-present), and was notable for his knuckleball pitch, which is a pitch with extremely low spin rate that causes it to move unpredictably, confusing hitters. It is very difficult to be a consistent knuckleball pitcher, leaving Dickey as one of the very few knuckleballers in recent seasons to have a sample size of pitches large enough for proper clustering to take place. However, MLB did not record pitch spin rates under about 500 RPM during the seasons when Dickey pitched, making this clustering incomplete. So, in this file I randomly sampled the missing data using extra code, in order to paint a picture of what Dickey's clusters look like. To run this, simply choose the desired season between 2015-2017 to look at using the `get_dickey_clusters(season)` function.

The corresponding articles (2 parts) for this project can be found below:

https://medium.com/@lukevh/pitch-clustering-for-real-this-time-part-1-b3f898f4fe63
https://medium.com/@lukevh/pitch-clustering-for-real-this-time-part-2-2cd8f097fe61


