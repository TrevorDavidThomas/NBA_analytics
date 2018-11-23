# NBA_analytics
The contents of this repository focus on gathering, analyzing, and visualizing NBA data, and form the basis for the work displayed on [VisualizingTheLeague.com](https://visualizingtheleague.com/). This readme describes the composition of those contents, with scripts and their descriptions split into categories of gathering/processing and analyzing/visualizing.

- [Gathering and Processing Data](#Gathering-and-Processing-Data)
  * [dataAssembly_historicalRoster.R](#dataAssembly_historicalRosterR)
  * [dataAssembly_coachingStaffs_bballRef.R](#dataAssembly_coachingStaffs_bballRefR)
  * [dataAssembly_draftCombine_defAdvStats.R](#dataAssembly_draftCombine_defAdvStatsR)
  * [dataAssembly_playerHometowns.R](#dataAssembly_playerHometownsR)

- [Analyzing and Visualizing Data](#Analyzing-and-Visualizing-Data)
  * [analysis_draftCombine_advDef_nnetPredictions.R](#analysis_draftCombine_advDef_nnetPredictionsR)
  * [analysis_draftCombine_advDef_linearMod.R](#analysis_draftCombine_advDef_linearModR)
  * [analysis_draftCombine_advDef_descriptives.R](#analysis_draftCombine_advDef_descriptivesR)
  * [analysis_coachingNetworks.R](#analysis_coachingNetworksR)

<!-- toc -->

## Gathering and Processing Data
The code described here consists mainly of R scripts that pull and process data from the stats.nba API and/or [basketball-reference.com](https://www.basketball-reference.com/). These scripts tend to output data to the 'data' folder in this repository, provided that 1) the data files are relatively small, 2) they're used as inputs in other scripts in the repository, and 3) they're unlikely to be frequently updated or otherwise edited.

### dataAssembly_historicalRoster.R
Code used to get full historical NBA roster from stats.nba API (using `httr` and `jsonlite` packages). Roster consists of name and other identifying information (including begin/end years and current team, but with no stats or other player attributes. Saves to dataframe 'players_fullRoster_df.RData'.

### dataAssembly_coachingStaffs_bballRef.R
Code used to scrape head and assistant coaching records from basketball-reference.com (using `rvest` package), saving the results to two lists, for head and assistant records respectively, containing a dataframe with year/team/outcome data for each coach on record (both lists contained in 'coachingLists_bbref.RData').

### dataAssembly_draftCombine_defAdvStats.R
Code used to get full set of available draft combine data from the stats.nba API, to get associated span of advanced defensive stats from Basketball Reference (via `ballr` package, which provides out of the box functions for scraping yearly and career statistical records), and to join and clean these datasets such that they can be used to model relationshp between advanced defensive stats and combine measures. Output files include a list of raw, year-specific combine dataframes ('combineStats_allYrList.RData'); raw, year specific advanced stats dataframes ('advanceStats_2000toPresent_allYrList.RData'); single, cleaned dataframes for advanced stats and combine measures ('advanceStats_fin_df_2000_01toPresent.RData' and 'combine_fin_df.RData'); and a joined dataframe in which the combine stats and defensive stats are joined on player name and cleaned ('combine_advDef_final_df.RData').

### dataAssembly_playerHometowns.R
Code used to scrape birth cities for all NBA players, both domestic- and foreign-born, from https://www.basketball-reference.com/friv/birthplaces.fcgi, using `rvest` package. These cities are then geocoded using `ggmap` package, providing mappable lat/lon birth coordinates for each player (represented using WGS 84 coordinate system). Outputs a final, manually cleaned dataframe with all successfully geocoded players/birth locations: 'player_geocode_final_df.RData'. Also outputs a number of intermediary lists of domestic and foreign state- and country-specific birth city dataframes.

## Analyzing and Visualizing Data
The code presented here i sused to generate the bulk of the content dispalyed on https://visualizingtheleague.com/. The code often contains some degree of data processing specific to different analytical tasks, but the bulk of the gathering and processing is done via scripts in the previous section.

### analysis_draftCombine_advDef_nnetPredictions.R
This script underlies the post https://visualizingtheleague.com/post/draft-combine-measures-defense-part-three-using-neural-networks-to-predict-blocked-shot-rates/. The script imports the draft combine and defensive advanced stats data generated by *dataAssembly_draftCombine_defAdvStats.R*, and fits a neural network to predict players' block shot rates based on their draft combine measures. Specifically, the code:
  - Partitions the data to allow for model training, validation, and testing;
  - Tunes a best-fitting neural network model using the `nnet` package;
  - Compares model performance to the performance of a hierarchical linear model (neural net, predictably, does a bit better);
  - Displays the full structure of the best performing neural net (using `NeuralNetTools::plotnet()`);
  - Assesses variable importance, using Garson's algorithm (`NeuralNetTools::garson()`) and a set of faceted marginal effects curves (`ggplot2`);
  - Plots predicted vs. observed plot rates for past player-seasons (1000+ minutes), highlighting and labeling over- and under-performers.
  - Generates predictions for incoming rookie class, with comparison to early-season returns.

<p float="right">
  <img src="https://visualizingtheleague.com/post/2018-11-15-draft-combine-measures-defense-part-three-using-neural-networks-to-predict-blocked-shot-rates_files/figure-html/neuralNetDiagram-1.png" title="plotnet diagram" width="190">
  <img src="https://visualizingtheleague.com/post/2018-11-15-draft-combine-measures-defense-part-three-using-neural-networks-to-predict-blocked-shot-rates_files/figure-html/marginalResponse-1.png" title="marginal response curves" width="190">
  <img src="https://visualizingtheleague.com/post/2018-11-15-draft-combine-measures-defense-part-three-using-neural-networks-to-predict-blocked-shot-rates_files/figure-html/projVsObsScatter-1.png" title="predicted vs observed values" width="190">
  <img src="https://visualizingtheleague.com/post/2018-11-15-draft-combine-measures-defense-part-three-using-neural-networks-to-predict-blocked-shot-rates_files/figure-html/rookiePreds-1.png" title="rookie class predictions" width="190">
</p>

### analysis_draftCombine_advDef_linearMod.R
This script corresponds to the post https://visualizingtheleague.com/post/draft-combine-measures-defense-part-two-full-inferential-models/. It imports the draft combine and defensive advanced stats data generated by *dataAssembly_draftCombine_defAdvStats.R*, and fits hierarchical linear models to the data, predicting 5 different defensive/rebounding advanced stats in terms of draft combine measures. The models are grouped on players, accounting for the repeated measures structure of the data (each observation consists of a single player-season, with most players having multiple seasons). Models are fit using the `lme4` package, parameters and fit statistics are extracted 'tidily' using `broom.mixed`, and predictor coefficients are graphed with `ggplot2`.
<p float="right">
  <img src="https://visualizingtheleague.com/post/2018-09-29-draft-combine-measures-defense-part-two-full-inferential-models_files/figure-html/combineHLMcoefsOrb-1.png" title="offensive rebounding rate fixed effect point estimates" width="190">
  <img src="https://visualizingtheleague.com/post/2018-09-29-draft-combine-measures-defense-part-two-full-inferential-models_files/figure-html/combineHLMcoefsDrb-1.png" title="deffensive rebounding rate fixed effect point estimates" width="190">
  <img src="https://visualizingtheleague.com/post/2018-09-29-draft-combine-measures-defense-part-two-full-inferential-models_files/figure-html/combineHLMcoefsBlk-1.png" title="block rate fixed effect point estimates" width="190">
  <img src="https://visualizingtheleague.com/post/2018-09-29-draft-combine-measures-defense-part-two-full-inferential-models_files/figure-html/combineHLMcoefsStl-1.png" title="steal rate fixed effect point estimates" width="190">
</p>

### analysis_draftCombine_advDef_descriptives.R
This script corresponds to the post https://visualizingtheleague.com/post/draft-combine-measures-defense-part-one-basic-relationships/. It imports the draft combine and defensive advanced stats data generated by *dataAssembly_draftCombine_defAdvStats.R*, and generates a variety of descriptive graphics for their relationships. Specifically, it produces a correlation/scatterplot matrix for all combine variables; a detailed look at pairwise relationships among height, wingspan, and standing reach; and polynomial fits between height/wingspan and rebounding/shot blocking/and steals. Packages used for these graphing tasks include `ggplot2`, `ggpmisc`, `GGally`, `ggrepel`, and `cowplot`.
<p float="right">
  <img src="https://visualizingtheleague.com/post/2018-08-22-draft-combine-measures-defense-part-one-basic-relationships_files/figure-html/scatMat-1.png" title="combine scatterplot matrix" width="190">
  <img src="https://visualizingtheleague.com/post/2018-08-22-draft-combine-measures-defense-part-one-basic-relationships_files/figure-html/htWing-1.png" title="height vs wingspan detail" width="190">
  <img src="https://visualizingtheleague.com/post/2018-08-22-draft-combine-measures-defense-part-one-basic-relationships_files/figure-html/blkWng-1.png" title="polynomial fit between blocked shots and wingspan" width="190">
  <img src="https://visualizingtheleague.com/post/2018-08-22-draft-combine-measures-defense-part-one-basic-relationships_files/figure-html/stlHt-1.png" title="polynomial fit between steals and height" width="190">
</p>

### analysis_coachingNetworks.R
This script corresponds to the post https://visualizingtheleague.com/post/coaching-trees-and-coaching-graphs/. It imports the coaching lists generated by 'dataAssembly_coachingStaffs_bballRef.R', processes them into directed networks via the `network` and `sna` packages, and then graphs them via `ggnetwork`, `ggplot`, `gridExtra`, and `cowplot`.
<p float="right">
  <img src="https://visualizingtheleague.com/post/2018-10-15-coaching-trees-and-coaching-graphs_files/figure-html/filteredGraphDegRank-1.png" title="chronologically filtered coaching networks, labeled by degree centrality" width="190">
  <img src="https://visualizingtheleague.com/post/2018-10-15-coaching-trees-and-coaching-graphs_files/figure-html/filteredGraphBtwnss-1.png" title="chronologically filtered coaching networks, labeled by betweenness centrality" width="190">
  <img src="https://visualizingtheleague.com/post/2018-10-15-coaching-trees-and-coaching-graphs_files/figure-html/localNetPopp-1.png" title="coaching graph focused on Gregg Popovich" width="190">
  <img src="https://visualizingtheleague.com/post/2018-10-15-coaching-trees-and-coaching-graphs_files/figure-html/localNetLB-1.png" title="coaching graph focused on Larry Brown" width="190">
</p>

