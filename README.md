# NBA_analytics
The contents of this repository focus on gathering, analyzing, and visualizing NBA data, and form the basis for the work displayed on [VisualizingTheLeague.com](https://visualizingtheleague.com/). This readme describes the composition of those contents, with scripts and their descriptions split into categories of gathering/processing and analyzing/visualizing.

## Gathering and Processing Data
The code described here consists mainly of R scripts that pull and process data from the stats.nba API and/or [basketball-reference.com](https://www.basketball-reference.com/)
* **dataAssembly_historicalRoster.R** Code used to get full historical NBA roster from stats.nba API. Roster consists of name and other identifying information (including begin/end years and current team, but with no stats or other player attributes. Saves to dataframe.
* **dataAssembly_coachingStaffs_bballRef.R** Code used to scrape head and assistant coaching records from basketball-reference.com, saving the results to two lists, for head and assistant records respectively, containing a dataframe with year/team/outcome data for each coach on record.
