# NBA_analytics
Gather, analyze, and display NBA data

## Gather data
Include R scripts to gather and process data from the stats.nba API, as well as by scraping basketball-reference.com
* **dataAssembly_historicalRoster.R** Code used to get full historical NBA roster from stats.nba API. Roster consists of name and other identifying information (including begin/end years and current team, but with no stats or other player attributes. Saves to dataframe.
* **dataAssembly_coachingStaffs_bballRef.R** Code used to scrape head and assistant coaching records from basketball-reference.com, saving the results to two lists, for head and assistant records respectively, containing a dataframe with year/team/outcome data for each coach on record.
