# 2018_smartdotsReport_template

This template is used to create reports from smartdots events.  The 
settings for the report are contained in the [config.json](config.json) 
file, in the bootstrap/initial/data folder.  Currently, the code requires 
direct access to the smartdots database and so must be run by someone 
with the appropriate access.  To run the template:

```r
library(icesTAF)
# download data
taf.bootstrap(taf = TRUE)
# run analysis and create reports
sourceAll()
```
<!-- 
Order to run:

1) bootstrap/smartdots_db.R
2) change_reader_number/merge_database_readers_score_smardots_data.R
3) data.R
4) model.R
5) report.R
-->
