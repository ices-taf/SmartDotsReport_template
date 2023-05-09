# 2023_smartdotsReport_template

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

To change the configuration and run the project, the script `worksheet.R`
is provided to set up the config file when changeing eventID for example.
