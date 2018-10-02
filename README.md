# 2018_smartdotsReport_template

This template is used to create reports from smartdots events.  The settings for the report are contained in the [config.json](config.json) file.  Currently, the code requires direct access to the smartdots database and so must be run by someone with the appropriate access.  To run the template:

```r
library(icesTAF)
sourceAll()
```

### Markdown reports

There are two MS word (.docx) reports created by this template
1. A full report created from the R markdown file: [report_full.Rmd](report_full.Rmd)
2. A summary report created from the R markdown file: [report_summary.Rmd](report_summary.Rmd)

The names of these reports are set [in the config file](https://github.com/ices-taf/SmartDotsReport_template/blob/639a8a5f75c457ee9e9671746b587f5e12adfcd8/config.json#L8-L9)

