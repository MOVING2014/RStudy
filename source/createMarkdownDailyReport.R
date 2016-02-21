library("knitr")
go.markdown <- function(a="ag1512"){
  rmarkdown::render("report/hedge_trends_monitor_sina.Rmd", output_file="trend_monitor_daily_report.pdf")
}


#go.markdown("SR601")

