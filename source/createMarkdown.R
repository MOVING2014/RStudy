require("knitr")
go.markdown <- function(a="ag1512"){
  rmarkdown::render(paste0(a,".Rmd"), output_file=paste0(a,"_",Sys.Date(),".html"))
}


#go.markdown("SR601")

