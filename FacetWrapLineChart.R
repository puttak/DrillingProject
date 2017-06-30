FacetWrapLineChart  <- function(df, variableName)
{
  #dfm = melt(df, id.vars = c("Depth","DateTime"))
  dfm = df
  dfm = dfm[dfm$variable %in% c(variableName),]
  title <- paste("Line Plots for ", variableName, " across time faceted by Bits")
  p = ggplot(data = dfm, aes_string(x = "DateTime", y = "value")) + geom_line()
  p = p + geom_point()
  p = p + facet_wrap(~ Depth , scales="free") 
  p = p + ggtitle(title)
  p
}