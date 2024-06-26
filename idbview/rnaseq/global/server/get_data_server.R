
get_df <- function(dataid){
  reactive({
    return( readRDS(dataid) )
  })
}

not_get_df <- DT::renderDataTable( {
  df <- data.frame(`中文`="请在 dataset 面版选择、提交数据后再进行此步分析。",
                   English="Please choose and submit data in dataset panel before this analysis."
  ) %>% t()
  colnames(df) <- "The submit data cannot be found."
  return(df)
} )
