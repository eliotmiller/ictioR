#' Summarize watershed fish totals
#'
#' Provide a report on watershed by fish totals to date
#'
#' @param db The exported database
#' @param watershed.code The watershed.id of the basin to summarize
#'
#' @details Summarize species totals caught per watershed. Any values with an NA in the
#' weight_kg field will be automatically dropped, as will any values without an
#' associated scientific name. Could possibly lump those in with Fish sp.
#'
#' @return A table of total catch by species per watershed
#'
#' @author Eliot Miller & Cullen Hanks
#'
#' @export
#'
#' @import dplyr
#'
#' @examples
#' #load the data
#' data(BDER_20220110)
#'
#' #note that tip states comes first here!
#' result <- basinFishTotals(db=BDER_20220110, watershed.code="W070408")
#'

basinFishTotals <- function(db, watershed.code)
{
  #subset to the user
  temp <- db[db$watershed_code==watershed.code,]

  #drop any values that have an NA for weight_kg
  temp <- temp[!is.na(temp$weight_kg),]

  #aggregate by fish species
  grouped <- dplyr::group_by(temp, scientific_name)

  #summarize and screw tibbles. sort by weight and drop any species without a name
  result <- as.data.frame(dplyr::summarize(grouped, total.kg=sum(weight_kg)))
  result <- result[result$scientific_name != "",]
  result <- result[order(result$total.kg, decreasing=TRUE),]
  row.names(result) <- NULL
  result
}
