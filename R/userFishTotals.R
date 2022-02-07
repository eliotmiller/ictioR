#' Summarize user fish totals
#'
#' Provide a report on user by fish totals to date
#'
#' @param db The exported database
#' @param user.id The user.id of the user to summarize
#' 
#' @details Summarize species totals caught per user. Any values with an NA in the
#' weight_kg field will be automatically dropped, as will any values without an
#' associated scientific name. Could possibly lump those in with Fish sp.
#'
#' @return A table of total catch by species per user
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
#' result <- userFishTotals(db=BDER_20220110, user.id="USER2385074")
#' 

userFishTotals <- function(db, user.id)
{
  #subset to the user
  temp <- db[db$user_id==user.id,]
  
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
