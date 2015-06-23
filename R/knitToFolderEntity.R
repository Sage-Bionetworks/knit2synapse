#' Allow users to leverage knitr when constructing Synapse Wiki content
#' 
#' @export
#' @param file path to a local .Rmd file which to knit and upload
#' @param parentId A synapse Project or Folder entity object which will be the parent of this folder entity 
#' @param entityName User supplied name of the folder entity
#' @param parentWikiId If the resulting WikiPage is to be a subpage of another WikiPage, this is the id for the parent WikiPage (NOTE: owner is still required)
#' @param wikiName A title for the resulting WikiPage - will default to the file name without the .Rmd extension
#' @param overwrite Only if owner specified and parentWikiId is NULL - flag for whether or not to overwrite the previous root WikiPage (if it exists)
#' @param knitmd Flag for whether or not to knit; if false and file already exists, don't knit it again
#' @return a WikiPage object as defined in the synapseClient
knitToFolderEntity <- function(file, parentId, entityName = NULL, parentWikiId=NULL, wikiName=NULL, overwrite=FALSE, knitmd=TRUE, ...) {
  # If entityName is not provided then the basename of the file is used as entity name
  if (is.null(entityName))
    entityName <- basename(tools::file_path_sans_ext(file))
  
  entity <- Folder(parentId=parentId, name=entityName)
  entity <- synStore(entity, ...)
  
  knitfile2synapse(file=file, owner=entity, parentWikiId=parentWikiId, wikiName=wikiName, overwrite=overwrite, knitmd=knitmd)
}
