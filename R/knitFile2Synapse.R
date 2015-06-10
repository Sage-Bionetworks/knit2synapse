## ALLOW USERS TO LEVERAGE KNITR WHEN UPLOADING FILES CONSTRUCTING SYNAPSE WIKI CONTENT
# THIS IS MODFIED VERSION OF https://gist.github.com/brian-bot/6117476#file-knit2synapse-r FROM BRIAN M. BOT

#######################################
## AUTHOR: Thanneer M Perumal     #####
## ORGANIZATION: Sage Bionetworks #####
#######################################

## PARAMETERS:
##   file: path to a local .Rmd file which to knit
##   parentId: a synapse object which will be the parent of the resulting file entity (usually Project or Folder)
##   entityName (optional): name of the synapse file entity, will default to the file name without the .Rmd extension
##   owner(optional) : a synapse object which will own the resulting WikiPage (usually a Project, Folder, or File), if NULL a new entity with entityName will be created as owner
##   parentWikiId (optional): if the resulting WikiPage is to be a subpage of another WikiPage, this is the id for the parent WikiPage
##   wikiName (optional): a title for the resulting WikiPage - will default to the file name without the .Rmd extension
##   overwrite (optional): default is false, if set true all the previous versions of wiki will be deleted and a new version will be added
##   ... : optional paramters to synStore
#####
## VALUE:
##   wiki: a wikiPage object as defined in the synapseClient
##   owner: a synapse entity as defined in the synapseClient
#####

knitFile2Synapse <- function(file,
                             parentId,
                             entityName=NULL,
                             owner=NULL, 
                             parentWikiId=NULL, 
                             wikiName=NULL,
                             knitmd = TRUE,
                             overwrite=FALSE,
                             ...){
  require(synapseClient)
  require(knitr)
  require(RCurl)
  require(stringr)
  require(tools)
    
  source('./hook_synapseMdSyntax_plot.R')
  
  ## CHECK TO MAKE SURE FILE EXISTS
  file <- path.expand(file)
  if( !file.exists(file) ){
    stop(sprintf("file %s does not exist at this location: %s", basename(file), file))
  }
  
  ## IF NO ENTITY NAME GIVEN, DEFAULT TO FILE NAME WITHOUT EXTENSION
  fName <- basename(file_path_sans_ext(file))
  if( is.null(entityName) ){
    entityName <- fName
  }
  
  ## IF NO WIKI NAME GIVEN, DEFAULT TO FILE NAME WITHOUT EXTENSION
  if( is.null(wikiName) ){
    wikiName <- fName
  }
  
  
    
  #####
  # SET SYNAPSE-SPECIFIC MARKDOWN HOOKS
  #####
  render_markdown()
  
  ## PLOTS  
  knit_hooks$set(plot=hook_synapseMdSyntax_plot)
  opts_chunk$set(tidy=FALSE)
  opts_chunk$set(error=FALSE)
    
  ## CREATE CACHE DIRECTORY  
  if (file.exists(paste(file_path_sans_ext(file),'cache',sep='_'))){ ## IF CACHE DIRECTORY EXISTS THEN DON'T CREATE NEW DIRECTORY
    knitDir <- paste(file_path_sans_ext(file),'cache',sep='_')
  } else {
    # CREATE TEMPORARY OUTPUT DIRECTORY
    knitDir <- tempfile(pattern="knitDir")    
    dir.create(knitDir)
  }
    
  ## CREATE PLOTS DIR
  knitPlotDir <- file.path(knitDir, "plots/")
  if (!file.exists(knitPlotDir))
    dir.create(knitPlotDir)  
  opts_chunk$set(fig.path = knitPlotDir)
  
  ## FILE NAME 
  mdName <- file.path(knitDir, paste(fName, ".md", sep=""))
  
  ## KNIT FILE AND CREATE MARKDOWN
  if ( knitmd ){
    mdFile <- knit(file,
                   envir = parent.frame(n=2),
                   output = mdName)
  } else if ( file.exists(mdName) ){ # if knitmd is false check already markdown exists
    mdFile <- mdName
  } else {
    stop(sprintf("markdown file %s does not exist at this location: %s", basename(mdName), mdName))
  }
  
  ## GET ALL PLOTS AS ATTACHEMENTS
  att <- list.files(knitPlotDir, full.names=TRUE)
    
  ## IF OWNER IS CHARACTER, TRY TO GET FROM SYNAPSE
  if( is.character(owner) & length(owner) == 1 ){
    owner <- synGet(owner, downloadFile=FALSE)
  } else { # create a new synapse entity
    owner <- File(file,parentId = parentId,name=entityName)
    owner <- synStore(owner, ...)
  }
    
  ## CREATE/RETRIVE AND STORE WIKI MARKDOWN TO STNAPSE
  w <- try(synGetWiki(owner),silent=T)
  if (class(w) == 'try-error'){ ## create new wiki if doesn't exisits
    w <- WikiPage(owner=owner, 
                  title=wikiName, 
                  markdown=readChar(mdFile, file.info(mdFile)$size))      
  } else if (overwrite) { ## delete exisitng wiki along with history
    w <- synGetWiki(owner)
    w <- synDelete(w)
    w <- WikiPage(owner=owner, 
                  title=wikiName, 
                  markdown=readChar(mdFile, file.info(mdFile)$size))
  } else {## update exisitng wiki    
    w <- synGetWiki(owner)    
    w@properties$title <- wikiName
    w@properties$markdown <- readChar(mdFile, file.info(mdFile)$size)
  }
  
  ## HAVE ATTACHMENTS
  if (length(att) > 0 ){
    w@attachments <- as.list(att)
  }
  
  ## HAVE PARENT WIKI ID
  if(!is.null(parentWikiId)){
    w@properties$parentWikiId <- parentWikiId
  }
  
  ## STORE TO SYNAPSE 
  w <- synStore(w)
  
  cat(paste("built wiki: '", wikiName, "'\n", sep=""))
  return(list(wiki = w, entity = owner))
}