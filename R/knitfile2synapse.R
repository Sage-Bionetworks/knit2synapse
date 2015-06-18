#' Allow users to leverage knitr when constructing Synapse Wiki content
#' 
#' @export
#' @param File path to a local .Rmd file which to knit
#' @param owner A Synapse object which will own the resulting WikiPage (usually a Project, Folder, or File)
#' @param parentWikiId If the resulting WikiPage is to be a subpage of another WikiPage, this is the id for the parent WikiPage (NOTE: owner is still required)
#' @param wikiName A title for the resulting WikiPage - will default to the file name without the .Rmd extension
#' @param overwrite Only if owner specified and parentWikiId is NULL - flag for whether or not to overwrite the previous root WikiPage (if it exists)
#' @return a WikiPage object as defined in the synapseClient
knitfile2synapse <- function(file, owner, parentWikiId=NULL, wikiName=NULL, overwrite=FALSE){
  ## CHECK TO MAKE SURE FILE EXISTS
  file <- path.expand(file)
  if( !file.exists(file) ){
    stop(sprintf("file %s does not exist at this location:\n", basename(f), f))
  }
  
  ## IF NO WIKI NAME GIVEN, DEFAULT TO FILE NAME WITHOUT EXTENSION
  fName <- basename(tools::file_path_sans_ext(file))
  if( is.null(wikiName) ){
    wikiName <- fName
  }
  
  ## IF OWNER IS CHARACTER, TRY TO GET FROM SYNAPSE
  if( is.character(owner) & length(owner) == 1 ){
    owner <- synapseClient::synGet(owner, downloadFile=FALSE)
  }
  
  #####
  ## SET SYNAPSE-SPECIFIC MARKDOWN HOOKS
  #####
  knitr::render_markdown()
  
  ## PLOTS
  old_knitr_opts <- knitr::opts_chunk$get()
  knitr::opts_chunk$set(tidy=FALSE, error=FALSE)
  
  old_knitr_hooks <- knitr::knit_hooks$get()
  knitr::knit_hooks$set(plot=hook_synapseMdSyntax_plot)
  
  
  ## CREATE TEMPORARY OUTPUT DIRECTORY FOR MD AND PLOTS
  knitDir <- tempfile(pattern="knitDir")
  dir.create(knitDir)
  knitPlotDir <- file.path(knitDir, "plots/")
  dir.create(knitPlotDir)
  knitr::opts_chunk$set(fig.path = knitPlotDir)
  
  mdName <- file.path(knitDir, paste(fName, ".md", sep=""))
  
  mdFile <- knitr::knit(file,
                        envir=parent.frame(n=2),
                        output=mdName)
  att <- list.files(knitPlotDir, full.names=TRUE)
  
  if( is.null(parentWikiId) ){ ## doesn't have a parentWiki
    if( length(att) > 0 ){ ## has attachments
      w <- synapseClient::WikiPage(owner=owner, 
                                   title=wikiName, 
                                   attachments=as.list(att),
                                   markdown=readChar(mdFile, file.info(mdFile)$size))
    } else{ ## doesn't have attachments
      w <- synapseClient::WikiPage(owner=owner, 
                                   title=wikiName, 
                                   markdown=readChar(mdFile, file.info(mdFile)$size))
    }
    
    if( overwrite ){
      ## TRY TO STORE
      tmp <- try(synapseClient::synStore(w), silent=TRUE)
      if( class(tmp) == "try-error" ){
        tmp <- synapseClient::synGetWiki(owner)
        tmp <- synapseClient::synDelete(tmp)
        w <- synapseClient::synStore(w)
      } else{
        w <- tmp
      }
    } else{
      w <- synapseClient::synStore(w)
    }
    
  } else{ ## has a parentWiki
    if( length(att) > 0 ){ ## has attachments
      w <- synapseClient::WikiPage(owner=owner, 
                                   title=wikiName, 
                                   attachments=as.list(att),
                                   markdown=readChar(mdFile, file.info(mdFile)$size),
                                   parentWikiId=parentWikiId)
    } else{ ## doesn't have attachments
      w <- synapseClient::WikiPage(owner=owner, 
                                   title=wikiName, 
                                   markdown=readChar(mdFile, file.info(mdFile)$size),
                                   parentWikiId=parentWikiId)
    }
    w <- synapseClient::synStore(w)
  }
  
  # Undo changes to options
  knitr::opts_chunk$restore(old_knitr_opts)
  knitr::knit_hooks$restore(old_knitr_hooks)
  
  cat(paste("built wiki: '", wikiName, "'\n", sep=""))
  return(w)
}
