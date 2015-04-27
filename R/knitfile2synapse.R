#' Allow users to leverage knit3 when constructing Synapse Wiki content
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
  hook_synapseMdSyntax_plot <- function(x, options){
    synPlotMdOpts <- character()
    
    ## SET URL ENCODING STRINGS
    urlEncodings <- c('{' = "%7B",
                      '}' = "%7D",
                      '-' = "%2D",
                      '_' = "%5F",
                      '.' = "%2E",
                      '!' = "%21",
                      '~' = "%7E",
                      '*' = "%2A",
                      '`' = "%60",
                      '\'' = "%27",
                      '(' = "%28",
                      ')' = "%29",
                      '[' = "%5B",
                      ']' = "%5D",
                      ':' = "%3A",
                      ';' = "%3B",
                      '\n' = "%0A",
                      '\r' = "%0D",
                      '/' = "%2F",
                      '?' = "%3F",
                      '&' = "%26",
                      '=' = "%3D",
                      '+' = "%2B",
                      ',' = "%2C",
                      '#' = "%23",
                      '$' = "%24")
    
    ## CHECK FOR ALIGN OPTION BEING SET
    if( any(names(options) == "align") ){
      ## CHECKS FOR ALIGN OPTION
      if( !is.character(options$align) ){
        stop("align must be one of none, left, right, or center")
      }
      if( !(options$align %in% c("none", "left", "right", "center")) ){
        stop("align must be one of none, left, right, or center")
      }
      synPlotMdOpts <- paste(synPlotMdOpts, "&align=", options$align, sep="")
    } else{
      synPlotMdOpts <- paste(synPlotMdOpts, "&align=none", sep="")
    }
    
    ## CHECK FOR SCALE OPTION BEING SET
    if( any(names(options) == "scale") ){
      ## RANGE CHECKS FOR SCALE OPTION
      if( !is.numeric(options$scale) ){
        stop("scale option must be numeric")
      }
      if( options$scale <= 0 | options$scale > 500 ){
        stop("scale option must be greater than 0 and less than 500")
      }
      
      synPlotMdOpts <- paste(synPlotMdOpts, "&scale=", options$scale, sep="")
    } else{
      synPlotMdOpts <- paste(synPlotMdOpts, "&scale=100", sep="")
    }
    
    paste("${image?fileName=", 
          RCurl::curlPercentEncode(basename(paste(x, collapse=".")), codes=urlEncodings), 
          synPlotMdOpts, "}\n", sep="")
  }
  knitr::knit_hooks$set(plot=hook_synapseMdSyntax_plot)
  knitr::opts_chunk$set(tidy=FALSE)
  knitr::opts_chunk$set(error=FALSE)
  
  
  ## CREATE TEMPORARY OUTPTU DIRECTORY FOR MD AND PLOTS
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
  cat(paste("built wiki: '", wikiName, "'\n", sep=""))
  return(w)
}