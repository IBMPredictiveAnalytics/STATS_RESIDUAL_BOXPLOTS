# author__ = "Jon K. Peck"
# version__ = "1.0.0"

# History
# 15-Mar-2021 Original Version


gtxt <- function(...) {
    return(gettext(...,domain="STATS_RESIDUAL_BOXPLOTS"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="STATS_RESIDUAL_BOXPLOTS"))
}


### MAIN ROUTINE ###

doplot = function(dv, pred, resid, nbins=NULL, loess=FALSE
    ) {

  setuplocalization("STATS_RESIDUAL_BOXPLOTS")
  
  # A warnings proc name is associated with the regular output
  # (and the same omsid), because warnings/errors may appear in
  # a separate procedure block following the regular output
  
  procname=gtxt("Residual Boxplots")
  warningsprocname = gtxt("Residual Boxplots: Warnings")
  omsid="STATSRESIDUALBOXPLOTS"
  warns = Warn(procname=warningsprocname,omsid=omsid)


  if (!is.null(spssdata.GetSplitVariableNames())) {
      warns$warn(
          gtxt("Split variables are not honored by this procedure"),
          dostop=FALSE)
  }

  alldata = c(pred, resid)
  allargs = as.list(environment())
  dta = spssdata.GetDataFromSPSS(alldata, missingValueToNA=TRUE)
  
  dta = dta[complete.cases(dta),]
  if(is.null(nbins)) {
    nbins = max(min(nrow(dta)/25, 30), 3) # between 3 and 25 bins
  }
  title = sprintf(gtxtf("Residuals Versus Predicted Values.  Dependent = %s", dv))
  q = quantile(dta[,2], probs = seq(0, 1, 1/nbins))
  bins <- cut(dta[,2], unique(q), include.lowest=TRUE)
  b = boxplot(dta[,1] ~ bins, main=title, lwd=2, lty=1, col="slateblue", border="grey75",
              xlab=gtxt("Predicted"), ylab=gtxt("Residual"), outcol="firebrick")
  # colors red, blue, black, blue, red for 5 loess lines
  lwds = c(1,1,2,1,1)
  colors = rgb(red=c(255, 0, 0, 0, 255), green=c(0,0, 0, 0, 0), blue=c(0, 255, 0, 255,0), maxColorValue=255)
  if (loess) (
    slice=1:5
  ) else (
    slice=3:3
  )
  res = sapply(slice, function(i) lines(lowess(1:nbins, b$stats[i,], f=.5), 
                                              col=colors[i], lwd=lwds[i]))
  abline(0,0)

  warns$display()
}


Warn = function(procname, omsid) {
    # constructor (sort of) for message management
    lcl = list(
        procname=procname,
        omsid=omsid,
        msglist = list(),  # accumulate messages
        msgnum = 0
    )
    # This line is the key to this approach
    lcl = list2env(lcl) # makes this list into an environment

    lcl$warn = function(msg=NULL, dostop=FALSE, inproc=FALSE) {
        # Accumulate messages and, if dostop or no message, display all
        # messages and end procedure state
        # If dostop, issue a stop.

        if (!is.null(msg)) { # accumulate message
            assign("msgnum", lcl$msgnum + 1, envir=lcl)
            # There seems to be no way to update an object, only replace it
            m = lcl$msglist
            m[[lcl$msgnum]] = msg
            assign("msglist", m, envir=lcl)
        } 

        if (is.null(msg) || dostop) {
            lcl$display(inproc)  # display messages and end procedure state
            if (dostop) {
                stop(gtxt("End of procedure"), call.=FALSE)  # may result in dangling error text
            }
        }
    }
    
    lcl$display = function(inproc=FALSE) {
        # display any accumulated messages as a warnings table or as prints
        # and end procedure state, if any

        if (lcl$msgnum == 0) {   # nothing to display
            if (inproc) {
                spsspkg.EndProcedure()
            }
        } else {
            if (!inproc) {
                procok =tryCatch({
                    StartProcedure(lcl$procname, lcl$omsid)
                    TRUE
                    },
                    error = function(e) {
                        FALSE
                    }
                )
            }
            if (procok) {  # build and display a Warnings table if we can
                table = spss.BasePivotTable("Warnings ","Warnings") # do not translate this
                rowdim = BasePivotTable.Append(table,Dimension.Place.row, 
                    gtxt("Message Number"), hideName = FALSE,hideLabels = FALSE)

                for (i in 1:lcl$msgnum) {
                    rowcategory = spss.CellText.String(as.character(i))
                    BasePivotTable.SetCategories(table,rowdim,rowcategory)
                    BasePivotTable.SetCellValue(table,rowcategory, 
                        spss.CellText.String(lcl$msglist[[i]]))
                }
                spsspkg.EndProcedure()   # implies display
            } else { # can't produce a table
                for (i in 1:lcl$msgnum) {
                    print(lcl$msglist[[i]])
                }
            }
        }
    }
    return(lcl)
}

# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 
# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}

Run = function(args) {
    #Execute the STATS RESIDUAL BOXPLOTS command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("DEPENDENT", subc="", ktype="existingvarlist",
            var="dv", islist=FALSE),
        spsspkg.Template("PREDVALS", subc="", ktype="existingvarlist", var="pred", islist=FALSE),
        spsspkg.Template("RESIDUALS", subc="", ktype="existingvarlist", var="resid", islist=FALSE),
        spsspkg.Template("NUMBINS", subc="", ktype="int", vallist=list(3, 40), var="nbins"),
        spsspkg.Template("LOESS", subc="", ktype="bool", var="loess")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "doplot")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}
