#' @import gridExtra
#' @import ggplotify
#' @import grid
#' @import ggplot2
#' @import stats
#' @import gtable
#' @import grDevices
NULL
# https://stackoverflow.com/questions/35750967/r-roxygen2-imported-packages-do-not-appear-in-namespace

#' clickableImageMapDemo
#'
#' @description demo to illustrate how to implement calibrate()
#'	and grid.locator() for a numerical matrix. This is just a stub
#'  to be replaced by the user's actual program.
#'
#' @param n integer number of values to be edited in matrix m
#' @param bounds list of 4 numerical values xmin, xmax, ymin, ymax
#' @param sleepTime numeric number of seconds to sleep to avoid potential race condition
#'
#' @details this package emulates edit() but allows full control
#'	over formatting and management of the edited matrix.
#'	sleepTime parameter can be set to nonzero (suggest trying sleepTime=0.5) in case a complicated \cr
#'	graphic causes a race condition evidenced by incomplete redrawing of the window. Too large a value \cr
#'	might cause a noticeable annoying delay in redrawing the window.
#'
#' @examples
#' if(interactive()){
#' m<-clickableImageMapDemo(2,bounds=list(xmin=.534,xmax=.553,ymin=.057,ymax=.067))
#' }
#'
#' @return returns the updated numerical matrix
#'
#' @export
clickableImageMapDemo<-
  #function(n=3,bounds=list(xmin=.558,xmax=.576,ymin=.058,ymax=.070)) {
	#function(n=3,bounds=list(xmin=.555,xmax=.569,ymin=.070,ymax=.082)) {
	function(n=3,bounds=list(xmin=.534,xmax=.553,ymin=.057,ymax=.067),sleepTime=0.5) {

	  w<-commandArgs()[1]
	  if(w=="RStudio") # calibration and zlocator work only in R Console
	    stop("clickableImageMapDemo must be run in R Console, cannot run in RStudio Console",call. = FALSE)

	  dev.new(width=21.5,height=11)

	  x_bounds<-bounds
	  #save(x_bounds,file=sprintf("%s/%s/%s",getwd(),"data","x_bounds.RData"))

    posIdent<-c(-1,-1)
    posAbort<-c(-2,-2)
    # posIdent defines a special location that is clicked to indicate stopping selections
    # pos<-zlocator(cal.m,TRUE,bounds)
    # if(identical(pos,posIdent))
    #  break

    # definitions of rows for inserting grobs into main gtable
    epsilon<-.001
    rows<-c(.35,.35,.1,.1,.05,.05)
    if(abs(sum(rows)-1)>epsilon)
      stop("sum of rows must equal 1")
    names(rows)<-c("scatterPlotRow","notUsedRow",
      "pullDownRow","ptabRow","annunciatorRow",
      "warningRow")

    x_rows<-rows
    #save(x_rows,file=sprintf("%s/%s/%s",getwd(),"data","x_rows.RData"))

    m<-matrix(round(runif(20),3),nrow=2,ncol=10)
    rownames(m)<-c("row1","row2")
    colnames(m)<-c("col1","col2","col3","col4","col5",
                   "col6","col7","col8","col9","col10")

    x_m<-m
    #save(x_m,file=sprintf("%s/%s/%s",getwd(),"data","x_m.RData"))

    clickCoord<-matrix(nrow=n,ncol=2)
    colnames(clickCoord)<-c("x","y")

    l<-construct_entire_gtab(m,rows,
      "Hello I am your friendly annunciator message\nPlease select a matrix entry or SKIP or ABORT",
      NULL)

    x_l<-l
    #save(x_l,file=sprintf("%s/%s/%s",getwd(),"data","x_l.RData"))

    gtable_show_layout(l$gtab)

    cal.pullDown<-l$cal.pullDown
    x_cal.pullDown<-cal.pullDown
    #save(x_cal.pullDown,file=sprintf("%s/%s/%s",getwd(),"data","x_cal.pullDown.RData"))

    cal.m<-l$cal.m
    x_cal.m<-cal.m
    #save(x_cal.m,file=sprintf("%s/%s/%s",getwd(),"data","x_cal.m.RData"))

    Sys.sleep(sleepTime)
    grid.newpage()
    grid.draw(l$gtab)

    for(i in 1:n) {
      # select an old data matrix value to be replaced
      pos<-zlocator(cal.m,TRUE,bounds)

      if(identical(pos,posIdent)) { # user clicked on "SKIP"
        message<-"you have successfully completed the analysis!!"
        l<-construct_entire_gtab(m,rows,message,NULL)
        Sys.sleep(sleepTime)
        grid.newpage()
        grid.draw(l$gtab)
        return(m)
      }
      # pos[1] is integer x position
      # pos[2] is integer y position
      if(identical(pos,posAbort)) { # user clicked on "ABORT"
        message<-"you have successfully aborted the analysis!!"
        l<-construct_entire_gtab(m,rows,message,NULL)
        Sys.sleep(sleepTime)
        grid.newpage()
        grid.draw(l$gtab)
        stop(message)
      }

      xReal<-pos[1]-1 # subtract 1 to correct for row and col names inside m
      yReal<-nrow(m)-pos[2]+1
      clickCoord[i,]<-c(yReal,xReal)
      replacedVal<-m[yReal,xReal] # capture the value that is being replaced

      # select a new replacement value from the pull down menu
      l<-construct_entire_gtab(m,rows,
          "select a new value from the pull down menu: ",clickCoord)
      m.pullDown<-l$m.pullDown

      Sys.sleep(sleepTime)
      grid.newpage()
      grid.draw(l$gtab)

      pos<-zlocator(cal.pullDown,FALSE,bounds)

      if(identical(pos,posIdent)) { # user clicked on "SKIP"
        message<-"you have successfully completed the analysis!!"
        l<-construct_entire_gtab(m,rows,message,NULL)
        Sys.sleep(sleepTime)
        grid.newpage()
        grid.draw(l$gtab)
        return(m)
      }
      # pos[1] is integer x position
      # pos[2] is integer y position

      xReal.pulldown<-pos[1]
      yReal.pulldown<-nrow(m)-pos[2]+1
      m[yReal,xReal]<-m.pullDown[yReal.pulldown,xReal.pulldown]

      if(i<n)
        message<-"Hello I am your friendly annunciator message\nPlease select a matrix entry or SKIP or ABORT"
      else
        message<-"you have successfully completed the analysis!!"
      l<-construct_entire_gtab(m,rows,message,clickCoord)
      cal.pullDown<-l$cal.pullDown
      cal.m<-l$cal.m

      Sys.sleep(sleepTime)
      grid.newpage()
      grid.draw(l$gtab)
    }
    return(m)
  }

#' zlocator
#'
#' @description wrapper to perform and decode grid.locator()
#'
#' @param cal return value of calibrate()
#' @param rcnames parameter passed to decode()
#' @param bounds parameter passed to exitClick()
#'
#' @details keeps looping until a valid click is detected
#'
#' @examples
#' if(interactive()){
#' load("data/x_cal.m.RData")
#' load("data/x_rcnames.RData")
#' load("data/x_bounds.RData")
#' zlocator(x_cal.m,x_rcnames,x_bounds)
#' }
#'
#' @return returns the return value of decode()
#'
#' @export
zlocator<-
  function(cal,rcnames,bounds) {
    pos<- -1
    while(length(pos)==1) { # keeps looping until a valid click is detected
      z <- grid::grid.locator("npc")
      y <- unlist(sapply(z, function(x) as.numeric(substring(x, 1, nchar(x)))))

      x_y<-y
      #save(x_y,file=sprintf("%s/%s/%s",getwd(),"data","x_y.RData"))

		bounds2<-bounds
		bounds2$xmin<-bounds$xmin+.027
		bounds2$xmax<-bounds$xmax+.036

		# programmers can uncomment next 7 lines to recalibrate SKIP and ABORT coords
		# print("ZLOCATOR::")
		# print("BOUNDS")
		# print(bounds)
		# print("BOUNDS2")
		# print(bounds2)
		# print("MOUSE CLICK:")
		# print(y)

		if(exitClick(bounds2,y))
		  return(c(-2,-2))
			#stop("ABORTED BY USER")

      if(exitClick(bounds,y))
        return(c(-1,-1))

		  x_cal2<-cal
		  #save(x_cal2,file=sprintf("%s/%s/%s",getwd(),"data","x_cal2.RData"))

		  x_rcnames<-rcnames
		  #save(x_rcnames,file=sprintf("%s/%s/%s",getwd(),"data","x_rcnames.RData"))

      pos<-decode(y,cal,rcnames)
    }

    return(pos)
  }

zlocatorSaved<-
  function(cal,rcnames,bounds) {
    pos<- -1
    while(length(pos)==1) { # keeps looping until a valid click is detected
      z <- grid::grid.locator("npc")
      y <- unlist(sapply(z, function(x) as.numeric(substring(x, 1, nchar(x)))))
      #y <- sapply(z, function(x) as.numeric(substring(x, 1, nchar(x))))
      pos<-decode(y,cal,rcnames)
    }
    return(pos)
  }

#' doubleClick
#'
#' @description detect a (left) double click (without moving cursor position)
#'
#' @param tol numeric tolerance for detecting same position
#'
#' @details I realized this is not very useful, as processing is stopped until 2 clicks are detected
#'
#' @examples
#' if(interactive()){
#' doubleClick()
#' }
#'
#' @return returns TRUE if a double click was detected
#'
#' @export
doubleClick<-
  function(tol=0.001) {
    z1<-grid::grid.locator("npc")
    y1 <- unlist(sapply(z1, function(x) as.numeric(substring(x, 1, nchar(x)))))
    z2<-grid::grid.locator("npc")
    y2 <- unlist(sapply(z2, function(x) as.numeric(substring(x, 1, nchar(x)))))

    if(abs(y1["x"]-y2["x"])<=tol & abs(y1["y"]-y2["y"])<=tol)
      return(TRUE)
    return(FALSE)
  }

#' exitClick
#'
#' @description test position of mouse click to see if user wants to exit
#'
#' @param bounds list of numeric xmin xmax ymin ymax defining screen target for exit
#' @param y numeric vector of x and y cursor position
#'
#' @examples
#' if(interactive()){
#' load("data/x_bounds.RData")
#' load("data/x_y.RData")
#' exitClick(x_bounds,x_y)
#'}
#'
#' @details use in conjunction with defineBounds()
#'
#' @return Boolean TRUE if y is within bounds
#'
#' @export
exitClick<-
  function(bounds,y) {
    # uncomment the next 3 lines to determine bounds for a new layout
    #print("exitClick")
    #print(bounds)
    #print(y)

    if((y[1]<=bounds$xmax) & (y[1]>=bounds$xmin) & (y[2]<=bounds$ymax) & (y[2]>=bounds$ymin))
      return(TRUE)
    return(FALSE)
  }

#' defineBounds
#'
#' @description use mouse clicks to define bounding box
#'
#' @return returns a list of numeric xmin xmax ymin ymax defining screen target for exit
#'
#' @details use in conjunction with exitClick()
#'
#' @examples
#' if(interactive()){
#' defineBounds()
#' }
#'
#' @export
defineBounds<-
  function() {
    l<-list()
    tl<-grid::grid.locator("npc")
    topleft <- unlist(sapply(tl, function(x) as.numeric(substring(x, 1, nchar(x)))))
    lr<-grid::grid.locator("npc")
    lowerright <- unlist(sapply(lr, function(x) as.numeric(substring(x, 1, nchar(x)))))

    l$xmax<-lowerright["x"]
    l$xmin<-topleft["x"]
    l$ymax<-topleft["y"]
    l$ymin<-lowerright["y"]

    return(l)
  }

#' construct_entire_gtab
#'
#' @description construct the main gtable into which grobs will be inserted
#'
#' @param m a matrix
#' @param rows numerical vector defining rows for inserting grobs into main gtable
#' @param message character string message to display in annunciator grob of gtable
#' @param clickCoord numerical matrix of 2 columns, each row contains x and y coords
#'  of a mouse click
#'
#' @examples
#' if(interactive()){
#' load("data/x_m.RData")
#' load("data/x_rows.RData")
#' load("data/x_clickCoord.RData")
#' gtab<-construct_entire_gtab(x_m,x_rows,"x_message",x_clickCoord)
#' }
#'
#' @return returns a list whose components are
#'  \itemize{
#'	  \item m.pullDown component m of return value of pullDown()
#'	  \item cal.pullDown return value of calibrate()
#'	  \item cal.m return value of calibrate()
#'	  \item gtab return value of annunciator()
#' }
#'
#' @export
construct_entire_gtab<-
  function(m,rows,message,clickCoord) {
    l<-list()

    gtab<-gtable(unit(1,"npc"),unit(rows,"npc"))

    scatter <- ggplotify::as.grob(expression(plot(runif(10),xlab="",ylab="")))
    scatterPlotRow<-which(names(rows)=="scatterPlotRow")
    gtab<-gtable::gtable_add_grob(gtab,scatter,scatterPlotRow,1,name="scatter")

    x_gtab2<-gtab
    #save(x_gtab2,file=sprintf("%s/%s/%s",getwd(),"data","x_gtab2.RData"))

    pullDownRow<-which(names(rows)=="pullDownRow")
    pd<-pullDown(gtab,pullDownRow,grepl("pull down",message))
    gtab<-pd$gtab
    m.pullDown<-pd$m
    l$m.pullDown<-m.pullDown
    l$cal.pullDown<-calibrate(m.pullDown,rows,pullDownRow)

    # start off with highlighted ptab, to focus user's attention for making a selection
    m<-rbind(colnames(m),m)
    m<-cbind(rownames(m),m)
    ptabRow<-which(names(rows)=="ptabRow")
    tab<-tabify(m,grepl("matrix",message),clickCoord)

    x_tab<-tab
    #save(x_tab,file=sprintf("%s/%s/%s",getwd(),"data","x_tab.RData"))

    gtab<-gtable_replace_grob(gtab,ptabRow,tab,name="ptab")

    x_gtab<-gtab
    #save(x_gtab,file=sprintf("%s/%s/%s",getwd(),"data","x_gtab.RData"))

    l$cal.m<-calibrate(m,rows,ptabRow)

    annunciatorRow<-which(names(rows)=="annunciatorRow")
    gtab<-annunciator(gtab,annunciatorRow,message,"annunciator")

    warningRow<-which(names(rows)=="warningRow")
    l$gtab<-annunciator(gtab,warningRow,"Warning - this app can be addictive","warning")

    return(l)
  }

#' annunciator
#'
#' @description post a message in the annunciator grob of gtab
#'
#' @param gtab return value of gtable_replace_grob()
#' @param row integer the row number of the annunciator grob in gtab
#' @param message character string message to be posted
#' @param name character string value of name field in gtab layout matrix
#'
#' @examples
#' if(interactive()){
#' load("data/x_rows.RData")
#' annunciatorRow<-which(names(x_rows)=="annunciatorRow")
#' load("data/x_gtab.RData")
#' annunciator(x_gtab,annunciatorRow,"message","annunciator")
#' }
#'
#' @return returns the return value of gtable_add_grob()
#'
#' @export
annunciator<-
  function(gtab,row,message,name) {
    colors<-c("blue","orange","red")
    names(colors)<-c("annunciator","warning","error")
    gMessage<-grid.text(message,gp=gpar(col=colors[name]))

    return(gtable::gtable_add_grob(gtab,gMessage,row,1,name=name))
  }

#' calibrate
#'
#' @description use coordinates of upper left and bottom right of matrix
#'	to construct mapping between viewport coordinates and matrix cells
#'
#' @param m matrix
#' @param rows list of row heights in the gtable object
#' @param pullDownRow integer number of the target row in the gtable object
#'
#' @examples
#' if(interactive()){
#' m<-matrix(1:20 * .05,nrow=2,ncol=10)
#' load("data/x_rows.RData")
#' pullDownRow<-which(names(x_rows)=="pullDownRow")
#' load("data/x_m.RData")
#' cal<-calibrate(x_m,x_rows,pullDownRow)
#' }
#'
#' @return returns a list whose components are matrices
#'	representing the upper and lower coordinates of the
#'	row and column cells
#'
#' @export
calibrate<-
  function(m,rows,pullDownRow) {
    nr<-nrow(m)
    nc<-ncol(m)

    v<-vector("numeric")
    lr<-length(rows)

    rrows<-rev(rows)

    v[1]<-0
    for(i in 1:lr)
      v[i+1]<-v[i]+rrows[[i]]
    yLower<-v[lr-pullDownRow+1]
    yUpper<-v[lr-pullDownRow+2]

    delr<-(yUpper-yLower)/nr
    delc<-1/nc

    mr<-matrix(nrow=nr,ncol=2)
    colnames(mr)<-c("lower","upper")

    mc<-matrix(nrow=nc,ncol=2)
    colnames(mc)<-c("lower","upper")

    for(r in 1:nr) {
      mr[r,"lower"]<-yLower+(r-1)*delr
      mr[r,"upper"]<-mr[r,"lower"]+delr
    }

    for(c in 1:nc) {
      mc[c,"lower"]<-(c-1)*delc
      mc[c,"upper"]<-mc[c,"lower"]+delc
    }

    l<-list()
    l$mr<-mr
    l$mc<-mc

    return(l)
  }

#' gtable_replace_grob
#'
#' @description replace an existing grob (in a row of a gtable)
#'  with an updated version
#'
#' @param gtab a gtable object
#' @param row integer target row number within the gtable
#' @param new_grob update grob to insert into gtable
#' @param name character string entry in the "name" field of gtable$layout
#'
#' @examples
#' if(interactive()){
#' load("data/x_gtab.RData")
#' load("data/x_tab.RData")
#' load("data/x_rows.RData")
#' ptabRow<-which(names(x_rows)=="ptabRow")
#' gtab<-gtable_replace_grob(x_gtab,ptabRow,x_tab,name="ptab")
#' }
#' @return returns the updated gtable object
#'
#' @export
gtable_replace_grob<-
  function(gtab,row,new_grob,name) {
    gtab<-gtable::gtable_add_grob(gtab,new_grob,row,1,name=name)

    n<-nrow(gtab[["layout"]])
    if(n==1)
      return(gtab)

    w<-which(gtab[["layout"]][1:(n-1),"name"]==gtab[["layout"]][n,"name"])

    if(length(w)>0) {
      gtab$layout<-gtab$layout[-w,]
      gtab[["grobs"]]<-gtab[["grobs"]][-w]
    }

    return(gtab)
  }

#' pullDown
#'
#' @description generate and insert a matrix, acting as a pull down menu,
#'  into a gtable object
#'
#' @param gtab a gtable object
#' @param row integer target row number within the gtable
#' @param focus Boolean if TRUE add emphasis to matrix cell
#'
#' @examples
#' if(interactive()){
#' load("data/x_gtab.RData")
#' load("data/x_rows.RData")
#' pullDownRow<-which(names(x_rows)=="pullDownRow")
#' message<-"select a new value from the pull down menu: "
#' pd<-pullDown(x_gtab,pullDownRow,grepl("pull down",message))
#' }
#'
#' @return returns a list whose components are the generated matrix
#'  and the gtable object
#' @export
pullDown<-
  function(gtab,row,focus) {
    l<-list()
    m<-matrix(1:20 * .05,nrow=2,ncol=10)
    m1<-sprintf("%.3f",as.numeric(m))
    m2<-matrix(m1,nrow(m),ncol(m))

    l$m<-m
    l$gtab<-gtable_replace_grob(gtab,row,tabify(m2,focus,NULL),name="PullDown")

    return(l)
  }

#' tabify
#'
#' @description adjust the width and height of a matrix to exactly fill the
#'  grob
#'
#' @param m a matrix
#' @param focus Boolean if TRUE add emphasis to matrix cell
#' @param clickCoord param for highlightOneCell()
#'
#' @examples
#' if(interactive()){
#' load("data/x_m.RData")
#' t<-tabify(x_m,FALSE,NULL)
#' }
#'
#' @return returns the grob containing the matrix
#'
#' @export
tabify<-
  function(m,focus=FALSE,clickCoord=NULL) {

    # great overview and relationship of different technologies
    # https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html

    # only example I could find of explicitly controlling size of table cells
    # https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
    # we may edit the cell sizes to align with other content on the page.
    # g <- g2 <- tableGrob(iris[1:4, 1:3], cols = NULL, rows=NULL)
    # g2$widths <- unit(rep(1/ncol(g2), ncol(g2)), "npc")

    mtab<-gridExtra::tableGrob(m,rows=NULL,cols=NULL)
    mtab$widths <- unit(rep(1/ncol(m), ncol(m)), "npc")
    mtab$heights <- unit(rep(1/nrow(m), nrow(m)), "npc")

    x_mtab2<-mtab
    #save(x_mtab2,file=sprintf("%s/%s/%s",getwd(),"data","x_mtab2.RData"))

    if(focus)
      mtab<-highlight(mtab,"blue",15)
    else
      mtab<-unhighlight(mtab)

    x_mtab<-mtab
    #save(x_mtab,file=sprintf("%s/%s/%s",getwd(),"data","x_mtab.RData"))

    if(!is.null(clickCoord)) {

      #x_clickCoord<-clickCoord
      #save(x_clickCoord,file=sprintf("%s/%s/%s",getwd(),"data","x_clickCoord.RData"))

      w<-which(!is.na(clickCoord[,"x"]))
      n<-length(w)
      if(n>0)
        for(i in 1:n)
          mtab<-highlightOneCell(mtab,clickCoord[i,"x"],clickCoord[i,"y"],i==n)
    }
    return(mtab)
  }

#' highlightOneCell
#'
#' @description highlight one cell of grob matrix in gtab
#'
#' @param gtab a gtable object
#' @param row integer row number of cell to highlight
#' @param col integer col number of cell to highlight
#' @param currentPick Boolean TRUE if this is the most recently chosen cell
#'  and we are to apply special highlighting
#'
#' @examples
#' if(interactive()){
#' load("data/x_mtab.RData")
#' load("data/x_clickCoord.RData")
#' highlightOneCell(x_mtab,x_clickCoord[1,"x"],x_clickCoord[1,"y"],FALSE)
#' }
#'
#' @return returns gtab, a gtable object
#'
#' @export
highlightOneCell<-
  function(gtab,row,col,currentPick) {
    # see https://cran.r-project.org/web/packages/gridExtra/
    #   vignettes/tableGrob.html
    # section "Accessing existing grobs in the table"
    l <- gtab$layout

    w<-which(l$t==row+1 & l$l==col+1 & l$name=="core-fg")

    gtab$grobs[w][[1]][["gp"]]$col<-"darkblue"
    gtab$grobs[w][[1]][["gp"]]$fontsize<-15

    if(currentPick) {
      w<-which(l$t==row+1 & l$l==col+1 & l$name=="core-bg")
      gtab$grobs[w][[1]][["gp"]]$fill<-"yellow"
    }

    return(gtab)
    }
#' unhighlight
#'
#' @description invoke highlight() to set font color and size to default
#'
#' @param gtab a gtable object
#'
#' @examples
#' if(interactive()){
#' load("data/x_gtab.RData")
#' unhighlight(x_gtab)
#' }
#'
#' @return returns the return value of highlight()
#'
#' @export
unhighlight<-
  function(gtab) {
    return(highlight(gtab,"black",10))
  }

#' highlight
#'
#' @description invoke highlight() to set highlight font color and size
#'
#' @param gtab a gtable object
#' @param color character string representing a color
#' @param fontsize integer font size
#'
#' @examples
#' if(interactive()){
#' load("data/x_gtab.RData")
#' highlight(x_gtab,"red",16)
#' }
#'
#' @return returns gtab
#'
#' @export
highlight<-
  function(gtab,color,fontsize) {
    # divide by 2 because half of the entries are for rect
    for(i in 1:(length(gtab$grobs)/2)) {
      gtab$grobs[i][[1]][["gp"]]$col<-color
      gtab$grobs[i][[1]][["gp"]]$fontsize<-fontsize
    }
    return(gtab)
  }

#' decode
#
#' @description map the screen coordinates to a cell of a matrix
#'
#' @param y parsed return value of grid.locator()
#' @param cal return value of calibrate()
#' @param rcnames Boolean if TRUE matrix has row names and col names
#'
#' @examples
#' if(interactive()){
#' load("data/x_y.RData")
#' load("data/x_rcnames.RData")
#' load("data/x_cal2.RData")
#' decode(x_y,x_cal2,x_rcnames)
#' }
#'
#' @return returns an integer vector of the index of a cell in a matrix
#'  or returns -1 if rcnames is TRUE and vector y is not within valid range
#'
#' @export
decode<-
  function(y,cal,rcnames) {
    n<-nrow(cal$mr)

    if(rcnames) {
      if(y["x"] < min(cal$mc[-1,]) | y["x"] > max(cal$mc[-1,]) |
         y["y"] < min(cal$mr[-n,]) | y["y"] > max(cal$mr[-n,])) {

        return(-1)
      }
    }
    else {
      if(y["x"] < min(cal$mc) | y["x"] > max(cal$mc) |
         y["y"] < min(cal$mr) | y["y"] > max(cal$mr)) {

        return(-1)
      }
    }

    wx<-which((cal$mc[,"lower"]<=y["x"]) & (cal$mc[,"upper"]>=y["x"]))
    wy<-which(cal$mr[,"lower"]<=y["y"] & cal$mr[,"upper"]>=y["y"])

    return(c(wx,wy))
  }
