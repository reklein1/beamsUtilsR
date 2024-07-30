## Clear all objects from r environmnet
rm(list=ls());
options(repos = c(CRAN="@CRAN@",cli.num_colors=1))
#options(warn=-1)

# List of packages for session
.packages = c("RJSONIO","htmlTable","broom", "svglite","ggplot2","formatR","gdtools","tidyverse","tsbox","Hmisc","data.table","psych","summarytools","rmarkdown","DataExplorer","jsonlite","DBI","odbc","rio","mongolite","maps","report","utils","quarto","nodbi")

# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst], repos = "http://cran.rstudio.com/")

# Load packages into session
lapply(.packages, require, character.only=TRUE)

# validFunction.gmt function
# called internally within this package to catch errors
validFunct.gmat <- function(usr_Funct, message = 'function', silent = TRUE){
  result =  tryCatch( {usr_Funct}, error = function(e){msg = conditionMessage(e); if(!silent)message(e); invisible((structure(msg, class = 'try-error')));});
  return(result);
};

# S4_to_dataframe.gmat function
# called internally within this package to capture output of an
# s4 object
S4_to_dataframe.gmat <- function(s4obj) {
  lst <- capture.output(str(s4obj))
  for(i in 1:length(list)){
    lst[i]=ifelse(lst[i]=="externalptr",capture.output(lst[i]),lst[i])
  }
  return(lst)
}


#' trycatchReturn.gmat function
#'
#' @description
#' trycatchReturn.gmat is used by beams to capture messages, warnings and
#' errors when submitting return user defined fuctions. Notice that
#' semi-colons are embedded into the string after each function.
#'
#' @param expr
#'
#' @return list of messages, warnings and errors
#' @export
#'
#' @examples
#' return_ERR = return_ERR = trycatchReturn.gmat({"+codeStr+"});
#' info = append(info, return_ERR);
#' rm(list = ls(pattern = '_ERR'));
trycatchReturn.gmat <- function(expr) {
  mess <- warn <- err <- value <- NULL
  value <- withCallingHandlers(
    tryCatch(expr
             , error=function(e) { err <<- e; NULL })
    , warning =function(w) {warn <<- w; invokeRestart("muffleWarning"); }
    , warning =function(w) {warn <<- w; invokeRestart("muffleWarning"); }
    , message =function(m) {mess <<- m; invokeRestart("muffleMessage"); })
  return(list( rtn_message=mess, rtn_warning=warn, rtn_error=err));
};

#' trycatchReturn.gmat function
#'
#' @description
#' trycatchExec.gmat is used by beams to capture messages, warnings and
#' errors when submitting user defined fuctions. Notice that
#' semi-colons are embedded into the string after each function.
#'
#' @param expr
#'
#' @return list of messages, warnings and errors
#' @export
#'
#' @examples
#' trycatchExec.gmat({"+rmEqStr+"});
#' info = return_ERR[2:4];
#'  rm(list = ls(pattern = '_ERR'));
trycatchExec.gmat <- function(expr) {
  mess <- warn <- err <- value <- NULL
  value <- withCallingHandlers(
    tryCatch(expr
             , error=function(e) { err <<- e; NULL })
    , warning =function(w) {warn <<- w; invokeRestart("muffleWarning"); }
    , message =function(m) {mess <<- m; invokeRestart("muffleMessage"); })
  if(typeof(value) == 'S4' || is.data.frame(value)){
    value <- S4_to_dataframe.gmat(value)
  }else if(typeof(value) == 'logical' ||is.numeric(value)){
    value <- as.character(value)
  }
  return(list( typeof = typeof(value),rtn_message=mess, rtn_warning=warn, rtn_error=err));
};

# trycatchCode.gmat <- function(expr) {
#   mess <- warn <- err <- NULL
#   value <- withCallingHandlers(
#     tryCatch(expr
#              , error=function(e) { err <<- e; NULL })
#     , warning =function(w) {warn <<- w; invokeRestart("muffleWarning"); }
#     , message =function(m) {mess <<- m; invokeRestart("muffleMessage"); })
#   return(list( value =value, code_message=mess, code_warning=warn, code_error=err));
# }


# trycatchCode_chrVector.gmat <- function(expr) {
#   mess <- warn <- err <- NULL
#   value <- withCallingHandlers(
#     tryCatch(expr
#              , error=function(e) { err <<- e; NULL })
#     , warning =function(w) {warn <<- w; invokeRestart("muffleWarning"); }
#     , message =function(m) {mess <<- m; invokeRestart("muffleMessage"); })
#   return(list( value = iconv(value, "latin1", "ASCII", sub=""), code_message=mess, code_warning=warn, code_error=err));
# }


#' is.infinite.data.frame function
#'
#' @description
#' Add is infinite method to data frame object to determine if object is infinite.
#'
#' @param obj Object to test
#'
#' @return true or false
#' @export
is.infinite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.infinite(x)))
}

#' is.finite.data.frame function
#'
#' @description
#' Add is infinite method to data frame object to determine if object is finite.
#'
#' @param obj Object to test
#'
#' @return true or false
#' @export
is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(!is.infinite(x)))
}


#' conObject.gmat function
#'
#' @description
#' Get list of available db in r environment
#'
#' @param conList connection to the database server
#'
#' @return list of db connections
#' @export
#'
#' @examples
#' info<-jsonlite::toJSON(conObject.gmat('dbsqlcon'));
conObject.gmat <- function(conList){
  if(is.null(conList)){
    objList1 = "No Connection Established"
  }
  else{
    state = structure(list(opened = logical(), disabled = logical(),selected = logical()), class = "data.frame");
    state = rbind(state,c(FALSE,FALSE,FALSE))
    names(state)=c("opened","disabled","selected")
    objList1 = structure(list(id = character(), parent = character(),text = character(), type = character(), state = data.frame()), class = "data.frame");
    for (z in 1:length(conList)){
      conVal = eval(parse(text=conList[z]))
      level1 = odbcListObjects( conVal );
      conInfo = dbGetInfo(conVal)
      conName = conList[[z]]
      level0 = data.frame(id=paste0(conName),parent="#",text=paste0(conName,"~",conInfo$dbms.name ),type="Connection", state='')
      level0$state = data.frame(state)
      objList1 =  jsonlite::rbind_pages(list(objList1,level0))
      for ( name in level1$name){
        level1a = data.frame(id=paste0(conName,"~",name),parent=conName,text=name,type=level1[level1$name==name,]$type, state='')
        level1a$state = data.frame(state)
        objList1 =  jsonlite::rbind_pages(list(objList1,level1a))
        level2 = odbcListObjects( conVal , catalog = name);
        for ( name2 in level2$name){
          level2a = data.frame(id=paste0(conName,"~",name,"~",name2),parent=paste0(conName,"~",name),text=name2,type=level2[level2$name == name2,]$type, state='')
          level2a$state = data.frame(state)
          objList1 =  jsonlite::rbind_pages(list(objList1,level2a))
          level3 = odbcListObjects( conVal , catalog = name, schema = name2);
          for ( name3 in level3$name){
            if(nrow(level3)>0){
              level3a = data.frame(id=paste0(conName,"~",name,"~",name2,"~",name3),parent=paste0(conName,"~",name,"~",name2),text=name3,type=level3[level3$name == name3,]$type, state='')
              level3a$state = data.frame(state)
              level3a$data = data.frame(connect = conName, table = name3,mGroup = "filetool", mSubGroup = "Database", obj = "DBRead", span = paste0("DBRead ",name3), type = "condata")
              objList1 =  jsonlite::rbind_pages(list(objList1,level3a))
            }
          }
        }
      }
    }
  }
  return(objList1);
}

#' dfObjects.gmat
#'
#' @description
#' Get list of installed packages with dataframes
#'
#' @return list of packages
#' @export
#'
#' @examples
#' info<-jsonlite::toJSON(dfObjects.gmat());
dfObjects.gmat <- function(){

  level0a <- (library())
  level0a <- as.data.frame(level0a$results)
  level0a = dplyr::distinct(level0a, Package, .keep_all = TRUE)
  level0a = dplyr::select(level0a, Package, Title)
  level0a =dplyr::rename(level0a, id = Package)
  level0a$text = level0a$id
  level0a$type = "package"
  level0a$package = level0a$id
  level0a$parent = "#"
  level0b <- as.data.frame(search())
  level0b$`search()` <- sub("package:", "", level0b$`search()`)
  level0b$loaded = TRUE
  names(level0b)=c("id","Loaded")
  level0 = merge(x=level0a, y=level0b, all.x=TRUE)
  state = structure(list(opened = logical(), disabled = logical(),selected = logical()), class = "data.frame");
  state = rbind(state,c(FALSE,FALSE,FALSE))
  names(state)=c("opened","disabled","selected")
  level0$state = list(state)
  level0$Loaded[is.na(level0$Loaded)] = ''
  level0$data = data.frame(title = level0$Title, library = level0$text, mGroup = "Data Templates", mSubGroup = "Data Tools", obj = "loadLibraryTempate", span = paste0("Load ",level0$text), loaded = level0$Loaded, type = "rlibrary" )
  level0$a_attr = data.frame(title = level0$Title,  style=ifelse( level0$Loaded == '','color:black;','color:black;'))
  level0$li_attr = data.frame(class = ifelse( level0$Loaded == '','unload','load'))
  level0$type = ifelse( level0$Loaded == '','package','opnpack')
  level0$text = ifelse(level0$Loaded=='TRUE',paste0(level0$text," (loaded)"),as.character(level0$text))
  level0 = level0[,-c(2,7)]
  level0$type = as.list(level0$type)
  level1a <- as.data.frame(data(package = .packages(all.available = TRUE))$results)
  level1a = dplyr::select(level1a, parent = Package, text = Item, Title)
  level1a$text = gsub("\\s*\\([^\\)]+\\)","",as.character(level1a$text))
  level1a$type = sapply(gsub("\\s*\\([^\\)]+\\)","",as.character(level1a$text)), function(x) as.character( validFunct.gmat(class(eval(parse(text=x))))))
  level1a$package = level1a$parent
  level1a$id = paste0(level1a$parent,"~",level1a$text)
  level1a$state = list(state)
  level1a$a_attr = data.frame(title =  paste0(level1a$Title,"\n(type=",level1a$type,")"))
  level1a$data = data.frame(title =level1a$Title, datasource = level1a$text, mGroup = "Data Templates", mSubGroup = "Data Tools", obj = "loadData", span = paste0("Load ",level1a$text), type = "intdata")
  level1a = level1a[,-c(3)]
  objList1 =  jsonlite::rbind_pages(list(level0,level1a))
  objCnt = dplyr::count(objList1,package)
  objCnt = objCnt[objCnt$n>1,]
  objCnt$package = as.character(objCnt$package)
  objList1 = objList1[as.character(objList1$package) %in% objCnt$package,]
  objList1$type[grep("not found", objList1$type)] = "unknown"
  objList1$type[grep("data.frame", objList1$type)] = "data.frame"
  objList1$type[grep("ts", objList1$type)] = "ts"
  objList1$type[grep("matrix", objList1$type)] = "matrix"
  objList1$data$obj  = paste0('load',objList1$type)
  objList1$data$obj[grep("loadpackage", objList1$data$obj)] = "loadLibraryTempate"
  return(objList1);
}

#

#' InMemList.gmat
#'
#' @description
#' Get list of in memory objects
#'
#' @param exclude list of objects to exclude
#'
#' @return list of objects
#' @export
#'
#' @examples
#' info<-jsonlite::toJSON(InMemList.gmat("warn|err|.gmat|is.infinite.data.frame|is.finite.data.frame|is.infinite.data.table"));
InMemList.gmat <- function(exclude=NULL){
  objs = data.frame(as.character(ls(envir = .GlobalEnv)),stringsAsFactors = FALSE )
  colnames(objs) = c('fieldName')
  objs = objs %>% dplyr::filter(!grepl(exclude, fieldName))
  objs$fieldType = sapply(objs$fieldName, function(x) as.character( class(eval(parse(text=x)))))
  print(objs)
  objs$fieldVal = "N"
  objs$fieldInput = ""
  objs = objs[order(objs$fieldType, objs$fieldName),]
  return(objs);
}


#get in memory objects in list
#InMemList.gmat <- function(exclude=NULL){
#
#     objs = data.frame(as.character(ls(envir = .GlobalEnv)),stringsAsFactors = FALSE )
#     colnames(objs) = c('fieldName')
#     objs$fieldType = sapply(objs$fieldName, function(x) as.character( class(eval(parse(text=x)))))
#   objs$fieldVal = "N"
#   objs$fieldInput = ""
#    objs = objs[order(objs$fieldType, objs$fieldName),]
# objs = objs %>% filter(!grepl(exclude, fieldName))
#    return(objs);
#


#' InMemObjects.gmat
#'
#' @description
#' Get list of in memory objects
#'
#' @return list of objects
#' @export
#'
#' @examples
#' test2<-jsonlite::toJSON(InMemObjects.gmat());
InMemObjects.gmat <- function(){
  state = structure(list(opened = logical(), disabled = logical(),selected = logical()), class = "data.frame");
  state = rbind(state,c(FALSE,FALSE,FALSE))
  names(state)=c("opened","disabled","selected")
  level1a = data.frame(as.character(ls(envir = .GlobalEnv)),stringsAsFactors = FALSE )
  colnames(level1a) = c('text')
  level1a$type = sapply(level1a$text, function(x) as.character( class(eval(parse(text=x)))))
  level1a$type[grep("not found", level1a$type)] = "unknown"
  level1a$type[grep("data.frame", level1a$type)] = "data.frame"
  level1a$type[grep("ts", level1a$type)] = "ts"
  level1a$type[grep("c\\(", level1a$type)] = unlist(level1a$type)[1]
  level1a = dplyr::filter(level1a, type!='function')
  #if(dim(level1a)[1]==0)
  #  return(level1a)
  level1a$parent = '#'
  #print(2)
  level1b = as.data.frame( sapply(ls(envir = .GlobalEnv),function(x){object.size(get(x))}))
  level1b = rownames_to_column(level1b)
  colnames(level1b) = c('text','size')
  #print(3)
  levelc = merge(x=level1a, y=level1b)
  levelc$package = levelc$parent
  levelc$id = paste0(levelc$parent,"~",levelc$text)
  #print(4)
  levelc$state = list(state)
  levelc$a_attr = data.frame(title =  paste0(levelc$text," (type=",levelc$type,", size = ",round(levelc$size/1024, digits=2)," Kb)"))
  levelc$data = data.frame(title = paste0(levelc$text,"(",levelc$type,")") , datasource = levelc$text, mGroup = "Data Templates", mSubGroup = "Data Tools", obj = paste0("load",levelc$type), span = paste0("Load ",levelc$text))
  #print(5)
  levelc = levelc[,-c(4)]

  return(levelc);
}

#' basicStatFunct.gmat
#'
#' @description
#' Return basic statistics for selected data
#'
#' @param sel_data selected user data
#'
#' @return list of objects
#' @export
basicStatFunct.gmat <- function(sel_dat){
  varNames <-data.frame(variable= character(1),type= character(1),stringsAsFactors = FALSE); for( i in names(sel_dat)){varNames = rbind(varNames, c(as.character(i),as.character(paste0(class(sel_dat[[i]]), collapse = '-'))));}
  names(varNames) <- c('variable', 'type');
  inforVar = validFunct.gmat(psych::describe( sel_dat ));
  is.na(inforVar) <- sapply(inforVar, is.infinite)
  if(class(inforVar)!='try-error'){
    inforVar = setDT(inforVar, keep.rownames=TRUE)[];
    colnames(inforVar)[colnames(inforVar)=='rn'] <- 'variable' ;
    varNames <-merge(x=varNames,y=inforVar, all.x = TRUE);
  }
  varNames;
}

##

##conInfoFunct.gmat <- function(sel_dat){
##    datStr <- capture.output(str( sel_dat ));
##     varNames <-data.frame(variable= character(1),type= character(1),stringsAsFactors = FALSE); for( i in names(sel_dat)){varNames = rbind(varNames, c(as.character(i),as.character(paste0(class(sel_dat[[i]]), collapse = ##'-'))));}
##     varNames = varNames[-1,]
##     inforVar = psych::describe( sel_dat ,skew=TRUE, ranges=TRUE);
##	 is.na(inforVar) <- sapply(inforVar, is.infinite)
##	 inforVar <-setDT(inforVar, keep.rownames=TRUE)[];
##	 colnames(inforVar)[colnames(inforVar)=='rn'] <- 'variable';
##     inforVar <-merge(x=varNames,y=inforVar, all.x = TRUE);
##     missVar = data.frame(sapply(sel_dat, function(x) sum(is.na(x))))
##     missVar = setDT(missVar, keep.rownames=TRUE)[];
##     missVar$count = nrow(sel_dat)
##     colnames(missVar) = c('variable','missing','count');
##     inforVar <-merge(x=inforVar,y=missVar, all.x = TRUE);
##	 inforVar;
##}


#' conInfoFunct2.gmat
#'
#' @description
#' Returns obs missing information
#'
#' @param sel_data selected user data
#' @param type data type
#'
#' @return variable name
#' @export
conInfoFunct2.gmat <- function(sel_dat,type=''){
  if(type == 'ts'){
    varNames <-data.frame(variable= character(1),type= character(1),stringsAsFactors = FALSE); for( i in names(ts_df( sel_dat ))){varNames = rbind(varNames, c(as.character(i),as.character(paste0(class(ts_df( sel_dat )[[i]]), collapse = '-'))));}
    varNames = varNames[-1,];

  }
  else if(type == 'list'){
    varNames <-data.frame(variable= character(1),type= character(1),stringsAsFactors = FALSE); for( i in names(as.data.frame( sel_dat ))){varNames = rbind(varNames, c(as.character(i),as.character(paste0(class(as.data.frame( sel_dat )[[i]]), collapse = '-'))));}
    varNames = varNames[-1,];
  }
  else{
    varNames <-data.frame(variable= character(1),type= character(1),stringsAsFactors = FALSE); for( i in names(sel_dat )){varNames = rbind(varNames, c(as.character(i),as.character(paste0(class(sel_dat[[i]]), collapse = '-'))));}
    varNames = varNames[-1,];
  }

  varNames;
}


#' WrapPlot.gmat
#'
#' @description
#' Returns svg object containing the plot type requested
#' which is required to dsiplay graph in beams
#'
#' @param plotfunction function that is to be wrapped
#'
#' @return svg
#' @export
WrapPlot.gmat <- function(plotfunction){
  file_TMP = tempfile("plot", fileext = ".svg")
  SVG_TMP = svg (file_TMP, width=6, height=6)
  gmat_TMP = plotfunction
  dev_TMP = dev.off()
  con_TMP = file( file_TMP )
  svg2_Plot = readLines( con_TMP )
  con2_TMP = close( con_TMP )
  return(svg2_Plot)
}



#' PlotFunct.gmat
#'
#' @description
#' Returns svg object containing the plot type requested
#' which is required to display graph in beams
#'
#' @param plotDat data object to be plotted
#' @param typeplot name of plot type
#'
#' @return svg
#' @export
PlotFunct.gmat <- function(plotDat, typePlot){
  if(length(grep('ERROR_FLG:', plotDat ))>0){
    svg2 <-'Error';
  }else if(typePlot=='plot_bar' & (sum(sapply(plotDat, is.factor))+sum(sapply(plotDat, is.character))==0)){
    svg2 <-'No Discrete Features Found';
  }else if(typePlot=='plot_histogram' & (sum(sapply(plotDat, is.numeric))+sum(sapply(plotDat, is.integer))==0)){
    svg2 <-'No Continous Features Found';
  }else{
    filename <- tempfile('plot', fileext = '.svg');

    if(typePlot=='plot'){
      svg(filename, width=6, height=6);
      par(mfrow=c(2,2));
      plot(plotDat);
      dev.off();
    } else if(typePlot=='ggplot'){
      svg(filename, width=6, height=6);
      print( ggplot2::ggplot(Boston,aes(medv,lstat))+ggplot2::geom_point()+ggplot2::geom_smooth() );
      dev.off();
    } else if(typePlot=='plot_bar'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_bar(plotDat);
      dev.off();
    } else if(typePlot=='plot_boxplot'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_boxplot(plotDat);
      dev.off();
    } else if(typePlot=='plot_correlation'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_correlation(plotDat);
      dev.off();
    } else if(typePlot=='plot_density'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_density(plotDat);
      dev.off();
    } else if(typePlot=='plot_histogram'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_histogram(plotDat);
      dev.off();
    } else if(typePlot=='plot_missing'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_missing(plotDat);
      dev.off();
    } else if(typePlot=='plot_prcomp'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_prcomp(plotDat);
      dev.off();
    } else if(typePlot=='plot_scatterplot'){
      svglite(filename, width=6, height=6 );
      DataExplorer::plot_scatterplot(plotDat);
      dev.off();
    }
    con <-file(filename, open="r");
    svg2 <- readLines(con);
    close(con);
  }
  svg2;
}


#

#' extract_libnews.gmat
#'
#' @description
#' Extract library news into a variable for display
#'
#' @param packName package name
#'
#' @return text
#' @export
extract_libnews.gmat <- function(packName)
{
  dM <- news(package=packName)
  db4 <- capture.output(print(dM, doBrowse = FALSE,
                              browser = getOption("browser")))

}

#' extract_langhelp.gmat
#'
#' @description
#' Extract language help into a variable for display
#'
#' @param pkg package name
#' @param href package location
#' @param lang language default R
#'
#' @return html
#' @export
extract_langhelp.gmat <- function(pkg,href,lang="R")
{
  rLoc <- paste0(R.home(),"/")
  if(href=="Undefined"){
    rawHTML <- paste(readLines(paste0(rLoc,"/doc/html/index.html")), collapse="")
  }else{
    rawHTML <- paste(readLines(paste0(rLoc,"/doc/",href)), collapse="")
  }
  rawHTML <- gsub("<div style=\"text-align: center;\">.*?</div>", "", rawHTML)
  rawHTML <- gsub("src=\"Rlogo.svg\"", "src=\"../icons/r/Rlogo.svg\"", rawHTML)

  rawHTML <- gsub('.html">', paste0('.html" data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event); return false;">'), rawHTML)
  rawStyle <- '<style> body {  font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) !important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}  {overflow-x: hidden;}</style>'
  rawNav <- '<div style="text-align: center;"><a href="../../../doc/html/packages.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/left.jpg" alt="[Up]"></a><a href="../../../doc/html/index.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/up.jpg" alt="[Top]"></a></div>'
  paste0(rawStyle,'<div class="container">',rawHTML,'</div>')
}


#' extract_libhelp.gmat
#'
#' @description
#' Extract library help into a variable for display
#'
#' @param pkg package name
#' @param lang language default R
#'
#' @return html
#' @export
extract_libhelp.gmat <- function(pkg,lang="R")
{
  #print("extract_libhelp.gmat")
  rawStyle <- '<style> body {  font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) !important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}</style>'

  libraryLoc <- paste0(.libPaths(),"/")
  rawHTML<- ""
  for(item in libraryLoc){

    value <- withCallingHandlers(
      tryCatch(readLines(paste0(item,pkg,"/html/00Index.html"))
               , error=function(e) { err <<- e; NULL })
      , warning =function(w) {warn <<- w; invokeRestart("muffleWarning"); }
      , message =function(m) {mess <<- m; invokeRestart("muffleMessage"); })
    if(!is.null(value)){
      rawHTML <- value
      break
    }
  }
  rawHTML <- gsub("<div style=\"text-align: center;\">.*?</div>", "", rawHTML)
  rawHTML <- gsub("../../../doc/html/", "../icons/r/", rawHTML)
  rawHTML <- gsub(".  Use .*? to run them.", "", rawHTML)
  rawHTML <- gsub("DESCRIPTION file</a>.", "DESCRIPTION</a>", rawHTML)
  rawHTML <- gsub('<a href="../DESCRIPTION">',paste0('<a href="../DESCRIPTION"  data-fun=\'\' data-href=\'\' data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
  rawHTML <- gsub('<a href="../demo">',paste0('<a href="../demo"  data-fun=\'\' data-pkg=\'',pkg,'\' data-href=\'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
  rawHTML <- gsub('.html">', paste0('.html"  data-fun=\'href\' data-href=\'\' data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
  rawNav <- '<div style="text-align: center;"><a href="../../../doc/html/packages.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/left.jpg" alt="[Up]"></a><a href="../../../doc/html/index.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/up.jpg" alt="[Top]"></a></div>'
  paste0(rawStyle,'<div class="container">',rawHTML,'</div>')
}

# extract_libhelp.gmat <- function(pkg,lang="R")
# {
# 	print("extract_libhelp.gmat")
# 	rawStyle <- '<style> body {  font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) !important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}</style>'
#
# 	libraryLoc <- paste0(.libPaths(),"/")
# 	rawHTML <- paste(readLines(paste0(libraryLoc,pkg,"/html/00Index.html")), collapse="")
# 	rawHTML <- gsub("<div style=\"text-align: center;\">.*?</div>", "", rawHTML)
# 	rawHTML <- gsub("../../../doc/html/", "../icons/r/", rawHTML)
# 	rawHTML <- gsub(".  Use .*? to run them.", "", rawHTML)
# 	rawHTML <- gsub("DESCRIPTION file</a>.", "DESCRIPTION</a>", rawHTML)
# 	rawHTML <- gsub('<a href="../DESCRIPTION">',paste0('<a href="../DESCRIPTION"  data-fun=\'\' data-href=\'\' data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
# 	rawHTML <- gsub('<a href="../demo">',paste0('<a href="../demo"  data-fun=\'\' data-pkg=\'',pkg,'\' data-href=\'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
# 	rawHTML <- gsub('.html">', paste0('.html"  data-fun=\'href\' data-href=\'\' data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
# 	rawNav <- '<div style="text-align: center;"><a href="../../../doc/html/packages.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/left.jpg" alt="[Up]"></a><a href="../../../doc/html/index.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/up.jpg" alt="[Top]"></a></div>'
# 	paste0(rawStyle,'<div class="container">',rawHTML,'</div>')
# }

#' extract_helpSearch.gmat
#'
#' @description
#' Extract help search results into a variable for display
#'
#' @param keyWord keyWord to be used in search
#'
#' @return html
#' @export
extract_helpSearch.gmat<- function(keyWord){
  if(keyWord=="")
    return


  rawHTML = help.search(keyWord)
  rawHTML <-  as.data.frame(rawHTML$matches)

  rawHTML = tibble::tibble(
    Package = unname(rawHTML[,5]),
    Topic = unname(rawHTML[,3]),
    Title = unname(rawHTML[,2]),
    Type = unname(rawHTML[,7]),
    libPath = unname(rawHTML[,6]),
  )

  rawHTML$libPathVin = sub("../../",R.home(),rawHTML$libPath, ignore.case = TRUE)
  rawHTML$PkgTopic = paste0(rawHTML$Package,"::",rawHTML$Topic)
  rawHTML.Vin =  rawHTML %>% dplyr::filter(Type=="vignette")
  rawHTML.help = rawHTML %>% dplyr::filter(Type=="help")
  rawHTML.demos = rawHTML %>% filter(Type=="demo")

  rawHTML.Vin$libPathVin = paste0(rawHTML.Vin$libPathVin,"/doc/")


  vinFilesFull = list.files(rawHTML.Vin$libPathVin, full.names = TRUE)
  vinFilesFull = sub(paste0(R.home()),"../..",vinFilesFull, ignore.case = TRUE)
  vinFilesName = tools::file_path_sans_ext(basename(vinFilesFull))
  vinFilesExt = tools::file_ext(basename(vinFilesFull))


  vinFiles = unique(data.frame(full = vinFilesFull,Topic = vinFilesName,Ext = vinFilesExt))
  vinFiles = vinFiles %>% pivot_wider(names_from =Ext, values_from = full)
  rawHTML.Vin = merge(rawHTML.Vin,vinFiles,by="Topic",all.x = TRUE)
  rawHTML.Vin = unique(rawHTML.Vin)


  if("html" %in% colnames(rawHTML.Vin)){
    rawHTML.Vin$LinkHTML = ifelse(rawHTML.Vin$html=="NULL","",paste0('<a href="',rawHTML.Vin$html,'" data-fun = "href" data-pkg="',rawHTML.Vin$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">HTML</a>'))
  }else{rawHTML.Vin$LinkHTML =""}
  if("pdf" %in% colnames(rawHTML.Vin)){
    rawHTML.Vin$LinkPDF = ifelse(rawHTML.Vin$pdf=="NULL","", paste0('<a href="',rawHTML.Vin$pdf,'" data-fun = "href" data-pkg="',rawHTML.Vin$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">PDF</a>'))
  }else{rawHTML.Vin$LinkPDF =""}
  if("R" %in% colnames(rawHTML.Vin)){
    rawHTML.Vin$LinkCode = ifelse(rawHTML.Vin$R=="NULL","",paste0('<a href="',rawHTML.Vin$R,'" data-fun = "href" data-pkg="',rawHTML.Vin$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">Code</a>'))
  }else{rawHTML.Vin$LinkCode =""}
  if("Rnw" %in% colnames(rawHTML.Vin)){
    rawHTML.Vin$LinkRnw = ifelse(rawHTML.Vin$Rnw=="NULL","",paste0('<a href="',rawHTML.Vin$Rnw,'" data-fun = "href" data-pkg="',rawHTML.Vin$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">Rnw</a>'))
  }else{rawHTML.Vin$LinkRnw =""}
  if("Rmd" %in% colnames(rawHTML.Vin)){
    rawHTML.Vin$LinkRmd = ifelse(rawHTML.Vin$Rmd=="NULL","",paste0('<a href="',rawHTML.Vin$Rmd,'" data-fun = "href" data-pkg="',rawHTML.Vin$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">Rmd</a>'))
  }else{rawHTML.Vin$LinkRmd =""}

  rawHTML.Vin = rawHTML.Vin %>% select(Package,Topic,PkgTopic,Title,LinkHTML,LinkPDF,LinkCode,LinkRnw,LinkRmd )
  rawHTML.Vin = rawHTML.Vin %>% arrange( tolower(Package), tolower(Topic))
  rawHTML.Vin$Source = ifelse(rawHTML.Vin$LinkRnw=="",rawHTML.Vin$LinkRmd,rawHTML.Vin$LinkRnw)
  rawHTML.Vin$View = ifelse(rawHTML.Vin$LinkHTML=="",rawHTML.Vin$LinkPDF,rawHTML.Vin$LinkHTML)
  rawHTML.Vin = rawHTML.Vin %>% select(PkgTopic,Title,View,LinkCode,Source )
  rawHTML.Vin = rawHTML.Vin %>% addHtmlTableStyle(align="l", css.header= 'border:0px !important;color:#ffffff;', css.table =' width:500px; border-color:#ffffff !important;font-size: 10px;', css.cell = c("width: 100px;border-color:#ffffff !important;padding:2px; word-wrap:break-word;","width: 250px;border-color:#ffffff !important;padding:2px; word-wrap:break-word;","width: 10px;border-color:#ffffff !important;padding:2px;","width: 10px;border-color:#ffffff !important;padding:2px;","width: 10px;border-color:#ffffff !important;padding:2px;")) %>%  htmlTable::htmlTable(rnames = FALSE)

  rawHTML.help$LinkHTML = paste0('<a href="',rawHTML.help$libPath,'/doc/',rawHTML.help$Topic,'.html','" data-fun = "href" data-pkg="',rawHTML.help$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">HTML</a>')
  rawHTML.help$LinkCode = ""
  rawHTML.help$LinkPDF = ""
  rawHTML.help$LinkRnw = ""
  rawHTML.help$LinkRmd = ""
  rawHTML.help = rawHTML.help %>% arrange( tolower(Package), tolower(Topic))
  rawHTML.help$Source = ifelse(rawHTML.help$LinkRnw=="",rawHTML.help$LinkRmd,rawHTML.help$LinkRnw)
  rawHTML.help$View = ifelse(rawHTML.help$LinkHTML=="",rawHTML.help$LinkPDF,rawHTML.help$LinkHTML)
  rawHTML.help = rawHTML.help %>% select(PkgTopic,Title,View,LinkCode,Source )
  rawHTML.help = rawHTML.help %>% addHtmlTableStyle(align="l", css.header= 'border:0px !important;color:#ffffff;', css.table =' width:500px; border-color:#ffffff !important;font-size: 10px;', css.cell = c("width: 100px;border-color:#ffffff !important;padding:2px; word-wrap:break-word;","width: 250px;border-color:#ffffff !important;padding:2px; word-wrap:break-word;","width: 10px;border-color:#ffffff !important;padding:2px;","width: 10px;border-color:#ffffff !important;padding:2px;","width: 10px;border-color:#ffffff !important;padding:2px;")) %>%  htmlTable::htmlTable(rnames = FALSE)

  rawHTML.demos$libPath = sub(paste0(R.home()),"../..",rawHTML.demos$libPath, ignore.case = TRUE)
  rawHTML.demos$LinkHTML = ""
  rawHTML.demos$LinkCode = paste0('<a href="',rawHTML.demos$libPath,'/demo/',rawHTML.demos$Topic,'.R','" data-fun = "href" data-pkg="',rawHTML.demos$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">Code</a>')
  rawHTML.demos$LinkPDF = ""
  rawHTML.demos$LinkRnw = ""
  rawHTML.demos$LinkRmd = ""
  rawHTML.demos = rawHTML.demos %>% arrange( tolower(Package), tolower(Topic))
  rawHTML.demos$Source = ifelse(rawHTML.demos$LinkRnw=="",rawHTML.demos$LinkRmd,rawHTML.demos$LinkRnw)
  rawHTML.demos$View = ifelse(rawHTML.demos$LinkHTML=="",rawHTML.demos$LinkPDF,rawHTML.demos$LinkHTML)
  rawHTML.demos = rawHTML.demos %>% select(PkgTopic,Title,View,LinkCode,Source )
  rawHTML.demos = rawHTML.demos %>% addHtmlTableStyle(align="l", css.header= 'border:0px !important;color:#ffffff;', css.table =' width:500px; border-color:#ffffff !important;font-size: 10px;', css.cell = c("width: 100px;border-color:#ffffff !important;padding:2px; word-wrap:break-word;","width: 250px;border-color:#ffffff !important;padding:2px; word-wrap:break-word;","width: 10px;border-color:#ffffff !important;padding:2px;","width: 10px;border-color:#ffffff !important;padding:2px;","width: 10px;border-color:#ffffff !important;padding:2px;")) %>%  htmlTable::htmlTable(rnames = FALSE)


  rawHTML = rbind("<h3>Vignettes</h3>",rawHTML.Vin,"<h3>Demos</h3>",rawHTML.demos,"<h3>Help</h3>",rawHTML.help)
  rawStyle <- '<style> body {  font-size:10px; font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) !important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}</style>'
  caption = paste0("<h1>Search results...<img class=\"toplogo\" src=\"../icons/r/Rlogo.svg\" alt=\"[R logo]\"></h1><hr><br>")
  paste0(rawStyle,caption,paste0(rawHTML, collapse=""))


}


#' extract_searchpage.gmat
#'
#' @description
#' Extract language search page help into a variable for display
#'
#' @param pkg package name
#' @param href package location
#' @param lang language default R
#'
#' @return html
#' @export
extract_searchpage.gmat <- function(pkg,href,lang='R')
{
  href=sub("../..",R.home(),href, ignore.case = TRUE)
  checkFile = file.exists(href)
  if(checkFile){
    if(grepl(".R",href, fixed=TRUE)){
      rawHTML <- paste(readLines(href), collapse="<br>")
    }else if(grepl(".pdf",href, fixed=TRUE)){
      #system(paste0('open ','"',href,'"'))
      browseURL(href)
      return("PDF openned in a new browser tab...")
    }else{
      rawHTML <- paste(readLines(href), collapse="")
    }
  }else{
    href = substr(href,max(unlist(lapply(strsplit(href, ''), function(x) which(x == '/'))))+1,nchar(href))
    href=gsub(".html","",href)
    rdbfile <- file.path(find.package(pkg), "help", pkg)
    rdb <- tools:::fetchRdDB(rdbfile, key = href)
    convertor <- switch("html",
                        txt   = tools::Rd2txt,
                        html  = tools::Rd2HTML,
                        latex = tools::Rd2latex,
                        ex    = tools::Rd2ex
    )
    f <- function(x) capture.output(convertor(x))
    if(is.null(href)) rawHTML <-lapply(rdb, f) else rawHTML <-f(rdb)
  }

  rawHTML = gsub('<a href="http','<a target="_blank" href="http', rawHTML)
  rawStyle <- '<style> body {  font-size:10px; font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) 		!important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}</style>'
  #paste0(rawStyle,'<base target="_blank" />','<div class="container">',rawHTML,'</div>')
  paste0(rawStyle,'<div class="container">',rawHTML,'</div>')
}


#' extract_searchpage.gmat
#'
#' @description
#' Extract language search page help into a variable for display
#'
#' @param pkg package name
#' @param href package location
#' @param lang language default R
#'
#' @return text
#' @export
extract_libpage.gmat <- function(pkg,href,lang='R')
{
  print(paste0("extract_libpage.gmat",pkg,href,lang))
  if(grepl("../demo",href, fixed=TRUE)){
    demoPack = demo(package = "stats")
    demoPackTable = as.data.frame(demoPack$results)
    demoPackTable$packDemo= paste0(demoPackTable$Package,"::",demoPackTable$Item)
    demoPackTable$codeDemo= paste0(demoPackTable$Item,".R")
    #demoPackTable$codeLinkDemo=paste0("<a href=\"../../library",demoPackTable$codeDemo,"\" onclick=\"return window.parent.demoCodeLink(event)\">","View","</a>")
    demoPackTable$codeLinkDemo=paste0('<a href="../../library/',demoPackTable$Package,'/demo/',demoPackTable$codeDemo,'" data-fun = "href" data-pkg="',demoPackTable$Package,'" data-lang="R" data-href="search.html" onclick="return window.parent.helpLink(event);">Code</a>')

    demoPackTable$modelLinkDemo=paste0("<a href=\"",demoPackTable$codeDemo,"\" onclick=\"return window.parent.demoModelLink(event)\">","Model","</a>")
    pageHeader = paste0("<h1>'",pkg,"' package description<img class=\"toplogo\" src=\"../icons/r/Rlogo.svg\" alt=\"[R logo]\"></h1><hr>")
    demoPage = demoPackTable[,c(5,4,7,8)]
    colnames(demoPage) = c("Name","Description","View Code","Model Code")
    rownames(demoPage) = NULL
    demoPage = demoPage %>% addHtmlTableStyle(align="l", css.cell = 'border-color:#ffffff !important;padding:2px;', css.header= 'border:0px !important;color:#ffffff;', css.table ='border-color:#ffffff !important' ) %>%  htmlTable::htmlTable(rnames = FALSE)
    rawHTML = paste0(pageHeader,"",demoPage)
  }else if(grepl("../DESCRIPTION",href, fixed=TRUE)){
    print(paste0("in description"))
    href = gsub('../',paste0(R.home(),"/library/",pkg,"/"), href)
    rawHTML <- paste(readLines(href), collapse="<br>")
    pageHeader = paste0("<h1>'",pkg,"' package description<img class=\"toplogo\" src=\"../icons/r/Rlogo.svg\" alt=\"[R logo]\"></h1><hr>")
    rawHTML = paste0(pageHeader,rawHTML)
  }else if(grepl("00Index.html",href, fixed=TRUE)){
    href = sub('../../',paste0(R.home(),"/"), href)
    rawHTML <- paste(readLines(href), collapse="")
  }else if(grepl("../../library",href, fixed=TRUE)){
    href = substr(href,max(unlist(lapply(strsplit(href, ''), function(x) which(x == '/'))))+1,nchar(href))
    fileLoc = list.files(R.home(),pattern = paste0("^",href,"$"), recursive = TRUE,full.names=TRUE)
    print(fileLoc)
    if(identical(fileLoc,character(0)) | min(grepl(paste0("library/",pkg),fileLoc, fixed=TRUE) )!=0){
      href=gsub(".html","",href)
      rdbfile <- file.path(find.package(pkg), "help", pkg)
      rdb <- tools:::fetchRdDB(rdbfile, key = href)
      convertor <- switch("html",
                          txt   = tools::Rd2txt,
                          html  = tools::Rd2HTML,
                          latex = tools::Rd2latex,
                          ex    = tools::Rd2ex
      )
      f <- function(x) capture.output(convertor(x))
      if(is.null(href)) rawHTML <-lapply(rdb, f) else rawHTML <-f(rdb)
      return(rawHTML)
    }else if(length(fileLoc)>1){
      fileIndex = grep(paste0("library/",pkg),fileLoc)
      if(length(fileIndex)>1)
        fileIndex = min(fileIndex)
      rawHTML <- paste(readLines(fileLoc[fileIndex]), collapse="")
    }else{
      rawHTML <- paste(readLines(fileLoc), collapse="")
    }
  }else if(grepl("../../",href, fixed=TRUE)){
    href = sub('../../',paste0(R.home(),"/"), href)
    rawHTML <- paste(readLines(href), collapse="")
  }else if(grepl("../",href, fixed=TRUE)){
    rootDir = paste0(R.home(),"/doc/")
    href = sub('../',rootDir, href)
    print(paste0("extract_libpage.gmat",href))
    rawHTML <- paste(readLines(href), collapse="")
  }else if(href=="packages.html"){
    rawHTML = installed.packages(fields=c('Package','Title'))
    rawHTML = tibble::tibble(
      Package = names(rawHTML[,3]),
      Title = unname(rawHTML[,17])
    )

    rawHTML$Link = paste0('<a href="../../library/',rawHTML$Package,'/html/00Index.html" data-fun = "href" data-pkg="',rawHTML$Package,'" data-lang="R" data-href="packages.html" onclick="return window.parent.helpLink(event);">',rawHTML$Package,'</a>')
    rawHTML$Char = toupper(substr(rawHTML$Package, 1, 1))
    listChar = as.data.frame(strsplit(intToUtf8(c(65:90)),""))
    colnames(listChar) = "Char"
    rownames(listChar) <- NULL

    listChar$Link = paste0('<b><a id="',paste0('pkgs-',listChar$Char),'">',paste0('---',listChar$Char,'---'),'</a></b>')
    listChar$Title = ""
    listChar$Package = ""

    rawHTML = rbind(rawHTML,listChar)

    rawHTML = rawHTML %>% arrange(Char,Package)
    rawHTML = data.frame(rawHTML[,c(3,2)])
    rownames(rawHTML) <- NULL

    rawHTML = rawHTML %>% addHtmlTableStyle(align="l", css.cell = 'border-color:#ffffff !important;padding:5px;',css.header= 'border:0px !important;color:#ffffff;', css.table ='border-color:#ffffff !important' ) %>% htmlTable::htmlTable(rnames = FALSE)
    htmlIndex = '<p align=\"center\"><a href=\"#pkgs-A\">A</a><a href=\"#pkgs-B\">B</a><a href=\"#pkgs-C\">C</a><a href=\"#pkgs-D\">D</a><a href=\"#pkgs-E\">E</a><a href=\"#pkgs-F\">F</a><a href=\"#pkgs-G\">G</a><a href=\"#pkgs-H\">H</a><a href=\"#pkgs-I\">I</a><a href=\"#pkgs-J\">J</a><a href=\"#pkgs-K\">K</a><a href=\"#pkgs-L\">L</a><a href=\"#pkgs-M\">M</a><a href=\"#pkgs-N\">N</a><a href=\"#pkgs-O\">O</a><a href=\"#pkgs-P\">P</a><a href=\"#pkgs-Q\">Q</a><a href=\"#pkgs-R\">R</a><a href=\"#pkgs-S\">S</a><a href=\"#pkgs-T\">T</a><a href=\"#pkgs-U\">U</a><a href=\"#pkgs-V\">V</a><a href=\"#pkgs-W\">W</a><a href=\"#pkgs-X\">X</a><a href=\"#pkgs-Y\">Y</a><a href=\"#pkgs-Z\">Z</a></p>'
    caption = paste0("<h1>Installed Packages<img class=\"toplogo\" src=\"../icons/r/Rlogo.svg\" alt=\"[R logo]\"></h1><hr><br>")
    rawHTML = paste0(caption,htmlIndex,rawHTML)


  } else{
    if(grepl("/",href, fixed=TRUE)){
      href = substr(href,max(unlist(lapply(strsplit(href, ''), function(x) which(x == '/'))))+1,nchar(href))
    }
    fileLoc = list.files(R.home(),pattern = paste0("^",href,"$"), recursive = TRUE,full.names=TRUE)
    if(identical(fileLoc,character(0))){
      href=gsub(".html","",href)
      rdbfile <- file.path(find.package(pkg), "help", pkg)
      rdb <- tools:::fetchRdDB(rdbfile, key = href)
      convertor <- switch("html",
                          txt   = tools::Rd2txt,
                          html  = tools::Rd2HTML,
                          latex = tools::Rd2latex,
                          ex    = tools::Rd2ex
      )
      f <- function(x) capture.output(convertor(x))
      if(is.null(href)) rawHTML <-lapply(rdb, f) else rawHTML <-f(rdb)
      rawHTML = gsub('<a href="http','<a target="_blank" href="http', rawHTML)
      return(rawHTML)
    }else if(length(fileLoc)>1){
      fileIndex = grep("/html/",fileLoc)
      rawHTML <- paste(readLines(fileLoc[fileIndex]), collapse="")
    }else{
      rawHTML <- paste(readLines(fileLoc), collapse="")

    }
  }
  rawHTML <- gsub("src=\"Rlogo.svg\"", "src=\"../icons/r/Rlogo.svg\"", rawHTML)
  rawHTML <- gsub("src=\"up.jpg\"", "src=\"../icons/r/up.jpg\"", rawHTML)
  rawHTML <- gsub(".  Use .*? to run them.", "", rawHTML)
  rawHTML <- gsub('<a href="../demo">',paste0('<a href="../demo"  data-fun=\'\' data-pkg=\'',pkg,'\' data-href=\'\' data-lang=\'',lang,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)

  rawHTML <- gsub("<div style=\"text-align: center;\">.*?</div>", "", rawHTML)
  rawHTML <- gsub("../../../doc/html/", "../icons/r/", rawHTML)

  rawHTML <- gsub('.html">', paste0('.html"  data-fun=\'href\' data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' data-href=\'',href,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
  rawHTML <- gsub('<a href="../DESCRIPTION">',paste0('.<a href="../DESCRIPTION"  data-fun=\'\'  data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' data-href=\'',href,'\' onclick = "return window.parent.helpLink(event);">'), rawHTML)
  rawHTML <- gsub('DESCRIPTION file','DESCRIPTION', rawHTML)
  rawHTML <- gsub('<html xmlns="http://www.w3.org/1999/.html" data-pkg="stats" data-lang="R" data-href="" data-fun="" onclick="return window.parent.helpLink(event);">', '<html xmlns="http://www.w3.org/1999/.html">', rawHTML)
  rawHTML = gsub('<a href="http','<a target="_blank" href="http', rawHTML)

  rawStyle <- '<style> body {  font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) !important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}</style>'
  rawNav <- '<div style="text-align: center;"><a href="../../../doc/html/packages.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/left.jpg" alt="[Up]"></a><a href="../../../doc/html/index.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/up.jpg" alt="[Top]"></a></div>'
  paste0(rawStyle,'<div class="container">',rawHTML,'</div>')
}


#' extract_help.gmat
#'
#' @description
#' Extract help page
#'
#' @param pkg package name
#' @param fn function
#' @param to return format
#' @param lang language default R
#'
#' @return text, html, latex, ex
#' @export
extract_help.gmat <- function(pkg, fn = NULL, to = c("txt", "html", "latex", "ex"), lang="R")
{
  print("extract_help.gmat")
  to <- match.arg(to)
  rdbfile <- file.path(find.package(pkg), "help", pkg)
  rdb <- tools:::fetchRdDB(rdbfile, key = fn)
  convertor <- switch(to,
                      txt   = tools::Rd2txt,
                      html  = tools::Rd2HTML,
                      latex = tools::Rd2latex,
                      ex    = tools::Rd2ex
  )
  f <- function(x) capture.output(convertor(x))
  if(is.null(fn)) rawHTML <-lapply(rdb, f) else rawHTML <-f(rdb)
  #rawHTML <- toString(rawHTML)

  rawHTML <- gsub('window.parent.helpNavigate(this.href)', paste0(' data-fun=\'\' data-pkg=\'',pkg,'\' data-lang=\'',lang,'\' return helpLink(event)'),rawHTML)
  rawStyle <- '<style> body { font-family: monospace;  background: white;    color: black;} a:link {    background: white;    color: blue !important; margin-right: 5px;} a:visited {    background: white;    color: rgb(50%, 0%, 50%) !important;} h1 {    background: white;    color: rgb(55%, 55%, 55%) !important;    font-family: monospace;    font-size: x-large;    text-align: center;} h2 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;    text-align: center;} h3 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-size: large;} h4 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;    font-size: large;} h5 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;} h6 {    background: white;    color: rgb(40%, 40%, 40%) !important;    font-family: monospace;    font-style: italic;}		 img.toplogo {    width: 4em;    vertical-align: middle;} img.arrow {    width: 30px;    height: 30px;    border: 0;} span.acronym {    font-size: small;} span.env {    font-family: monospace;} span.file {    font-family: monospace;} sp0an.option{    font-family: monospace;} span.pkg {    font-weight: bold;} span.samp{    font-family: monospace;} div.vignettes a:hover {    background: rgb(85%, 85%, 85%) !important;}  ul{margin-left:2px;}  li{margin-left:2px;} .toplogo{width:32px ! important;height:32px; margin-left: 10px;}</style>'
  rawNav <- '<div style="text-align: center;"><a href="../../../doc/html/packages.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/left.jpg" alt="[Up]"></a><a href="../../../doc/html/index.html" onclick="return helpLink(event); return false"><img class="arrow" src="../icons/r/up.jpg" alt="[Top]"></a></div>'

  paste0(rawStyle,rawHTML)

}

# Duplicate - Add is infinite method to data table object
#is.infinite.data.frame <- function(obj){
# sapply(obj,FUN = function(x) all(is.infinite(x)))
#}


#' is.infinite.data.table
#'
#' @description
#' Add is.infinite method to data table object
#'
#' @param selected object
#'
#' @return true or false
#' @export
is.infinite.data.table <- function(obj){
  sapply(obj,FUN = function(x) all(is.infinite(x)))

}

##########################################################################
#
# Creating List of Aavailable Functions
#
##########################################################################
#get function info
#funcInfo <- function(packTarg, targVal){
#
#  state = structure(list(opened = logical(), disabled = logical(),selected = logical()), class = "data.frame");
#  state = rbind(state,c(FALSE,FALSE,FALSE))
#  names(state)=c("opened","disabled","selected")
#  myhelp <- getHelpList.gmat(targVal, as.character(packTarg));
#  levelc= data.frame( id=paste0(packTarg,"(",myhelp$alias,")"), text = myhelp$alias, type= 'function',parent = packTarg )
#  levelc$state = list(state)
#  levelc$a_attr = data.frame(title =  paste0(levelc$text," (type=",levelc$type,")"),  style='color:black;',stringsAsFactors = FALSE)
#  levelc$data = data.frame(title = myhelp$title , datasource = levelc$text, mGroup = "Data Templates", mSubGroup = "Data Tools", obj = paste0("load ",levelc$text), span = myhelp$title,stringsAsFactors = FALSE)
#  return(levelc)
#}


#' fnObjects.gmat
#'
#' @description
#' get Installed packages with dataframes
#'
#' @return list of packages that export data frames
#' @export
fnObjects.gmat <- function(){
  allPacks = sapply(.packages(all.available = TRUE), function(obj) packageDescription(obj))
  Title = as.character(unlist(sapply(allPacks, "[", "Title")))
  id = as.character(unlist(sapply(allPacks, "[", "Package")))
  level0a = data.frame(id,Title);
  level0a$text = level0a$id
  level0a$type = "package"
  level0a$package = level0a$id
  level0a$parent = "#"
  level0b <- as.data.frame(search())
  level0b$`search()` <- sub("package:", "", level0b$`search()`)
  level0b$loaded = TRUE
  names(level0b)=c("id","Loaded")
  level0 = merge(x=level0a, y=level0b, all.x=TRUE)
  state = structure(list(opened = logical(), disabled = logical(),selected = logical()), class = "data.frame");
  state = rbind(state,c(FALSE,FALSE,FALSE))
  names(state)=c("opened","disabled","selected")
  level0$state = list(state)
  level0$Loaded[is.na(level0$Loaded)] = ''
  level0$data = data.frame(title = level0$Title, library = level0$text, mGroup = "Data Templates", mSubGroup = "Data Tools", obj = "loadLibraryTempate", span = paste0("Load ",level0$text), loaded = level0$Loaded )
  level0$a_attr = data.frame(title = level0$Title,  style=ifelse( level0$Loaded == '','color:black;','color:black;'))
  level0$li_attr = data.frame(class = ifelse( level0$Loaded == '','unload','load'))
  level0$type = ifelse( level0$Loaded == '','package','opnpack')
  level0$text = ifelse(level0$Loaded=='TRUE',paste0(level0$text," (loaded)"),as.character(level0$text))
  level0 = level0[,-c(2,7)]

  loadedPacks = level0[level0$li_attr=='load',]

  for (i in 1:nrow(loadedPacks)){
    subTargets = as.data.frame(unlist(sapply(paste0("package:",loadedPacks[i,4]),lsf.str)),stringsAsFactors = FALSE)
    if(nrow(subTargets)>0){
      names(subTargets)[1]= "text"
      subTargets$parent = loadedPacks[i,4]
      subTargets$package = loadedPacks[i,4]
      subTargets$type = 'function'
      subTargets$id=paste0(subTargets$parent,"(",subTargets$text,")")
      subTargets = dplyr::select(subTargets, id, text, type,package, parent)
      subTargets$state = list(state)
      subD = structure(list(title = character(), mGroup = character(), mSubGroup = character(),  obj = character(), span = character(), library = character(), type = character(), id = character()), class = "data.frame");
      subA = structure(list(title = character(), style = character()), class = "data.frame");
      for(z in 1:nrow(subTargets)){
        myhelp <- getHelpList.gmat(as.character(subTargets[z,2]), as.character(subTargets[z,4]));
        subD[z,1] =  myhelp$title
        subD[z,2]  = subTargets[z,3]
        subD[z,3] = 'Functions'
        subD[z,4] = "emptyTool"
        subD[z,5] = paste0(subTargets[z,2])
        subD[z,6] = as.character(loadedPacks[i,4])
        subD[z,7] = 'function'
        subD[z,8] = paste0(subTargets[z,5],'(',subTargets[z,2],')')
        subA[z,1] = paste0( sub("\n","",iconv(myhelp$description, "latin1", "ASCII", sub="")) ,"(type= ",subTargets[z,3],")")
        subA[z,2]  ='color:black;'
      }

      subTargets$data  =  subD
      subTargets$a_attr  =  subA
      level0 =  jsonlite::rbind_pages(list(level0,subTargets))
    }
  }
  return(level0[!is.na(level0$id),]);
}
##########################################################################
#
# Creating Functions
#
##########################################################################

#' Rd2list.gmat
#'
#' @description
#' convert RD object to list
#'
#' @return list
#' @export
Rd2list.gmat <- function(Rd){
  names(Rd) <- substring(sapply(Rd, attr, "Rd_tag"),2);
  temp_args <- Rd$arguments;

  Rd$arguments <- NULL;
  myrd <- lapply(Rd, unlist);
  myrd <- lapply(myrd, paste, collapse="");

  temp_args <- temp_args[sapply(temp_args , attr, "Rd_tag") == "\\item"];
  temp_args <- lapply(temp_args, lapply, paste, collapse="");
  temp_args <- lapply(temp_args, "names<-", c("arg", "description"));
  myrd$arguments <- temp_args;
  return(myrd);
}

# getHelpList.gmat <- function(...){
#   thefile <- help(...)
#   if(is.na(thefile[1:1]))
#   {
#     return(list("title" = 'not found', "name" = 'not found', "alias" = 'not found', "description" = 'not found', "usage" = 'not found', "value" = 'not found', "section" = 'not found',"examples" = 'not found', "arguments" = 'not found'))
#   }
#   myrd <- utils:::.getHelpFile(thefile);
#   Rd2list.gmat(myrd);
# }

# clean help description
# internal function
cleanDesc.gmat <- function(desc){
  desc = gsub("list\\(\"","", desc)
  desc = gsub("\"\\)","", desc)
  desc = gsub("\"","", desc)
  desc = gsub("\\\\","", desc)
  desc = gsub("  "," ", desc)
  #desc = gsub("\n "," ", desc)
  desc = gsub("\n"," ", desc)
  desc = gsub("'","", desc)
  desc = gsub(",","&comma;", desc)
  return(desc)
}

##exDef <- function(val){
##  if(grepl("=",val, fixed=TRUE)){
##    subarg = trimws(unlist(strsplit(val, "=")))
##    return(subarg[2])
##  }else{
##    return("")
##  }
##}

##substrRight <- function(x, n){
##  substr(x,1,nchar(x)-n)
##}

#' toolCode.gmat
#'
#' @description
#' return beam canvas object for selected function in a give package
#'
#' @param packTarg package name
#' @param targVal function name
#'
#' @return array
#' @export
toolCode.gmat <- function(packTarg, targVal){
  myhelp <- getHelpList.gmat(targVal, as.character(packTarg));
  data <- as.list(sapply(myhelp,function(x) {x <- gsub("\n", "",x)}))
  data$usage = gsub(paste0(data$alias,"\\("), "",data$usage)
  data$usage = gsub(paste0(", \\)"), "",data$usage)
  pos = trimws(unlist(strsplit(data$usage, ",")))
  argFun = structure(list(param = character(), help = character(), paramVal = character(), req = character(), quoted = character(), type =  character(), attr =  character()), class = "data.frame");
  x=0
  optFlag = 'Y'
  func = "( "
  if( length(myhelp$arguments)>0)
    for (i in 1:length(myhelp$arguments)){
      x=x+1
      z=0
      if(grepl(",",myhelp$arguments[[i]]$arg, fixed=TRUE)){
        subarg = trimws(unlist(strsplit(myhelp$arguments[[i]]$arg, ",")))
        z=0
        for (z in 1:length(subarg)){
          argFun[x+(z-1),1] = subarg[z]
          argFun[x+(z-1),2] = cleanDesc.gmat(myhelp$arguments[[i]]$description)
          #argFun[x+(z-1),3] = exDef(pos[x+(z-1)])
          argFun[x+(z-1),3] = ''
          if(grepl("option",argFun[x+(z-1),2], fixed=TRUE)){
            optFlag = 'N'}
          argFun[x+(z-1),4] =  optFlag
          if(optFlag == 'Y')
            func = paste0(func,argFun[x+(z-1),1]," , ")
          if(grepl("\"",argFun[x+(z-1),3], fixed=TRUE)){
            argFun[x+(z-1),5] =  "Y"
          }else{
            argFun[x+(z-1),5] =  "N"
          }
          if(argFun[x+(z-1),4] == 'Y'){
            argFun[x+(z-1),6] = 'Input'
            argFun[x+(z-1),7] = 'stndAttrs'
          }else{
            argFun[x+(z-1),6] = 'KeyPair'
            argFun[x+(z-1),7] = 'custAttrs'
          }
        }
        x=x+(z-1)

      }else{
        argFun[x,1] = myhelp$arguments[[i]]$arg
        argFun[x,2] = cleanDesc.gmat(myhelp$arguments[[i]]$description)
        #argFun[x,3] = exDef(pos[x])
        argFun[x,3] = ''
        if(grepl("option",argFun[x,2], fixed=TRUE)){
          optFlag = 'N'}
        argFun[x,4] =  optFlag
        if(optFlag == 'Y')
          func = paste0(func, argFun[x,1]," , ")
        if(grepl("\"",argFun[x,3], fixed=TRUE)){
          argFun[x,5] =  "Y"
        }else{
          argFun[x,5] =  "N"
        }
      }

      if(argFun[x,4] == 'Y'){
        argFun[x,7] = 'stndAttrs'
        argFun[x,6] = 'Input'
      }else{
        argFun[x,7] = 'custAttrs'
        argFun[x,6] = 'KeyPair'
      }
    }

  #Update argFun if not empty
  if(nrow(argFun)){
    argFun[,4] = 'N'
    argFun[is.na(argFun)] <- ""
  }

  fullFun = data.frame(ID = paste0(packTarg,'(',targVal,')'))
  fullFun$group = "r_Tool"
  fullFun$Divclass = "icGrpSlide default datatool draw2d_droppable"
  fullFun$datatype = "svg"
  fullFun$datashape = "tool_SVG"
  fullFun$desc = cleanDesc.gmat(myhelp$description)
  fullFun$hTool = "34"
  fullFun$wTool = "44"
  fullFun$hCanvas = "60"
  fullFun$wCanvas = "60"
  fullFun$type = "node"
  fullFun$srcTool = "srcSVG"
  fullFun$srcComp = ""
  fullFun$toolSVG = ""
  fullFun$srcSVG = "<path id='path820' style='fill:none;fill-opacity:0;stroke:#000000;stroke-width:1.03193653;stroke-miterlimit:4;stroke-dasharray:none;stroke-opacity:1' d='m 30.12203,83.584225 c 6.134952,0 9.391831,-5.194217 10.349633,-8.262738 0.01595,-0.04834 0.02793,-0.09792 0.04269,-0.148302 2.231639,-8.762655 5.786714,-22.651064 8.42034,-32.753259 h 9.422956 c 1.771713,0 3.208395,-1.361436 3.208395,-3.04158 0,-1.680102 -1.436682,-3.041542 -3.208395,-3.041542 H 50.52944 c 0.842919,-3.182937 1.459865,-5.449903 1.702718,-6.223546 l 0.269748,-0.866035 c 0.994672,-3.217878 3.323797,-10.755409 6.925759,-10.755409 3.340077,0 3.837951,4.87649 3.860617,5.112268 0.142454,1.669619 1.678497,2.930703 3.443906,2.786344 1.767002,-0.127987 3.089322,-1.591692 2.952893,-3.265823 -0.299675,-3.707311 -2.832266,-10.717459 -10.258494,-10.717459 -8.40877,0 -11.56557,10.217544 -13.0823,15.129017 l -0.258174,0.828574 c -0.308056,0.971916 -1.116938,3.966531 -2.171665,7.972069 h -11.22575 c -1.77227,0 -3.208396,1.36144 -3.208396,3.041542 0,1.680144 1.436126,3.04158 3.208396,3.04158 h 9.63628 c -3.294269,12.666819 -7.551604,29.374566 -8.019951,31.213981 -0.174776,0.481956 -1.490433,3.866662 -4.181481,3.866662 -4.109295,0 -5.88839,-5.46782 -5.902634,-5.508817 -0.493128,-1.612657 -2.267993,-2.546139 -3.974423,-2.075639 -1.701682,0.467939 -2.682628,2.154544 -2.1895,3.768256 1.122683,3.676351 4.925878,9.899897 12.064962,9.899897 z m 47.296139,-8.135282 c 1.890824,0 3.47974,-1.273028 3.47974,-3.299749 0,-0.941444 -0.397041,-1.791023 -1.242794,-2.73393 l -6.662357,-7.258848 6.464796,-7.164989 c 0.596356,-0.707211 1.043115,-1.46135 1.043115,-2.451227 0,-1.793013 -1.542029,-3.111995 -3.531375,-3.111995 -1.341831,0 -2.384947,0.801107 -3.281617,1.839902 l -5.021248,6.220051 -4.922207,-6.221026 c -0.995712,-1.177181 -2.038308,-1.838927 -3.529778,-1.838927 -1.891304,0 -3.480777,1.274044 -3.480777,3.300765 0,0.941485 0.398237,1.791024 1.241756,2.732428 l 6.216077,6.788387 -6.812993,7.635936 c -0.646436,0.707211 -1.043116,1.46135 -1.043116,2.451226 0,1.792527 1.54151,3.111061 3.529259,3.111061 1.342869,0 2.386544,-0.801107 3.281618,-1.838967 l 5.368448,-6.692459 5.370004,6.692459 c 0.997785,1.180187 2.041978,1.839902 3.533449,1.839902 z M 15.666932,1.0786049 h 67.78264 c 8.502583,0 15.348298,7.0037691 15.348298,15.7034871 v 65.925576 c 0,8.699718 -6.845715,15.703492 -15.348298,15.703492 h -67.78264 c -8.5035806,0 -15.34933367,-7.003774 -15.34933367,-15.703492 V 16.782092 c 0,-8.699718 6.84575307,-15.7034871 15.34933367,-15.7034871 z' />"
  fullFun$srcPNG = ""
  fullFun$srcClass = ""
  fullFun$spanClass = "caption"
  fullFun$span = targVal
  fullFun$Code = paste0(trimws(targVal),func," )")
  fullFun$codeLib = ""
  fullFun$codeFunc = myhelp$alias
  fullFun$outVar = ""
  fullFun$postProcess = ""
  fullFun$inVar = ""
  fullFun$retVar = list(data.frame(retVar="info_Sum",retTitle="Data Frames Internal Structure",retCode=paste0("capture.output( str( ",paste0("gmat_",targVal)," ))")))
  fullFun$retCode = ""
  fullFun$retRes = ""
  fullFun$preProcess = ""
  fullFun$fieldStruc = data.frame(editType ="",editSource="",secTitle="",tblHeader="", rowBtns = "")
  fullFun$varAttrs = list(data.frame(varType="dataFrame",varHelp="Output from function.",varName=paste0("gmat_",targVal),varRole="data source", varMod ="Y"))
  fullFun$codeVar = list(data.frame(codeVar=paste0("gmat_",targVal),codeFlag="command",codeCode=paste0(" ",paste0(packTarg,"::",myhelp$alias,"()"))))
  fullFun$fieldAttrs = ""
  help = data.frame(pack=packTarg,func=targVal,stringsAsFactors=FALSE)
  fullFun$Help = list(help)
  fullFun$stndAttrs = list(argFun[argFun$attr=='stndAttrs',1:6])
  fullFun$custAttrs = list(argFun[argFun$attr=='custAttrs',1:6])
  portAttrs = data.frame(portType="in",portName="data source",portLabel="Input",portVar=0,stringsAsFactors=FALSE )
  portAttrs = rbind(portAttrs,c(portType="out",portName="data source",portLabel="Output",portVar=0))
  fullFun$portAttrs = list(portAttrs)
  return(fullFun)
}

#' packsCode.gmat
#'
#' @description
#' extract and return list of packages used in script and environment
#'
#' @param codeTxt code text to be evaluate
#' @param comment keep comments embedded in codeTxt
#' @param blank keep blank rows
#' @param arrow replace = with <-
#' @param envInfo include R environment information
#'
#' @return array
#' @export
packsCode.gmat <- function(codeTxt, comment = TRUE, blank = FALSE, arrow = TRUE, envInfo = FALSE){

  #Get Script Packages
  m <- gregexpr('[[:alnum:]]+::', codeTxt)
  pkg <- regmatches(codeTxt, m)
  pkgRef <- unlist(lapply(pkg, function(l) {str_remove(l, '::')}))
  pkgLib <- unlist(str_extract_all(codeTxt,"(?<=library\\(()).+(?=\\))"))
  pkgReq <- unlist(str_extract_all(codeTxt,"(?<=required\\(()).+(?=\\))"))
  pkgLst <- unique(c(pkgRef,pkgLib,pkgReq))
  pkgLst <- unlist(lapply(pkgLst, function(l) {str_trim(l,"both")}))

  #Get Environment Packages
  pkgEnv <- as.data.frame(search())
  envLst <- as.vector(pkgEnv$`search()` <- sub("package:", "", pkgEnv$`search()`))
  envLst <- envLst[!envLst %in% c('.GlobalEnv','Autoloads')]
  envInfoPlatform <- devtools::session_info()$platform
  envInfoPackages <- ''
  if(envInfo==TRUE)
    envInfoPackages <- invisible(capture.output(devtools::session_info()$packages))

  #Format Script
  codeFmt <- invisible(tidy_source(text = codeTxt, comment = comment, blank = blank, arrow = arrow)$text.tidy)
  pkgLst <- invisible(tidy_source(text = pkgLst, comment = comment, blank = blank, arrow = arrow)$text.tidy)
  envLst <- invisible(tidy_source(text = envLst, comment = comment, blank = blank, arrow = arrow)$text.tidy)

  return (list(pkgLst,envLst,codeFmt,envInfoPlatform,envInfoPackages))
}
# get list of packages in script and environment
tidyCode.gmat <- function(codeTxt, comment = TRUE, blank = FALSE, arrow = TRUE, envInfo = FALSE){

  #Get Script Packages
  m <- gregexpr('[[:alnum:]]+::', codeTxt)
  pkg <- regmatches(codeTxt, m)
  pkgRef <- unlist(lapply(pkg, function(l) {str_remove(l, '::')}))
  pkgLib <- unlist(str_extract_all(codeTxt,"(?<=library\\(()).+(?=\\))"))
  pkgReq <- unlist(str_extract_all(codeTxt,"(?<=required\\(()).+(?=\\))"))
  pkgLst <- unique(c(pkgRef,pkgLib,pkgReq))
  pkgLst <- unlist(lapply(pkgLst, function(l) {str_trim(l,"both")}))

  #Get Environment Packages
  pkgEnv <- as.data.frame(search())
  envLst <- as.vector(pkgEnv$`search()` <- sub("package:", "", pkgEnv$`search()`))
  envLst <- envLst[!envLst %in% c('.GlobalEnv','Autoloads')]
  envInfoPlatform <- devtools::session_info()$platform
  envInfoPackages <- ''
  if(envInfo==TRUE)
    envInfoPackages <- invisible(capture.output(devtools::session_info()$packages))

  #Format Script
  codeFmt <- invisible(tidy_source(text = codeTxt, comment = comment, blank = blank, arrow = arrow)$text.tidy)
  pkgLst <- invisible(tidy_source(text = pkgLst, comment = comment, blank = blank, arrow = arrow)$text.tidy)
  envLst <- invisible(tidy_source(text = envLst, comment = comment, blank = blank, arrow = arrow)$text.tidy)

  return (list(pkgLst,envLst,codeTxt,envInfoPlatform,envInfoPackages))
}


#' convRowName.gmat
#'
#' @description
#' convert rownames to numeric is valid (Used in beams View Object)
#'
#' @param rowname row name to convert
#'
#' @return array
#' @export
convRowName.gmat <- function(rowname){
  if (suppressWarnings(all(!is.na(as.numeric(as.character(rowname)))))) {
    as.numeric(as.character(rowname))
  } else {
    rowname
  }
}


#' fnObjectsList.gmat
#'
#' @description
#' funList <- fnObjects.gmat()
#'
#' @return fnObjects.gmat() results
#' @export
fnObjectsList.gmat <- function(){
  return(fnObjects.gmat())
}


#' fnObjectsList.gmat
#'
#' @description
#' Return columns and class for a data source
#'
#' @param df data frame
#'
#' @return dataframe
#' @export
colClasses.gmat <- function(df) {
  colInfo = as.data.frame(lapply(df, function(x) paste(class(x), collapse = ',')))
  colInfo = as.data.frame(t(colInfo))
  colInfo$names <- rownames(colInfo)
  rownames(colInfo) <- NULL;
  colnames(colInfo) <- c("Type","Name")

  return (colInfo)
}

#' dsClasses.gmat
#'
#' @description
#' Return class and other information for a data source
#'
#' @param df data frame
#' @param dfName data frame name
#'
#' @return dataframe
#' @export
dsClasses.gmat <- function(df,dfName) {
  dfInfo = data.frame('text' = dfName)
  colnames(dfInfo) = c('text')
  dfInfo$type = paste( unlist(sapply(dfInfo$text, function(x) as.character( class(eval(parse(text=x)))))), collapse=', ')
  dfInfo$nrow = nrow(df)
  dfInfo$ncol = ncol(df)
  dfInfo$size = as.character(format(object.size(df),units = "KB"))

  return (dfInfo)
}

#' colDS.gmat
#'
#' @description
#' Return column information for dataframe
#'
#' @param df data frame
#' @param dfName data frame name
#'
#' @return dataframe
#' @export
colDS.gmat <- function(df,dfName){
  if(is.vector(df, mode = "numeric")& !is.null(names(df)) & !any(is.na(names(df)))){
    return("")
  }else if(is.vector(df, mode = "numeric")){
    return("")
  }else if(is.vector(df, mode = "character")& !is.null(names(df)) & !any(is.na(names(df)))){
    return("")
  }else if(is.vector(df, mode = "character")){
    return("")
  }else if(is.factor(df) ){
    return("")
  }else if(is.ts(df)){
    return("")
  }else if(is.data.frame(df)){
    colInfo = as.data.frame(lapply(df, function(x) paste(class(x), collapse = ',')))
    colInfo = as.data.frame(t(colInfo))
    colInfo$names <- rownames(colInfo)
    rownames(colInfo) <- NULL;
    colnames(colInfo) <- c("Type","Name")
    colinfoStr = ""
    for(i in c(1:nrow(colInfo))){
      colInfoStr2 = paste("column ",i," name:'",colInfo[i,2], "', column ",i," type:'",colInfo[i,1],"'",sep="")
      colinfoStr = paste(colinfoStr,colInfoStr2, " | ",sep="")
    }

    return (
      paste0('The ',dfName,' data set is made up of the following columns and types: ' ,colinfoStr,sep="")
    )

  }else if(is.table(df)){
    return("")
  }else if(is.matrix(df)){
    return("")
  }else if(is.list(df)){
    return("")
  }else if(is.array(df)){
    return("")
  }else{
    return("")
  }
}

#' checkDS.gmat
#'
#' @description
#' Return object type for a selected object
#'
#' @param df data frame
#' @param dfName data frame name
#'
#' @return dataframe
#' @export
checkDS.gmat <- function(df,dfName){
  if(is.vector(df, mode = "numeric")& !is.null(names(df)) & !any(is.na(names(df)))){
    return(
      paste0('The ',dfName,' object is a "Named Numeric Vector" with a length of ',
             length(df),'. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.vector(df, mode = "numeric")){
    return(
      paste0('The ',dfName,' object is a "Numeric Vector" with a length of ',
             length(df),'. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.vector(df, mode = "character")& !is.null(names(df)) & !any(is.na(names(df)))){
    return(
      paste0('The ',dfName,' object is a "Named Character Vector" with a length of ',
             length(df),'. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.vector(df, mode = "character")){
    return(
      paste0('The ',dfName,' object is a "Character Vector" with a length of ',
             length(df),'. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.factor(df) ){
    return(
      paste0('The ',dfName,' object is a "Factor Vector" with a length of ',
             length(df),'. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.ts(df)){
    return(
      paste0('The ',dfName,' object is a "Time Series" with a Start Date of ',
             tsp(df)[1],' and of End Date of ',tsp(df)[2], ' with a Unit of Time ',tsp(df)[3],
             '. Total number of Observations is',length(df))
    )
  }else if(is.data.frame(df)){
    colInfo = as.data.frame(lapply(df, function(x) paste(class(x), collapse = ',')))
    colInfo = as.data.frame(t(colInfo))
    colInfo$names <- rownames(colInfo)
    rownames(colInfo) <- NULL;
    colnames(colInfo) <- c("Type","Name")
    colinfoStr = ""
    for(i in c(1:nrow(colInfo))){
      colInfoStr2 = paste("column ",i," = name:'",colInfo[i,2], "', type:'",colInfo[i,1],"'",sep="")
      colinfoStr = paste(colinfoStr,colInfoStr2, "<br>",sep="")
    }

    return (
      paste0('The "',dfName,'" object is a Data Frame with nrow=',
             nrow(df),' and ncol=' ,ncol(df),
             '. It\'s size is ',as.character(format(object.size(df),units = "KB")),'.<br><br>The "',dfName,'" data frame has the following column names and types:<br>' ,colinfoStr,sep="")
    )

  }else if(is.table(df)){
    return(
      paste0('The ',dfName,' object is a "Table" with ',
             length(df),' rows and ' ,length(dimnames(df)),
             ' attributes. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.matrix(df)){
    return(
      paste0('The ',dfName,' object is a "Matrix" with nrow=',
             nrow(df),' and ncol=' ,ncol(df),
             '. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.list(df)){
    return(
      paste0('The ',dfName,' object is a "List" with ',
             length(df),' attributes',
             '. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else if(is.array(df)){
    return(
      paste0('The ',dfName,' object is an "Array" with ',
             length(dimnames(df)),' attributes',
             '. It\'s size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }else{
    return(
      paste0('The ', dfName,' is an "unknown" object. It has ',
             length(df),' attributes',
             ' and its size is ',as.character(format(object.size(df),units = "KB"))
      )
    )
  }
}

