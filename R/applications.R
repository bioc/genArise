# Miscelaneous apps of genArise
require(xtable)

# Get the annotations for a given data set
annotations <-  function(specie.data, specie, column, symbol=NULL, output.file = "annotations.html"){
  species <- c("Mus musculus","Saccharomyces Cerevisiae","Rattus norvegicus", "NCBI")
  if(! is.element(specie,species))
    stop("The specie option must be one of the following: Mus musculus, Saccharomyces Cerevisiae, Rattus norvegicus or NCBI")

  the.specie <- which(specie == species)

  # send to the corresponding database
  switch(the.specie,
         {specie.data[,column] <- paste('<a href="http://www.informatics.jax.org/javawi2//servlet/WIFetch?page=searchTool&query=',
                                specie.data[,column],'&selectedQuery=Genes+and+Markers">', specie.data[,column], sep = "")
          if(!is.null(symbol))
          specie.data[,symbol] <- paste('<a href="http://www.informatics.jax.org/javawi2//servlet/WIFetch?page=searchTool&query=',specie.data[,symbol],'&selectedQuery=Genes+and+Markers">',specie.data[,symbol],sep="")
       },
         {specie.data[,column] <- paste('<a href="http://db.yeastgenome.org/cgi-bin/locus.pl?locus=',specie.data[,column],'">',
                                       specie.data[,column], sep = "")
          if(!is.null(symbol))
            specie.data[,symbol] <- paste('<a href="http://db.yeastgenome.org/cgi-bin/locus.pl?locus=',specie.data[,symbol],'">',specie.data[,symbol], sep = "")},
         {specie.data[,column] <-  paste('<a href="http://rgd.mcw.edu/generalSearch/RgdSearch.jsp?quickSearch=1&searchKeyword=',
                                         specie.data[,column],'">', specie.data[,column], sep = "")
          if(!is.null(symbol))
            specie.data[,symbol] <- paste('<a href="http://rgd.mcw.edu/generalSearch/RgdSearch.jsp?quickSearch=1&searchKeyword=', specie.data[,symbol],'">', specie.data[,symbol], sep = "")},
         {specie.data[,column] <- paste('<a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?CMD=search&DB=gene&term=',
                                specie.data[,column],'">', specie.data[,column], sep = "")
          if(!is.null(symbol))
            specie.data[,symbol] <- paste('<a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?CMD=search&DB=gene&term=',
                  specie.data[,symbol],'">', specie.data[,symbol], sep = "")})
  
  specie.data <- data.frame(specie.data)
  
                                        # construct the table with the html references
  annotation.table <- xtable(specie.data)
  print.xtable(annotation.table, type = "html", file = output.file)
}

# Create the file notes.txt for any important note of the researcher
note <-  function(envir){
  file.note <- file.path(get("path.project", envir = envir), "notes.txt")
  if(!is.element("notes.txt", get("path.project", envir = envir))){
    con <- file(file.note, "a")
    close(con)
  }
  system(paste(get("editor", envir = envir),  file.note, sep = " "))
}

                                   # Post analysis configuration Window
projects.select <- function(envir, nombre = "sets-analysis"){
  if(.Platform$OS.type == "unix")
    nombre <- paste(Sys.getenv("HOME"),nombre,sep=.Platform$file.sep)
  else{ if(.Platform$OS.type == "windows")
    nombre <- paste(Sys.getenv("R_USER"),nombre,sep=.Platform$file.sep)}
  tt <- tktoplevel()
  tkfocus(tt)
  tkwm.title(tt,"File Selector")
  upper.frame <- tkframe(tt)
  frameOverall <- tkframe(upper.frame)
  framerange <- tkframe(frameOverall,relief="groove",borderwidth=2)
  frameupdown <- tkframe(frameOverall,relief="groove",borderwidth=2)
  range  <- tclVar(1)
  updown <- tclVar(1)
  output <- tkentry(frameOverall,width="30", bg = "white", textvariable=tclVar(nombre))

  radio1 <- tkradiobutton(framerange, value=1, variable=range, command=function(){
    tkconfigure(output, state="normal")
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(updown)),
               
           {
                tkinsert(output,"end",paste(nombre,"1-1_5sd","Up",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1-1_5sd","Down",sep=""))
               })})
  radio2 <- tkradiobutton(framerange, value=2, variable=range,command=function(){
    tkconfigure(output, state="normal")
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(updown)),
           {
                tkinsert(output,"end",paste(nombre,"1_5-2sd","Up",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1_5-2sd","Down",sep=""))
               })})
  radio4 <- tkradiobutton(framerange, value=4, variable=range,command=function(){
    tkconfigure(output, state="normal")
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(updown)),
           {
             tkinsert(output,"end",paste(nombre,"Greater1.5sd","Up",sep=""))
           },
           {
             tkinsert(output,"end",paste(nombre,"Greater1.5sd","Down",sep=""))
           })})
  radio3 <- tkradiobutton(framerange, value=3, variable=range,command=function(){
    tkconfigure(output, state="normal")
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(updown)),
           {
             tkinsert(output,"end",paste(nombre,"Greater2sd","Up",sep=""))
           },
           {
             tkinsert(output,"end",paste(nombre,"Greater2sd","Down",sep=""))
           })})
    
  tkgrid(tklabel(framerange,text="Select the range"),pady = "2",padx="10")
  tkgrid(tklabel(framerange,text="1 - 1.5 sd"),radio1,pady = "2",padx="10")
  tkgrid(tklabel(framerange,text="1.5 - 2 sd"),radio2,pady = "2",padx="10")
  tkgrid(tklabel(framerange,text="more than 1.5 sd"),radio4,pady = "2",padx="10")
  tkgrid(tklabel(framerange,text="more than 2 sd"),radio3,pady = "2",padx="10")
  radioup <- tkradiobutton(frameupdown, value=1, variable=updown, command=function(){
    tkconfigure(output, state="normal")
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(range)),
           {
                tkinsert(output,"end",paste(nombre,"1-1_5sd","Up",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1_5-2sd","Up",sep=""))
               },
            {
                tkinsert(output,"end",paste(nombre,"Greater1_5sd","Up",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"Greater2sd","Up",sep=""))
               }
           )})
  radiodown <- tkradiobutton(frameupdown, value=2, variable=updown, command=function(){
    tkconfigure(output, state="normal")
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(range)),
           {
                tkinsert(output,"end",paste(nombre,"1-1_5sd","Down",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1_5-2sd","Down",sep=""))
               },
           {
                tkinsert(output,"end",paste(nombre,"Greater1_5sd","Down",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"Greater2sd","Down",sep=""))
               }
           )})
  tkgrid(tklabel(frameupdown,text="Select the desired option"),pady = "2")
  tkgrid(tklabel(frameupdown,text="Up-regulated"),radioup,pady = "2")
  tkgrid(tklabel(frameupdown,text="Down-regulated"),radiodown,pady = "2")
  tkgrid(tklabel(frameOverall,text="Where you want to save the output"), padx = "10", pady = "10")
  
  out.function <-function(){
    dirname <- tclvalue(tkgetSaveFile())
    if (!nchar(dirname))
      tkmessageBox(message="No file was selected!",icon = "error", default = "ok")
    else{
      tkconfigure(output,textvariable = tclVar(dirname))
      nombre <<- dirname
      }
  }
  
  out.but <- tkbutton(frameOverall,text="Browse",command=function() out.function())

  tkgrid(output,out.but, padx = "5")
  
  left.frame <-  tkframe(upper.frame,relief = "groove", borderwidth=2)
  
  tree.frame <- tkframe(left.frame, relief = "groove", borderwidth=2)
  xscr <- tkscrollbar(tree.frame, repeatinterval=5, command = function(...)tkxview(files.list,...),orient="horizontal")
  yScr <- tkscrollbar(tree.frame, command = function(...)tkyview(files.list,...), orient = "vertical")
  files.list <- tklistbox(tree.frame, background = "white", xscrollcommand=function(...)tkset(xscr,...),
                          yscrollcommand=function(...)tkset(yScr,...))
  tkconfigure(files.list, width = "30", height = "10")
  
  tkgrid(files.list,yScr)
  tkgrid.configure(yScr,stick="nsw")
  tkgrid(xscr)
  tkgrid.configure(xscr,stick="new")
  files.frame <- tkframe(tree.frame)
  
  onDelete <-  function(){
    selected.file <-  tkcurselection(files.list)
    if(length(as.character(selected.file)) > 0)
      tkdelete(files.list, as.integer(selected.file))
  }
  
  onAdd <-  function(){
    tempo <- tclvalue(tkgetOpenFile(filetypes = "{{PRJ Files} {.prj}} {{All files} *}",multiple = "1"))
    tempo <- unlist(strsplit(tempo," "))
    if(length(tempo)!= 0){
      for(i in 1:length(tempo)){ 
        if(nchar(tempo[i])){
          tkinsert(files.list,"end",tempo[i])
        }
      }
    }
  }
  
  files.button.frame <- tkframe(left.frame)
  
  delete.button <-  tkbutton(files.button.frame, text = "Delete file", command = onDelete)
  
  add.button <-  tkbutton(files.button.frame, text = "Add file", command = onAdd)
  
  tkgrid(tklabel(left.frame,text = "Select all the .prj files with which you \nwill perform the combinatorial analysis"), padx = "10", pady = "10")
  tkgrid(tree.frame, pady = "10")
  tkgrid(add.button, delete.button, padx = "20")
  tkgrid(files.button.frame, pady= "10")

  button.frame <- tkframe(tt, borderwidth=2)
  
  operacion <- function(){
    values <- get.paths()
    if (length(values) < 1)
      tkmessageBox(message="No file was selected!",icon = "error", default = "ok")
    else{
      switch(as.integer(tclvalue(range)),
              { minimo <- 1; maximo <- 1.5
              },
             {minimo <- 1.5; maximo <- 2
            },
             {minimo <- 2; maximo <- NULL
            },
             {minimo <- 1.5; maximo <- NULL
            }
             )
      if(as.integer(tclvalue(updown)) == 1){
        up.down <- "up"
      }else{
        up.down <- "down"
      }
      dirname <- substr(tclvalue(tkget(output)),0,nchar(tclvalue(tkget(output))))
      post.analysis(values,
                    minimo, maximo, up.down = up.down, output = dirname)


    }
  }

  ok.but <- tkbutton(button.frame, text ="OK", command = function(){
    tkconfigure(tt, cursor="watch")
    operacion()
    tkconfigure(tt, cursor="arrow")
    tkmessageBox(title = "Post-Analysis", icon="info", message=paste("The Post-Analysis has done.\n The results files are in this directory:", tclvalue(tkget(output))), type="ok")
  })
  
  cancel.but <- tkbutton(button.frame, text ="Cancel", command = function(){tkdestroy(tt)
                                                                            genArise.init(envir)})
  
  tkgrid(framerange,pady="10")
  tkgrid(frameupdown,pady="10")
  tkgrid(left.frame, frameOverall,padx = "10", pady = "10")
  tkgrid(ok.but, cancel.but, padx = "20")
  tkgrid(upper.frame, pady = "10")
  tkgrid(button.frame)
  
  get.paths <- function(){
    list.projects <-  list()
    tkselection.set(files.list,"0", "end")
    selected.file <-  tkcurselection(files.list)
    paths <- unlist(strsplit(tclvalue(selected.file), " "))
    for(i in 1:length(paths)){
      list.projects <-  c(list.projects, tclvalue(tkget(files.list, paths[as.numeric(i)])), recursive = TRUE)
    }
    list.projects
  }
}
