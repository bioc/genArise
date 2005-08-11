# Miscelaneous apps of genArise
require(xtable)

# Get the annotations for a given data set
annotations <-  function(specie.data, specie, column, output.file = "annotations.html"){
  species <- c("Mus musculus","Saccharomyces Cerevisiae","Rattus norvegicus", "NCBI")
  if(! is.element(specie,species))
    stop("The specie option must be one of the following: Mus musculus, Saccharomyces Cerevisiae, Rattus norvegicus or NCBI")

  the.specie <- which(specie == species)

  # send to the corresponding database
  switch(the.specie,
         {specie.data[,column] <- paste('<a href="http://www.informatics.jax.org/javawi2//servlet/WIFetch?page=searchTool&query=',
                                specie.data[,column],'&selectedQuery=Genes+and+Markers">', specie.data[,column], sep = "")},
         {specie.data[,column] <- paste('<a href="http://db.yeastgenome.org/cgi-bin/locus.pl?locus=',specie.data[,column],'">',
                                       specie.data[,column], sep = "")},
         {specie.data[,column] <-  paste('<a href="http://rgd.mcw.edu/generalSearch/RgdSearch.jsp?quickSearch=1&searchKeyword=',
                                         specie.data[,column],'">', specie.data[,column], sep = "")},
         {specie.data[,column] <- paste('<a href="http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?CMD=search&DB=gene&term=',
                                specie.data[,column],'">', specie.data[,column], sep = "")})
  
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

# Create a new project Window 
new.project <- function(envir){
  select <- tktoplevel()
  window.configuration.project <- function(){
    active.options <- function(is.ifc = TRUE){
      if(is.ifc){
        state <- "disable"
      }else{
        state <- "normal"
      }
      tkconfigure(titulo2, state= state)
      tkconfigure(rows.label, state= state)
      tkconfigure(columns.label, state= state)
      tkconfigure(meta.rows.label, state= state)
      tkconfigure(meta.columns.label, state= state)
      tkconfigure(rows.entry, state= state)
      tkconfigure(columns.entry, state= state)
      tkconfigure(meta.rows.entry, state= state)
      tkconfigure(meta.columns.entry, state= state)
    }

    spot.frame <- tkframe(select, relief = "groove", borderwidth=2)
    
    tkwm.title(select,"Project Configuration")
    tkgrid(tklabel(select, text="Fill the text entries with the path and name of your new Project"), padx = "10", pady = "10")
    tkgrid(tklabel(spot.frame,text="Input"), padx = "10", pady = "10")
    entry.frame <- tkframe(spot.frame)
    association.entry <- tkentry(entry.frame,width="30", bg = "white", textvariable= tclVar(""))

    browse1.function <- function(){
      fileName <- tclvalue(tkgetOpenFile())
      if (!nchar(fileName))
        tkmessageBox(message="No file was selected!",icon = "error", default = "ok")
      else{
        tkconfigure(association.entry, textvariable = tclVar(fileName))
      }
    }
    
    association.but <- tkbutton(entry.frame,text="browse...",    command=function() browse1.function())
    tkbind(association.but, "<Return>", browse1.function)
    
    tkgrid(tklabel(entry.frame,text="Location Spot File:"), association.entry, association.but, padx = "5", sticky = "w", pady = "2")
    radio.frame <- tkframe(spot.frame)
    rbValue <- tclVar("IFC")
    rb1 <- tkradiobutton(radio.frame, text = "Foreign", value = "Foreign", variable = rbValue, command = function(){active.options(FALSE)})
    rb2 <- tkradiobutton(radio.frame, text = "IFC", value = "IFC", variable = rbValue, command = function(){active.options(TRUE)})
    tkgrid(rb1, rb2, sticky = "w", pady = "2",padx="70")
    tkgrid(entry.frame,padx="5", pady = "10")
    tkgrid(radio.frame,padx="5", pady = "10")
    tkgrid(tklabel(spot.frame, text = "Fill the text entries with the correct columns in your file"), padx = "10", pady = "5")
    columns.frame <- tkframe(spot.frame)
    Cy3 <- tclVar("")
    Cy5 <- tclVar("")
    BgCy3 <- tclVar("")
    BgCy5 <- tclVar("")
    Id <- tclVar("")
    tkgrid(tklabel(columns.frame,text="Cy3"),tkentry(columns.frame,width="5",textvariable=Cy3, bg = "white"),
           tklabel(columns.frame,text="Cy5"),tkentry(columns.frame,width="5",textvariable=Cy5, bg = "white"),
           tklabel(columns.frame,text="BgCy3"),tkentry(columns.frame,width="5",textvariable=BgCy3, bg = "white"),
           tklabel(columns.frame,text="BgCy5"),tkentry(columns.frame,width="5",textvariable=BgCy5, bg = "white"),
           tklabel(columns.frame,text="Id"),tkentry(columns.frame,width="5",textvariable=Id, bg = "white"),pady="10",padx="4")
    tkgrid(columns.frame)
    
    Row <- tclVar("")
    Col <- tclVar("")
    MR <- tclVar("")
    MC <- tclVar("")
    rc.frame <- tkframe(spot.frame,borderwidth=2)
    titulo2 <- tklabel(spot.frame, text = "Especify the grid dimensions")
    rows.label <- tklabel(rc.frame,text="Rows")
    columns.label <- tklabel(rc.frame,text="Columns")
    meta.rows.label <- tklabel(rc.frame,text="Meta-rows")
    meta.columns.label <- tklabel(rc.frame,text="Meta-columns")
    rows.entry <- tkentry(rc.frame,width="5",textvariable=Row, bg = "white")
    columns.entry <- tkentry(rc.frame,width="5",textvariable=Col, bg = "white")
    meta.rows.entry <- tkentry(rc.frame,width="5",textvariable=MR, bg = "white")
    meta.columns.entry <- tkentry(rc.frame,width="5",textvariable=MC, bg = "white")
    
    tkgrid(titulo2, padx = "10", pady = "10")
    tkgrid(rows.label, rows.entry, columns.label, columns.entry, meta.rows.label,meta.rows.entry, meta.columns.label, meta.columns.entry)
    active.options(TRUE)
    tkgrid(rc.frame,padx="10", padx = "10")
    tkgrid(spot.frame,padx="5", pady = "10")
    lower.frame <- tkframe(select,relief="groove",borderwidth=2)
    tkgrid(tklabel(lower.frame,text="Output"), padx = "10", pady = "10")
    output.frame <- tkframe(lower.frame)
    project.entry <- tkentry(output.frame,width="30", bg = "white", textvariable= tclVar(""))
    

    browse2.function <- function(){
      fileName <- tclvalue(tkgetSaveFile())
      if (!nchar(fileName))
        tkmessageBox(message="No file was selected!",icon = "error", default = "ok")
      else{
        tkconfigure(project.entry, textvariable = tclVar(fileName))
      }
    }

    project.but <- tkbutton(output.frame,text="browse...",    command=function() browse2.function())
    tkgrid(tklabel(output.frame,text="Project Name:"), project.entry, project.but, padx = "5", sticky = "w", pady = "2")
    tkbind(project.but, "<Return>", browse2.function)

    graphic.file <- tclVar("Plots")
    output.file <- tclVar("Results")

    graphic.entry <- tkentry(output.frame,width="10", bg = "white", textvariable = graphic.file)
    output.entry  <- tkentry(output.frame,width="10", bg = "white", textvariable = output.file)
    
     
    tkgrid(tklabel(output.frame,text="Location for Plots:"), graphic.entry, sticky = "w", padx = "5", pady = "2")
    tkgrid(tklabel(output.frame,text="Location for Results:"), output.entry,  sticky = "w", padx = "5", pady = "2")
    tkgrid(output.frame, padx="5")
    tkgrid(lower.frame, padx="5")
    buttons.frame <- tkframe(select)

    ok.function <-  function(){
      if(nchar(tclvalue(tkget(association.entry))) && !file.exists(tclvalue(tkget(association.entry)))){
        tkmessageBox(message= "This file doesn't exists! ",icon = "error", default = "ok")
      }
      
      if(!nchar(tclvalue(tkget(association.entry))) || !nchar(tclvalue(tkget(project.entry))) ||
         !nchar(tclvalue(graphic.file)) || !nchar(tclvalue(output.file))){
        tkmessageBox(message= "No input or output file was specified! ", icon = "error", default = "ok")
      }
      else{
        # read the file containing the data of the experiment
        inputFile <- tclvalue(tkget(association.entry))
        name.project <-  tclvalue(tkget(project.entry))
        assign("path", inputFile, envir = envir)
        name.tmp <- unlist(strsplit(inputFile, paste("\\", .Platform$file.sep, sep = "")))
        tmp <- unlist(strsplit(name.tmp[length(name.tmp)], "\\."))
        name.tmp <- paste(tmp[-length(tmp)],sep=".",collapse=".")
        assign("spot.name", name.tmp, envir = envir)
        set.path.project(name.project, tclvalue(output.file),
                         tclvalue(graphic.file), envir)
        temp <- read.csv(inputFile, header = FALSE, sep = "\t", comment.char = "")
        temp <- apply(temp, 2, as.vector)

        # check if the experiment was performed in the Cellular Physiology Institute of UNAM
        if(as.character(tclvalue(rbValue)) == "IFC"){
          temp <- temp[-1,] # the header
          row1 <- temp[,1]
          flag <- which(rev(row1) == "Mean value")
          if(length(flag)==0){
            tkmessageBox(message= "This experiment is not from the IFC", icon = "error", default = "ok")
          }
          empty.row <- length(row1[row1==""])
          total.rows <-  length(temp[,1])
          temp <- temp[-((total.rows-(flag-1 + empty.row)):total.rows),]
          temp <- apply(temp, 2, as.vector)
          total.rows <- length(temp[,1])
          ans <- paste(max(as.integer(temp[,2]),na.rm=TRUE), max(as.integer(temp[,3]),na.rm=TRUE),
                       max(as.integer(temp[,4]),na.rm=TRUE), max(as.integer(temp[,5]),na.rm=TRUE), sep = " ")
          final <- matrix(ncol = 5)
          dimensiones <-  unlist(strsplit(ans, " "))
          dimensiones <- as.numeric(dimensiones)

          # fill up if there is some lost subgrid
          ubications <- function(mr,mc,r,c){
            lista <- seq(1:mr)
            lista <- sapply( lista, paste, seq(1:mc))
            lista <- as.vector(lista)
            lista <- sapply( lista, paste, seq(1:r))
            lista <- as.vector(lista)
            lista <- sapply(lista, paste, seq(1:c))
            lista <- as.vector(lista)
            lista
          }

          lista <-  ubications(dimensiones[1],dimensiones[2],dimensiones[3],dimensiones[4])
          conf <-  paste(temp[,2],temp[,3],temp[,4],temp[,5], sep = " ")
          index <- is.element(lista,conf)
          result.list <-  matrix(ncol = 5, nrow=length(lista))
          result.list[!index,1:4] <- as.integer(0)
          result.list[!index,5] <- "empty"
          koala <- match(conf,lista)
          result.list[koala,] <- c(temp[koala, as.numeric(tclvalue(Cy3))], temp[koala,as.numeric(tclvalue(Cy5))], temp[koala, as.numeric(tclvalue(BgCy3))], temp[koala, as.numeric(tclvalue(BgCy5))], temp[koala, as.numeric(tclvalue(Id))])

          set.grid.properties(envir,name.project, dimensiones[3], dimensiones[4], dimensiones[1],dimensiones[2])
          
                                        # return the Spot object
          assign("o.spot", new ("Spot", name = get("spot.name", envir = envir),
                                spotData = list(Cy3 = as.numeric(result.list[,1]), Cy5 = as.numeric(result.list[,2]),
                  BgCy3 = as.numeric(result.list[,3]), BgCy5 = as.numeric(result.list[,4]), Id = as.vector(result.list[,5]))), envir = envir)
        }

        # if the experiment was performed in any other institution, you must enter the whole configuration in the GUI
        else{
          row1 <- temp[,1]
          flag <- which(rev(row1) == "Mean value")
           if(!length(flag)==0){
            tkmessageBox(message= "This is not the format for a foreign experiment file.\nPlese check the documentation of genArise for the file format.", icon = "error", default = "ok")
          }else{
          total <- as.integer(tclvalue(Row)) * as.integer(tclvalue(Col)) * as.integer(tclvalue(MR)) * as.integer(tclvalue(MC))
          if((total + 1) == length(temp[,1])){
            temp <- temp[-1,]
            temp <- apply(temp, 2, as.vector)
          }
          set.grid.properties(envir,name.project,
                              as.integer(tclvalue(Row)), as.integer(tclvalue(Col)),as.integer(tclvalue(MR)), as.integer(tclvalue(MC)))
          
          assign("o.spot", new ("Spot", name = get("spot.name", envir = envir),
                                spotData = list(Cy3 = as.numeric(temp[,as.numeric(tclvalue(Cy3))]),
                                  Cy5 = as.numeric(temp[,as.numeric(tclvalue(Cy5))]),
                                  BgCy3 = as.numeric(temp[,as.numeric(tclvalue(BgCy3))]),
                                  BgCy5 = as.numeric(temp[,as.numeric(tclvalue(BgCy5))]),
                                  Id = as.vector(temp[,as.numeric(tclvalue(Id))]))), envir = envir)
        }}
        create.project(name.project,tclvalue(output.file), tclvalue(graphic.file))
        history.project <- paste(name.project,".prj", sep = "")
        set.history.project(history.project, "Name:", get("spot.name", envir = envir))
        set.history.project(history.project, "Rows", get("nr", envir = envir))
        set.history.project(history.project, "Columns", get("nc", envir = envir))
        set.history.project(history.project, "MetaRows", get("nmr", envir = envir))
        set.history.project(history.project, "MetaColumns", get("nmc", envir = envir))
        assign("history.project",history.project, envir = envir)
        tkdestroy(select)
        principal(envir)
      }}
    
    ok.but <- tkbutton(buttons.frame,text="  OK  ",command = ok.function)
    
    cancel.function <- function(){
      tkdestroy(select)
      genArise()}
    
    cancel.but <- tkbutton(buttons.frame,text="Cancel",command = function() cancel.function())
    tkbind(cancel.but, "<Return>", cancel.function)
    tkgrid(ok.but,cancel.but,pady="9", padx = "20")
    tkgrid(buttons.frame,pady="9")
  }
  window.configuration.project()
}

# Post analysis configuration Window
projects.select <- function(envir, nombre = "sets-analysis"){
  if(.Platform$OS.type == "unix")
    nombre <- paste(Sys.getenv("HOME"),nombre,sep=.Platform$file.sep)
  else{ if(.Platform$OS.type == "windows")
    nombre <- paste(Sys.getenv("R_USER"),nombre,sep=.Platform$file.sep)}
  tt <- tktoplevel()
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
   # nombre <- tclvalue(tkget(output))
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
   # nombre <- tclvalue(tkget(output))
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(updown)),
           {
                tkinsert(output,"end",paste(nombre,"1_5-2sd","Up",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1_5-2sd","Down",sep=""))
               })})
  radio3 <- tkradiobutton(framerange, value=3, variable=range,command=function(){
    tkconfigure(output, state="normal")
    #nombre <- tclvalue(tkget(output))
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
  tkgrid(tklabel(framerange,text="more than 2 sd"),radio3,pady = "2",padx="10")
  radioup <- tkradiobutton(frameupdown, value=1, variable=updown, command=function(){
    tkconfigure(output, state="normal")
    #nombre <- tclvalue(tkget(output))
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(range)),
           {
                tkinsert(output,"end",paste(nombre,"1-1_5sd","Up",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1_5-2sd","Up",sep=""))
               },
           {
                 tkinsert(output,"end",paste(nombre,"Greater2sd","Up",sep=""))
               }
           )})
  radiodown <- tkradiobutton(frameupdown, value=2, variable=updown, command=function(){
    tkconfigure(output, state="normal")
   # nombre <- tclvalue(tkget(output))
    tkdelete(output,"0","end")
    switch(as.integer(tclvalue(range)),
           {
                tkinsert(output,"end",paste(nombre,"1-1_5sd","Down",sep=""))
              },
           {
                 tkinsert(output,"end",paste(nombre,"1_5-2sd","Down",sep=""))
               },
           {
                 tkinsert(output,"end",paste(nombre,"Greater2sd","Down",sep=""))
               }
           )})
  tkgrid(tklabel(frameupdown,text="Select the desired option"),pady = "2")
  tkgrid(tklabel(frameupdown,text="Up-regulated"),radioup,pady = "2")
  tkgrid(tklabel(frameupdown,text="Down-regulated"),radiodown,pady = "2")
  tkgrid(tklabel(frameOverall,text="Where you want to save the output"), padx = "10", pady = "10")
  
 

 #   tkinsert(output,"end",nombre)
 # tkconfigure(output, state="disabled")
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
    tempo <- tclvalue(tkgetOpenFile(filetypes = "{{PRJ Files} {.prj}} {{All files} *}"))
    if(nchar(tempo)){
      tkinsert(files.list,"end",tempo)
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
            })
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
    tkdestroy(tt)
    genArise()
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
