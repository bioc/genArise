# Post analysis configuration Window

swap.select <- function(envir){
  tt <- tktoplevel()
  tkfocus(tt)
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
  tkwm.resizable(tt,0,0)
  tkwm.title(tt,"File Selector")
  upper.frame <- tkframe(tt)
  right.frame <-  tkframe(upper.frame,relief = "groove")
  frameOverall <- tkframe(right.frame)
  left.frame <-  tkframe(upper.frame,relief = "groove", borderwidth=2)
  tree.frame <- tkframe(left.frame, relief = "groove", borderwidth=2)
  button.frame <- tkframe(right.frame, borderwidth=2)
  
  xscr <- tkscrollbar(tree.frame, repeatinterval=5, command = function(...)tkxview(files.list,...),orient="horizontal")
  yScr <- tkscrollbar(tree.frame, command = function(...)tkyview(files.list,...), orient = "vertical")
  files.list <- tklistbox(tree.frame, background = "white", xscrollcommand=function(...)tkset(xscr,...),
                          yscrollcommand=function(...)tkset(yScr,...))
  tkconfigure(files.list, width = "30", height = "10")
  tkgrid(files.list,yScr)
  tkgrid.configure(yScr,sticky="nsw")
  tkgrid(xscr)
  tkgrid.configure(xscr,sticky="new")
  rbValue <- tclVar("IFC")
  
  spot.frame <- tkframe(frameOverall, relief = "groove", borderwidth=2)
  radio.frame <- tkframe(spot.frame)
  
  rb1 <- tkradiobutton(radio.frame, text = "Foreign", value = "Foreign", variable = rbValue, command = function(){active.options(FALSE)})
  rb2 <- tkradiobutton(radio.frame, text = "IFC", value = "IFC", variable = rbValue, command = function(){active.options(TRUE)})
  tkgrid(tklabel(spot.frame,text="Input"), padx = "10", pady = "2")
  tkgrid(rb1, rb2, sticky = "w", pady = "2",padx="70")
  tkgrid(radio.frame,padx="5", pady = "10")
  tkgrid(tklabel(spot.frame, text = "Fill the text entries with the correct columns in your file"), columnspan=11)
  columns.frame <- tkframe(spot.frame)
  symbol.frame <- tkframe(spot.frame)
  Cy3 <- tclVar("")
  Cy5 <- tclVar("")
  BgCy3 <- tclVar("")
  BgCy5 <- tclVar("")
  Id <- tclVar("")
  Symbol <- tclVar("")
  tkgrid(tklabel(columns.frame,text="Cy3"),tkentry(columns.frame,width="5", textvariable=Cy3, bg = "white"),
         tklabel(columns.frame,text="Cy5"),tkentry(columns.frame,width="5", textvariable=Cy5, bg = "white"),
         tklabel(columns.frame,text="BgCy3"),tkentry(columns.frame,width="5", textvariable=BgCy3, bg = "white"),
         tklabel(columns.frame,text="BgCy5"),tkentry(columns.frame,width="5", textvariable=BgCy5, bg = "white"),
         tklabel(columns.frame,text="Id"),tkentry(columns.frame,width="5", textvariable=Id ,bg = "white"),pady=20,padx="4")
  tkgrid(tklabel(symbol.frame,text="Symbol"),tkentry(symbol.frame,width="5",textvariable=Symbol, bg = "white"), tklabel(symbol.frame,text="[optional]"))
  tkgrid(columns.frame)
  tkgrid(symbol.frame)
  
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
  
  tkgrid(titulo2, columnspan=11,pady=10)
  tkgrid(rows.label, rows.entry, columns.label, columns.entry, meta.rows.label,meta.rows.entry, meta.columns.label, meta.columns.entry)
  active.options(TRUE)
  tkgrid(rc.frame,padx="10", padx = "10")
  tkgrid(spot.frame,padx="5", pady = "10")
  
  lower.frame <- tkframe(frameOverall,relief="groove",borderwidth=2)
  tkgrid(tklabel(lower.frame,text="Output"), padx = "10", pady = "10")
  output.frame <- tkframe(lower.frame)
  project.entry <- tkentry(output.frame,width="30", bg = "white", textvariable= tclVar(""))
#  name.project <-  tclvalue(tkget(project.entry))
 
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
  
  
  files.frame <- tkframe(tree.frame)
  
  onDelete <-  function(){
    selected.file <-  tkcurselection(files.list)
    if(length(as.character(selected.file)) > 0)
      tkdelete(files.list, as.integer(selected.file))
  }
  
  onAdd <-  function(){
    tempo <- tclvalue(tkgetOpenFile(filetypes = "{{CSV Files} {.csv}} {{TXT Files} {.txt}} {{All files} *}"))
    tempo <- unlist(strsplit(tempo," "))
    for(i in 1:length(tempo)){    
      if(nchar(tempo[i])){
        tkinsert(files.list,"end",tempo[i])
      }
    }
  }
  
  files.button.frame <- tkframe(left.frame)
  delete.button <-  tkbutton(files.button.frame, text = "Delete file", command = onDelete)
  add.button <-  tkbutton(files.button.frame, text = "Add file", command = onAdd)
  tkgrid(tklabel(left.frame,text = "Select the swap dye data file pair with \nwhich you will perform the analysis"), padx = "10", pady = "10")
  tkgrid(tree.frame, pady = "10")
  tkgrid(add.button, delete.button, padx = "20")
  tkgrid(files.button.frame, pady= "10")
  ok.but <- tkbutton(button.frame, text ="OK", command = function(){
    tkconfigure(tt, cursor="watch")
    operacion()
    tkconfigure(tt, cursor="arrow")
    tkdestroy(tt)
  })

  cancel.but <- tkbutton(button.frame, text ="Cancel", command = function(){
    tkdestroy(tt)
    genArise()})
  tkgrid(ok.but, cancel.but, padx = "20")
  tkgrid(frameOverall,padx = "10", pady = "10")

  tkgrid(left.frame,right.frame, pady = "10")
  tkgrid(upper.frame)
  tkgrid(button.frame,pady="30")
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
  
  ##############################################################

  ##############################################################
  operacion <- function(){
    values <- get.paths()
    if(!nchar(tclvalue(tkget(project.entry))) ||
       !nchar(tclvalue(graphic.file)) || !nchar(tclvalue(output.file))){
      tkmessageBox(message= "No output file was specified! ", icon = "error", default = "ok")
    }
    else{
      name.project <-  tclvalue(tkget(project.entry))
      assign("path", values[1], envir = envir)
      name.tmp <- unlist(strsplit(values[1], paste("\\", .Platform$file.sep, sep = "")))
      tmp <- unlist(strsplit(name.tmp[length(name.tmp)], "\\."))
      name.tmp <- paste(tmp[-length(tmp)],sep=".",collapse=".")
      assign("spot.name", name.tmp, envir = envir)
      set.path.project(name.project, tclvalue(output.file),
                       tclvalue(graphic.file), envir)
      
      assign("op.counter",0,envir = envir)
      if(as.character(tclvalue(rbValue)) == "IFC"){
        if(as.character(tclvalue(Symbol)) == ""){
          make.swap(values[1],values[2],as.numeric(tclvalue(Cy3)),as.numeric(tclvalue(Cy5)),as.numeric(tclvalue(BgCy3)),as.numeric(tclvalue(BgCy5)),as.numeric(tclvalue(Id)), Symdesc=NULL, header = TRUE, is.ifc = TRUE,envir=envir,0,0)
        }
        else{
          make.swap(values[1],values[2],as.numeric(tclvalue(Cy3)),as.numeric(tclvalue(Cy5)),as.numeric(tclvalue(BgCy3)),as.numeric(tclvalue(BgCy5)),as.numeric(tclvalue(Id)), as.numeric(tclvalue(Symbol)), header = TRUE, is.ifc = TRUE,envir=envir,0,0)
        }
      }else{
        if(as.character(tclvalue(Symbol)) == ""){
          make.swap(values[1],values[2],as.numeric(tclvalue(Cy3)),as.numeric(tclvalue(Cy5)),as.numeric(tclvalue(BgCy3)),as.numeric(tclvalue(BgCy5)),as.numeric(tclvalue(Id)), Symdesc=NULL, header = FALSE, is.ifc = FALSE,envir=envir,as.numeric(tclvalue(Row)),as.numeric(tclvalue(Col)))
        }
        else{
          make.swap(values[1],values[2],as.numeric(tclvalue(Cy3)),as.numeric(tclvalue(Cy5)),as.numeric(tclvalue(BgCy3)),as.numeric(tclvalue(BgCy5)),as.numeric(tclvalue(Id)), as.numeric(tclvalue(Symbol)), header = FALSE, is.ifc = FALSE,envir=envir,as.numeric(tclvalue(Row)),as.numeric(tclvalue(Col)))
        }
      }
      create.project(name.project,tclvalue(output.file), tclvalue(graphic.file))
      history.project <- paste(name.project,".prj", sep = "")
      set.history.project(history.project, "Name:", get("spot.name", envir = envir))
      set.history.project(history.project, "Rows", get("nr.global", envir = envir))
      set.history.project(history.project, "Columns", get("nc.global", envir = envir))
      set.history.project(history.project, "MetaRows", get("nmr.global", envir = envir))
      set.history.project(history.project, "MetaColumns", get("nmc.global", envir = envir))
      assign("history.project",history.project, envir = envir)
      write.spot(get("o.spot", envir = envir), paste(get("path.results", envir = envir),
                                 .Platform$file.sep, "original.txt",sep = ""))
      set.history.project(get("history.project", envir = envir), "Original Spot", "original.txt")
      principal(envir,1)
    }
  }
}

