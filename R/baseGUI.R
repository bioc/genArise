# Windows for diagnostic plots and the analysis
require(tkrplot)
require(xtable)

back.gui <- function(envir){
    answer <- as.character(tkmessageBox(message = "All the performed operations will be deleted from this project. Are you sure?",
                                        icon = "question", type = "yesnocancel", default = "yes"))
    if( answer == "yes" ){
      proj.file <- read.table(get("history.project", envir = envir),header=FALSE,sep="\t")
      proj.tmp <- proj.file[1:(length(proj.file[,1])-op.counter),]
      op.counter <<- 0
      reset.history(get("history.project", envir = envir),proj.tmp)}
  }

principal <-  function(envir){
  tclRequire("BWidget")
  tt <- tktoplevel()
  o.spot <- get("o.spot", envir = envir)
  datos <- attr(o.spot, "spotData")
  M <- log(datos$Cy5, 2) - log(datos$Cy3, 2)
  tkwm.geometry(tt,"+50+50")
  tkwm.title(tt,"GenArise Microarray Analyzer")

  lower.menu <- tkframe(tt, relief = "groove", borderwidth=2)
  
  image.menu.frame <- tkframe(lower.menu)
  
  tkcmd("image","create","photo","analysis",file=file.path(get("icons.dir", envir = envir),"icons/analysis.png"))
  analysis.button <- tkbutton(image.menu.frame,image="analysis", command = function(){ follow.wizard()})
  tkconfigure(analysis.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","acroread",file= file.path(get("icons.dir", envir = envir),"icons/acroread.png"))
  acroread.button <- tkbutton(image.menu.frame,image="acroread", command = function(){save.as.pdf()}) 
  tkconfigure(acroread.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","editor",file=file.path(get("icons.dir", envir = envir),"icons/editor.png"))
  editor.button <- tkbutton(image.menu.frame,image="editor", command = function() note(envir))
  tkconfigure(editor.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","help",file=file.path(get("icons.dir", envir = envir),"icons/help.png"))
  help.button <- tkbutton(image.menu.frame,image="help", command = function() help())
  tkconfigure(help.button, heigh = 16, width = 16)

  tkcmd("image","create","photo","exit",file=file.path(get("icons.dir", envir = envir),"icons/logout.png"))
  exit.button <- tkbutton(image.menu.frame,image="exit", command = function() tkdestroy(tt))
  tkconfigure(exit.button, heigh = 16, width = 16)
  
  tkgrid(analysis.button, acroread.button, editor.button, help.button, exit.button, sticky = "nw")
  
  etiqueta <- tklabel(lower.menu, text = "")
  tkconfigure(etiqueta, text = "", width = "90") 
  tkgrid(image.menu.frame, etiqueta, sticky = "w")
  tkgrid(lower.menu)
  
  label.function <-  function(texto.etiqueta) tkconfigure(etiqueta, text = texto.etiqueta, width = "90")
  
  
  tkbind(analysis.button, "<Enter>", function() label.function("Make analysis data"))
  tkbind(analysis.button, "<Leave>", function() label.function(""))
  
  tkbind(acroread.button, "<Enter>", function() label.function("Save graphic as PDF"))
  tkbind(acroread.button, "<Leave>", function() label.function(""))
  
  tkbind(editor.button, "<Enter>", function() label.function("Notes about this experiment"))
  tkbind(editor.button, "<Leave>", function() label.function(""))
  
  tkbind(help.button, "<Enter>", function() label.function("Display the genArise's help"))
  tkbind(help.button, "<Leave>", function() label.function(""))

  tkbind(exit.button, "<Enter>", function() label.function("Exit GenArise GUI"))
  tkbind(exit.button, "<Leave>", function() label.function(""))

  
  follow.wizard <- function(){
    respuesta <- as.character(tkmessageBox(message = "Do you want to follow the wizard?",
                                           icon = "question", type = "yesnocancel", default = "yes"))
    
    if(respuesta == "yes"){ # tambien debemos hacer inactivo el menu para el caso de windows
      tkconfigure(tt,cursor="watch")
      tkentryconfigure(topMenu, "0", state ="disable" )
      tkentryconfigure(topMenu, "1", state ="disable" )
      tkentryconfigure(topMenu, "2", state ="disable" )
      tkentryconfigure(topMenu, "3", state ="disable" )
      tkentryconfigure(topMenu, "4", state ="disable" )
      analysis.window("", follow.wizard = TRUE, envir)
      tkdestroy(tt)
      
    }else{
      if(respuesta == "no"){
        tkconfigure(tt,cursor="watch")
        analysis.window(tclvalue(tkget(txt,"0.0","end")), follow.wizard = FALSE, envir)
        tkdestroy(tt)
      }
    }
  }
  
  topMenu <- tkmenu(tt)
  tkconfigure(tt,menu=topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(fileMenu,"command",label="Quit", command=function() tkdestroy(tt))
  tkadd(topMenu,"cascade",label="File", menu=fileMenu)
  tkadd(topMenu,"command",label="Back", command = function(){
    tkdestroy(tt)
    genArise()
  })
  
  tkadd(topMenu,"command",label="Analyze", command = function(){
    follow.wizard()
  })
  
  save.as.pdf <-  function(){
    name <- tclvalue(tkgetSaveFile(initialdir = get("path.graphics", envir = envir),
                                   initialfile=get("spot.name",envir=envir),filetypes="{{PDF Files} {.pdf}} {{All files} *}"))
    if (!nchar(name))
      tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
    else{
      rbVal <- as.character(tclvalue(rbValue))
      if(rbVal == "Red & Green"){
        pdf(paste(name, "R&G.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "R&G.pdf", sep = "_"))
        imageLimma(log(datos$Cy5, 2) - log(datos$Cy3, 2), row = get("nr", envir = envir), column = get("nc", envir = envir),
                   meta.row = get("nmr", envir = envir),meta.column = get("nmc", envir = envir))
        dev.off(dev.cur())
      }
      else{
        if(rbVal == "Red"){
          pdf(paste(name, "R.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "R.pdf", sep = "_"))
          imageLimma(log(datos$Cy5, 2) - log(datos$Cy3, 2), get("nr", envir = envir), get("nc", envir = envir),
                     get("nmr", envir = envir),
                     get("nmc", envir = envir), low = "white", high = "red")
          dev.off(dev.cur())
        }
        else{
          pdf(paste(name, "G.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "G.pdf", sep = "_"))
          imageLimma(log(datos$Cy5, 2) - log(datos$Cy3, 2),get("nr", envir = envir), get("nc", envir = envir),
                     get("nmr", envir = envir),
                     get("nmc", envir = envir), low = "white", high = "green")
          dev.off(dev.cur())
        }
      }
    }
  }
  
  optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function(){
    save.as.pdf()})
  tkadd(optionsMenu,"command",label="Note",command = function(){
    note(envir)
  })
  
  tkadd(topMenu,"cascade",label="Options", menu =optionsMenu)
  helpMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(helpMenu,"command",label="About GenArise",command = function() help())
  tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
  upper.frame <- tkframe(tt)
  img <- tkrplot(upper.frame, fun = function() imageLimma(M, get("nr", envir = envir), get("nc", envir = envir),
                                get("nmr", envir = envir),
                                get("nmc", envir = envir)), hscale= get("Myhscale", envir = envir),vscale = get("Myvscale",
                                                                                                     envir = envir))
  frameOverall <- tkframe(upper.frame)
  frameUpper <- tkframe(frameOverall,relief="groove",borderwidth=2)
  tkgrid(tklabel(frameUpper, text = "Spot Features", font = "Helvetica 12"), pady = "7")
  tkgrid(tklabel(frameUpper, text = paste("Name:   ", get("spot.name", envir = envir))), sticky = "w", pady = "2")
  tkgrid(tklabel(frameUpper, text = paste("Rows:           ", get("nr", envir = envir))), sticky = "w", pady = "2")
  tkgrid(tklabel(frameUpper, text = paste("Columns:       ", get("nc", envir = envir))), sticky = "w", pady = "2")
  tkgrid(tklabel(frameUpper, text = paste("MetaRows  :     ", get("nmr", envir = envir))), sticky = "w", pady = "2")
  tkgrid(tklabel(frameUpper, text = paste("MetaColumns:  ", get("nmc", envir = envir))), sticky = "w", pady = "2")
  tkgrid(frameUpper, padx  = "10", pady = "10")
  
  
  radio.frame <- tkframe(frameOverall,relief="groove",borderwidth=2)

  # diagnostic plots
  rbValue <- tclVar("Red & Green")
  rb1 <- tkradiobutton(radio.frame, text = "Red & Green", value = "Red & Green", variable = rbValue,
                       command = function() {tkrreplot(img, fun = function() imageLimma(M,
                                                              get("nr", envir = envir),
                                                              get("nc", envir = envir),
                                                              get("nmr", envir = envir),
                                                              get("nmc", envir = envir)))})
  rb2 <- tkradiobutton(radio.frame, text = "Red", value = "Red", variable = rbValue,
                       command = function() {tkrreplot(img, fun = function()imageLimma(log(datos$BgCy3,2),
                                                              get("nr", envir = envir),
                                                              get("nc", envir = envir),
                                                              get("nmr", envir = envir),
                                                              get("nmc", envir = envir), low = "white", high = "red"))})
  rb3 <- tkradiobutton(radio.frame, text = "Green", value = "Green", variable = rbValue,
                       command = function() {tkrreplot(img, fun = function()imageLimma(log(datos$BgCy5, 2),
                                                              get("nr", envir = envir),
                                                              get("nc", envir = envir),
                                                              get("nmr", envir = envir),
                                                              get("nmc", envir = envir), low = "white", high = "green"))})
  tkgrid(rb1, sticky = "w", pady = "2",padx="10")
  tkgrid(rb2, sticky = "w", pady = "2",padx="10")
  tkgrid(rb3, sticky = "w", pady = "2",padx="10")
  tkgrid(frameUpper, padx  = "20", pady = "120")
  tkgrid(radio.frame, padx  = "20", pady = "10")
  tkgrid(img, frameOverall, sticky = "nw")
  tkgrid(upper.frame, pady = "10")
  
  area.frame <- tkframe(tt,relief="groove",borderwidth=2)
  yscr <- tkscrollbar(area.frame, repeatinterval = 5,
                      command=function(...)tkyview(txt,...))
  txt <- tktext(area.frame,bg="white",font="courier",width ="70",height="5",yscrollcommand=function(...)tkset(yscr,...))
  tkgrid(txt, yscr, sticky = "w")
  tkgrid.configure(yscr,rowspan=4,sticky="ns")
  tkinsert(txt,"end","GenArise Microarray Analyzer\n")
  tkinsert(txt,"end","Institute of Cellular Physiology UNAM")
  tkconfigure(txt, state="disabled")
  tkgrid(area.frame, padx = "10", pady = "10", sticky = "w")
}

# Auxiliar function to call the Zscore.plot function in the GUI
Zscore.points <-  function(type="ri",text, envir){
  tt <- tktoplevel()
  tkwm.title(tt,"GenArise Microarray Analyzer")
  topMenu <- tkmenu(tt)
  tkconfigure(tt,menu=topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  upper.frame <- tkframe(tt)
  sd.frame <- tkframe(upper.frame, borderwidth=2)
  ratio.buttons.frame <- tkframe(sd.frame,relief="groove",borderwidth=2)
  dist  <- tclVar(5)
  
  radio1 <- tkradiobutton(ratio.buttons.frame, text="Zscore < 1",
                          value=1, variable=dist, command=function(){
                            tkrreplot(img, fun = function()
                                      Zscore.plot(get("Zscore.spot", envir = envir), all = FALSE, Zscore.max = 1, col = "green"))})
  radio2 <- tkradiobutton(ratio.buttons.frame, text="1 < Zscore < 1.5",
                          value=2, variable=dist,command = function(){
                            tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = envir),
                                             Zscore.max = 1.5, Zscore.min = 1, all = FALSE, col = "blue"))})
  radio3 <- tkradiobutton(ratio.buttons.frame, text="1.5 < Zscore < 2",
                          value=3, variable=dist,command = function(){
                            tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = envir),
                                             Zscore.max = 2, Zscore.min = 1.5, all = FALSE, col = "cyan"))})
  radio4 <- tkradiobutton(ratio.buttons.frame, text="Zscore > 2",
                          value=4, variable=dist, command=function(){
                            tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = envir),
                                             Zscore.min = 2, all = FALSE, col = "snow"))})
  radio5 <- tkradiobutton(ratio.buttons.frame, text="All",
                          value=5, variable=dist, command=function(){
                            tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = envir)))})
  
  tkgrid(radio1, sticky="w")
  tkgrid(radio2, sticky="w")
  tkgrid(radio3, sticky="w")
  tkgrid(radio4, sticky="w")
  tkgrid(radio5, sticky="w")

  # get the information for the annotation function
  annotations.but <- tkbutton(sd.frame,text = "Annotations", command=function(){
    choose.specie <- tktoplevel()
    tkwm.title(choose.specie,"Specie Selector")
    tkgrid(tklabel(choose.specie,text="Choose the specie"))
    species <- c("Mus musculus","Saccharomyces Cerevisiae","Rattus norvegicus", "NCBI")
    comboBox <- tkwidget(choose.specie, "ComboBox",editable=FALSE,values=species)
    tkgrid(comboBox)
    
    OnOK <- function()
      {
        datos <- attr(get("Zscore.spot", envir = envir), "dataSets")
        kk <- as.data.frame(datos)
        kk <- apply(kk, 2, as.vector)
        la.especie <- as.numeric(tclvalue(tkcmd(comboBox,"getvalue")))
        annotations(kk, species[(la.especie + 1)], 3, file.path(get("path.project", envir = envir), "annotations.htm"))
        tkdestroy(choose.specie)
      }
    
    OnCancel <-  function(){
      tkdestroy(choose.specie)
    }
    
    OK.but <-tkbutton(choose.specie,text="   OK   ",command=OnOK)
    Cancel.but <-tkbutton(choose.specie,text="   Cancel   ",command=OnCancel)
    tkgrid(OK.but, Cancel.but, padx = "20", pady = "10")
    tkfocus(tt)
  })
  
  tkgrid(ratio.buttons.frame, sticky="w", pady = "20")
  tkgrid(annotations.but, sticky="w", pady = "10")
  
  tkselect(radio5)
  tkadd(fileMenu,"command",label="Close",command=function(){
    tkdestroy(tt)
    genArise.init(envir)})
  tkadd(fileMenu,"command",label="Exit",command=function() tkdestroy(tt))
  tkadd(topMenu,"cascade",label="File",menu=fileMenu)
  tkadd(topMenu,"command",label="Back", command = function(){
    back.gui(envir)
    tkdestroy(tt)
    principal(envir)
  })
  
  optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
  
  make.file.name <- function(file.type, ext){
    switch(as.integer(tclvalue(dist)),
           suggested.name <- paste(get("spot.name",envir=envir), "_lower_1.", ext, sep = ""),
           suggested.name <- paste(get("spot.name",envir=envir), "_1-1_5.", ext, sep = ""),
           suggested.name <- paste(get("spot.name",envir=envir), "_1_5-2.", ext, sep = ""),
           suggested.name <- paste(get("spot.name",envir=envir), "_greater_2.", ext, sep = ""),
           suggested.name <- paste(get("spot.name",envir=envir), "_all.", ext, sep = ""),
           )
    if(ext == "pdf"){
      salida <- "path.graphics"
    }else{
      salida <-  "path.results"
    }
    tclvalue(tkgetSaveFile(initialdir = get(salida, envir = envir),
                           initialfile = suggested.name, filetypes = file.type))     
  }    
  tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function() {
    name <- make.file.name(file.type = "{{PDF Files} {.pdf}} {{All files} *}", ext ="pdf")
    if (!nchar(name))
      tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
    else{
      pdf(name, horiz = F, height = 8, width = 8, title = name)
      switch(as.integer(tclvalue(dist)),
             Zscore.plot(get("Zscore.spot", envir = envir), all = FALSE, Zscore.max = 1, col = "green"),
             Zscore.plot(get("Zscore.spot", envir = envir), Zscore.max = 1.5, Zscore.min = 1, all = FALSE, col = "blue"),
             Zscore.plot(get("Zscore.spot", envir = envir), Zscore.max = 2, Zscore.min = 1.5, all = FALSE, col = "cyan"),
             Zscore.plot(get("Zscore.spot", envir = envir), Zscore.min = 2, all = FALSE, col = "snow"),
             Zscore.plot(get("Zscore.spot", envir = envir)))
      dev.off(dev.cur())
    }
  })

  # save the Zscore for the data set in a file
  tkadd(optionsMenu,"command",label="Write as outputfile",command = function() {
    name <- make.file.name(file.type = "{{TXT Files} {.txt}} {{All files} *}", ext="txt")
    if (!nchar(name))
      tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
    else{
      val <- as.integer(tclvalue(dist))
      if( val == 5){
        write.table(attr(get("Zscore.spot", envir = envir), "dataSets"),
                    name, quote = FALSE, col.names = FALSE, row.names = FALSE)
      }else if(val == 4){
        write.dataSet(get("Zscore.spot", envir = envir), name, Zscore.min = 2)
      }
      else if(val == 3){
        write.dataSet(get("Zscore.spot", envir = envir), name, Zscore.min = 1.5, Zscore.max = 2)
      }
      else if(val == 2){
        write.dataSet(get("Zscore.spot", envir = envir), name, Zscore.min = 1, Zscore.max = 1.5)
      }
      else if(val == 1){
        write.dataSet(get("Zscore.spot", envir = envir), name, Zscore.max = 1)
      }
    }})
  
  tkadd(optionsMenu,"command",label="Notes",command = function()note(envir))
  tkadd(topMenu,"cascade",label="Options",menu =optionsMenu)
  
  helpMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(helpMenu,"command",label="About genArise...", command=function() help())
  tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
  
  
  frame.label <- tkframe(tt,relief="groove",borderwidth=2)
  tkfocus(tt)
  img <- tkrplot(upper.frame,fun = function()Zscore.plot(get("Zscore.spot", envir = envir),type),
                 hscale= get("Myhscale", envir = envir),vscale = get("Myvscale",envir = envir))
  tkgrid(img,sd.frame, padx = "10", pady = "10")
  tkgrid(upper.frame, pady = "10")
  area.frame <- tkframe(tt,relief="groove",borderwidth=2)
  yscr <- tkscrollbar(area.frame, repeatinterval = 5,
                      command=function(...)tkyview(txt,...))
  
  txt <- tktext(area.frame,bg="white",font="courier",width ="70",height="5",yscrollcommand=function(...)tkset(yscr,...))
  tkgrid(txt, yscr)
  tkgrid.configure(yscr,rowspan=4,sticky="nsw")
  tkinsert(txt,"end",text)
  tkinsert(txt,"end","\n\nZscore Done!!")
  tkconfigure(txt, state="disabled")
  tkgrid(area.frame, padx = "10")
}

# Window for the analysis
analysis.window <-  function(texto, follow.wizard = FALSE, envir){
  assign("corrected", 0, envir = envir)
  my.file <- paste(get("name.project", envir = envir), ".prj", sep = "")

  # check if the background correction will be done
  bg.question <- function(){
    answer <-tkmessageBox(message="Do you want to make background correction?",icon="question",type="yesnocancel",default="yes")
    ans <- as.character(answer)
    op.counter <<- op.counter + 1
    write.spot(get("o.spot", envir = envir), paste(get("path.results", envir = envir),
                               .Platform$file.sep, "original.txt",sep = ""))
    set.history.project(get("history.project", envir = envir), "Original Spot", "original.txt")
    
    if(ans == "yes"){
      c.spot <- bg.correct(get("a.spot", envir = envir))
      op.counter <<- op.counter + 1
      tkconfigure(txt, state="normal")
      tkinsert(txt,"end","\n\nBackground correction done.............")
      tkconfigure(txt, state="disable")
      assign("c.spot", c.spot, envir = envir)
      write.spot(get("c.spot", envir = envir), paste(get("path.results", envir = envir),
                                 .Platform$file.sep, "corrected.txt",sep = ""))
      set.history.project(get("history.project", envir = envir), "Corrected Spot", "corrected.txt")
      assign("a.spot", c.spot, envir = envir)
      tkpack(corr.radio,anchor="w",padx="10")
      tkselect(corr.radio)
      assign("corrected",1, envir = envir)
    }
    else {
      if(ans == "no"){
        assign("corrected",1, envir = envir)
      }
      else{
        assign("corrected",2, envir = envir)
      }
    }
  }

  # GUI behavior after the data normalization
  normalized.gui <- function(type = 1, history.entry, analysis.plot){ # 1 by grid, 2 global
    if(get("corrected",envir=envir) == 0) bg.question()
    if(get("corrected",envir = envir) == 1){
      tkconfigure(tt,cursor="watch")
      switch(type,
             {n.spot <- grid.norm(get("a.spot", envir = envir),
                                  get("nr", envir = envir), get("nc", envir = envir))
              op.counter <<- op.counter + 1
              history.text <- "\n\nNormalization by grid done!..."},
             {n.spot <- global.norm(get("a.spot", envir = envir))
              history.text <- "\n\nGlobal normalization done!..."})
      tkconfigure(history.entry, state="normal")
      tkinsert(history.entry,"end",history.text)
      tkconfigure(history.entry, state="disable")
      assign("n.spot", n.spot, envir = envir)
      write.spot(get("n.spot", envir = envir), paste(get("path.results", envir = envir),
                                 .Platform$file.sep, "normalized.txt",sep = ""))
      set.history.project(get("history.project", envir = envir), "Normalized Spot", "normalized.txt")
      assign("a.spot", n.spot, envir = envir)
      tkrreplot(analysis.plot,fun=function()graphic.choose(get("a.spot", envir = envir),
                                get("graphic.type", envir = envir)))
      tkpack(normal.radio,anchor="w",padx="10")
      tkselect(normal.radio)
      tkentryconfigure(topMenu, "4", state ="disable" )
      tkconfigure(normal.button, state = "disable")
      tkconfigure(tt,cursor="arrow")
    }
  }

  # GUI behavior after the data filtering
  filter.gui <-  function(history.text, analysis.plot){
    if(get("corrected",envir=envir) == 0) bg.question()
    tkconfigure(tt,cursor="watch")
    f.spot <- filter.spot(get("a.spot", envir = envir))
    op.counter <<- op.counter + 1
    tkconfigure(history.text, state="normal")
    tkinsert(history.text,"end","\n\nIntensity-based filtering done.............")
    tkconfigure(history.text, state="disable")
    assign("f.spot", f.spot, envir = envir)
    write.spot(get("f.spot", envir = envir), paste(get("path.results", envir = envir),
                               .Platform$file.sep, "filter.txt",sep = ""))
    set.history.project(get("history.project", envir = envir), "Filtered Spot", "filter.txt")
    assign("a.spot", f.spot, envir = envir)
    tkrreplot(analysis.plot,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))
    tkpack(fil.radio,anchor="w",padx="10")
    tkselect(fil.radio)
    tkentryconfigure(filterMenu, "0", state ="disable" )
    tkconfigure(filter.button, state = "disable")
    count.filter <<- count.filter +1
    tkentryconfigure(normalMenu, "0", state ="disable" )
    if(count.filter == 2){
      tkentryconfigure(topMenu, "3", state ="disable" )}
    tkconfigure(tt,cursor="arrow")
  }
  
  # GUI behavior after remove replicates
  # 1 mean replicate filtering, 2 non-extreme values replicate, 3 geometric mean
  remove.duplicates.gui <-  function(type = 1, history.entry, analysis.plot){
    if(get("corrected",envir=envir) == 0) bg.question()
    if(get("corrected",envir=envir) == 1){
      tkconfigure(tt,cursor="watch")
      switch(type,
             {u.spot <- meanUnique(get("a.spot", envir = envir))
              op.counter <<- op.counter + 1
              history.text <-  "\n\nMean replicates filtering done!..."
              history.project.text <- "Mean replicates filter"},
             {u.spot <- alter.unique(get("a.spot", envir = envir))
              op.counter <<- op.counter + 1
              history.text <- "\n\nNon-extreme values replicate"
              history.project.text <- "Non-extreme filter dup"},
             {u.spot <- spotUnique(get("a.spot", envir = envir))
              op.counter <<- op.counter + 1
              history.text <- "\n\nGeometric mean filtering done!..."
              history.project.text <- "Geometric mean filter dup"})
      tkconfigure(history.entry, state="normal")
      tkinsert(history.entry,"end", history.text)
      tkconfigure(history.entry, state="disable")
      assign("u.spot", u.spot, envir = envir)
      write.spot(get("u.spot", envir = envir), paste(get("path.results", envir = envir),
                                 .Platform$file.sep, "withoutDup.txt",sep = ""))
      set.history.project(get("history.project", envir = envir), history.project.text, "withoutDup.txt")
      assign("a.spot", u.spot, envir = envir)
      tkrreplot(analysis.plot,fun=function()graphic.choose(get("a.spot", envir = envir),
                                get("graphic.type", envir = envir)))
      tkpack(uniq.radio,anchor="w",padx="10")
      tkselect(uniq.radio)
      tkentryconfigure(filterMenu, "1", state ="disable" )
      count.filter <- count.filter + 1
      tkentryconfigure(normalMenu, "0", state ="disable" )
      if(count.filter == 2){
        tkentryconfigure(topMenu, "3", state ="disable" )}}
    tkconfigure(tt,cursor="arrow")
  }

  # GUI behavior after perform Zscore
  zscore.gui <- function(){
    tkconfigure(tt,cursor="watch")
    assign("Zscore.spot", Zscore(get("a.spot",envir=envir)), envir = envir)
    write.zscore(get("Zscore.spot", envir = envir), file.path(get("path.results", envir = envir), "zscore.txt"))
    set.history.project(get("history.project", envir = envir), "RI Zscore", "zscore.txt")
    Zscore.points(type="ri",text=tclvalue(tkget(txt,"0.0","end")), envir)
    tkdestroy(tt)
  }
  
  count.filter <-  0
  tt <- tktoplevel()
  tkwm.title(tt,"GenArise Microarray Analyzer")
  topMenu <- tkmenu(tt)
  tkconfigure(tt,menu=topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  upper.frame <- tkframe(tt)
  frame1 <- tkframe(upper.frame,relief="groove",borderwidth=2)
  dist  <- tclVar(1)
  ori.radio <- tkradiobutton(frame1, text="Original Spot",
                             value=1, variable=dist, command=function(){
                               assign("a.spot", get("o.spot", envir = envir), envir = envir)
                               tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  corr.radio <- tkradiobutton(frame1, text="Corrected Spot",
                              value=2, variable=dist,command = function(){
                                assign("a.spot", get("c.spot", envir = envir), envir = envir)
                                tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  
  normal.radio <- tkradiobutton(frame1, text="Normal Spot",
                                value=3, variable=dist,command = function(){
                                  assign("a.spot", get("n.spot", envir = envir), envir = envir)
                                  tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  
  fil.radio <- tkradiobutton(frame1, text="Filter Spot",
                             value=4, variable=dist,command = function(){
                               assign("a.spot", get("f.spot", envir = envir), envir = envir)
                               tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  uniq.radio <- tkradiobutton(frame1, text="Without duplicates",
                              value=5, variable=dist,command = function(){
                                assign("a.spot", get("u.spot", envir = envir), envir = envir)
                                tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  tkpack(ori.radio,anchor="w",padx="10")
  
  tkadd(fileMenu,"command",label="Close",command=function(){
    tkdestroy(tt)
    genArise.init(envir)})
  tkadd(fileMenu,"command",label="Exit",command=function() tkdestroy(tt))
  tkadd(topMenu,"cascade",label="File",menu=fileMenu)

  tkadd(topMenu,"command",label="Back", command = function(){
    back.gui(envir)
    tkdestroy(tt)
    principal(envir)
    
  }) 
  filterMenu <- tkmenu(topMenu,tearoff=FALSE)
  
  tkadd(filterMenu,"command",label="By Intensity", state ="active",command = function() {
    filter.gui(txt, img)})
  
  duplicatesMenu <- tkmenu(filterMenu,tearoff=FALSE)
  tkadd(duplicatesMenu,"command",label="Mean Replicate Filtering", command = function(){
    remove.duplicates.gui(1,txt,img)})
  tkadd(duplicatesMenu,"command",label="Non-extreme values replicate filtering", command = function(){
    remove.duplicates.gui(2, txt, img)})
  tkadd(duplicatesMenu,"command",label="Geometric Mean Filter", command = function(){
    remove.duplicates.gui(3, txt, img)})
  tkadd(filterMenu,"cascade",label="Duplicates Analysis", state ="active",menu= duplicatesMenu)
  
  tkadd(topMenu,"cascade",label="Filter",menu =filterMenu)
  
  normalMenu <- tkmenu(topMenu,tearoff=FALSE)
  
  tkadd(normalMenu,"command",label="By grid", state ="active", command=function(){
    normalized.gui(1,txt, img)
  })
  tkadd(normalMenu,"command",label="Global", state ="active", command=function(){
    normalized.gui(2,txt, img)
  })

  cys.plot <-  function(){
    assign("graphic.type", 1, envir = envir)
    tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))}

  tkadd(topMenu,"cascade",label="Normalize",menu = normalMenu)
  graphicMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(graphicMenu,"command",label="Cy3 vs Cy5",command = function() {
    cys.plot()})
  tkadd(graphicMenu,"command",label="R vs I",command = function() {
    assign("graphic.type", 2, envir = envir)
    tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  tkadd(graphicMenu,"command",label="M vs A",command = function() {
    assign("graphic.type", 3, envir = envir)
    tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
  tkadd(topMenu,"cascade",label="Graphics",menu =graphicMenu)
  
  ZscoreMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(topMenu,"cascade",label="Zscore",menu = ZscoreMenu)
  tkadd(ZscoreMenu,"command",label="R vs I", state="active", command=function(){
    zscore.gui()
  })
  tkadd(ZscoreMenu,"command",label="M vs A", state="active",command=function(){
    assign("Zscore.spot", Zscore(get("a.spot",envir=envir), type = "ma"), envir = envir)
    write.zscore(get("Zscore.spot", envir = envir),file.path(get("path.results", envir = envir), "zscore.txt"))
    set.history.project(get("history.project", envir = envir), "MA Zscore", "zscore.txt")
    Zscore.points(type="ma",text=tclvalue(tkget(txt,"0.0","end")), envir)
    tkdestroy(tt)
  })
  
  optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function(){
    name <- tclvalue(tkgetSaveFile(initialdir = get("path.graphics", envir = envir),
                                   initialfile=get("spot.name",envir=envir),
                                   filetypes="{{PDF Files} {.pdf}} {{All files} *}"))
    if (!nchar(name))
      tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
    else{
      rbVal <- as.integer(tclvalue(dist))
      gt <- get("graphic.type", envir = envir)
      switch(rbVal,
             {if(gt == 1){
               pdf(paste(name, "OriginalCy3vsCy5.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "OriginalCy3vsCy5.pdf", sep = "_"))}
              else if(gt == 2){
                pdf(paste(name, "OriginalRvsI.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "OriginalRvsI.pdf", sep = "_"))}
              else{
                pdf(paste(name, "OriginalMvsA.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "OriginalMvsA.pdf", sep = "_"))};
              graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir))
              dev.off(dev.cur())},
             {if(gt == 1){
               pdf(paste(name, "CorrectedCy3vsCy5.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "CorrectedCy3vsCy5.pdf", sep = "_"))}
              else if(gt == 2){
                pdf(paste(name, "CorrectedRvsI.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "CorrectedRvsI.pdf", sep = "_"))}
              else{
                pdf(paste(name, "CorrectedMvsA.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "CorrectedMvsA.pdf", sep = "_"))};
              graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir))
              dev.off(dev.cur())},
             {if(gt == 1){
               pdf(paste(name, "NormalCy3vsCy5.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NormalCy3vsCy5.pdf", sep = "_"))}
              else if(gt == 2){
                pdf(paste(name, "NormalRvsI.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NormalRvsI.pdf", sep = "_"))}
              else{
                pdf(paste(name, "NormalMvsA.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NormalMvsA.pdf", sep = "_"))};
              graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir))
              dev.off(dev.cur())},
             {if(gt == 1){
               pdf(paste(name, "FilterCy3vsCy5.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "FilterCy3vsCy5.pdf", sep = "_"))}
              else if(gt == 2){
                pdf(paste(name, "FilterRvsI.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "FilterRvsI.pdf", sep = "_"))}
              else{
                pdf(paste(name, "FilterMvsA.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "FilterMvsA.pdf", sep = "_"))};
              graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir))
              dev.off(dev.cur())},
             {if(gt == 1){
               pdf(paste(name, "NoDuplicatesCy3vsCy5.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NoDuplicatesCy3vsCy5.pdf", sep = "_"))}
              else if(gt == 2){
                pdf(paste(name, "NoDuplicatesRvsI.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NoDuplicatesRvsI.pdf", sep = "_"))}
              else{
                pdf(paste(name, "NoDuplicatesMvsA.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NoDuplicatesMvsA.pdf", sep = "_"))};
              graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir))
              dev.off(dev.cur())})
    }
  })
  tkadd(optionsMenu,"command",label="Notes",command = function() note(envir))
  tkadd(topMenu,"cascade",label="Options",menu =optionsMenu)
    
  
  helpMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(helpMenu,"command",label="About genArise...", command=function() help())
  tkadd(topMenu,"cascade",label="Help",menu = helpMenu)

  sub.menu.frame <- tkframe(tt, relief = "groove", border = "2")
  
  image.menu.frame <- tkframe(sub.menu.frame, relief = "groove", border = "2")
  
  tkcmd("image","create","photo","filter",file=file.path(get("icons.dir", envir = envir),"icons/filter.png"))
  filter.button <- tkbutton(image.menu.frame,image="filter", command = function(){filter.gui(txt, img)})
  tkconfigure(filter.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","normal",file=file.path(get("icons.dir", envir = envir),"icons/norm.png"))
  normal.button <- tkbutton(image.menu.frame,image="normal", command = function(){normalized.gui(1, txt, img)}) 
  tkconfigure(normal.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","zscore",file=file.path(get("icons.dir", envir = envir),"icons/zscore.png"))
  zscore.button <- tkbutton(image.menu.frame,image="zscore", command = function() zscore.gui())
  tkconfigure(zscore.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","cys",file=file.path(get("icons.dir", envir = envir),"icons/cy3vscy5.png"))
  cys.button <- tkbutton(image.menu.frame,image="cys", command = function() cys.plot())
  tkconfigure(cys.button, heigh = 16, width = 16)

  tkcmd("image","create","photo","acroread",file=file.path(get("icons.dir", envir = envir),"icons/acroread.png"))
  acroread.button <- tkbutton(image.menu.frame,image="acroread", command = function(){save.as.pdf()}) 
  tkconfigure(acroread.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","editor",file=file.path(get("icons.dir", envir = envir),"icons/editor.png"))
  editor.button <- tkbutton(image.menu.frame,image="editor", command = function() note(envir))
  tkconfigure(editor.button, heigh = 16, width = 16)
  
  tkcmd("image","create","photo","help",file=file.path(get("icons.dir", envir = envir),"icons/help.png"))
  help.button <- tkbutton(image.menu.frame,image="help",command = function() help())
  tkconfigure(help.button, heigh = 16, width = 16)

  tkcmd("image","create","photo","back",file=file.path(get("icons.dir", envir = envir),"icons/regresar.png"))
  back.button <- tkbutton(image.menu.frame,image="back",command =  function(){
    back.gui(envir)
    tkdestroy(tt)
    principal(envir)
  } )
  tkconfigure(back.button, heigh = 16, width = 16)
  
  etiqueta <- tklabel(sub.menu.frame, text = "")
  tkconfigure(etiqueta, text = "", width = "80") 
  
  
  tkgrid(filter.button, normal.button, zscore.button, cys.button, acroread.button, editor.button, back.button, help.button, sticky = "nw")
  tkgrid(image.menu.frame, etiqueta, sticky = "w")
  tkgrid(sub.menu.frame)
  
  label.function <-  function(texto.etiqueta) tkconfigure(etiqueta, text = texto.etiqueta, width = "80")
  
  
  tkbind(filter.button, "<Enter>", function() label.function("Filter spots by intensity"))
  tkbind(filter.button, "<Leave>", function() label.function(""))
  
  tkbind(normal.button, "<Enter>", function()  label.function("Grid normalized spot"))
  tkbind(normal.button, "<Leave>", function() label.function(""))
  
  tkbind(zscore.button, "<Enter>", function() label.function("Obtain the z-score value"))
  tkbind(zscore.button, "<Leave>", function() label.function(""))

  tkbind(cys.button, "<Enter>", function() label.function("Plot Cy5 vs Cy3 values"))
  tkbind(cys.button, "<Leave>", function() label.function(""))
  
  tkbind(acroread.button, "<Enter>", function() label.function("Save graphic as PDF"))
  tkbind(acroread.button, "<Leave>", function() label.function(""))
  
  tkbind(editor.button, "<Enter>", function() label.function("Notes about this experiment"))
  tkbind(editor.button, "<Leave>", function() label.function(""))
 
  tkbind(back.button, "<Enter>", function() label.function("Back to the last window"))
  tkbind(back.button, "<Leave>", function() label.function(""))
  
  tkbind(help.button, "<Enter>", function() label.function("Help about genArise"))
  tkbind(help.button, "<Leave>", function() label.function(""))

  
  
  frame.label <- tkframe(tt,relief="groove",borderwidth=2)
  tkfocus(tt)
  assign("a.spot", get("o.spot", envir = envir), envir = envir)
  img <- tkrplot(upper.frame,fun = function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)), hscale= get("Myhscale", envir = envir),vscale = get("Myvscale",
                                                                                                                                                                        envir = envir))
  tkgrid(img,frame1,padx="10",pady="10")
  tkgrid(upper.frame, pady = "10")
  
  area.frame <- tkframe(tt,relief="groove",borderwidth=2)
  yscr <- tkscrollbar(area.frame, repeatinterval = 5, command=function(...)tkyview(txt,...))
  txt <- tktext(area.frame,bg="white",font="courier",width ="70",height="5",yscrollcommand=function(...)tkset(yscr,...))
  tkgrid(txt, yscr)
  tkgrid.configure(yscr,rowspan=4,sticky="nsw")
  tkinsert(txt,"end",texto)
  tkconfigure(txt, state="disabled")
  tkgrid(area.frame, padx = "10")
  if(follow.wizard){
    op.counter <<- op.counter + 1
    write.spot(get("o.spot", envir = envir), paste(get("path.results", envir = envir),
                               .Platform$file.sep, "original.txt",sep = ""))
    set.history.project(get("history.project", envir = envir), "Original Spot", "original.txt")
    c.spot <- bg.correct(get("o.spot", envir = envir))
    op.counter <<- op.counter + 1
    tkconfigure(txt, state="normal")
    tkinsert(txt,"end","\n\nBackground correction done.............")
    tkconfigure(txt, state="disable")
    assign("c.spot", c.spot, envir = envir)
    write.spot(get("c.spot", envir = envir), paste(get("path.results", envir = envir),
                               .Platform$file.sep, "corrected.txt",sep = ""))
    set.history.project(get("history.project", envir = envir), "Corrected Spot", "corrected.txt")
    assign("a.spot", c.spot, envir = envir)
    tkpack(corr.radio,anchor="w",padx="10")
    tkselect(corr.radio)
    assign("corrected",1, envir = envir)
    normalized.gui(1, txt, img)
    filter.gui(txt, img)
    remove.duplicates.gui(3, txt, img)
  }    
}
