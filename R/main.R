require(tkrplot)
genArise <- function(){
  genArise.env <- new.env()
  assign("annotation", "GenArise Microarray Analyzer\n Institute of Cellular Physiology UNAM\n\n\n\n", envir = genArise.env)
  assign("platform", .Platform, envir = genArise.env)
  if(get("platform", envir = genArise.env)$OS.type == "unix"){
    assign("Myhscale", 1 , envir = genArise.env)
    assign("Myvscale", 1 , envir = genArise.env)
    assign("height", "600", envir = genArise.env)
    assign("width", "25", envir = genArise.env)
  }else{
    assign("Myhscale", 1.3 , envir = genArise.env)
    assign("Myvscale", 1.3 , envir = genArise.env)
    assign("height", "600", envir = genArise.env)
    assign("width", "5", envir = genArise.env)
  }
  
  annotation <-  function(){
    fileName <- paste(get("path.project", envir = genArise.env), get("platform", envir = genArise.env)$file.sep,"annotation.txt", sep = "")
    objeto <- file( fileName, "w")
    tt  <- tktoplevel()
    topMenu <- tkmenu(tt)
    tkconfigure(tt,menu=topMenu)
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    area.frame <- tkframe(tt,relief="groove",borderwidth=2)
    xscr <- tkscrollbar(area.frame, repeatinterval=5,orient="horizontal", command=function(...)tkxview(txt,...))
    yscr <- tkscrollbar(area.frame, repeatinterval=5,command=function(...)tkyview(txt,...))
    txt <- tktext(area.frame,bg="white",font="courier",xscrollcommand=function(...)tkset(xscr,...),
                  yscrollcommand=function(...)tkset(yscr,...), wrap="none")
    tkinsert(txt,"end", get("annotation", envir = genArise.env))
    tkadd(fileMenu,"command",label = "Save",command= function(){
      unlink(fileName)
      assign("annotation", tclvalue(tkget(txt,"0.0","end")), envir = genArise.env)
      cat("\n",tclvalue(tkget(txt,"0.0","end")),file=fileName)
    })
    tkadd(fileMenu,"command",label="Quit",command=function(){
      resp <- as.character(tkmessageBox(message="Do you want to save before quitting?",icon="question",type="yesnocancel",default="yes"))
      if(resp == "yes"){
        unlink(fileName)
        assign("annotation", tclvalue(tkget(txt,"0.0","end")), envir = genArise.env)
        cat("\n",tclvalue(tkget(txt,"0.0","end")),file=fileName)
        close(objeto)
      }
      tkdestroy(tt)
    })
    tkadd(topMenu,"cascade",label="File",menu=fileMenu)
    tkwm.title(tt, "Annotation")


    tkgrid(txt,yscr)
    tkgrid(xscr)
    tkgrid.configure(yscr,sticky="ns")
    tkgrid.configure(xscr,sticky="ew")
    tkfocus(txt)
    tkgrid(area.frame, padx = "10")
  }
  new.project  <- function(){
    select <- tktoplevel()
    tkwm.title(select,"Project Configuration")
    tkgrid(tklabel(select,text="Fill the text entries with the path and name of your new Project"), padx = "10", pady = "10")
    spot.frame <- tkframe(select,relief="groove",borderwidth=2)
    tkgrid(tklabel(spot.frame,text="Input"), padx = "10", pady = "10")
    entry.frame <- tkframe(spot.frame)
    assign("inputFile", tclVar(""), envir = genArise.env)
    association.entry <- tkentry(entry.frame,width="30", bg = "white", textvariable= get("inputFile", envir = genArise.env))
    association.but <- tkbutton(entry.frame,text="browse...",    command=function() {
      fileName <- tclvalue(tkgetOpenFile())
      if (!nchar(fileName))
        tkmessageBox(message="No file was selected!")
      else{
        if(tclvalue(get("inputFile", envir = genArise.env)) == "")
          tkinsert(association.entry, "end", fileName)
        else{
          inputFile <- tclVar("")
          tkconfigure(association.entry, textvariable = inputFile)
          tkinsert(association.entry, "end", fileName)
          assign("inputFile", inputFile, envir = genArise.env)
        }
      }
    })
    tkgrid(tklabel(entry.frame,text="Location Spot File:"), association.entry, association.but, padx = "5", sticky = "w", pady = "2")
    
    radio.frame <- tkframe(spot.frame)
    rbValue <- tclVar("IFC")
    rb1 <- tkradiobutton(radio.frame, text = "Foreign", value = "Foreign", variable = rbValue)
    rb2 <- tkradiobutton(radio.frame, text = "IFC", value = "IFC", variable = rbValue)
    tkgrid(rb1, rb2,sticky = "w", pady = "2",padx="70")
    tkgrid(entry.frame,padx="5", pady = "10")
    tkgrid(radio.frame,padx="5", pady = "10")

    tkgrid(tklabel(spot.frame, text = "Fill the text entries with the correct columns in your file"), padx = "10", pady = "5")
#   
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
    tkgrid(spot.frame,padx="5", pady = "10")
    
    
    lower.frame <- tkframe(select,relief="groove",borderwidth=2)
    tkgrid(tklabel(lower.frame,text="Output"), padx = "10", pady = "10")
    output.frame <- tkframe(lower.frame)
    assign("name.project", tclVar(""), envir = genArise.env)
    graphic.file <- tclVar("Plots")
    output.file <- tclVar("Results")
    project.entry <- tkentry(output.frame,width="30", bg = "white", textvariable= get("name.project", envir = genArise.env))
    graphic.entry <- tkentry(output.frame,width="10", bg = "white", textvariable = graphic.file)
    output.entry  <- tkentry(output.frame,width="10", bg = "white", textvariable = output.file)
    project.but <- tkbutton(output.frame,text="browse...",    command=function() {
      fileName <- tclvalue(tkgetSaveFile(initialdir = "."))
      if (!nchar(fileName))
        tkmessageBox(message="No file was selected!")
      else{
        name.project <- tclVar("")
        tkconfigure(project.entry, textvariable = name.project)
        tkinsert(project.entry, "end", fileName)
        assign("name.project", name.project, envir = genArise.env)
      }
    })
    tkgrid(tklabel(output.frame,text="Project Name:"), project.entry, project.but, padx = "5", sticky = "w", pady = "2")
    tkgrid(tklabel(output.frame,text="Location for Plots:"), graphic.entry, sticky = "w", padx = "5", pady = "2")
    tkgrid(tklabel(output.frame,text="Location for Results:"), output.entry,  sticky = "w", padx = "5", pady = "2")
    tkgrid(output.frame,padx="5")
    tkgrid(lower.frame,padx="5")
    
    buttons.frame <- tkframe(select)
    ok.but <- tkbutton(buttons.frame,text="  OK  ",command = function(){
      if(!nchar(tclvalue(get("inputFile", envir = genArise.env))))
        tkmessageBox(message= "No input file was selected! ", icon = "error", default = "ok")
      else{
        if(!nchar(tclvalue(get("name.project", envir = genArise.env))) || !nchar(tclvalue(graphic.file)) || !nchar(tclvalue(output.file)))
          tkmessageBox(message= "No output file was specified! ", icon = "error", default = "ok")
        else{
          # aqui vamos a armar el spot de forma manual para el manejo de errores
          #Hay que ver como leer solo la primera linea
          if(FALSE)#length(temp[1,]) < 5 || is.null(temp[,11]) || is.null(temp[,12]) || length(dim(temp[,11])) > 2 || length(dim(temp[,12])) > 2)
            mensaje <- tkmessageBox(message = "Not valid input file",  icon = "error")
          else{
            assign("path", tclvalue(get("inputFile", envir = genArise.env)), envir = genArise.env)
            name.tmp <- unlist(strsplit(tclvalue(get("inputFile", envir = genArise.env)), paste("\\", get("platform", envir = genArise.env)$file.sep, sep = "")))
            assign("spot.name", unlist(strsplit(name.tmp[length(name.tmp)], "\\."))[1], envir = genArise.env)
            assign("path.project", tclvalue(get("name.project", envir = genArise.env)), envir = genArise.env)
            assign("path.graphics", paste(tclvalue(get("name.project", envir = genArise.env)),
                                          get("platform", envir = genArise.env)$file.sep, tclvalue(graphic.file), sep = ""), envir = genArise.env)
            assign("path.results",paste(tclvalue(get("name.project", envir = genArise.env)),
                                        get("platform", envir = genArise.env)$file.sep, tclvalue(output.file), sep = ""), envir = genArise.env)

            if(as.character(tclvalue(rbValue)) == "IFC"){
              temp <- read.csv(tclvalue(get("inputFile", envir = genArise.env)), header = FALSE, sep = "\t", comment.char = "")
              temp <- temp[-1,] # aqui estamos quitar los encabezados
              row1 <- temp[,1]
              flag <- which(rev(row1) == "Mean value") # regresa un solo valor
              empty.row <- length(row1[row1==""])
              total.rows <-  length(temp[,1])
              temp <- temp[-((total.rows-(flag-1 + empty.row)):total.rows),]
              total.rows <- length(temp[,1])
              temp <- apply(temp, 2, as.vector)
              assign("o.spot", new ("Spot", name = get("spot.name", envir = genArise.env),
                                    spotData = list(Cy3 = as.numeric(temp[,as.numeric(tclvalue(Cy3))]), Cy5 = as.numeric(temp[,as.numeric(tclvalue(Cy5))]), BgCy3 = as.numeric(temp[,as.numeric(tclvalue(BgCy3))]), BgCy5 = as.numeric(temp[,as.numeric(tclvalue(BgCy5))]),
                                                      Id = as.vector(temp[,as.numeric(tclvalue(Id))]))), envir = genArise.env)
                     
              dir.create(paste(tclvalue(get("name.project", envir = genArise.env)), sep = ""))
              dir.create(paste(tclvalue(get("name.project", envir = genArise.env)),
                               get("platform", envir = genArise.env)$file.sep, tclvalue(graphic.file), sep = ""))
              dir.create(paste(tclvalue(get("name.project", envir = genArise.env)),
                               get("platform", envir = genArise.env)$file.sep, tclvalue(output.file), sep = ""))
              
              assign("nr",  as.integer(temp[length(temp[,1]),4]), envir = genArise.env)
              assign("nc",  as.integer(temp[length(temp[,1]), 5]), envir = genArise.env)
              assign("nmr", as.integer(temp[length(temp[,1]), 2]), envir = genArise.env)
              assign("nmc", as.integer(temp[length(temp[,1]), 3]), envir = genArise.env)
              assign("graphic.type", 2, envir = genArise.env)
              principal()
            }
            else{
              selectColumns(as.numeric(tclvalue(Cy3)),as.numeric(tclvalue(Cy5)),as.numeric(tclvalue(BgCy3)), as.numeric(tclvalue(BgCy5)), as.numeric(tclvalue(Id)), tclvalue(graphic.file), tclvalue(output.file))
                          }
          tkdestroy(select)
        }
        }}})
    cancel.but <- tkbutton(buttons.frame,text="Cancel",command = function(){
      tkdestroy(select)
      genArise()})
    tkgrid(ok.but,cancel.but,pady="9", padx = "20")
    tkgrid(buttons.frame,pady="9")
  }
  
  genArise.init <- function(){
    getfile<-function(tt){
      name <- tclvalue(tkgetOpenFile())
      if (!nchar(name))
        tkmessageBox(parent = tt,  message= "No file was selected!", icon = "error", default = "ok")
      else{ #Vamos a armar el spot de forma manual para manejar los errores
        temp <- read.csv(name, header = FALSE, sep = "\t", comment.char = "")
        if(length(temp[1,]) < 5 || is.null(temp[,1]) || is.null(temp[,2]) || length(dim(temp[,1])) > 2 || length(dim(temp[,2])) > 2)
          mensaje <- tkmessageBox(message = "Not valid input file",  icon = "error")
        else{
          name.tmp <- unlist(strsplit(name, "\\/"))
          assign("spot.name", unlist(strsplit(name.tmp[length(name.tmp)], "\\."))[1], envir = genArise.env)
          assign("o.spot",
                 new ("Spot", name = get("spot.name", envir = genArise.env), spotData = list(Cy3 = temp[,1], Cy5 = temp[,2],
                                                  BgCy3 = temp[,3], BgCy5 = temp[,4], Id = as.vector(temp[,5]))), envir = genArise.env)
          assign("nr", 23, envir = genArise.env)
          assign("nc", 24, envir = genArise.env)
          assign("nmr", 6, envir = genArise.env)
          assign("nmc", 4, envir = genArise.env)
          assign("graphic.type", 2, envir = genArise.env)
          
          tkdestroy(tt)
          principal()
        }
      }
    }

    
    tt <- tktoplevel()
    tkwm.geometry(tt,"+50+50")
    tkwm.maxsize(tt, get("height", envir = genArise.env), get("width", envir = genArise.env))
    tkwm.minsize(tt, get("height", envir = genArise.env), get("width", envir = genArise.env))
    tkwm.title(tt,"GenArise Microarray Analyzer")
    topMenu <- tkmenu(tt)
    tkconfigure(tt,menu=topMenu)
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    optMenu <- tkmenu(topMenu,tearoff=FALSE)
    helpMenu <- tkmenu(topMenu,tearoff=FALSE)

    openRecentMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(openRecentMenu,"command",label="New Project", command=function(){
      tkdestroy(tt)
      new.project()})
    
    tkadd(fileMenu,"cascade",label="Project",menu=openRecentMenu)
    tkadd(fileMenu,"command",label="Quit",command=function() tkdestroy(tt))
    tkadd(topMenu,"cascade",label="File",menu=fileMenu)

    helpMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(helpMenu,"command",label="About genArise...", command=function(){
      tkmessageBox(title="genArise package",message="This library was made in the\n Institute of Cellular Physiology UNAM\n \nmailto: genArise@ifc.unam.mx",icon="info",type="ok")
    })
    
    tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
  }
  selectColumns <- function(Cy3, Cy5, BgCy3, BgCy5, Id, graphic.file, output.file){
    select <- tktoplevel()
    tkwm.title(select,"Spot Configuration")
    entries.frame <- tkframe(select)
    tkgrid(tklabel(select,text="Fill the correct dimensions of microarray"))

    Row <- tclVar("")
    Col <- tclVar("")
    MR <- tclVar("")
    MC <- tclVar("")
    rc.frame <- tkframe(entries.frame,relief="groove",borderwidth=2)
    tkgrid(tklabel(rc.frame,text="Rows and Columns"),sticky="n",padx="5",pady="5")
    tkgrid(tklabel(rc.frame,text="Grid Rows"),tkentry(rc.frame,width="5",textvariable=Row, bg = "white"),pady="5")
    tkgrid(tklabel(rc.frame,text="Grid Columns"),tkentry(rc.frame,width="5",textvariable=Col, bg = "white"),pady="5")
    tkgrid(tklabel(rc.frame,text="Meta Rows"),tkentry(rc.frame,width="5",textvariable=MR, bg = "white"),pady="5")
    tkgrid(tklabel(rc.frame,text="Meta Columns"),tkentry(rc.frame,width="5",textvariable=MC, bg = "white"), padx = "10",pady="5")
    tkgrid(rc.frame,padx="10", padx = "30")
    buttons.frame <- tkframe(select)
    
    ok.but <- tkbutton(buttons.frame,text="   Ok   ",command = function(){
      temp <- read.csv(get("path", envir = genArise.env), header = FALSE, sep = "\t", comment.char = "")
      total.rows <- length(temp[,1])
      
      if(length(temp[1,]) < 5 || is.null(temp[,1]) || is.null(temp[,2]) || length(dim(temp[,1])) > 2 || length(dim(temp[,2])) > 2)
        mensaje <- tkmessageBox(message = "Not valid input file",  icon = "error")
      else{
        name.tmp <- unlist(strsplit(get("path", envir = genArise.env), "\\/"))
        assign("spot.name",unlist(strsplit(name.tmp[length(name.tmp)], "\\."))[1], envir = genArise.env)
#Aqui vamos a ver si el archivo de entrada tiene o no encabezado
        total <- as.integer(tclvalue(Row)) * as.integer(tclvalue(Col)) * as.integer(tclvalue(MR)) * as.integer(tclvalue(MC))

        if((total + 1) == length(temp[,1])){#entonces este archivo SI tiene encabezado
          temp <- temp[-1,]
          temp <- apply(temp, 2, as.vector)
        }
        assign("o.spot",  new ("Spot", name = get("spot.name", envir = genArise.env),
                               spotData = list(Cy3 = as.numeric(temp[,Cy3]),
                                 Cy5 = as.numeric(temp[,Cy5]),
                                 BgCy3 = as.numeric(temp[,BgCy3]),
                                 BgCy5 = as.numeric(temp[,BgCy5]),
                                 Id = as.vector(temp[,Id]))),
               envir = genArise.env)
        dir.create(paste(tclvalue(get("name.project", envir = genArise.env)), sep = ""))
        dir.create(paste(tclvalue(get("name.project", envir = genArise.env)),
                         get("platform", envir = genArise.env)$file.sep, graphic.file, sep = ""))
        dir.create(paste(tclvalue(get("name.project", envir = genArise.env)),
                         get("platform", envir = genArise.env)$file.sep, output.file, sep = ""))
        
        
      
        assign("nr",  as.numeric(tclvalue(Row)), envir = genArise.env)
        assign("nc",  as.numeric(tclvalue(Col)), envir = genArise.env)
        assign("nmr", as.numeric(tclvalue(MR)), envir = genArise.env)
        assign("nmc", as.numeric(tclvalue(MC)), envir = genArise.env)
        assign("graphic.type", 2, envir = genArise.env)
        tkdestroy(select)
        principal()}})
    cancel.but <- tkbutton(buttons.frame,text=" Cancel ",command = function()tkdestroy(select))
    tkgrid(entries.frame,pady="10", padx = "20")
    tkgrid(ok.but,cancel.but,pady="10", padx = "25")
    tkgrid(buttons.frame,pady="9")
    }
    
  principal <-  function(){
    tt <- tktoplevel()
    o.spot <- get("o.spot", envir = genArise.env)
    datos <- attr(o.spot, "spotData")
    M <- log(datos$Cy3, 2) - log(datos$Cy5, 2)
    tkwm.geometry(tt,"+50+50")
    tkwm.title(tt,"GenArise Microarray Analyzer")
    topMenu <- tkmenu(tt)
    tkconfigure(tt,menu=topMenu)
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(fileMenu,"command",label="Quit", command=function() tkdestroy(tt))
    tkadd(topMenu,"cascade",label="File", menu=fileMenu)
    tkadd(topMenu,"command",label="Back", command = function(){
      tkdestroy(tt)
      genArise()})
    tkadd(topMenu,"command",label="Analyze",
          command = function() {
            respuesta <- as.character(tkmessageBox(message = "Do you want to follow the wizard?",
                                                   icon = "question", type = "yesnocancel", default = "yes"))
            
            if(respuesta == "yes"){ # tambien debemos hacer inactivo el menu para el caso de windows
              tkentryconfigure(topMenu, "0", state ="disable" )
              tkentryconfigure(topMenu, "1", state ="disable" )
              tkentryconfigure(topMenu, "2", state ="disable" )
              tkentryconfigure(topMenu, "3", state ="disable" )
              tkentryconfigure(topMenu, "4", state ="disable" )
              c.spot <- bg.correct(get("o.spot", envir = genArise.env))
              write.spot(c.spot, paste(get("path.results", envir = genArise.env), get("platform", envir = genArise.env)$file.sep,
                                       "corrected.csv",sep = ""))
              assign("c.spot", c.spot, envir = genArise.env)
              tkconfigure(txt, state="normal")
              tkinsert(txt,"end","\n\nBackground correction............")
              tkconfigure(txt, state="disable")
              n.spot <- grid.norm(c.spot, nr = get("nr", envir = genArise.env), nc = get("nc", envir = genArise.env))
              assign("n.spot", n.spot, envir = genArise.env)
              write.spot(n.spot, paste(get("path.results", envir = genArise.env), get("platform", envir = genArise.env)$file.sep,
                                       "normalized.csv",sep = ""))
              tkconfigure(txt, state="normal")
              tkinsert(txt,"end","\n\nNormalization.........")
              tkinsert(txt,"end","\n\tBy grid")
              tkconfigure(txt, state="disable")
              tkconfigure(txt, state="normal")
              f.spot <- filter.spot(n.spot)
              assign("f.spot", f.spot, envir = genArise.env)
              write.spot(f.spot, paste(get("path.results", envir = genArise.env), get("platform", envir = genArise.env)$file.sep,
                         "filter.csv",sep = ""))
              tkinsert(txt,"end","\n\nFilter spot............\n")
              tkconfigure(txt, state="disable")
              u.spot <- spotUnique(f.spot)
              assign("u.spot", u.spot,envir = genArise.env)
              write.spot(u.spot, paste(get("path.results", envir = genArise.env), get("platform", envir = genArise.env)$file.sep,
                                       "withoutDup.csv",sep = ""))
              tkconfigure(txt, state="normal")
              tkinsert(txt,"end","\n\nWithout duplicates by geometric mean............")
              tkconfigure(txt, state="disable")
              assign("a.spot", o.spot, envir = genArise.env)
              graphic.type <- 2
              assign("historial", tclvalue(tkget(txt, "0.0", "end")),envir = genArise.env)
              tkdestroy(tt)
              siguiendo.wizard()
            }else{
              if(respuesta == "no"){
                no.siguiendo.wizard(tclvalue(tkget(txt,"0.0","end")))
                tkdestroy(tt)
              }else{
                                        #Se cancela la pediticion del analisis
              }
            }
          })
    optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function(){
      name <- tclvalue(tkgetSaveFile(initialdir = get("path.graphics", envir = genArise.env),
                                     initialfile=get("spot.name",envir=genArise.env),filetypes="{{PDF Files} {.pdf}} {{All files} *}"))
      if (!nchar(name))
        tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
      else{
      rbVal <- as.character(tclvalue(rbValue))
      if(rbVal == "Red & Green"){
        pdf(paste(name, "R&G.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "R&G.pdf", sep = "_"))
        imageLimma(log(datos$Cy3, 2) - log(datos$Cy5, 2), row = get("nr", envir = genArise.env), column = get("nc", envir = genArise.env),
                    meta.row = get("nmr", envir = genArise.env),meta.column = get("nmc", envir = genArise.env))
      }
      else{
        if(rbVal == "Red"){
          pdf(paste(name, "R.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "R.pdf", sep = "_"))
          imageLimma(log(datos$Cy3, 2), get("nr", envir = genArise.env), get("nc", envir = genArise.env),
                      get("nmr", envir = genArise.env),
                      get("nmc", envir = genArise.env), low = "white", high = "red")
        }
        else{
          pdf(paste(name, "G.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "G.pdf", sep = "_"))
          imageLimma(log(datos$Cy5, 2),get("nr", envir = genArise.env), get("nc", envir = genArise.env),
                      get("nmr", envir = genArise.env),
                      get("nmc", envir = genArise.env), low = "white", high = "green")
        }
      }
    }
    })
    tkadd(optionsMenu,"command",label="Annotation",command = function(){
      annotation()
    })
    
    tkadd(topMenu,"cascade",label="Options", menu =optionsMenu)
    helpMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(helpMenu,"command",label="About GenArise",command = function()
          tkmessageBox(message = "GenArise Microarray Analiyzer\nhttp://www.ifc.unam.mx", icon = "info"))
    tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
    tkfocus(tt)
    upper.frame <- tkframe(tt)
    img <- tkrplot(upper.frame, fun = function() imageLimma(M, get("nr", envir = genArise.env), get("nc", envir = genArise.env),
                                  get("nmr", envir = genArise.env),
                                  get("nmc", envir = genArise.env)), hscale= get("Myhscale", envir = genArise.env),vscale = get("Myvscale",
                                                                                                                     envir = genArise.env))
    frameOverall <- tkframe(upper.frame)
    frameUpper <- tkframe(frameOverall,relief="groove",borderwidth=2)
    tkgrid(tklabel(frameUpper, text = "Spot Features", font = "Helvetica 12"), pady = "7")
    tkgrid(tklabel(frameUpper, text = paste("Nombre:   ", get("spot.name", envir = genArise.env))), sticky = "w", pady = "2")
    tkgrid(tklabel(frameUpper, text = paste("Rows:           ", get("nr", envir = genArise.env))), sticky = "w", pady = "2")
    tkgrid(tklabel(frameUpper, text = paste("Columns:       ", get("nc", envir = genArise.env))), sticky = "w", pady = "2")
    tkgrid(tklabel(frameUpper, text = paste("Metarows  :     ", get("nmr", envir = genArise.env))), sticky = "w", pady = "2")
    tkgrid(tklabel(frameUpper, text = paste("MetaColumns:  ", get("nmc", envir = genArise.env))), sticky = "w", pady = "2")
    tkgrid(frameUpper, padx  = "10", pady = "10")


    radio.frame <- tkframe(frameOverall,relief="groove",borderwidth=2)
    rbValue <- tclVar("Red & Green")
    rb1 <- tkradiobutton(radio.frame, text = "Red & Green", value = "Red & Green", variable = rbValue,
                         command = function() {tkrreplot(img, fun = function() imageLimma(M,
                                                                get("nr", envir = genArise.env),
                                                                get("nc", envir = genArise.env),
                                                                get("nmr", envir = genArise.env),
                                                                get("nmc", envir = genArise.env)))})
    rb2 <- tkradiobutton(radio.frame, text = "Red", value = "Red", variable = rbValue,
                         command = function() {tkrreplot(img, fun = function()imageLimma(log(datos$BgCy3,2),
                                                                get("nr", envir = genArise.env),
                                                                get("nc", envir = genArise.env),
                                                                get("nmr", envir = genArise.env),
                                                                get("nmc", envir = genArise.env), low = "white", high = "red"))})
    rb3 <- tkradiobutton(radio.frame, text = "Green", value = "Green", variable = rbValue,
                         command = function() {tkrreplot(img, fun = function()imageLimma(log(datos$BgCy5, 2),
                                                                get("nr", envir = genArise.env),
                                                                get("nc", envir = genArise.env),
                                                                get("nmr", envir = genArise.env),
                                                                get("nmc", envir = genArise.env), low = "white", high = "green"))})
    tkgrid(rb1, sticky = "w", pady = "2",padx="10")
    tkgrid(rb2, sticky = "w", pady = "2",padx="10")
    tkgrid(rb3, sticky = "w", pady = "2",padx="10")
    tkgrid(frameUpper, padx  = "20", pady = "120")
    tkgrid(radio.frame, padx  = "20", pady = "10")
    tkgrid(img, frameOverall, pady = "10", sticky = "w")
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
  
  grafica <-  function(){
    a.spot <- get("a.spot", envir = genArise.env)
    graphic.type <-  get("graphic.type", envir = genArise.env)
    switch(graphic.type,
           cys.plot(a.spot),
           ri.plot(a.spot),
           ma.plot(a.spot))
  }

  
  
  siguiendo.wizard <-  function(){
    tt <- tktoplevel()
    tkwm.geometry(tt,"+50+50")
    tkwm.title(tt,"GenArise Microarray Analyzer")
    topMenu <- tkmenu(tt)
    tkconfigure(tt,menu=topMenu)
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    upper.frame <- tkframe(tt)
    frame1 <- tkframe(upper.frame,relief="groove",borderwidth=2)
    
    dist  <- tclVar(1)
    ori.radio <- tkradiobutton(frame1, text="Original Spot",
                               value=1, variable=dist, command=function(){
                                 assign("a.spot", get("o.spot", envir = genArise.env), envir = genArise.env)
                                 tkrreplot(img,fun=function()grafica())
                               })
    corr.radio <- tkradiobutton(frame1, text="Corrected Spot",
                                value=2, variable=dist,command = function(){
                                  assign("a.spot", get("c.spot", envir = genArise.env), envir = genArise.env)
                                  tkrreplot(img,fun=function()grafica())})
    norm.radio <- tkradiobutton(frame1, text="Normalized Spot",
                                value=3, variable=dist,command = function(){
                                  assign("a.spot", get("n.spot", envir = genArise.env), envir = genArise.env)
                                  tkrreplot(img,fun=function()grafica())})
    fil.radio <- tkradiobutton(frame1, text="Filter Spot",
                               value=4, variable=dist,command = function(){
                                 assign("a.spot", get("f.spot", envir = genArise.env), envir = genArise.env)
                                 tkrreplot(img,fun=function()grafica())})
    uniq.radio <- tkradiobutton(frame1, text="Without duplicates",
                                value=5, variable=dist,command = function(){
                                  assign("a.spot", get("u.spot", envir = genArise.env), envir = genArise.env)
                                  tkrreplot(img,fun=function()grafica())})
    tkpack(ori.radio,anchor="w", pady = "2",padx="10")
    tkpack(corr.radio,anchor="w", pady = "2",padx="10")
    tkpack(norm.radio,anchor="w", pady = "2",padx="10")
    tkpack(fil.radio,anchor="w", pady = "2",padx="10")
    tkpack(uniq.radio,anchor="w", pady = "2",padx="10")
    tkadd(fileMenu,"command",label="Quit", command=function() tkdestroy(tt))
    tkadd(topMenu,"cascade",label="File",menu=fileMenu)
    tkadd(topMenu,"command",label="Back", command = function(){
      principal()
      tkdestroy(tt)})
    
    graphicMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(graphicMenu,"command",label="Cy3 vs Cy5", command = function() {
      assign("graphic.type", 1, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())})
    tkadd(graphicMenu,"command",label="R vs I",command = function() {
      assign("graphic.type", 2, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())})
    tkadd(graphicMenu,"command",label="M vs A",command = function() {
      assign("graphic.type", 3, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())})
    tkadd(topMenu,"cascade",label="Graphics",menu =graphicMenu)
    
    ZscoreMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(topMenu,"cascade",label="Zscore",menu = ZscoreMenu)

    tkadd(ZscoreMenu,"command",label="R vs I", state="active", command=function(){
      assign("Zscore.spot", Zscore(get("a.spot",envir=genArise.env)), envir = genArise.env)
      tkconfigure(txt, state="normal")
      tkinsert(txt,"end","\n\nR-I Zscore done.............")
      tkconfigure(txt, state="disable")
      Zscore.points(type="ri",text=tclvalue(tkget(txt,"0.0","end")))
      tkdestroy(tt)
    })
    tkadd(ZscoreMenu,"command",label="M vs A", state="active",command=function(){
      assign("Zscore.spot", Zscore(get("a.spot",envir=genArise.env), type = "ma"), envir = genArise.env)
      tkconfigure(txt, state="normal")
      tkinsert(txt,"end","\n\nM-A Zscore done.............")
      tkconfigure(txt, state="disable")
      Zscore.points(type="ma",text=tclvalue(tkget(txt,"0.0","end")))
      tkdestroy(tt)
    })
        
    optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function() {
        name <- tclvalue(tkgetSaveFile(initialdir = get("path.graphics", envir = genArise.env),
                                       initialfile=get("spot.name",envir=genArise.env),
                                       filetypes="{{PDF Files} {.pdf}} {{All files} *}"))
      if (!nchar(name))
        tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
      else{
        rbVal <- as.integer(tclvalue(dist))
        switch(rbVal,
               {pdf(paste(name, "Original.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Original.pdf", sep = "_"));
               grafica()},
               {pdf(paste(name, "Corrected.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Corrected.pdf", sep = "_"));
               grafica()},
               {pdf(paste(name, "Normal.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Normal.pdf", sep = "_"));
               grafica()},
               {pdf(paste(name, "Filter.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Filter.pdf", sep = "_"));
               grafica()},
               {pdf(paste(name, "NoDuplicates.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NoDuplicates.pdf", sep = "_"));
                grafica()})
      }
    }
          )
    tkadd(optionsMenu,"command",label="Annotation",command = function() annotation())
    tkadd(topMenu,"cascade",label="Options", menu =optionsMenu)

    helpMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(helpMenu,"command",label="About genArise...", command=function(){
      tkmessageBox(title="genArise package",message="This library was made in the\n Institute of Cellular Physiology UNAM\n \nmailto: genArise@ifc.unam.mx",icon="info",type="ok")
    })
    
    tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
    
    frame.label <- tkframe(tt,relief="groove",borderwidth=2)
    tkfocus(tt)
    img <- tkrplot(upper.frame,fun = function()grafica(), hscale= get("Myhscale", envir = genArise.env),vscale = get("Myvscale",
                                                                                                                     envir = genArise.env))
    tkgrid(img,frame1, padx = "10", pady = "10")
    tkgrid(upper.frame, pady = "10")
    area.frame <- tkframe(tt,relief="groove",borderwidth=2)
    yscr <- tkscrollbar(area.frame, repeatinterval = 5,
                        command=function(...)tkyview(txt,...))
    txt <- tktext(area.frame,bg="white",font="courier",width ="70",height="5",yscrollcommand=function(...)tkset(yscr,...))
    tkgrid(txt, yscr)
    tkgrid.configure(yscr,rowspan=4,sticky="nsw")
    texto <-  get("historial", envir = genArise.env)
    tkinsert(txt,"end",texto)
    tkconfigure(txt, state="disabled")
    tkgrid(area.frame, padx = "10")
  }
  
  no.siguiendo.wizard <-  function(texto){
    assign("corrected", 0, envir = genArise.env)
    bg.question <- function(){
      answer <-tkmessageBox(message="Do you want to make background correction?",icon="question",type="yesnocancel",default="yes")
      ans <- as.character(answer)
      if(ans == "yes"){
        c.spot <- bg.correct(get("a.spot", envir = genArise.env))
        tkconfigure(txt, state="normal")
        tkinsert(txt,"end","\n\nBackground correction done.............")
        tkconfigure(txt, state="disable")
        assign("c.spot", c.spot, envir = genArise.env)
        assign("a.spot", c.spot, envir = genArise.env)
        tkpack(corr.radio,anchor="w",padx="10")
        tkselect(corr.radio)
        assign("corrected",1, envir = genArise.env)
      }
      else {
        if(ans == "no"){
          assign("corrected",1, envir = genArise.env)
        }
        else{
          assign("corrected",2, envir = genArise.env)
        }
      }
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
                                 assign("a.spot", get("o.spot", envir = genArise.env), envir = genArise.env)
                                 tkrreplot(img,fun=function()grafica())})
    corr.radio <- tkradiobutton(frame1, text="Corrected Spot",
                                value=2, variable=dist,command = function(){
                                  assign("a.spot", get("c.spot", envir = genArise.env), envir = genArise.env)
                                  tkrreplot(img,fun=function()grafica())})

    normal.radio <- tkradiobutton(frame1, text="Normal Spot",
                                  value=3, variable=dist,command = function(){
                                    assign("a.spot", get("n.spot", envir = genArise.env), envir = genArise.env)
                                    tkrreplot(img,fun=function()grafica())})
    
    fil.radio <- tkradiobutton(frame1, text="Filter Spot",
                               value=4, variable=dist,command = function(){
                                 assign("a.spot", get("f.spot", envir = genArise.env), envir = genArise.env)
                                 tkrreplot(img,fun=function()grafica())})
    uniq.radio <- tkradiobutton(frame1, text="Without duplicates",
                                value=5, variable=dist,command = function(){
                                  assign("a.spot", get("u.spot", envir = genArise.env), envir = genArise.env)
                                  tkrreplot(img,fun=function()grafica())})
    tkpack(ori.radio,anchor="w",padx="10")
    
    tkadd(fileMenu,"command",label="Close",command=function(){
      tkdestroy(tt)
      genArise.init()})
    tkadd(fileMenu,"command",label="Exit",command=function() tkdestroy(tt))
    tkadd(topMenu,"cascade",label="File",menu=fileMenu)
    
    tkadd(topMenu,"command",label="Back", command = function(){
      principal()
      tkdestroy(tt)})

    filterMenu <- tkmenu(topMenu,tearoff=FALSE)

    tkadd(filterMenu,"command",label="By Intensity", state ="active",command = function() {
      if(get("corrected",envir=genArise.env) == 0) bg.question()
      f.spot <- filter.spot(get("a.spot", envir = genArise.env))
      tkconfigure(txt, state="normal")
      tkinsert(txt,"end","\n\nIntensity-based filtering done.............")
      tkconfigure(txt, state="disable")
      assign("f.spot", f.spot, envir = genArise.env)
      assign("a.spot", f.spot, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())
      tkpack(fil.radio,anchor="w",padx="10")
      tkselect(fil.radio)
      tkentryconfigure(filterMenu, "0", state ="disable" )
      count.filter <<- count.filter +1
      tkentryconfigure(normalMenu, "0", state ="disable" )
      if(count.filter == 2){
        tkentryconfigure(topMenu, "3", state ="disable" )}})

    duplicatesMenu <- tkmenu(filterMenu,tearoff=FALSE)
    tkadd(duplicatesMenu,"command",label="Mean Replicate Filtering", command = function(){
      if(get("corrected",envir=genArise.env) == 0) bg.question()
      if(get("corrected",envir=genArise.env) == 1){
        u.spot <- meanUnique(get("a.spot", envir = genArise.env))
        tkconfigure(txt, state="normal")
        tkinsert(txt,"end","\n\nMean replicates filtering done.............")
        tkconfigure(txt, state="disable")
        assign("u.spot", u.spot, envir = genArise.env)
        assign("a.spot", u.spot, envir = genArise.env)
        tkrreplot(img,fun=function()grafica())
        tkpack(uniq.radio,anchor="w",padx="10")
        tkselect(uniq.radio)
        tkentryconfigure(filterMenu, "1", state ="disable" )
        count.filter <- count.filter +1
        tkentryconfigure(normalMenu, "0", state ="disable" )
        if(count.filter == 2){
          tkentryconfigure(topMenu, "3", state ="disable" )}}})

    tkadd(duplicatesMenu,"command",label="Non-extreme values replicate filtering", command = function(){
      if(get("corrected",envir=genArise.env) == 0) bg.question()
      if(get("corrected",envir=genArise.env) == 1){
        u.spot <- alter.unique(get("a.spot", envir = genArise.env))
        tkconfigure(txt, state="normal")
        tkinsert(txt,"end","\n\nNon-extreme values replicate filtering done..............")
        tkconfigure(txt, state="disable")
        assign("u.spot", u.spot, envir = genArise.env)
        assign("a.spot", u.spot, envir = genArise.env)
        tkrreplot(img,fun=function()grafica())
        tkpack(uniq.radio,anchor="w",padx="10")
        tkselect(uniq.radio)
        tkentryconfigure(filterMenu, "1", state ="disable" )
        count.filter <- count.filter +1
        tkentryconfigure(normalMenu, "0", state ="disable" )
        if(count.filter == 2){
          tkentryconfigure(topMenu, "3", state ="disable" )}}})
    
    tkadd(duplicatesMenu,"command",label="Geometric Mean Filter", command = function(){
      if(get("corrected",envir=genArise.env) == 0) bg.question()
      if(get("corrected",envir=genArise.env) == 1){
        u.spot <- spotUnique(get("a.spot", envir = genArise.env))
        tkconfigure(txt, state="normal")
        tkinsert(txt,"end","\n\nGeometric mean filtering done..............")
        tkconfigure(txt, state="disable")
        assign("u.spot", u.spot, envir = genArise.env)
        assign("a.spot", u.spot, envir = genArise.env)
        tkrreplot(img,fun=function()grafica())
        tkpack(uniq.radio,anchor="w",padx="10")
        tkselect(uniq.radio)
        tkentryconfigure(filterMenu, "1", state ="disable" )
        count.filter <- count.filter +1
        tkentryconfigure(normalMenu, "0", state ="disable" )
        if(count.filter == 2){
          tkentryconfigure(topMenu, "3", state ="disable" )}}})
    
    tkadd(filterMenu,"cascade",label="Duplicates Analysis", state ="active",menu= duplicatesMenu)

    tkadd(topMenu,"cascade",label="Filter",menu =filterMenu)
    
    normalMenu <- tkmenu(topMenu,tearoff=FALSE)

    tkadd(normalMenu,"command",label="By grid", state ="active", command=function(){
      if(get("corrected",envir=genArise.env) == 0) bg.question()
      if(get("corrected",envir = genArise.env) == 1){
      n.spot <- grid.norm(get("a.spot", envir = genArise.env), get("nr", envir = genArise.env), get("nc", envir = genArise.env))
      tkconfigure(txt, state="normal")
      tkinsert(txt,"end","\n\nNormalization by grid done.............")
      tkconfigure(txt, state="disable")
      assign("n.spot", n.spot, envir = genArise.env)
      assign("a.spot", n.spot, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())
      tkpack(normal.radio,anchor="w",padx="10")
      tkselect(normal.radio)
      tkentryconfigure(topMenu, "4", state ="disable" )
    }
    })

    tkadd(normalMenu,"command",label="Global", state ="active", command=function(){
      if(get("corrected",envir=genArise.env) == 0) bg.question()
      if(get("corrected",envir = genArise.env) == 1){
        n.spot <- global.norm(get("a.spot", envir = genArise.env))
        tkconfigure(txt, state="normal")
        tkinsert(txt,"end","\n\nGlobal normalization done.............")
        tkconfigure(txt, state="disable")
        assign("n.spot", n.spot, envir = genArise.env)
        assign("a.spot", n.spot, envir = genArise.env)
        tkrreplot(img,fun=function()grafica())
        tkpack(normal.radio,anchor="w",padx="10")
        tkselect(normal.radio)
        tkentryconfigure(topMenu, "4", state ="disable" )
      }
    })

    tkadd(topMenu,"cascade",label="Normalize",menu = normalMenu)
    graphicMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(graphicMenu,"command",label="Cy3 vs Cy5",command = function() {
      assign("graphic.type", 1, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())})
    tkadd(graphicMenu,"command",label="R vs I",command = function() {
      assign("graphic.type", 2, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())})
    tkadd(graphicMenu,"command",label="M vs A",command = function() {
      assign("graphic.type", 3, envir = genArise.env)
      tkrreplot(img,fun=function()grafica())})
    tkadd(topMenu,"cascade",label="Graphics",menu =graphicMenu)

    ZscoreMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(topMenu,"cascade",label="Zscore",menu = ZscoreMenu)
    tkadd(ZscoreMenu,"command",label="R vs I", state="active", command=function(){
      assign("Zscore.spot", Zscore(get("a.spot",envir=genArise.env)), envir = genArise.env)
      Zscore.points(type="ri",text=tclvalue(tkget(txt,"0.0","end")))
      tkdestroy(tt)
    })
    tkadd(ZscoreMenu,"command",label="M vs A", state="active",command=function(){
      assign("Zscore.spot", Zscore(get("a.spot",envir=genArise.env)), envir = genArise.env)
       Zscore.points(type="ma",text=tclvalue(tkget(txt,"0.0","end")))
      tkdestroy(tt)
    })
    
    optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function(){
      name <- tclvalue(tkgetSaveFile(initialdir = get("path.graphics", envir = genArise.env),
                                     initialfile=get("spot.name",envir=genArise.env),
                                     filetypes="{{PDF Files} {.pdf}} {{All files} *}"))
      if (!nchar(name))
        tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
      else{
        rbVal <- as.integer(tclvalue(dist))
        switch(rbVal,
               {pdf(paste(name, "Original.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Original.pdf", sep = "_"));
                grafica()},
               {pdf(paste(name, "Corrected.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Corrected.pdf", sep = "_"));
                grafica()},
               {pdf(paste(name, "Normal.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Normal.pdf", sep = "_"));
                grafica()},
               {pdf(paste(name, "Filter.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "Filter.pdf", sep = "_"));
                grafica()},
               {pdf(paste(name, "NoDuplicates.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "NoDuplicates.pdf", sep = "_"));
                grafica()})
      }
      })
    tkadd(optionsMenu,"command",label="Annotations",command = function() annotation())
    tkadd(topMenu,"cascade",label="Options",menu =optionsMenu)
    

     helpMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(helpMenu,"command",label="About genArise...", command=function(){
      tkmessageBox(title="genArise package",message="This library was made in the\n Institute of Cellular Physiology UNAM\n \nmailto: genArise@ifc.unam.mx",icon="info",type="ok")
    })
    
    tkadd(topMenu,"cascade",label="Help",menu = helpMenu)


    frame.label <- tkframe(tt,relief="groove",borderwidth=2)
    tkfocus(tt)
    assign("a.spot", get("o.spot", envir = genArise.env), envir = genArise.env)
    img <- tkrplot(upper.frame,fun = function()grafica(), hscale= get("Myhscale", envir = genArise.env),vscale = get("Myvscale",
                                                                                                          envir = genArise.env))
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
  }

  Zscore.points <-  function(type="ri",text){
    tt <- tktoplevel()
    tkwm.title(tt,"GenArise Microarray Analyzer")
    topMenu <- tkmenu(tt)
    tkconfigure(tt,menu=topMenu)
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    upper.frame <- tkframe(tt)
    sd.frame <- tkframe(upper.frame,relief="groove",borderwidth=2)
    dist  <- tclVar(5)

    radio1 <- tkradiobutton(sd.frame, text="Zscore < 1",
                               value=1, variable=dist, command=function(){
                                 tkrreplot(img, fun = function()
                                         Zscore.plot(get("Zscore.spot", envir = genArise.env), all = FALSE, Zscore.max = 1, col = "green"))})
    radio2 <- tkradiobutton(sd.frame, text="1 < Zscore < 1.5",
                                 value=2, variable=dist,command = function(){
                                   tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = genArise.env),
                                                    Zscore.max = 1.5, Zscore.min = 1, all = FALSE, col = "blue"))})
    radio3 <- tkradiobutton(sd.frame, text="1.5 < Zscore < 2",
                               value=3, variable=dist,command = function(){
                                 tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = genArise.env),
                                                  Zscore.max = 2, Zscore.min = 1.5, all = FALSE, col = "cyan"))})
    radio4 <- tkradiobutton(sd.frame, text="Zscore > 2",
                               value=4, variable=dist, command=function(){
                                 tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = genArise.env),
                                                  Zscore.min = 2, all = FALSE, col = "snow"))})
    radio5 <- tkradiobutton(sd.frame, text="All",
                               value=5, variable=dist, command=function(){
                                 tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = genArise.env)))})
    
        
    tkgrid(radio1, sticky="w")
    tkgrid(radio2, sticky="w")
    tkgrid(radio3, sticky="w")
    tkgrid(radio4, sticky="w")
    tkgrid(radio5, sticky="w")

    tkselect(radio5)
    
    tkadd(fileMenu,"command",label="Close",command=function(){
      tkdestroy(tt)
      genArise.init()})
    tkadd(fileMenu,"command",label="Exit",command=function() tkdestroy(tt))
    tkadd(topMenu,"cascade",label="File",menu=fileMenu)
    tkadd(topMenu,"command",label="Back", command = function(){
      genArise()
      tkdestroy(tt)})
    
    optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function() {
      name <- tclvalue(tkgetSaveFile(initialdir = get("path.graphics", envir = genArise.env),
                                     initialfile=get("spot.name",envir=genArise.env),
                                     filetypes="{{PDF Files} {.pdf}} {{All files} *}"))
      if (!nchar(name))
        tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
      else{
        rbVal <- as.integer(tclvalue(dist))
        switch(rbVal,
               {pdf(paste(name, "lower_1.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "1SD.pdf", sep = "_"));
                Zscore.plot(get("Zscore.spot", envir = genArise.env), all = FALSE, Zscore.max = 1, col = "green")},          
               {pdf(paste(name, "1-1.5.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "1SD.pdf", sep = "_"));
                Zscore.plot(get("Zscore.spot", envir = genArise.env),Zscore.max = 1.5, Zscore.min = 1, all = FALSE, col = "blue")},
               {pdf(paste(name, "1.5-2.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "1SD.pdf", sep = "_"));
                Zscore.plot(get("Zscore.spot", envir = genArise.env),Zscore.max = 2, Zscore.min = 1.5, all = FALSE, col = "cyan")},
               {pdf(paste(name, "greater_2.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "1SD.pdf", sep = "_"));
                Zscore.plot(get("Zscore.spot", envir = genArise.env),Zscore.min = 2, all = FALSE, col = "snow")},
               {pdf(paste(name, "all.pdf", sep = "_"),horiz=F, height=8,width=8,title=paste(name, "1SD.pdf", sep = "_"));
                Zscore.plot(get("Zscore.spot", envir = genArise.env))})
      }
    })
    tkadd(optionsMenu,"command",label="Write as outputfile",command = function() {
      name <- tclvalue(tkgetSaveFile(initialdir = get("path.results", envir = genArise.env),
                                     initialfile=get("spot.name",envir=genArise.env),
                                     filetypes="{{CVS Files} {.cvs}} {{All files} *}"))
      if (!nchar(name))
        tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
      else{
        rbVal <- as.integer(tclvalue(dist))
        switch(rbVal,
               {write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "lower_1", sep = "_"), Zscore.max = 1)},
               {write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "1-1.5", sep = "_"), Zscore.max = 1.5, Zscore.min=1)},
               {write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "1.5-2", sep = "_"), Zscore.max = 2, Zscore.min = 1)},
               {write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "greater_2", sep = "_"), Zscore.min = 2)},
               {
               write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "lower_1", sep = "_"), Zscore.max = 1);
               write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "1-1.5", sep = "_"), Zscore.max = 1.5, Zscore.min=1);
               write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "1.5-2", sep = "_"), Zscore.max = 2, Zscore.min = 1);
               write.dataSet(get("Zscore.spot", envir = genArise.env), paste(name, "greater_2", sep = "_"), Zscore.min = 2)})
      }
    })
    tkadd(optionsMenu,"command",label="Annotations",command = function()annotation())
    tkadd(topMenu,"cascade",label="Options",menu =optionsMenu)

 helpMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(helpMenu,"command",label="About genArise...", command=function(){
      tkmessageBox(title="genArise package",message="This library was made in the\n Institute of Cellular Physiology UNAM\n \nmailto: genArise@ifc.unam.mx",icon="info",type="ok")
    })
    
    tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
    
    
    frame.label <- tkframe(tt,relief="groove",borderwidth=2)
    tkfocus(tt)
    img <- tkrplot(upper.frame,fun = function()Zscore.plot(get("Zscore.spot", envir = genArise.env),type),
                   hscale= get("Myhscale", envir = genArise.env),vscale = get("Myvscale",envir = genArise.env))
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
  genArise.init()
}
