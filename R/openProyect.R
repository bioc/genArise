require(tkrplot)
old.project <-  function(project.name, envir, parent){
  selected.original <-  function(state = TRUE){
    
  }
  selected.zscore <-  function(Zscore.min = NULL, Zscore.max = NULL, all = TRUE, col = "green"){
    tkentryconfigure(topMenu,"2",state="disabled")
    tkentryconfigure(topMenu,"3",state="normal")
    tkrreplot(img, fun = function() Zscore.plot(get("Zscore.spot", envir = envir),
                     Zscore.min = Zscore.min, Zscore.max = Zscore.max, all = all, col = col))
  }
  save.pdf <- function(name,Zscore.min = NULL, Zscore.max = NULL, all = TRUE, col = "green"){
    pdf(name,horiz=F, height=8,width=8,title=paste(name, sep = "_"))
    Zscore.plot(get("Zscore.spot", envir = envir),
                     Zscore.min = Zscore.min, Zscore.max = Zscore.max, all = all, col = col)
    dev.off(dev.cur())
  }

  imageLimma.plot <-  function(datos, low = NULL, high = NULL){
    tkrreplot(img, fun = function() imageLimma(datos, as.numeric(as.vector(spots[3,2])), as.numeric(as.vector(spots[4,2])),
                     as.numeric(as.vector(spots[5,2])), as.numeric(as.vector(spots[6,2])),low =low, high = high))
    tkentryconfigure(topMenu,"2",state="disabled")
    tkentryconfigure(topMenu,"3",state="disabled")
  }
  
  assign("graphic.type", 2, envir = envir)
  spots <- read.table(project.name, header = FALSE, sep = "\t")
  if(length(spots[,1]) < 7){
    tkmessageBox(parent = parent,  message= "This project doesn't have operations", icon = "error", default = "ok")
    stop("This project doesn't have operations")
  }
  else{
    tt <- tktoplevel()
    tkfocus(tt)
    tkwm.geometry(tt,"+50+50")
    tkwm.title(tt,"GenArise Microarray Analyzer")
    tmp <- unlist(strsplit(project.name, .Platform$file.sep))
    tmp[length(tmp)] <- unlist(strsplit(tmp[length(tmp)], "\\."))[1]
    noext <- paste(tmp, collapse = .Platform$file.sep)    
    results.file  <-  spots[1,1]
    graphics.file <-  spots[1,2]
    
    topMenu <- tkmenu(tt)
    tkconfigure(tt,menu=topMenu)
    fileMenu <- tkmenu(topMenu,tearoff=FALSE)
    upper.frame <- tkframe(tt)
    frameOverall <- tkframe(upper.frame)
    frameimage <- tkframe(upper.frame, relief="groove",borderwidth=2)
    frameFeatures <-  tkframe(frameOverall, relief="groove")
    tkgrid(tklabel(frameFeatures, text = "Spot Features"), pady = "7")
#Array geometry
    spot.name <- unlist(strsplit(as.character(spots[2,2]), " "))
    tkgrid(tklabel(frameFeatures, text = paste(spots[2,1], spot.name[1], sep = "\t")), padx = "0",pady = "2", sticky = "w")
    if(is.na(spot.name[3]))
      symdesc <- NULL
    else
      symdesc <- 1
    for(i in 3:6){
      tkgrid(tklabel(frameFeatures, text = paste(spots[i,1], spots[i,2], sep = "\t")), padx = "0",pady = "2", sticky = "w")
    }
#original data
    assign("a.spot1", read.spot(paste(noext,.Platform$file.sep,results.file,.Platform$file.sep,spots[7,2],sep=""), header=TRUE,cy3=1,cy5=2,bg.cy3=3,bg.cy5=4,ids=5,envir=envir),
           envir =  envir)
    datos <- attr(get("a.spot1",envir=envir), "spotData")
    M <- log(datos$Cy5, 2) - log(datos$Cy3, 2)
    prjlength <- length(spots[,1])
    name <- as.character(as.vector(spots[2,2]))
    
    frame1 <- tkframe(frameOverall,relief="groove",borderwidth=2)
    frame2 <- tkframe(frameOverall,relief="groove",borderwidth=2)
    dist  <- tclVar(1)
    
    otra.funcion <-  function(i){
      if(is.element(paste("a.spot",i, sep =""), ls(envir = envir))){
        assign("a.spot", get(paste("a.spot",i, sep =""), envir = envir), envir = envir)
        tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))        
      }else{
        assign(paste("a.spot",as.numeric(tclvalue(dist)),sep=""),
               read.spot(paste(noext,.Platform$file.sep,results.file,.Platform$file.sep,spots[as.numeric(tclvalue(dist)),2],sep=""),header=TRUE,cy3=1,cy5=2,bg.cy3=3,bg.cy5=4,ids=5,envir=envir),
               envir =  envir)
        assign("a.spot", get(paste("a.spot",i, sep =""), envir = envir), envir = envir)
        tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))        
      }
      tkentryconfigure(topMenu,"2",state="normal")
      tkentryconfigure(topMenu,"3",state="normal")
    }
    #Operations
    tkpack(tklabel(frame1, text = "OPERATIONS"), pady = "7")
    if(spots[(length(spots[,1])),1] == "RI Zscore" || spots[(length(spots[,1])),1] == "MA Zscore" ){
      radio.original <- tkradiobutton(frame1, text="Original Spot", value=7, variable=dist,command=function(){
          otra.funcion(as.numeric(tclvalue(dist)))
        })
      tkpack(radio.original,anchor="w", pady = "2",padx="10")
      
      for(i in 8:(length(spots[,1])-1)){
        radio <- tkradiobutton(frame1, text=as.vector(spots[i,1]), value=i, variable=dist,command=function(){
          otra.funcion(as.numeric(tclvalue(dist)))
        })
        tkpack(radio,anchor="w", pady = "2",padx="10")
      }
      
      frame2 <- tkframe(frameOverall,relief="groove",borderwidth=2)
      tkpack(tklabel(frame2, text = "Z-SCORE"), pady = "7")
        assign("Zscore.spot", read.dataset(paste(noext,.Platform$file.sep,results.file,.Platform$file.sep,spots[(length(spots[,1])),2],sep=""),symdesc=symdesc),
               envir =  envir)

      zscore1 <- tkradiobutton(frame2, text="Zscore < 1", value=1, variable=dist,command=function(){
        selected.zscore(all = FALSE, Zscore.max = 1, col = "green")
      })
      tkpack(zscore1,anchor="w", pady = "2",padx="10")
      
      zscore2 <- tkradiobutton(frame2, text="1 < Zscore < 1.5", value=2, variable=dist,command=function(){
        selected.zscore(Zscore.max = 1.5, Zscore.min = 1, all = FALSE, col = "blue")})
      tkpack(zscore2,anchor="w", pady = "2",padx="10")
      
      zscore3 <- tkradiobutton(frame2, text="1.5 < Zscore < 2", value=3, variable=dist,command=function(){
        selected.zscore(Zscore.max = 2, Zscore.min = 1.5, all = FALSE, col = "cyan")})
      tkpack(zscore3,anchor="w", pady = "2",padx="10")
      
      zscore4 <- tkradiobutton(frame2, text="Zscore > 1.5", value=4, variable=dist,command=function(){
        selected.zscore(Zscore.min = 1.5, all = FALSE, col = "yellow")})
      tkpack(zscore4,anchor="w", pady = "2",padx="10")
      
      zscore5 <- tkradiobutton(frame2, text="Zscore > 2", value=5, variable=dist,command=function(){
        selected.zscore(Zscore.min = 2, all = FALSE, col = "snow")})
      tkpack(zscore5,anchor="w", pady = "2",padx="10")
      
      zscore6 <- tkradiobutton(frame2, text="All", value=6, variable=dist,command=function(){
        selected.zscore()})
      tkpack(zscore6,anchor="w", pady = "2",padx="10")
    }
    else{
      radio.original <- tkradiobutton(frame1, text="Original Spot", value=7, variable=dist,command=function(){
          otra.funcion(as.numeric(tclvalue(dist)))
        })
      tkpack(radio.original,anchor="w", pady = "2",padx="10")
      for(i in 8:(length(spots[,1]))){
        radio <- tkradiobutton(frame1, text=as.vector(spots[i,1]), value=i, variable=dist,command=function(){
          otra.funcion(as.numeric(tclvalue(dist)))
        })
        tkpack(radio,anchor="w", pady = "2",padx="10")
      }
    }
    redgreenimg <- tkradiobutton(frameimage, text="Red & Green", value=12, variable=dist,command=function(){
      imageLimma.plot(M)})
    
    redimg <- tkradiobutton(frameimage, text="Background Cy5", value=13, variable=dist,command = function(){
      imageLimma.plot(log(datos$BgCy5, 2),"white","red")})
    
    greenimg <- tkradiobutton(frameimage, text="Background Cy3", value=14, variable=dist,command=function(){
      imageLimma.plot(log(datos$BgCy3, 2),"white","green")})
    
    tkgrid(redgreenimg,redimg,greenimg, pady = "2",padx="10")  
    
    assign("a.spot1",read.spot(paste(noext,.Platform$file.sep,results.file,.Platform$file.sep,spots[7,2],sep=""),header=TRUE,cy3=1,cy5=2,bg.cy3=3,bg.cy5=4,ids=5, envir=envir), envir = envir)
    assign("a.spot", get("a.spot1",envir = envir),envir=envir)
    img <-  tkrplot(upper.frame, fun = function() graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))
    tkadd(fileMenu,"command",label="Back to main", command=function(){ tkdestroy(tt); genArise() })
    tkadd(fileMenu,"command",label="Exit", command=function() tkdestroy(tt))

    tkadd(topMenu,"cascade",label="File",menu=fileMenu)
    
    graphicMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(graphicMenu,"command",label="Cy3 vs Cy5", command = function() {
      assign("graphic.type", 1, envir = envir)
      tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
    tkadd(graphicMenu,"command",label="R vs I",command = function() {
      assign("graphic.type", 2, envir = envir)
      tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
    tkadd(graphicMenu,"command",label="M vs A",command = function() {
      assign("graphic.type", 3, envir = envir)
      tkrreplot(img,fun=function()graphic.choose(get("a.spot", envir = envir), get("graphic.type", envir = envir)))})
    tkadd(topMenu,"cascade",label="Graphics",menu =graphicMenu)
    
    optionsMenu <- tkmenu(topMenu,tearoff=FALSE)
    make.file.name <- function(file.type, ext){
      switch(as.integer(tclvalue(dist)),
             suggested.name <- paste(spot.name[1], "_lower_1.", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_1-1_5.", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_1_5-2.", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_greater_1_5.", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_greater_2.", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_all.", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_", unlist(strsplit(as.character(spots[7,2]), "\\."))[1],".", ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_",unlist(strsplit(as.character(spots[8,2]), "\\."))[1], ".",ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_",unlist(strsplit(as.character(spots[9,2]), "\\."))[1], ".",ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_",unlist(strsplit(as.character(spots[10,2]), "\\."))[1], ".",ext, sep = ""),
             suggested.name <- paste(spot.name[1], "_",unlist(strsplit(as.character(spots[11,2]), "\\."))[1], ".",ext, sep = ""),
             )
      tclvalue(tkgetSaveFile(initialdir = paste(noext,.Platform$file.sep,graphics.file,sep=""),
                             initialfile = suggested.name, filetypes = file.type))
    }
    tkadd(optionsMenu,"command",label="Save graphic as PDF",command = function() {
      name <- make.file.name(file.type = "{{PDF Files} {.pdf}} {{All files} *}", ext ="pdf")
      if (!nchar(name))
       tkmessageBox(parent = tt,  message= "You must write a name of file!", icon = "error", default = "ok")
      else{
        name <- unlist(strsplit(name, "\\."))[1]
        pdf(paste(name,".pdf",sep=""), horiz = FALSE, height = 8, width = 8, title = name)
        switch(as.integer(tclvalue(dist)),
               Zscore.plot(get("Zscore.spot", envir = envir), all = FALSE, Zscore.max = 1, col = "green"),
               Zscore.plot(get("Zscore.spot", envir = envir), Zscore.max = 1.5, Zscore.min = 1, all = FALSE, col = "blue"),
               Zscore.plot(get("Zscore.spot", envir = envir), Zscore.max = 2, Zscore.min = 1.5, all = FALSE, col = "cyan"),
               Zscore.plot(get("Zscore.spot", envir = envir), Zscore.min = 1.5, all = FALSE, col = "yellow"),
               Zscore.plot(get("Zscore.spot", envir = envir), Zscore.min = 2, all = FALSE, col = "snow"),
               Zscore.plot(get("Zscore.spot", envir = envir)),
               ri.plot(get("a.spot", envir = envir)),
               ri.plot(get("a.spot", envir = envir)),
               ri.plot(get("a.spot", envir = envir)),
               ri.plot(get("a.spot", envir = envir)),
               ri.plot(get("a.spot", envir = envir)),
               )
        dev.off(dev.cur())
      }
    })
#    tkadd(optionsMenu,"command",label="Notes",command = function()note(envir))
    tkadd(topMenu,"cascade",label="Options", menu =optionsMenu)
    
    helpMenu <- tkmenu(topMenu,tearoff=FALSE)
    tkadd(helpMenu,"command",label="About genArise...", command=function() help())#{
    
    tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
    
    frame.label <- tkframe(tt,relief="groove",borderwidth=2)
    tkfocus(tt)
    tkselect(radio.original)
    tkgrid(img,frameOverall, padx = "10", pady = "10",sticky="w")
    tkgrid(frameimage, padx = "90", pady = "10",sticky="w")
    tkgrid(frameFeatures)
    tkgrid(frame1, pady = "10")
    tkgrid(frame2, pady = "10")
    tkgrid(upper.frame, pady = "10")
    area.frame <- tkframe(tt,relief="groove",borderwidth=2)
    tkgrid(area.frame, padx = "10")
     }
}
