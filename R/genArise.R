# Main Window of genArise
# Contains menus for all the options in the system
genArise.init <- function(envir){ 
if(.Platform$pkgType == "mac.binary")
  addTclPath("/Library/Tcl")
  tclRequire("BWidget")
  tclRequire("Img")
  op.counter <<- 0  
  tt <- tktoplevel()
  tkfocus(tt)
  tkwm.geometry(tt,"+50+50")
  tkwm.maxsize(tt, get("height", envir = envir), get("width", envir = envir))
  tkwm.minsize(tt, get("height", envir = envir), get("width", envir = envir))
  tkwm.title(tt,"GenArise Microarray Analyzer")
  topMenu <- tkmenu(tt)
  tkconfigure(tt, menu = topMenu)
  fileMenu <- tkmenu(topMenu,tearoff=FALSE)
  optMenu <- tkmenu(topMenu,tearoff=FALSE)
  helpMenu <- tkmenu(topMenu,tearoff=FALSE)
  project.menu <- tkmenu(topMenu,tearoff=FALSE)

  # open a project already performed
  abre.project <- function(){
    file <- tclvalue(tkgetOpenFile(filetypes = "{{PRJ Files} {.prj}} {{All files} *}"))
    if(nchar(file)){
      old.project(file, envir,tt)
      tkdestroy(tt)
    }
  }

  # open a new project to be performed
  nuevo.project <-  function(){
    tkdestroy(tt)
#    new.project(envir)}
    select.experiments(envir)}

  # select the projects to perform the post-analysis
  projectos.select <- function() {
    tkdestroy(tt)
    projects.select(envir = envir)
  }

  swap.analysis <- function(){
    tkdestroy(tt)
    swap.select(envir = envir)
  }
  
  destroy <- function(){
    tkdestroy(tt)
  }
  
  tkadd(project.menu,"command",label="New Project     (Ctrl-N)", command=function(){
    nuevo.project()
  })
  tkadd(project.menu,"command",label="Open Project    (Ctrl-O)", command=function(){
    abre.project()
  })
  tkadd(fileMenu,"cascade",label="Project",menu=project.menu)
  tkadd(fileMenu,"command",label="Swap Analysis   (Ctrl-S)",command=function(){
    swap.analysis()})
  tkadd(fileMenu,"command",label="Post-analysis     (Ctrl-P)", command = function(){
    projectos.select()})
  tkadd(fileMenu,"command",label="Quit                   (Ctrl-X)",command=function() destroy())
  tkadd(topMenu,"cascade",label="File",menu=fileMenu)
  helpMenu <- tkmenu(topMenu,tearoff=FALSE)
  tkadd(helpMenu,"command",label="About genArise...", command=function() help())
  tkadd(topMenu,"cascade",label="Help",menu = helpMenu)
  lower.frame <- tkframe(tt)
  
  butt.frame <- tkframe(lower.frame)
  label.frame <- tkframe(lower.frame)
  etiqueta <- tklabel(label.frame, text = "")
  tkconfigure(etiqueta, text = "", width = "100")

  # shortcuts
  tkbind(tt, "<Control-O>",abre.project)   
  tkbind(tt, "<Control-o>",abre.project)
  tkbind(tt, "<Control-N>",nuevo.project)
  tkbind(tt, "<Control-n>",nuevo.project)
  tkbind(tt, "<Control-S>",swap.analysis)
  tkbind(tt, "<Control-s>",swap.analysis)   
  tkbind(tt, "<Control-P>",projectos.select)
  tkbind(tt, "<Control-p>",projectos.select)   
  tkbind(tt, "<Control-X>",destroy)
  tkbind(tt, "<Control-x>",destroy)
  openButton <- .Tcl(paste("button",.Tk.subwin(butt.frame),"-image [Bitmap::get open]",
                           .Tcl.args(command = abre.project))) 
  newButton <- .Tcl(paste("button",.Tk.subwin(butt.frame),"-image [Bitmap::get new]",
                          .Tcl.args(command = nuevo.project)))

  # icons in the bar menu
  tcl("image","create","photo","help",file=file.path(get("icons.dir", envir = envir),"icons/help.png"))
  help.button <- tkbutton(butt.frame,image="help", command = function() help())
  tkconfigure(help.button, heigh = 16, width = 16)

  tcl("image","create","photo","swap",file=file.path(get("icons.dir", envir = envir),"icons/swap.png"))
  swap.button <- tkbutton(butt.frame,image="swap", command = function() swap.analysis())
  tkconfigure(swap.button, heigh = 16, width = 16)
  
  tcl("image","create","photo","exit",file=file.path(get("icons.dir", envir = envir),"icons/logout.png"))
  exit.button <- tkbutton(butt.frame,image="exit", command = function(){
    tkdestroy(tt)})
  tkconfigure(exit.button, heigh = 16, width = 16)
                  
  tkpack(etiqueta, anchor = "e")
  tkgrid(newButton, openButton, swap.button, help.button, exit.button, sticky = "nw")
  tkgrid(butt.frame, label.frame)

  
  label.function <-  function(texto.etiqueta) tkconfigure(etiqueta, text = texto.etiqueta, width = "100", fg = "red")

  tkbind(openButton, "<Enter>", function() label.function("Open a project"))
  tkbind(openButton, "<Leave>", function() label.function(""))

  tkbind(newButton, "<Enter>", function()  label.function("Create a  new project"))
  tkbind(newButton, "<Leave>", function() label.function(""))

  tkbind(swap.button, "<Enter>", function() label.function("Dye-Swap Analysis"))
  tkbind(swap.button, "<Leave>", function() label.function(""))

  tkbind(help.button, "<Enter>", function() label.function("Display genArise's Help"))
  tkbind(help.button, "<Leave>", function() label.function(""))

  tkbind(exit.button, "<Enter>", function() label.function("Exit of genArise GUI"))
  tkbind(exit.button, "<Leave>", function() label.function(""))
  
  tkpack(lower.frame, padx = "0", anchor = "w")
}
  

genArise <- function(){
  genArise.env <- new.env()
  set.project.properties(genArise.env)
  assign("info.message",
         "This package was made in the Institute of cellular Physiology UNAM\nmailto: genArise@ifc.unam.mx",
         envir = genArise.env)
  genArise.init(genArise.env)
}
