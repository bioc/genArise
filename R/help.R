help <- function(){
  tclRequire("BWidget")
  tt <- tktoplevel()
  tkwm.title(tt,"genArise Help")
  upper.frame <- tkframe(tt)
  
  frameOverall <- tkframe(upper.frame)
  
  yscr <- tkscrollbar(frameOverall, command=function(...)tkyview(txt,...))
  txt <- tktext(frameOverall,bg="white",font="Helvetica 12",yscrollcommand=function(...)tkset(yscr,...))
  tkgrid(txt,yscr)
  tkgrid.configure(yscr, stick="nsw")

  
  tree.frame <- tkframe(upper.frame, relief = "groove", borderwidth=2)
  
  xScr       <- tkscrollbar(tree.frame,command=function(...)tkxview(treeWidget,...),orient="horizontal")
  yScr       <- tkscrollbar(tree.frame,command=function(...)tkyview(treeWidget,...))
  treeWidget <- tkwidget(tree.frame,"Tree",xscrollcommand=function(...)tkset(xScr,...),
                         yscrollcommand=function(...)tkset(yScr,...),width=30,height=20)
  tkgrid(treeWidget,yScr)
  tkgrid.configure(yScr,stick="nsw")
  tkgrid(xScr)
  tkgrid.configure(xScr,stick="new")

   tkinsert(treeWidget,"end","root", "About genArise", text = "About genArise")
  
  tkinsert(treeWidget,"end","root", "Project", text = "Project")
  tkinsert(treeWidget,"end",  "Project", "Creating a New Project", text = "Creating a New Project")
  tkinsert(treeWidget,"end",  "Creating a New Project", "File Format", text = "File Format")
  tkinsert(treeWidget,"end",  "Project", "Opening an Old Project", text = "Opening an Old Project")
  
  tkinsert(treeWidget,"end","root", "Diagnostic Plots", text = "Diagnostic Plots")
  tkinsert(treeWidget,"end",  "Diagnostic Plots", "Intensity Ratio Plot", text = "Intensity Ratio Plot")
  tkinsert(treeWidget,"end",  "Diagnostic Plots", "Background of Cy3 Plot", text = "Background of Cy3 Plot")
  tkinsert(treeWidget,"end",  "Diagnostic Plots", "Background of Cy5 Plot", text = "Background of Cy5 Plot")
  
  tkinsert(treeWidget,"end","root", "Microarray Analysis", text = "Microarray Analysis")
  tkinsert(treeWidget,"end",  "Microarray Analysis", "Background Correction", text = "Background Correction")  
  tkinsert(treeWidget,"end",  "Microarray Analysis", "Normalization", text = "Normalization")
  tkinsert(treeWidget,"end",  "Normalization", "By Grid", text = "By Grid")
  tkinsert(treeWidget,"end",  "Normalization", "Global", text = "Global")
  tkinsert(treeWidget,"end",  "Microarray Analysis", "Filter", text = "Filter")
  tkinsert(treeWidget,"end",  "Microarray Analysis", "Duplicates Analysis", text = "Duplicates Analysis")
  tkinsert(treeWidget,"end",  "Duplicates Analysis", "Mean Replicates Filter", text = "Mean Replicates Filter")
  tkinsert(treeWidget,"end",  "Duplicates Analysis", "Non-extreme Values Filter", text = "Non-extreme Values Filter")
  tkinsert(treeWidget,"end",  "Duplicates Analysis", "Geometric Mean Filter", text = "Geometric Mean Filter")

  tkinsert(treeWidget,"end","root", "Graphics", text = "Graphics")
  tkinsert(treeWidget,"end",  "Graphics", "Cy3 -vs- Cy5", text = "Cy3 -vs- Cy5")
  tkinsert(treeWidget,"end",  "Graphics", "R -vs- I", text = "R -vs- I")
  tkinsert(treeWidget,"end",  "Graphics", "M -vs- A", text = "M -vs- A")

  tkinsert(treeWidget,"end",  "root", "Write Output", text = "Write Output")

  tkinsert(treeWidget,"end","root", "Z-score", text = "Z-score")
  
  tkinsert(treeWidget,"end","root", "Post-Analysis", text = "Post-Analysis")

  tkinsert(treeWidget,"end",  "root", "genArise Keyboard Shortcuts", text = "genArise Keyboard Shortcuts")
  
  tkselection.set(treeWidget,"About genArise")
  topic <- tclvalue(tkcmd(treeWidget,"selection","get"))
  topic <- unlist(strsplit(unlist(strsplit(topic,"\}"))[1],"\{"))
  topic <- topic[topic != ""]
  line <- readLines(file.path(system.file(),"..","genArise","doc","help","contents"))
  title <- which(line == topic)
  lim <- which(line[title:length(line)] == "*")[1]
  text <- paste(line[title:((lim-1)+(title-1))],"",collapse="\n\t",sep="")
  tkinsert(txt,"0.0",text)
  events <- function(){
    tkconfigure(txt, state="normal")
    tkdelete(txt,"0.0","end")
    topic <- tclvalue(tkcmd(treeWidget,"selection","get"))
    topic <- unlist(strsplit(unlist(strsplit(topic,"\}"))[1],"\{"))
    topic <- topic[topic != ""]
    line <- readLines(file.path(system.file(),"..", "genArise","doc","help","contents"))
    title <- which(line == topic)
    lim <- which(line[title:length(line)] == "*")[1]
    text <- paste(line[title:((lim-1)+(title-1))],"",collapse="\n\t",sep="")
    tkinsert(txt,"0.0",text)
    tkconfigure(txt, state="disabled")
  }
      



  tkbind(tt, "<Button-1>",events)  
  tkgrid(tklabel(upper.frame,text = "Contents" ), padx = "10")
  tkgrid(tree.frame, frameOverall,padx = "10", pady = "10")
  tkgrid(upper.frame, pady = "10")
}
