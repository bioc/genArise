# Miscelaneous functions to get information for the project to perform

# Just set the options for the GUI
set.project.properties <- function(envir){

  assign("graphic.type", 2, envir = envir)
  assign("icons.dir", file.path(system.file(),"..", "genArise", "doc"), envir = envir)

  if(.Platform$OS.type == "unix"){
    assign("Myhscale", 1 , envir = envir)
    assign("Myvscale", 1 , envir = envir)
    assign("barWidth", 70, envir=envir)
    assign("barHeight", 5, envir=envir)
    assign("height","550",envir=envir)
    assign("width", "55", envir = envir)
    assign("editor", "emacs", envir = envir)
  }else{
    assign("Myhscale", 1.3 , envir = envir)
    assign("Myvscale", 1.3 , envir = envir)
    assign("height","550",envir=envir)
    assign("barWidth", 50, envir=envir)
    assign("barHeight", 3, envir=envir)
    assign("width", "55", envir = envir)
    assign("editor", "notepad", envir = envir)
  }
}

# Set properties of the experiment
# nr: Rows, nc: Columns, nmr: Meta-rows, nmc: Meta-columns
set.grid.properties <- function(envir, name, nr, nc, nmr, nmc){
  assign("name.project", name, envir = envir)
  assign("nr", nr, envir = envir)
  assign("nc", nc, envir = envir)
  assign("nmr", nmr, envir = envir)
  assign("nmc", nmc, envir = envir)
}

# The path of the project's output 
set.path.project <-  function(path, results.file, graphics.file, envir){
  assign("path.project", path, envir = envir)
  assign("path.results", file.path(path, results.file), envir = envir)
  assign("path.graphics", file.path(path, graphics.file), envir = envir)
}

# Create the directory's hierarchy of the project
create.project <-  function(project.name, results.file = "Results", graphics.file = "Graphics"){
  if(length(unlist(strsplit(results.file, .Platform$file.sep))) > 1 || length(unlist(strsplit(graphics.file, .Platform$file.sep))) > 1 )
    stop("Argument 2 and 3 must be the file name without the path!..")
  path <-  unlist(strsplit(project.name, .Platform$file.sep))
  history.project <- file(paste(file.path(project.name), ".prj", sep = ""), "w")
  set.history.project(paste(file.path(project.name), ".prj", sep = ""), results.file, graphics.file)
  close(history.project)
  dir.create(project.name)
  dir.create(file.path(project.name, results.file))
  dir.create(file.path(project.name, graphics.file))
}

# Concatenate the complete text that appear in the history file of the project
set.history.project <- function(history.file, id.name, data.file){
  history.project <- file(history.file, "a")
  cat(paste(id.name, data.file, sep = "\t"), file = history.file, append = TRUE, sep = "\n")
  close(history.project)
}

# Select the type of graphic to be shown
graphic.choose <- function(spot.object, graphic.type){
  switch(graphic.type,
         cys.plot(spot.object),
         ri.plot(spot.object),
         ma.plot(spot.object))
}

#Triming a string
trim <- function(word){
  sub("^[ \t\n\r]*", "", sub("[ \t\n\r]*$", "", word))
}

# Update the information of the history file of the project
reset.history <- function(history.file, text){
  write.table(text, history.file, sep ="\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
}


