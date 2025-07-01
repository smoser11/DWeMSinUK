
## directories for Rwanda tie definition paper
##
set.dirs <- function(root.dir=getwd()) {

  # recall that <<- binds a variable in the enclosing environment,
  # not the current one. so the statements below define these directory
  # variables for the environment this function is called in.
  # this is not great style, but i haven't come up with a better way to 
  # do this yet

    root.dir <<- root.dir

    data.dir <<- file.path(root.dir, "data")
    out.dir <<- file.path(root.dir, "out")
    theme.dir <<- file.path(root.dir, "code")

}

