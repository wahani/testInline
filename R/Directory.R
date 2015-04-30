Directory <- function(name) {

    dirName <- if(!missing(name)) {
        if(!file.exists(name)) {
            message("Creating new directory '", name, "' ...")
            dir.create(name)
        }
        name
    } else {
        getwd()
    }

    dirName %paste0% "/"
}
