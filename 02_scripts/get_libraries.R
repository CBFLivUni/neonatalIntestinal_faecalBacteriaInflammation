

GetLibsFromJSON <- function(path_name="./", pattern="*.json", exclude_this="github", install_pkgs=TRUE, ...) {

  # Test if json isn't installed 
  if (!require("rjson", quietly = TRUE)) { 
    
    # Install json
    print("##### Installing required rjson package")
    install.packages("rjson") 
    
  }
  
  # Load json
  require(rjson)
  
  # Read in libraries
  files2read <- list.files(path=path_name, pattern=pattern)
  files2read <- files2read[!grepl(exclude_this,files2read)]
  libs <- lapply(files2read, function(filename) { 
                   
                   # Read in libraries from .json file
                   json_file <- rjson::fromJSON(file=paste0(path_name,filename)) 
                   
                   # Return
                   return(json_file)
  
  })[[1]]
  
  # If install as well
  if (install_pkgs) { InstallAndLoadMissingPackages(libs, ...) }
  
  # Return
  return(libs)
  
}

GetLibsFromGitHubJSON <- function(path_name="./", pattern=".*github.*.json", install_pkgs=TRUE) {

  # Test if json isn't installed 
  if (!require("rjson", quietly = TRUE)) { 
    
    # Install json
    print("##### Installing required rjson package")
    install.packages("rjson") 
    
  # Else test if devtools isn't installed
  } else if (!require("devtools", quietly = TRUE)) { 
    
    # Install devtools
    print("##### Installing required devtools package")
    install.packages("devtools") 
    
  }
  
  # Load json
  require(rjson)
  require(devtools)
  
  # Read in libraries
  files2read <- list.files(path=path_name, pattern=pattern)
  libs <- lapply(files2read, function(filename) { 
                   
                   # Read in libraries from .json file
                   json_file <- rjson::fromJSON(file=paste0(path_name,filename)) 
                   
                   # Return
                   return(json_file)
  
  })[[1]]
  
  # If install as well
  if (install_pkgs) { InstallAndLoadMissingGithubPackages(libs) }
  
  # Return
  return(libs)
  
}

InstallAndLoadMissingPackages <- function(libs, ...) {

  # Check if BiocManager isn't installed, install if not
  if (!require("BiocManager", quietly = TRUE)) { install.packages("BiocManager") }
  
  ## Install packages
  # Loop over package set names
  for (pkgset_name in names(libs)) {
  
    # Test if packages installed
    pkgs <- data.frame(installed.packages())$Package
    missing_pkgs <- libs[[pkgset_name]][!libs[[pkgset_name]] %in% pkgs]
  
    # Install missing packages
    if (length(missing_pkgs) > 0) {
      
      # Set timeout high if install larger (ie annotation) pkgs
      options(timeout = 3600) # 1 hour
      
      # Install
      print(paste("##### INSTALLING MISSING PACKAGES :- ", 
                  paste(missing_pkgs, collapse=", ")))
      BiocManager::install(missing_pkgs, ...)
      
    }
  
    # Load libraries
    x <- lapply(libs[[pkgset_name]], require, character.only = TRUE)
    
  }
  
  # End
  print("##### DONE!")
}

InstallAndLoadMissingGithubPackages <- function(libs) { 
    
  # Test if packages installed
  pkgs <- data.frame(installed.packages())$Package
  missing_pkgs <- names(libs)[!names(libs) %in% pkgs]
    
  ## Install packages
  # Loop over .jsons
  for (pkg_name in missing_pkgs) {
      
    # Install missing packages
    if (length(missing_pkgs) > 0) {
      
      # Loop over packages
      for (pkg_code in unname(missing_pkgs)) {
        
        # Install packages
        eval(parse(text=pkg_code))
        
      }
      
    }
    
  }  
      
  # Load packages
  x <- lapply(names(libs), require, character.only = TRUE)
    
}


