# Set user object based on computer signature
#   Because we all can't work on the same server, this script can be used,
#   in combination with 'if (user == "SMW") ...' statements, to specify 
#   which files paths to use depending on whom is running the code


# TODO: Jameal
#   1) Run Sys.info()["nodename"]
#   2) Add nodename and initials below

user <- switch(
  Sys.info()["nodename"], 
  "SWC-SWOODMAN-L" = "SMW"
  # "Jameal nodename" = "JS"
)
