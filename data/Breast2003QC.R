if (!is.R()) {
	## install required package RSQLite (and DBI) from CSAN if necessary
	
	# create a temporary installation directory for install.packages() calls.
	# setting destdir explicitly in install.packages() avoids a message printed
	# to the command line defining where the temporary installation took place.
	tmpdir <- file.path(tempfile("dir"), "downloaded_packages")
	if(!file.exists(tmpdir) && !dir.create(tmpdir, recursive=T))
	  stop(sprintf("Unable to create temporary directory '%s'", tmpdir))
	
	"quietRequire" <- function(package)
	{
	  if (!is.character(package) || length(package) > 1)
	    stop("package must be a scalar character string")
	  package <- as.character(substitute(package))
	  val <- try(library(package, character.only=T))
	  return(!inherits(val, "Error"))
	}
	
	# pkgutils
	pkgutils.pos <- attached.where("pkgutils", nomatch=0)
	if (!pkgutils.pos) {
	  dscfile <- system.file(package="pkgutils", "DESCRIPTION")
	  if (!file.exists(dscfile)) {
	    cat("Updating required library: pkgutils\n")
	    install.pkgutils(update=T)
	  }
	
	  cat("Loading required library: pkgutils.\n")
	  library("pkgutils")
	}
	
	# DBI
	DBI.pos <- attached.where("DBI", nomatch=0)
	if (!DBI.pos) {
	  if (!quietRequire("DBI")) {
	    cat("Installing required library: DBI.\n")
	    install.packages("DBI", lib=.libPaths()[1], destdir=tmpdir)
	  }
	
	  cat("Loading required library: DBI.\n")
	  library("DBI")
	}
	
	# RSQLite
	RSQLite.pos <- attached.where("RSQLite", nomatch=0)
	if (!RSQLite.pos) {
	  if (!quietRequire("RSQLite")) {
	    cat("Installing required library: RSQLite.\n")
	    install.packages("RSQLite", lib=.libPaths()[1], destdir=tmpdir)
	  }
	
	  cat("Loading required library: RSQLite.\n")
	  library("RSQLite")
	}
}

if(!require(RSQLite)) stop("RSQLite is required but not available")
dbname <- ifelse(is.R(), paste(system.file("data", package="msBreast"), "Breast2003QC.db", sep="/"), "Breast2003QC.db")	
con <- dbConnect(drv = dbDriver("SQLite"), dbname = dbname)
mz <- dbReadTable(con, "mz", row.names="row_names", check.names=FALSE)
intensity <- dbReadTable(con, "intensity", row.names="row_names", check.names=FALSE)
type <- dbReadTable(con, "type", row.names="row_names", check.names=FALSE)
Breast2003QC <- list(mz=mz[,1], intensity=as.matrix(intensity), type=as.factor(type[,1]))
attr(Breast2003QC, "class") <- "msSet"
attr(Breast2003QC, "data.name") <- "Breast Cancer QC"
dbDisconnect(con)
rm(dbname, con, mz, intensity, type)