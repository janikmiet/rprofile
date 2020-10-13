# Custom user Rprofile

# options(show.signif.stars=FALSE) # Don't show those silly significanct stars

## Credentials for connections. Not the most secure option but easiest.
## Simple way to storage username and password for different connections. Use Sys.getenv(USERNAME) function to get USERNAME.
# Sys.setenv(USERNAME = "tunnus") 
# Sys.setenv(PASSWORD = "salasana") 

## Don't ask me for my CRAN mirror every time
options("repos" = c(CRAN = "https://cran.uni-muenster.de/"))

## Create a new invisible environment for all the functions to go in so it doesn't clutter your workspace.
.env <- new.env()

## Rprofile version
.env$rprofile_version <- "1.07.5"

## Update RProfile
.env$rprofile.update <- function(){
  download.file(url = "", destfile = "~/.Rprofile")
  rstudioapi::restartSession()
}

## Install R packages from a list
.env$install.packages.list <- function(list = "https://research.janimiettinen.fi/data/r_packages.txt"){
  if(substr(list,1,4) == "http"){ 
    PACKAGES <- scan(url(list), what="character")
  }else{
    PACKAGES <- scan(list, what="character")
  }
  message("Trying to install packages: ")
  message(paste0(PACKAGES, collapse = ", "))
  inst <- match(PACKAGES, .packages(all=TRUE))
  need <- which(is.na(inst))
  if (length(need) > 0) install.packages(PACKAGES[need])
}

## Returns a logical vector TRUE for elements of X not in Y
.env$"%nin%" <- function(x, y) !(x %in% y)

## Returns names(df) in single column, numbered matrix format.
.env$n <- function(df) matrix(names(df))

## Single character shortcuts for summary() and head().
.env$s <- base::summary
.env$h <- utils::head

## ht==headtail, i.e., show the first and last 10 items of an object
.env$ht <- function(d) rbind(head(d,10),tail(d,10))

## Show the first 5 rows and first 5 columns of a data frame or matrix
.env$hh <- function(d) if(class(d) %in% c("matrix","data.frame","tbl_df")) d[1:5,1:5]

## Show the first 5 rows and first 5 columns of a data frame or matrix
.env$print_cs1 <- function(d) cat(paste(shQuote(d, type="cmd"), collapse=", "))
.env$print_cs2 <- function(d) cat(paste(d, collapse=","))

## Strip row names from a data frame (stolen from plyr)
.env$unrowname <- function(x) {
  rownames(x) <- NULL
  x
}

# for qplots
.env$ggdens <- function(var){
  ggplot2::qplot(x = var, geom="density")
}

.env$ggscat <- function(varx,vary){
  ggplot2::qplot(x = varx, y=vary, geom="point")
}

.env$gghist <- function(var){
  ggplot2::qplot(x = var, geom="histogram")
}

.env$ggbox <- function(var){
  ggplot2::qplot(x = var, geom="boxplot")
}

## List objects and classes (from @_inundata, mod by ateucher)
.env$lsa <- function() {
  obj_type <- function(x) class(get(x, envir = .GlobalEnv)) # define environment
  foo = data.frame(sapply(ls(envir = .GlobalEnv), obj_type))
  foo$object_name = rownames(foo)
  names(foo)[1] = "class"
  names(foo)[2] = "object"
  return(unrowname(foo))
}

## List all functions in a package (also from @_inundata)
.env$lsp <-function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

## table showing Na by default
.env$table_na <- function (..., exclude = NULL, useNA = "always", dnn = list.names(...), deparse.level = 1){
  list.names <- function(...) {
    l <- as.list(substitute(list(...)))[-1L]
    nm <- names(l)
    fixup <- if (is.null(nm))
      seq_along(l)
    else nm == ""
    dep <- vapply(l[fixup], function(x) switch(deparse.level +
                                                 1, "", if (is.symbol(x)) as.character(x) else "",
                                               deparse(x, nlines = 1)[1L]), "")
    if (is.null(nm))
      dep
    else {
      nm[fixup] <- dep
      nm
    }
  }
  if (!missing(exclude) && is.null(exclude))
    useNA <- "always"
  useNA <- match.arg(useNA)
  args <- list(...)
  if (!length(args))
    stop("nothing to tabulate")
  if (length(args) == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    if (length(dnn) != length(args))
      dnn <- if (!is.null(argn <- names(args)))
        argn
    else paste(dnn[1L], seq_along(args), sep = ".")
  }
  bin <- 0L
  lens <- NULL
  dims <- integer()
  pd <- 1L
  dn <- NULL
  for (a in args) {
    if (is.null(lens))
      lens <- length(a)
    else if (length(a) != lens)
      stop("all arguments must have the same length")
    cat <- if (is.factor(a)) {
      if (any(is.na(levels(a))))
        a
      else {
        if (is.null(exclude) && useNA != "no")
          addNA(a, ifany = (useNA == "ifany"))
        else {
          if (useNA != "no")
            a <- addNA(a, ifany = (useNA == "ifany"))
          ll <- levels(a)
          a <- factor(a, levels = ll[!(ll %in% exclude)],
                      exclude = if (useNA == "no")
                        NA)
        }
      }
    }
    else {
      a <- factor(a, exclude = exclude)
      if (useNA != "no")
        addNA(a, ifany = (useNA == "ifany"))
      else a
    }
    nl <- length(ll <- levels(cat))
    dims <- c(dims, nl)
    if (prod(dims) > .Machine$integer.max)
      stop("attempt to make a table with >= 2^31 elements")
    dn <- c(dn, list(ll))
    bin <- bin + pd * (as.integer(cat) - 1L)
    pd <- pd * nl
  }
  names(dn) <- dnn
  bin <- bin[!is.na(bin)]
  if (length(bin))
    bin <- bin + 1L
  y <- array(tabulate(bin, pd), dims, dimnames = dn)
  class(y) <- "table"
  y
}

## Open Finder to the current directory on mac
#   .env$macopen <- function(...) if(Sys.info()[1]=="Darwin") system("open .")
.env$o       <- function(...){
  if(Sys.info()[1]=="Linux") system("nemo .")
  if(Sys.info()[1]=="Windows") shell.exec(file = ".")
  if(Sys.info()[1]=="Darwin") system("open .")
}

## Read data on clipboard.
.env$read_cb <- function(...) {
  ismac <- Sys.info()[1]=="Darwin"
  if (!ismac) read.table(file="clipboard", ...)
  else read.table(pipe("pbpaste"), ...)
}

## List all rprofile functions
.env$rprofile_functions <- function(){
  cat(paste0("User .Rprofile \n") ,sep="")
  cat("s() - shortcut for summary\n",sep="")
  cat("h() - shortcut for head\n",sep="")
  cat("o() - shortcut for file explorer\n",sep="")
  cat("table_na() - table showing Na by default\n", sep="")
  cat("unrowname() - remove data frame row names\n",sep="")
  cat("read_cb() - read from clipboard\n",sep="")
  cat("lsa() - list objects and classes\n",sep="")
  cat("lsp() - list all functions in a package\n",sep="")
  cat("ggdens() - quick density plot\n",sep="")
  cat("ggscat() - quick scatter plot\n",sep="")
  cat("gghist() - quick histogram plot\n",sep="")
  cat("ggbox() - quick boxplot\n",sep="")
}

## Attach all the variables above
attach(.env)

## .First() run at the start of every R session.
.First <- function() {
  hello_message <- c(
    "Hello, sunshine",
    "Howdy, partner",
    "Hey, howdy, hi",
    "What’s kickin’, little chicken",
    "Peek-a-boo",
    "Howdy-doody",
    "Hey there, freshman",
    "Hi, mister",
    "I come in peace",
    "Put that cookie down",
    "Ahoy, matey",
    "Hiya",
    "Ello, gov'nor",
    "Top of the mornin’ to ya",
    "What’s crackin’",
    "GOOOOOD MORNING, VIETNAM",
    "‘Sup, homeslice",
    "This call may be recorded for training purposes",
    "Howdy, howdy ,howdy",
    "I'm Batman",
    "At least, we meet for the first time for the last time",
    "Hello, who's there, I'm talking",
    "You know who this is",
    "Ghostbusters, whatya want",
    "Yo",
    "Whaddup",
    "Greetings and salutations",
    "Doctor"
  )
  stringi <- paste0("\n", hello_message[sample(1:length(hello_message), 1)], " ",Sys.info()["user"][[1]],"! Loaded .Rprofile (v.",rprofile_version,") at ", strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n")
  cat(stringi)
}

## .Last() run at the end of the session
.Last <- function() {
  goodbye_message <- c("See you later, alligator!", "After a while, crocodile.", "Stay out of trouble.", "I’m out of here.", "Okay...bye, fry guy!", "If I don’t see you around, I'll see you square.", "Stay classy.", "Fare thee well.", "Catch you on the rebound.", "Gotta go, buffalo.", "Peace out!", "Gotta hit the road.", "Long live and prosper!", "Well, I'm off!", "Smoke me a kipper, I'll be back for breakfast.", "Bye bye, butterfly.", "Gotta get going.", "To the winch, wench!", "It has been emotional, bye.", "Out to the door, dinosaur.", "Catch you on the flip side.", "Gotta bolt!", "See you soon, racoon.", "You're still here? It's over. Go home. Go!", "Don't get run over!", "Give a hug, ladybug.", "I gotta bounce.", "Toodle-pip!", "Calc you later!", "See you on the other side.", "Take care, butterfly.", "Have fun storming the castle!", "Adieu, cockatoo!", "Blow a kiss, goldfish.", "It's been a pleasure and a privilege to meet you.", "Don't get attacked by a bear, it's night-time!", "Be good and don't get caught.", "Thank you for your cooperation. Farewell.", "I look forward to our next meeting.", "I'm gonna make a like a bakery truck and haul buns.", "Chop chop, lollipop!", "Gotta roll!", "Can’t star, blue jay.", "Oh, and in case I don't see you—good afternoon, good evening, and good night!", "Influence everyone in a good way!", "Don't forget to come back!", "Once more unto the breach, dear friends!", "See ya, wouldn't wanna be ya.", "Peace out, girl scout!", "Adios, hippos.", "Time to scoot, little newt.", "Smell ya later!", "I gotta jet.", "Happy trails!", "Cheerio!", "Bye for now.", "Tootle-loo, kangaroo.", "Don't get lost on your way to class!", "Love, peace, and chicken grease.", "I'm off like a dirty shirt.", "See you when I see you.", "In a while, crocodile.", "Catch ya later, future dudes!", "Cya. (Clearly, this is just short for ‘see you,’ which makes no sense because you utter ‘cya’ and not write it. Oh, whatever!)", "As you wish, jellyfish!", "Later, skater!", "May the force be with you... always.", "Shine on, you crazy diamonds.", "Parting is such sweet sorrow, that I shall say good night till it be tomorrow.", "Don't let the door hit ya where the good lord split ya.", "Better shake, rattlesnake!", "Later, potato!", "Don't forget to be awesome.", "Later, nerds!", "Stay cool, my dude.", "Don't get cut by a blade of grass!", "Be sweet, parakeet.", "Be careful! Don't get mauled by a squirrel!", "See you later, aggregator!", "Don't trip on a raindrop!", "See you soon, baboon!", "Bye! I tolerate you!", "Gotta go, the power of the shower compels me.", "Make new friends on the sidewalk!", "I’m late for my bus, gigantopithecus!", "Move out, brussels sprout!", "Make sure the doormat says goodbye!", "I’ll show you to the door!", "Ciao ciao, brown cow!", "Screw you guys, I'm going home!", "I shall return.", "Catch you round like a rissole!", "Take it easy, greasy. You've got a long way to slide.", "Toodaloo, caribou!", "I'm outtie.", "Adios, amigos.", "That's all folks.", "Take care, polar bear!", "Peace out, rainbow trout!", "I'm outta here like spit through a trumpet.", "Au revoir!", "See you in the future.", "Begone!", "Until next time.", "So long, suckers!", "Hasta lasagna, don't get any on ya.", "Sayonara, muchachos!", "Next time, bring more cookies.", "Party easy, drive safe, and return with a smile on your face.", "After two, kangaroo!", "After three, chimpanzee!", "After four, dinosaur.", "Come back when you can't stay so long.", "Don’t forget to send a letter.", "Goodbye forever.", "See you in another life, brotha!", "We may not talk for a long time, but I hope we don't lose touch.", "Never look back!", "See you on the internet!", "Forever and forever farewell. If we do meet again, we'll smile indeed. If not, 'tis true parting was well made.", "You will do well.", "See you at the restaurant at the edge of the universe!", "I'd say goodbye, but you're not worth it.")
  cat(paste0("\n ", goodbye_message[sample(1:length(goodbye_message), 1)]," You finished at ", strftime(Sys.time(),"%Y-%m-%d %H:%M:%S"), "\n"))
}
