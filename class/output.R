
require(compiler)


.output.report <- cmpfun(function(infos, file.type='HTML') {
  switch(
    file.type,
    HTML = .output.report.html(infos)
  )
  
  
})

.output.report.name <- cmpfun(function(infos) {
  # ''' build file name for output file '''
  # 2016-08-11: Version 1.0
  if (nrow(infos) == 1) {
    account <- infos$Account
    if (is.na(account)) {
      account <- 'Unknow'
    }
    name <- infos$Name
    if (is.na(name)) {
      name <- 'Unknow'
    }
  } else {
    account <- 'Multi'
    name <- 'Combi'
  }
  sprintf('Report-%s_%s', account, name)
})# FINISH

#### OUTPUT: html ####

.output.report.html <- cmpfun(function(markdown, infos) {
  windowsFonts(CON = windowsFont("Consolas"))
  
  
  
  report.output.file(markdown, infos, dir, environment())
})

.output.report.html.render <- cmpfun(function(markdown, infos, envir) {
  # ''' report output file '''
  # 2017-01-30: Version 1.0
  # markdown <- ifelse(comment, './Markdowns/Output.Comment.Rmd', './Markdowns/Output.Rmd')
  file.name <- paste0(.output.report.name(infos), '.html')
  Sys.setlocale(locale = 'us')
  render(markdown, html_document(), output_file = file.name, quiet = T, envir = envir)
  Sys.setlocale(locale = 'Chinese')
})# FINISH

.output.report.html.infos <- cmpfun(function(infos) {
  # ''' infos for output '''
  # 2016-08-11: Version 1.0
  infos$Time <- as.character(infos$Time)
  htmlTable(infos,
            css.table = "width: 100%; margin-top: 1em; margin-bottom: 1em;",
            rnames = F)
})# FINISH

