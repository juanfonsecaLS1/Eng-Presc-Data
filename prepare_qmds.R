template <- read_lines(file = "template_results.qmd")

for (i in CCG_boundaries$code){
  temp_file <- template
  temp_file[4] <- paste0('t_code <- \"',i,'\"')
  write_lines(temp_file,file = paste0("CCG_results_",match(i,CCG_boundaries$code),".qmd"))
}

