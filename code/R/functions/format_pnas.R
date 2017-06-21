
#' Format .tex file to follow PNAS style
#'
#' This function formats a .tex file to follow PNAS conventions.
#' @param input_file \code{character} file path for input .tex file
#' @param output_file \code{character} file path for input .tex file
#' @return \code{invisible()}
format_pnas <- function(input_file, output_file) {
	## initialization
	# load tex file
	input_file <- readLines(input_file)
	# input_file <- gsub("documentclass[9pt,twocolumn,twoside]", "documentclass[9pt,twocolumn,twoside,lineno]", input_file, fixed = TRUE)

	## main processing
	# parse bibliography
	input_file <- gsub('\\hypertarget{refs}{}', '\\begin{thebibliography}{100}', input_file, fixed=TRUE)
	for (i in seq(grep('\\begin{thebibliography}', input_file, fixed=TRUE)+1, length(input_file))) {
		# replace hypertargets with bibitems
		if (grepl('\\hypertarget{ref-',input_file[i], fixed=TRUE)) {
			curr_ref <- gsub('\\hypertarget{ref-', '', input_file[i], fixed=TRUE)
			curr_ref <- gsub('}{}', '', curr_ref, fixed=TRUE)
			input_file[i] <- paste0('\\bibitem{',curr_ref,'}')
		}
		# remove numbers from references
		if (grepl('\\bibitem{', input_file[i-1], fixed=TRUE)) {
			input_file[i] <- paste(stringi::stri_split_fixed(str=input_file[i], pattern='. ')[[1]][-1], collapse='. ')
		}
	}
	# add in end of bibliograpgy
	end_pos <- which(input_file=='\\end{document}')
	input_file <- c(input_file[seq_len(end_pos-1)], '\\end{thebibliography}', input_file[seq(end_pos, length(input_file))])
	# parse table
	input_file <- gsub('\\begin{tabular}{', '\\begin{tabular*}{\\hsize}{@{\\extracolsep{\\fill}}', input_file, fixed=TRUE)
	input_file <- gsub('\\end{tabular}', '\\end{tabular*}', input_file, fixed=TRUE)
	input_file <- gsub('\\begin{table}[H]', '\\begin{table*}', input_file, fixed=TRUE)
	input_file <- gsub('\\end{table}', '\\end{table*}', input_file, fixed=TRUE)
	# parse figure file paths
	fig_pos <- grep('\\includegraphics', input_file, fixed=TRUE)
	input_file[fig_pos] <- gsub('}', '.pdf}', input_file[fig_pos], fixed=TRUE)
	# parse corresponding author
	input_file <- gsub('\\correspondingauthor{\\textsuperscript{} }','\\correspondingauthor{\\textsuperscript{1} }',input_file,fixed=TRUE)
	## exports
	writeLines(input_file, output_file)
	return(invisible())
}
