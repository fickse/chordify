
reformat <- function(groups, ids, el){
  
  # reformat data into matrix 
#         to
#                 
#           a    b    (a+b)     c      d     e   (c + d + e) ...
# from   a 

#        b

#       (a + b)

#        c

#        d

#        e

#        (c + d + e)

  
  
  # TODO put a bunch of checks on inputs here
  
  el$v <- as.numeric(el$v)
  
  # change edge list to matrix ordered by groups and ids

	d<-matrix(0, length(ids), length(ids))
	rownames(d) <- colnames(d) <- ids
	el$from <- match(el$from, ids)
	el$to <- match(el$to, ids)
	el <- as.matrix(el)
	for(i in 1:NROW(el)) d[ el[i,1], el[i,2] ] <- el[i,3]  # SEE UPDATE
	
	regions <- groups

	# within region

	country2regionExports <- apply(d, 1, function(x) aggregate(x, by = list(regions), FUN = sum))
	region2countryImports <- apply(d, 2, function(x) aggregate(x, by = list(regions), FUN = sum))


	subtotExp <- t(do.call(cbind, lapply(country2regionExports, function(x) {x$x})))
	colnames(subtotExp) <- country2regionExports[[1]]$Group.1
    

	subtotImp <- do.call(cbind, lapply(region2countryImports, function(x) x$x))
	rownames(subtotImp) <- region2countryImports[[1]]$Group.1


	corners <- apply( subtotExp, 2, function(x) aggregate(x, by = list(regions), FUN = sum))
	corn <- do.call(cbind, lapply(corners, function(x) x$x))
	rownames(corn) <- colnames(corn)

	# make a vector of new column names
  #regIND  = c(0, which(diff(match(groups, groups)) > 0))
	
	regIND = match(names(corners), groups) -1
	val <- c(rownames(d), names(corners))
	id <- c(seq_along(rownames(d)), regIND)
	val <- val[order(id)]

	# iterate to build new matrix
	out <- matrix(NA, length(val), length(val))
	colnames(out) <- rownames(out) <- val


	for (TO in val) { 
#		cat(TO, '\n\n')
		for (FROM in val) {
#			cat('\t', FROM, '\r')
			if( FROM %in% regions){
				
				if( TO %in% regions){
					out[FROM, TO]  <- corn[FROM, TO]
				} else {
					out[FROM, TO] <- subtotImp[FROM, TO]
				}
			} else {
				
				if( TO %in% regions ){
					out[FROM,TO] <- subtotExp[FROM, TO]
				} else {
					out[FROM,TO] <- d[FROM,TO]
				}
			}
		}
	}

	gindex <- which(val %in% regions) - 1

	mat <- out

	
	finalnames <- colnames(out)

	colnames(out) <- rownames(out) <- NULL

	final <- list( 'names' = finalnames,
					"matrix" = list( '2005' = mat),
					'regions' = gindex)

	final 				
}

insert_data <- function(dta, outfile, infile = 'globalmigration_template.html') {
  require(jsonlite)
	fin <- readLines(infile)
	i <- grep('var data =', fin)
	fin[i+1] <- prettify(toJSON(dta))
	
	cat(fin, file = outfile, sep = '\n')
}
