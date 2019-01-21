#' Discretizar
#'
#' This function allows you to discretize continuous quantitative data into classes above or below the median, and above, below or between SD of the mean.
#' @param data data.frame or matrix
#' @param mu data.frame with either mean or median data
#' @param median: logical. TRUE means mu contains medians, FALSE = mu contains averages and sd.
#' @keywords
#' @export
#' @examples
#' 
#' discretizar()

discretizar <- function(data=data, mu=mu, median=TRUE) {

	# extract mean/median values from df mu
	x1 = mu[1,2] # value LH
	x2 = mu[2,2] # value RY

	# create matrix to store the variables in
	mat <- matrix(nrow=nrow(datos), ncol=5)
	# lower <- as.numeric(rep(999, times=nrow(data)))
	# upper <- as.numeric(rep(999, times=nrow(data)))
	# middle <- as.numeric(rep(999, times=nrow(data))) # solo cuando se calculan medias

	mat[,1] <- datos$site # LH = 1, RY = 2
	mat[,2] <- datos$trait
	
	# si type = "median"
  for(i in 1:nrow(data)){

	 if(median=="TRUE"){
		
		mat[i,3] <- ifelse(mat[i,1] == 1 & mat[i,2] <= x1,1, ifelse(mat[i,1] == 2 & mat[i,2] <= x2,1,0))

		mat[i,4] <- ifelse(mat[i,1] == 1 & mat[i,2] > x1,1, ifelse(mat[i,1] == 1 & mat[i,2] > x2,1,0))

		mat[i,5] <- NA

	} 	else {

		sd1 <- mu[1,3] # SD LH.
		sd2 <- mu[2,3] # SD RY.
		# upper and lower sd limits
		sdlow1 <- x1-sd1
		sdhigh1 <- x1+sd1
		sdlow2 <- x2-sd2
		sdhigh2 <- x2+sd2
	
		mat[i,3] <- ifelse(mat[i,1] == 1 & mat[i,2] < sdlow1,1, ifelse(mat[i,1] == 2 & mat[i,2] < sdlow2, 1, 0))

		mat[i,4] <- ifelse(mat[i,1] == 1 & mat[i,2]  > sdhigh1, 1, ifelse(mat[i,1] == 1 & mat[i,2] > sdhigh2,1,0))

		mat[i,5] <- ifelse(mat[i,1] == 1 & mat[i,2] >= sdlow1 & mat[i,2]  <= sdhigh1,1, ifelse(mat[i,1] == 2 & mat[i,2] >= sdlow2 & mat[i,2] <= sdhigh2,1,0))
	}
}
return(mat)
}


