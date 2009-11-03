

#' Plots the Scatter Plot
#' Make a scatter plot with a given data set
#'
#' @param data data set using
#' @param ... other arguments to add to geom_point by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_points(mtcars, x = "disp", y = "hp")
#' ggally_points(mtcars, x = "disp", y = "hp", colour = "as.factor(cyl)", size = "gear")
ggally_points <- function(data, ...)
{
	ggplot(data = data, aes_string(...)) + geom_point()
}

#' Plots the Scatter Plot with Smoothing
#' Add a smoothed condition mean with a given scatter plot
#'
#' @param data data set using
#' @param ... other arguments to add to geom_point by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_smooth(iris, x = "Sepal.Length", y = "Sepal.Width")
#' ggally_smooth(iris, x = "Sepal.Length", y = "Petal.Length", colour = "Species")
ggally_smooth <- function(data, ...)
{
	ggplot(data = data, aes_string(...)) +
		geom_smooth(method="lm", colour = I("black")) +
		geom_point() 
}

#' Plots the Scatter Density Plot
#' Make a scatter density plot from a given data
#'
#' @param data data set using
#' @param ... parameters sent to aes_string(...).  Will effect either stat_density2d or geom_density2d
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_density(iris, x = "Sepal.Length", y = "Petal.Length")
#' ggally_density(iris, x = "Sepal.Length", y = "Petal.Length", filled = "TRUE")
#' ggally_density(iris, x = "Petal.Length", y = "Petal.Width",filled = "TRUE") + scale_fill_gradient(breaks = c(0.05, 0.1,0.15,0.2))
ggally_density <- function(data, ...)
{
  
  aesString <- aes_string(...)
  filled <- as.logical(aesString$filled)
  print(filled)
  if(length(filled) < 1)
    filled <- FALSE
  aesString$filled <- NULL
  
  print(filled)
  print(str(aesString))
  
  
  p <- ggplot(data = data, aesString)
	if(filled)
		p <- p + stat_density2d(aes(fill = ..level..), geom="polygon")
	else
	  p <- p + geom_density2d( colour = I("black"))
	
	p
}

#' Correlation from the Scatter Plot
#' estimate correlation from the given data
#'
#' @param data data set using
#' @param ... other arguments being supplied to geom_text by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_cor(iris, x = "Sepal.Length", y = "Petal.Length")
#' ggally_cor(iris, x = "Sepal.Length", y = "Petal.Length", size = 15, colour = "red")
ggally_cor <- function(data, ...)
{

  aesString <- aes_string(...)

  xVar <- data[,as.character(aesString$x)]
  yVar <- data[,as.character(aesString$y)]

	ggally_text(
		label = paste(
			"Corr:\n",
			signif(
				cor(xVar,yVar),
				3
			),
			sep="",collapse=""
		),
		xP=0.5,
		yP=0.5,
		xrange = range(xVar),
		yrange = range(yVar),
		...
	) +  theme_bw() + opts(legend.position = "none")

}
		

#' Plots the Box Plot
#' Make a box plot with a given data set
#'
#' @param data data set using
#' @param ... other arguments being supplied to geom_boxplot by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_box(iris, x = "Petal.Width", y = "Species")
#' ggally_box(iris, y = "Petal.Width", x = "Species", colour = "Species", outlier.colour = "red", outlier.shape = 13, outlier.size = 18)
ggally_box <- function(data, ...)
{
  ggally_dotAndBox(data, ..., boxPlot = TRUE)
}


#' Plots the Box Plot with Dot
#' Add jittering with the box plot
#'
#' @param data data set using
#' @param ... other arguments being supplied to geom_jitter by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_dot(iris, x = "Petal.Width", y = "Species")
#' ggally_dot(iris, x = "Species", y = "Petal.Width", colour = "Species")
#ggally_dot(iris, x = "Species", y = "Petal.Width", colour = "Species", shape = "Species") + scale_shape(solid=FALSE)
ggally_dot <- function(data, ...)
{
  ggally_dotAndBox(data, ..., boxPlot = FALSE)
}


#' Plots either Box Plot or Dot Plots
#' Place box plots or dot plots on the graph
#' @param data data set using
#' @param ... parameters passed to aes_string(...)
#' @param boxPlot boolean to decide to plot either box plots (TRUE) or dot plots (FALSE)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' example(ggally_box)
#' example(ggally_dot)
ggally_dotAndBox <- function(data, ..., boxPlot = TRUE)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])
  
  if(horizontal)
  {
    aesString$tmp <- aesString$x
    aesString$x <- aesString$y
    aesString$y <- aesString$tmp
    aesString$tmp <- NULL
  } 
#print(str(aesString))

  p <- ggplot(data = data, aesString)
  

  outlierColor <- as.character(aesString$outlier.colour)
  outlierShape <- as.numeric(aesString$outlier.shape)
  outlierSize <- as.numeric(aesString$outlier.size)
  
  if(is.null(aesString$outlier.colour))
    outlierColor <- "black"
  if(is.null(aesString$outlier.shape))
    outlierShape <- 16
  if(is.null(aesString$outlier.size))
    outlierSize <- 2
  

  if(boxPlot)
    p <- p + geom_boxplot(
      outlier.colour = outlierColor, 
      outlier.shape = outlierShape, 
      outlier.size = outlierSize
    )
  else
    p <- p + geom_jitter()

	if(horizontal)
	{
		p <- p + coord_flip() + opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}
	p
}


#' Plots the Histograms by Faceting
#' Make histograms by displaying subsets of the data in different panels
#'
#' @param data data set using
#' @param ... parameters sent to aes_string(...).  It will effect stat_bin()
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_facethist(iris, x = "Petal.Width", y = "Species")
#' ggally_facethist(iris, x = "Species", y = "Petal.Width", binwidth = "0.1")
ggally_facethist <- function(data, ...)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])

	if(!horizontal)
	{
	   aesString$tmp <- aesString$x
	   aesString$x <- aesString$y
	   aesString$y <- aesString$tmp
	}
	else
	{
	   levels(data[,as.character(aesString$y)]) <- levels(data[,as.character(aesString$y)])[length(levels(data[,as.character(aesString$y)])):1]

	}
#cat("Horizontal: ", horizontal, "\n")	
#print(str(aesString))
#print(head(data))
	
	
	
  xVal <- aesString$x
	yVal <- aesString$y
  aesString$y <- NULL
#str(aesString)
#str(xVal)
#str(yVal)

		p <- ggplot(data = data, aesString)
		aesString$x <- NULL
		p <- p + stat_bin(
		      binwidth = aesString$binwidth, 
		      origin = aesString$origin, 
		      breaks = aesString$breaks, 
		      width = aesString$width, 
		      drop = aesString$drop
		    ) 

		if(horizontal)
		{
  		p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
		}
		else
		{
  		p <- p + coord_flip() 
  		p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
		}		
		p <- p + scale_y_continuous(as.character(yVal)) + scale_x_continuous(as.character(xVal))
		p
}


#' Plots the density plots by faceting
#' Make density plots by displaying subsets of the data in different panels
#'
#' @param data data set using
#' @param ... other arguments being sent to stat_density's by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_facetdensity(iris, x = "Petal.Width", y = "Species")
#' ggally_facetdensity(iris, x = "Species", y = "Petal.Width", colour = "Species")
ggally_facetdensity <- function(data, ...)
{
  ggally_facetdensitystrip(data, ..., den_strip = FALSE)
}


#' Plots a tile plot with facets
#' Make Tile Plot as densely as possible
#'
#' @param data data set using
#' @param ... other arguments being sent to stat_bin's by aes_string(...)
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_denstrip(iris, x = "Petal.Width", y = "Species")
#' ggally_denstrip(iris, x = "Species", y = "Petal.Width", binwidth = "0.2") + scale_fill_gradient(low = "grey80", high = "black")
ggally_denstrip <- function(data,...)
{
  ggally_facetdensitystrip(data, ..., den_strip = TRUE)
}



#' Plots a density plot with facets or a tile plot with facets
#' Make Tile Plot as densely as possible
#'
#' @param data data set using
#' @param ... other arguments being sent to stat_bin's by aes_string(...)
#' @param den_strip boolean to deceide whether or not to plot a density strip(TRUE) or a facet density(FALSE) plot.
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' example(ggally_facetdensity)
#' example(ggally_denstrip)
ggally_facetdensitystrip <- function(data, ..., den_strip = FALSE)
{
	aesString <- aes_string(...)
  horizontal <-  is.factor(data[,as.character(aesString$y)])
  if(!horizontal)
  {
    aesString$tmp <- aesString$x
    aesString$x <- aesString$y
    aesString$y <- aesString$tmp
    aesString$tmp <- NULL

  } 
  else
  {
    levels(data[,as.character(aesString$y)]) <- levels(data[,as.character(aesString$y)])[length(levels(data[,as.character(aesString$y)])):1]
  }

  xVal <- aesString$x
	yVal <- aesString$y
  aesString$y <- NULL

		p <- ggplot(data = data, aesString) + 
		    scale_y_continuous(as.character(yVal)) + 
		    scale_x_continuous(as.character(xVal))
		    
		if(den_strip)
		{
  		p <- p +    
    		stat_bin(
    		  aes(
      		  y = 1,
    		    fill = ..density..
    		  ), 
    		  position = "identity", 
    		  geom = "tile",
		      binwidth = aesString$binwidth, 
		      origin = aesString$origin, 
		      breaks = aesString$breaks, 
		      width = aesString$width, 
		      drop = aesString$drop

    		)	
    		print("Density Strip")	  
		}
		else
		{
  		p <- p +   
    		stat_density(
    		  aes(
    		    y = ..scaled.. * diff(range(x)) + min(x)
    		  ), 
    		  position = "identity", 
    		  geom = "line"
    		)
		}
    		
		
		if(horizontal)
		{
#print("horizontal")
		  p + facet_grid(Species ~ .)
  		p$facet$facets <- paste(as.character(yVal), " ~ .", sep = "")
  		if(den_strip)
  		  p <- p + opts(axis.text.y = theme_blank())
		}
		else
		{
  		p <- p + coord_flip()
  		p$facet$facets <- paste(". ~ ", as.character(yVal), sep = "")
  		if(den_strip)
        p <- p + opts(axis.text.x = theme_blank())

		}		
		p
}


#' Plots a mosaic plots
#' Plots the mosaic plot by using fluctuation
#'
#' @param data data set using
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_rata(movies[,c("mpaa","Action")])
#' ggally_rata(movies[,c("mpaa","Action")]) + coord_equal()
#' ggally_rata(movies[,c("Action","mpaa")]) + opts(aspect.ratio = (length(levels(movies[,"mpaa"])) ) / (length(levels(as.factor(movies[,"Action"]))) ) )
ggally_rata <- function(data)
{
	dataNames <- colnames(data)
	ggfluctuation2(table(data[,2], data[,1])) + labs(x = dataNames[1], y = dataNames[2])
}


#' Plots the Density Plots by Using Diagonal
#' Plots the density plots by using Diagonal
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_densityDiag(iris,x = "Petal.Width")
#' ggally_densityDiag(movies, x="rating")
#' ggally_densityDiag(movies, x="rating", colour = "mpaa")
ggally_densityDiag <- function(data, ...)
{
	aesString <- aes_string(...)
#str(aesString)

	ggplot(data, aesString) + 
		scale_x_continuous() + 
		scale_y_continuous() + 
		stat_density(
			aes(
				y = ..scaled.. * diff(range(x)) + min(x)
			),
			position = "identity", 
			geom = "line"
		)
}

#' Plots the Bar Plots by Using Diagonal
#' Plots the bar plots by using Diagonal
#'
#' @param data data set using
#' @param ...
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_barDiag(movies, x = "mpaa")
#' ggally_barDiag(movies, x ="rating", binwidth = ".1")
ggally_barDiag <- function(data, ...)
{
	aesString <- aes_string(...)
	aesString$y <- NULL

	numer <- is.null(attributes(data[,as.character(aesString$x)])$class)
	
	if(numer)
	{
    p <- ggplot(data = data, aesString) + geom_bar(binwidth = aesString$binwidth)
 	}
	else
	{
	 ## create a temporary data set that computes the count manually
		dataTmp <- as.factor(data[,as.character(aesString$x)])
		levels(dataTmp) <- levels(dataTmp)[length(levels(dataTmp)):1]


		count <- rep(0, length(levels(dataTmp)))

		for(z in 1:length(levels(dataTmp)))
			count[z] <- length(dataTmp[dataTmp==levels(dataTmp)[z]])
		
		dataTmp <- cbind(levels(dataTmp), count)
		dataTmp <- as.data.frame(dataTmp)
		
		colnames(dataTmp) <- c(as.character(aesString$x), "Count")
		
		## Makes sure the count is numeric instead of factor
		dataTmp$Count <- as.numeric(as.character(dataTmp$Count))
		
#print(head(dataTmp))
#str(dataTmp)
		
		p <- ggplot(data = dataTmp, aesString) + 
		  geom_bar(aes(y = Count), stat="identity") +
			coord_flip() +  
			opts(
				axis.text.y = theme_text(
					angle = 90, 
					vjust = 0, 
					colour = "grey50"
				)
			)
	}	
	p
}



#' GGplot Text
#' Plot text for a plot
#'
#' @param xP horizontal position percentage
#' @param yP vertical position percentage
#' @param label text that you want to appear
#' @param xrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param yrange range of the data around it.  Only nice to have if plotting in a matrix
#' @param ... other arguments for geom_text
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggally_text("Example 1")
#' ggally_text("Example\nTwo",size = 15, colour = "red")
ggally_text <- function(
                  label, 
                  xP = 0.5, 
                  yP = 0.5,
                  xrange = c(0,1), 
                  yrange = c(0,1),
#                  colour = "black",
                  ...
                )
{

  aesString <- aes_string(...)
  aesString$x <- aesString$y <- aesString$xmin <- aesString$xax <- aesString$ymin <- aesString$ymax <- NULL

  colour <- as.character(aesString$colour)
  if(is.null(aesString$colour))
    colour <- "black" 
  aesString$colour <- NULL
  
	p <- ggplot(data = 
			data.frame(
				x0 = xP * diff(xrange) + min(xrange),
				y0 = yP * diff(yrange) + min(yrange),
				x1 = xrange[1], 
				x2 = xrange[2], 
				y1 = yrange[1], 
				y2 = yrange[2]
			),
			aes(
				x = x0,
				y = y0,
				xmin=x1,
				xmax = x2, 
				ymin = y1, 
				ymax = y2
			)
		)+
		geom_rect(fill= I("white")) + 
		labs(x = NULL, y = NULL)
	
	p <- p + 
	   geom_text( label = label, aesString, colour = colour) + 
	   opts(legend.position = "none")
	
	p

}




#' Fluctuation plot
#' Create a fluctuation plot.
#'
#' A fluctutation diagram is a graphical representation of a contingency table. This fuction currently only supports 2D contingency tables.
#' The function was adopted from experiemntal functions within GGplot2
#'
#' @param table a table of values, or a data frame with three columns, the last column being frequency
#' @param floor don't display cells smaller than this value
#' @param ceiling max value to compare to
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
#' @examples
#' ggfluctuation2(table(movies$Action, movies$Comedy))
#' ggfluctuation2(table(movies$Action, movies$mpaa))
#' ggfluctuation2(table(warpbreaks$breaks, warpbreaks$tension))
ggfluctuation2 <- function (table, floor = 0, ceiling = max(table$freq, na.rm = TRUE)) 
{

	yNames <- rownames(table)
	xNames <- colnames(table)
	oldnames <- names(table)

	if (is.table(table)) 
	{
		table <- as.data.frame(t(table))
		
		oldnames <- c("X","Y")		
		
	}

	names(table) <- c("x", "y", "result")
	table <- add.all.combinations(table, list("x", "y"))
	table <- transform(table, x = as.factor(x), y = as.factor(y), 
		freq = result)

	table <- transform(table, freq = sqrt(pmin(freq * .95, ceiling)/ceiling), 
		border = ifelse(is.na(freq), "grey90", ifelse(freq > 
			ceiling, "grey30", "grey50")))
	table[is.na(table$freq), "freq"] <- 1
	table <- subset(table, freq * ceiling >= floor)
	
	xNew <- as.numeric(table$x) + 1/2 * table$freq
	yNew <- as.numeric(table$y) + 1/2 * table$freq
	
	maxLen <- max(diff(range(as.numeric(table$x))), diff(range(as.numeric(table$y))) )
	

	table <- cbind(table, xNew, yNew)
	#print(table)
	#print(xNames)
	#print(yNames)
	
	#cat("\nmaxLen");print(maxLen)

	
	p <- ggplot(
			table, 
			aes_string(
				x = "xNew", 
				y = "yNew", 
				height = "freq", 
				width = "freq", 
				fill = "border"
			)
		) + 
		geom_tile(colour = "white") + 
		scale_fill_identity() + 
		scale_x_continuous(
			name=oldnames[1], 
#			limits=c(1,maxLen + 2), 
#			breaks=1:(maxLen + 2), 
#			labels=c(xNames,rep("",maxLen - length(xNames) + 2)), 
			limits=c(1,length(xNames) + 1), 
			breaks=1:(length(xNames) + 1), 
			labels=c(xNames,""), 
			minor_breaks=FALSE
		) + 
		scale_y_continuous(
			name=oldnames[2], 
#			limits=c(1,maxLen + 2), 
#			breaks=1:(maxLen + 2), 
#			labels=c(yNames,rep("",maxLen - length(yNames) + 2)), 
			limits=c(1,length(yNames) + 1), 
			breaks=1:(length(yNames) + 1), 
			labels=c(yNames,""), 
			minor_breaks=FALSE
		) + 
#		coord_equal() + 
		opts(
			axis.text.x = theme_text(
				hjust = 0, 
				vjust = 1,
				colour = "grey50"
			),
			axis.text.y = theme_text(
				hjust = 0, 
				vjust = 0,
				angle = 90,
				colour = "grey50"
			)
		)
	
	p
}


#' Blank
#' Drawing nothing
#' 
#' Makes a "blank" ggplot object that will only draw white space
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}, Haesung Kim \email{hae0510@@iastate.edu}, Di Cook \email{dicook@@iastate.edu} and Heike Hofmann \email{hofmann@@iastate.edu}
#' @keywords hplot
ggally_blank <- function()
{
	a <- as.data.frame(cbind(1:2,1:2))
	colnames(a) <- c("X","Y")
	
	ggplot(data = a, aes(x = X, y = Y)) + geom_point( colour = "transparent") + opts(
		axis.line = theme_blank(),
		axis.text.x = theme_blank(),
		axis.text.y = theme_blank(),
		axis.ticks = theme_blank(),
		axis.title.x = theme_blank(),
		axis.title.y = theme_blank(),
		legend.background = theme_blank(),
		legend.key = theme_blank(),
		legend.text = theme_blank(),
		legend.title = theme_blank(),
		panel.background = theme_blank(),
		panel.border = theme_blank(),
		panel.grid.major = theme_blank(),
		panel.grid.minor = theme_blank(),
		plot.background = theme_blank(),
		plot.title = theme_blank(),
		strip.background = theme_blank(),
		strip.text.x = theme_blank(),
		strip.text.y = theme_blank()
	)
}