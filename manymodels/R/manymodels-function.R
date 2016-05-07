#' A many model function
#' 
#' This function plots the coefficients from multiple models
#' @param fitlist A list of model fit objects
#' @param methodnamevector A vector of corresponding model method names
#' @return plot of coefficients
#' @export
#' @examples
#' manymodels()

manymodels <- function(fitlist, methodnamevector){
  outdata <- c()
  createSameDataShape <- function(fitlist, methodnamevector){
    count <- length(methodnamevector)
    Variable <- lapply(fitlist, function(X) rownames(summary(X)$coef))
    Coef <- lapply(fitlist, coef)
    seList <- lapply(fitlist, function(X)  summary(X)$coef[, 2])
    Method <- as.list(methodnamevector)
    for(i in 1:count){
      tempdata <- data.frame(Variable = Variable[[i]], Coef = Coef[[i]],
                             SE = seList[[i]], Method = Method[[i]])
      outdata <- rbind(outdata,tempdata)
    }
    return(outdata)
  }
  
  allmodels <- createSameDataShape(fitlist,methodnamevector)
  
  myplot <- ggplot(data = allmodels, aes(color = Method)) +
    geom_hline(yintercept = 0, color = grey(1/2), lty = 2) +
    geom_linerange(aes(x = Variable, ymin = Coef - SE*1.96,
                       ymax = Coef + SE*1.96), lwd  = 1, 
                   position = position_dodge(width = 1/2)) +
    geom_pointrange(aes(x = Variable, y = Coef,
                        ymin = Coef - SE*2.58, ymax = Coef + SE*2.58),
                    lwd = 1/2, position = position_dodge(width = 1/2),
                    shape = 21, fill = "WHITE") +
    coord_flip() +
    ggtitle("Comparing coefficient estimates") + 
    theme_bw() +
    theme(legend.justification = c(1,1), legend.position = c(1,1))
  print(myplot)
}

