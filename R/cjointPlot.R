#' @name cjointPlot
#' @docType package
#' @title amceオブジェクトのプロッティング関数
#' @description \code{cjointPlot}は\code{cjoint}パッケージから生成されたamceオブジェクトを引数とし、推定値のプロットを表示します。
#' \code{cjoint}内蔵の\code{plot}メソッドとは違い、多様な言語に対応し、より軽量化を図りました。
#' @param cjoint.obj : 'amce' class object
#' @param show.est : Show a estimated coefficients. Default is TRUE
#' @param show.guide : Show a guidelines. Default is TRUE
#' @param xlab : Customize x-axis label. Default is NULL
#' @param title : Customise a plot title. Default is NULL
#' @return A class of a return value is "NULL"
#' @author SONG Jaehyun
#' (jaehyun.song [at] stu.kobe-u.ac.jp ||
#' http://www.JaySong.net)
#' @examples
#' cjoint.model <- amce(selected ~ X1 + X2 + X3, data = choice4to2.df, respondent.id = "respondentIndex", cluster = TRUE, design = cjoint.design)
#' cjointPlot(cjoint.model)
#' cjointPlot(cjoint.model, show.est = FALSE, title = "Test Plot")
NULL

cjointPlot <- function(cjoint.obj,
                       show.est = TRUE,
                       show.guide = TRUE,
                       xlab = NULL,
                       title = NULL){

        if(class(cjoint.obj) != "amce"){
                print("amceクラスのオブジェクトちゃいますね。")
                break
        }

        y.axis.make <- function(x.list, y.list){
                result.vec1 <- c()
                result.vec2 <- c()
                result.vec3 <- c()
                for(i in 1:length(x.list)){
                        base <- y.list[[i]]
                        remi <- y.list[[i]] == x.list[[i]]

                        result.vec1 <- c(result.vec1,
                                         names(x.list)[i],
                                         paste("(Base: ", y.list[[i]], ")", sep = ""),
                                         sort(x.list[[i]][!remi]))

                        result.vec2 <- c(result.vec2,
                                         rep(FALSE, 2),
                                         rep(TRUE, length(x.list[[i]]) - 1))

                        result.vec3 <- c(result.vec3,
                                         FALSE,
                                         rep(TRUE, length(x.list[[i]])))
                }
                result.vec1 <- rev(result.vec1)
                result.vec2 <- rev(result.vec2)
                result.vec3 <- rev(result.vec3)
                result.list <- list(result.vec1, result.vec2, result.vec3)
                return(result.list)
        }

        coef.extract <- function(x.list){
                index <- 0
                result.df <- data.frame(attr = NA, levels = NA,
                                        y = NA, coef = NA, se = NA,
                                        ul = NA, ll = NA)
                for(i in c(rev(names(cjoint.obj$attributes)))){
                        for(j in sort(colnames(x.list[[i]]), decreasing = TRUE)){
                                index <- index + 1
                                result.df[index, 1:2] <- c(i, j)
                                result.df[index, 4:5] <- c(x.list[[i]][, j])
                        }
                }
                temp.y       <- 1:length(attr.list[[1]])
                result.df$y  <- temp.y[attr.list[[2]]]
                result.df$ul <- result.df$coef + 1.98 * result.df$se
                result.df$ll <- result.df$coef - 1.98 * result.df$se
                return(result.df)
        }

        attr.list <- y.axis.make(cjoint.obj$attributes,
                                 cjoint.obj$baselines)
        coef      <- coef.extract(cjoint.obj$estimates)


        mar.l  <- max(nchar(attr.list[[1]]))/1.6
        xlim.l <- round(min(coef$ll) * 1.1, 1)
        xlim.u <- round(max(coef$ul) * 1.1, 1)

        original.mar <- par()$mar

        if(is.null(title)){
                title <- "Keio is better than Waseda"
        }
        if(is.null(xlab)){
                xlabel = "Change in Probability(%)"
        }else{
                xlabel = xlab
        }

        par(mar = c(5.1, mar.l, 4.1, 2.1))
        plot(y = coef$y, x = coef$coef, yaxt = "n",
             ylim = c(1, length(attr.list[[1]])),
             xlim = c(xlim.l, xlim.u),
             pch = 16, ylab = "", xlab = xlabel,
             main = title)
        mtext(paste("(N = ", cjoint.obj$numrespondents,")", sep = ""),
              side = 3, cex = 0.8)
        text(x = min(xlim.l) - 0.02,
             y = (1:length(attr.list[[1]]))[attr.list[[3]]],
             pos = 2, xpd = TRUE,
             labels = attr.list[[1]][attr.list[[3]]], cex = 0.8)
        text(x = min(xlim.l) - 0.02,
             y = (1:length(attr.list[[1]]))[!attr.list[[3]]],
             pos = 2, xpd = TRUE, font = 2,
             labels = attr.list[[1]][!attr.list[[3]]], cex = 0.8)

        if(show.guide == TRUE){
                for(i in coef$y){
                        abline(h = i, col = rgb(0, 0, 0, 0.1))
                }
        }else{

        }
        for(i in 1:nrow(coef)){
                arrows(coef$ll[i], coef$y[i],
                       coef$ul[i], coef$y[i],
                       length = 0, lwd = 1.5)
                if(show.est == TRUE){
                        text(x = coef$coef[i], y = coef$y[i] + 0.5,
                             labels = round(coef$coef[i], 3),
                             cex = 0.5)
                }else{}

        }
        abline(v = 0)
        par(mar = original.mar)
}
