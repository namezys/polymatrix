\name{predict.polyMatrix}
\alias{predict.polyMatrix}
\title{Calculate the value of a polynomial matrix for real or complex values}

\description{
Calculate the value of the polynom elements of the
\sQuote{\code{polyMatrix}} class object for real and complex argument also.
}

\usage{\method{predict}{polyMatrix}(object,M,\dots)}

\arguments{
\item{object}{a \sQuote{\code{polyMatrix}} class object}
\item{M}{a real or complex argument}
\item{\dots}{additional arguments}
}

\value{
A matrix the just the same size as the input,
filled by the values of the polynom elements, at the given argument.
}

\seealso{
\code{\link{predict.polynomial}}, \code{\link{predict.charpn}}
}

\keyword{arith}

\examples{
# for predict the value of a polynomial matrix at a real or complex value
#    the used method: predict.polyMatrix()

pm <- polyMgen.d(2,2,ch2pn(c("1","x","x^2","0")))
pm # 1, x^2 \\ x, 0

predict(pm,2) # 1, 4 \\ 2, 0

# for predict the result of a linear model
#    the used method: predict.lm()

x<-1:5;y<-rnorm(5);predict(lm(y~x))

# clean up
# rm(pm, x, y)
}

% -----
% fine

