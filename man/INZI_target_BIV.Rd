\name{INZI_target_BIV}
\alias{INZI_target_BIV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Tidying for bivariate data for certain interested target
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
INZI_target_BIV(data=mydata,target=NA,dfname='mybiv',pos=.GlobalEnv,assign=F,completeonly=T,report=T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A dataset with rows of individual data.
}
  \item{target}{
  variables to analyse, can take 2 or 3, as a vector.
  1 - dependent
  2 - independent
  3 - reference and extension studies
  }
  \item{completeonly}{
  Only using completed cases of the subsetted data
  }
  \item{report}{
  Reporting by printing out what the funcoiton is doing.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hao Li
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
INZI_target_BIV(iris,target = c(2,3,4))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
