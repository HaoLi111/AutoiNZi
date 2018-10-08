\name{INZI_setup}
\alias{INZI_setup}
\title{
Setup INZI datafile
}
\description{
Specifying the data file to work on, with optional assigning and workspace
}
\usage{
INZI_setup(wd='E:/',fl='data',dataname='mydata',type='.csv',
                     header=T,swd = F,assign = F,
                     pos = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
} \item{assign}{
whether to assign the loaded data to directed position or not
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\link{https://www.r-bloggers.com/read-excel-files-from-r/}
load xls/xlsx file
%% ~put references to the literature/web site here ~
}
\author{
Hao Li
}
\note{
%%  ~~further notes~~
}
\seealso{
\code{\link{read.csv}}
\code{\link{setwd}}
}
\examples{
INZI_setup()#Using default version, save your CSV to E:/ as data.csv
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
