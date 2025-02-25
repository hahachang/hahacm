% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{ft.theme_tii}
\alias{ft.theme_tii}
\title{flextable 的Tii預設模版}
\usage{
ft.theme_tii(ft)
}
\arguments{
\item{ft}{}
}
\value{

}
\description{
flextable 的Tii預設模版
}
\examples{
Data \%>\%
filter(Date==11103) \%>\%
 mutate(Date = as.character(Date)) \%>\%
 select(Date,Name,稅前損益,資金來源總計) \%>\%
 add.name.order() \%>\%
 arrange(Name) \%>\%
 wd.trans.name_and_date() \%>\%
 flextable() \%>\%
 ft.theme_tii() \%>\%
 save_as_image(path = dir.desktop \%+\%"test.png", webshot = "webshot2")

}
