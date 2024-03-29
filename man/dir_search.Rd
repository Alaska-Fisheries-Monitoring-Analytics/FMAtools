% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{dir_search}
\alias{dir_search}
\title{Check for Subfolders Within Folders}
\usage{
dir_search(dribble)
}
\arguments{
\item{dribble}{a \code{dribble} containing the contents of a Shared Gdrive folder}
}
\value{
Returns a list of two dribbles, the first is the parent folder and the second is the children folders
}
\description{
\code{dir_search()} is used recursively by \code{gdrive_ls()} to identify the folder structure of the Shared Google Drive
}
