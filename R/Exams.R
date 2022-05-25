# --------------------------------------
# Author: Andreas Alfons
#         Erasmus Universiteit Rotterdam
# --------------------------------------

#' Exam results of an applied statistics course
#'
#' Data on grades for an applied statistics course at Erasmus University
#' Rotterdam for students who took both the regular exam and the resit.
#' Grades in the Netherlands are on a scale from 1 to 10, with a higher
#' grade being better, and a minimum of 5.5 is required to pass.
#'
#' @usage data("Exams")
#'
#' @format
#' A data frame with 45 observations on the following 2 variables.
#' \describe{
#'
#'   \item{\code{Regular}}{the student's grade based on the regular exam at the
#'   end of the course.}
#'
#'   \item{\code{Resit}}{the student's grade based on the resit exam at the end
#'   of the academic year.}
#'
#' }
#'
#' @examples
#' data("Exams")
#' summary(Exams)
#'

"Exams"
