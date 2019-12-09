severities <- c(
  "EMERG" = 0L,
  "ALERT" = 1L,
  "CRITICAL" = 2L,
  "ERR" = 3L,
  "WARNING" = 4L,
  "NOTICE" = 5L,
  "INFO" = 6L,
  "DEBUG" = 7L
)

facilities <- c(
  "KERN" = 0L,
  "USER" = 1L,
  "MAIL" = 2L,
  "DAEMON" = 3L,
  "AUTH" = 4L,
  "SYSLOG" = 5L,
  "LPR" = 6L,
  "NEWS" = 7L,
  "UUCP" = 8L,
  "CRON" = 9L,
  "AUTHPRIV" = 10L,
  "FTP" = 11L,
  "LOCAL0" = 16L,
  "LOCAL1" = 17L,
  "LOCAL2" = 18L,
  "LOCAL3" = 19L,
  "LOCAL4" = 20L,
  "LOCAL5" = 21L,
  "LOCAL6" = 22L,
  "LOCAL7" = 23L
)

#' Send log message to syslog server
#'
#' Send log message to syslog server.
#'
#' @param message text message (string).
#' @param severity severity level (string).
#' @param facility log facility (string).
#' @param host machine that originally sends the message (string).
#' @param app_name application name that originally sends the message (string).
#' @param proc_id process id that originally sends the message (numeric).
#' @param server syslogd server hostname (string).
#' @param port syslogd server port (integer).
#'
#' @return Number of bytes written to socket.
#'
#' @examples
#' \dontrun{
#' syslog("log message", "INFO", app_name = 'program', server = 'logserver')
#' }
#' @export
syslog <- function(
  message, severity = "NOTICE", facility = "USER",
  host = Sys.info()[["nodename"]], app_name = Sys.info()[["user"]],
  proc_id = Sys.getpid(),
  server = "localhost", port = 601L)
{
  sock <- utils::make.socket(server, port)
  on.exit(utils::close.socket(sock))
  str <- payload(message, severity, facility, host, app_name, proc_id)
  nb <- utils::write.socket(sock, str)
  return(nb)
}

payload <- function(message, severity, facility, host, app_name, proc_id)
{
  fac <- facilities[[facility]]
  sev <- severities[[severity]]
  prival <- bitwOr(bitwShiftL(fac, 3L), sev)
  priver <- paste0("<", prival, ">", "1")
  tstamp <- "-"
  msg_id <- "-"
  struc_data <- "-"
  paste(priver, tstamp, host, app_name, proc_id, msg_id, struc_data, message)
}
