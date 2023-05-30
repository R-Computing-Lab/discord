#' Custom Conditions for the discord package
#'
#' @param type One of the following conditions: c("error", "warning", "message")
#' @param msg Message
#' @param class Default is to prefix the 'type' argument with "discord", but can be more specific to the problem at hand.
#' @param call What triggered the condition?
#' @param ... Additional arguments that can be coerced to character or single condition object.
#'
#' @return A condition for discord.
#'
#' @examples
#'
#' \dontrun{
#'
#'   derr <- function(x) discord_cond("error", x)
#'   dwarn <- function(x) discord_cond("warning", x)
#'   dmess <- function(x) discord_cond("message", x)
#'
#'   return_class <- function(func) {
#'
#'     tryCatch(func,
#'              error = function(cond) class(cond),
#'              warning = function(cond) class(cond),
#'              message = function(cond) class(cond)
#'     )
#'
#'   }
#'
#'   return_class(derr("error-class"))
#'   return_class(dwarn("warning-class"))
#'   return_class(dmess("message-class"))
#'
#' }
#' 

discord_cond <- function(type, msg, class = paste0("discord-", type), call = NULL, ...) {
  
  # Checking if the 'type' is one of the allowed values
  if (!type %in% c("error", "warning", "message")) {
    stop("Invalid type. Type must be one of 'error', 'warning' or 'message'.")
  }
  
  # Defining a condition with the specified type and message
  cond <- structure(
    list(
      message = msg,
      call = call,
      ...
    ),
    class = c(class, type, "condition")
  )
  
  # Use 'switch' to handle different types of conditions
  switch(type,
         "error" = stop(cond),
         "warning" = warning(cond),
         "message" = message(cond)
         )
}

derr <- function(message, ...) discord_cond("error", message, ...)
dwarn <- function(message, ...) discord_cond("warning", message, ...)
dmess <- function(message, ...) discord_cond("message", message, ...)

