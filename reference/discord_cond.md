# Custom Conditions for the discord package

Custom Conditions for the discord package

## Usage

``` r
discord_cond(type, msg, class = paste0("discord-", type), call = NULL, ...)
```

## Arguments

- type:

  One of the following conditions: c("error", "warning", "message")

- msg:

  Message

- class:

  Default is to prefix the 'type' argument with "discord", but can be
  more specific to the problem at hand.

- call:

  What triggered the condition?

- ...:

  Additional arguments that can be coerced to character or single
  condition object.

## Value

A condition for discord.

## Examples

``` r
if (FALSE) { # \dontrun{

derr <- function(x) discord_cond("error", x)
dwarn <- function(x) discord_cond("warning", x)
dmess <- function(x) discord_cond("message", x)

return_class <- function(func) {
  tryCatch(func,
    error = function(cond) class(cond),
    warning = function(cond) class(cond),
    message = function(cond) class(cond)
  )
}

return_class(derr("error-class"))
return_class(dwarn("warning-class"))
return_class(dmess("message-class"))
} # }
```
