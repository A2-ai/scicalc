# Static dataflow capture for assembly audits.
#
# The graph is intentionally syntax-based. It records top-level object
# assignments, tagged expressions nested in them, and symbol references between
# those expressions. It does not interpret dplyr (or any other data package).

#' @noRd
audit_static_lineage <- function(script, targets, target_object) {
  graph <- audit_ast_graph(parse(file = script, keep.source = FALSE))
  paths <- audit_ast_paths(graph, target_object)
  rows <- list()

  add_row <- function(...) {
    values <- list(...)
    fields <- names(audit_empty_lineage())
    row <- lapply(fields, function(field) {
      value <- values[[field]]
      if (is.null(value) || length(value) == 0L) NA_character_ else as.character(value[[1]])
    })
    names(row) <- fields
    rows[[length(rows) + 1L]] <<- tibble::as_tibble(row)
  }

  if (!target_object %in% names(graph$objects)) {
    for (target in targets) {
      add_row(
        target = target, relation = "terminal", expression = target_object,
        detail = paste0("No assignment to `", target_object, "` was found in the script.")
      )
    }
    return(audit_lineage_rows(rows))
  }

  # Nearest definitions by true path depth. Sibling objects at the same depth
  # (e.g. two frames later combined with bind_rows()) all define the column, so
  # equal-depth definitions tie and are all returned.
  find_definition <- function(symbol, object = NULL) {
    definitions <- graph$columns[graph$columns$column == symbol, , drop = FALSE]
    if (!is.null(object)) {
      local <- definitions[definitions$object == object, , drop = FALSE]
      if (nrow(local) > 0L) return(utils::tail(local, 1L))
    }
    definitions <- definitions[definitions$object %in% names(paths), , drop = FALSE]
    if (nrow(definitions) == 0L) return(definitions)
    definitions$distance <- vapply(
      strsplit(unlist(paths[definitions$object]), " -> ", fixed = TRUE),
      length, integer(1)
    )
    definitions <- definitions[definitions$distance == min(definitions$distance), , drop = FALSE]
    definitions[order(definitions$order, decreasing = TRUE), , drop = FALSE]
  }

  source_object <- function(object) audit_ast_terminal_input(graph, object)

  trace_expression <- NULL
  trace_symbol <- function(symbol, object, target, reference, seen) {
    definitions <- find_definition(symbol, object)
    if (nrow(definitions) == 0L) {
      add_row(
        target = target, relation = "source", symbol = reference,
        source_object = source_object(object), source_column = symbol,
        path = paths[[object]]
      )
      return(invisible())
    }
    for (definition_object in unique(definitions$object)) {
      definition <- definitions[definitions$object == definition_object, , drop = FALSE]
      if (nrow(definition) > 1L) {
        add_row(
          target = target, relation = "terminal", symbol = reference,
          expression = symbol, detail = paste0("Multiple definitions for `", symbol, "`."),
          path = paths[[definition_object]]
        )
        next
      }
      key <- paste(definition_object, definition$column[[1]], sep = "$")
      if (key %in% seen) {
        add_row(
          target = target, relation = "terminal", symbol = reference,
          expression = key, detail = "Cyclic column dependency.", path = paths[[object]]
        )
        next
      }
      trace_expression(
        definition$expression[[1]], definition_object, target, reference,
        c(seen, key)
      )
    }
    invisible()
  }

  trace_expression <- function(expression, object, target, reference, seen) {
    for (symbol in audit_ast_symbols(expression)) {
      origin <- if (identical(reference, target)) symbol else reference
      trace_symbol(symbol, object, target, origin, seen)
    }
    for (terminal in audit_ast_terminal_calls(expression)) {
      add_row(
        target = target, relation = "terminal", symbol = reference,
        expression = terminal, path = paths[[object]]
      )
    }
    invisible()
  }

  for (target in targets) {
    definitions <- find_definition(target)
    if (nrow(definitions) == 0L) {
      add_row(
        target = target, relation = "source", symbol = target,
        source_object = source_object(target_object), source_column = target,
        path = paths[[target_object]]
      )
      next
    }
    for (definition_object in unique(definitions$object)) {
      definition <- definitions[definitions$object == definition_object, , drop = FALSE]
      if (nrow(definition) > 1L) {
        add_row(
          target = target, relation = "terminal", expression = target,
          detail = paste0("Multiple definitions for `", target, "`."), path = paths[[definition_object]]
        )
        next
      }
      expression <- definition$expression[[1]]
      add_row(
        target = target, relation = "definition", object = definition_object,
        expression = audit_ast_deparse(expression), path = paths[[definition_object]]
      )
      trace_expression(
        expression, definition_object, target, target,
        paste(definition_object, target, sep = "$")
      )
    }
  }

  audit_lineage_rows(rows)
}

#' @noRd
audit_empty_lineage <- function() {
  tibble::tibble(
    target = character(), relation = character(), object = character(),
    symbol = character(), expression = character(), detail = character(),
    source_object = character(), source_column = character(), path = character()
  )
}

#' @noRd
audit_lineage_rows <- function(rows) {
  if (length(rows) == 0L) return(audit_empty_lineage())
  dplyr::distinct(dplyr::bind_rows(rows))
}

#' @noRd
audit_ast_graph <- function(expressions) {
  objects <- list()
  columns <- list()
  order <- 0L

  collect_tagged_expressions <- function(expression, object) {
    if (!is.call(expression)) return(invisible())
    arguments <- audit_ast_arguments(expression)
    tags <- names(arguments)
    if (!is.null(tags)) {
      for (index in which(nzchar(tags))) {
        order <<- order + 1L
        columns[[length(columns) + 1L]] <<- tibble::tibble(
          object = object, column = tags[[index]], expression = list(arguments[[index]]),
          order = order
        )
      }
    }
    for (argument in arguments) collect_tagged_expressions(argument, object)
    invisible()
  }

  for (expression in expressions) {
    if (!is.call(expression) || !audit_ast_call_name(expression) %in% c("<-", "=")) next
    lhs <- expression[[2]]
    if (!is.symbol(lhs)) next
    object <- as.character(lhs)
    rhs <- expression[[3]]
    objects[[object]] <- rhs
    collect_tagged_expressions(rhs, object)
  }

  columns <- if (length(columns) == 0L) {
    tibble::tibble(object = character(), column = character(), expression = list(), order = integer())
  } else {
    dplyr::bind_rows(columns)
  }
  list(objects = objects, columns = columns)
}

# Object-to-object paths use the generic symbol graph. Function names have no
# special status, so this works equally for dplyr, data.table, or local helpers.
#' @noRd
audit_ast_paths <- function(graph, target_object) {
  paths <- stats::setNames(list(target_object), target_object)
  queue <- target_object
  while (length(queue) > 0L) {
    object <- queue[[1]]
    queue <- queue[-1]
    references <- intersect(audit_ast_symbols(graph$objects[[object]]), names(graph$objects))
    for (reference in references) {
      if (!reference %in% names(paths)) {
        paths[[reference]] <- c(paths[[object]], reference)
        queue <- c(queue, reference)
      }
    }
  }
  lapply(paths, paste, collapse = " -> ")
}

# A source terminal is the left-most expression in an ordinary R pipe. A
# non-piped call is retained whole as its terminal expression: the graph does
# not assume that any argument position represents data.
#' @noRd
audit_ast_terminal_input <- function(graph, object, seen = character()) {
  if (object %in% seen) return(object)
  expression <- graph$objects[[object]]
  if (is.null(expression)) return(object)
  input <- audit_ast_pipe_lhs(expression)
  if (is.symbol(input)) {
    name <- as.character(input)
    if (name %in% names(graph$objects)) {
      return(audit_ast_terminal_input(graph, name, c(seen, object)))
    }
    return(name)
  }
  audit_ast_deparse(input)
}

#' @noRd
audit_ast_pipe_lhs <- function(expression) {
  if (!is.call(expression)) return(expression)
  if (audit_ast_call_name(expression) %in% c("|>", "%>%")) {
    return(audit_ast_pipe_lhs(expression[[2]]))
  }
  expression
}

#' @noRd
audit_ast_symbols <- function(expression) {
  reserved <- c("TRUE", "FALSE", "NULL", "NA", "NA_real_", "NA_integer_", "NA_character_", "Inf", "NaN", "T", "F", ".")
  walk <- function(node) {
    if (is.symbol(node)) {
      name <- as.character(node)
      return(if (name %in% reserved) character() else name)
    }
    if (!is.call(node)) return(character())
    if (audit_ast_call_name(node) %in% c("<-", "=")) return(walk(node[[3]]))
    unlist(lapply(audit_ast_arguments(node), walk), use.names = FALSE)
  }
  unique(walk(expression))
}

# Preserve calls which depend only on external runtime values. For example,
# `Sys.getenv("COLUMN")` is a terminal source expression, not an error.
#' @noRd
audit_ast_terminal_calls <- function(expression) {
  terminals <- character()
  walk <- function(node) {
    if (!is.call(node)) return(invisible())
    fun <- audit_ast_call_name(node)
    if (!fun %in% c("<-", "=") && length(audit_ast_symbols(node)) == 0L) {
      terminals <<- c(terminals, audit_ast_deparse(node))
    }
    for (argument in audit_ast_arguments(node)) walk(argument)
    invisible()
  }
  walk(expression)
  unique(terminals)
}

#' @noRd
audit_ast_call_name <- function(expression) {
  if (!is.call(expression)) return(NA_character_)
  fun <- expression[[1]]
  if (is.symbol(fun)) return(as.character(fun))
  if (is.call(fun) && identical(as.character(fun[[1]]), "::")) return(as.character(fun[[3]]))
  audit_ast_deparse(fun)
}

# Missing call arguments are valid R syntax (for example `f(, x)`) and occur
# in generated/knitted scripts. They carry no dependency edge, so skip them.
#' @noRd
audit_ast_arguments <- function(expression) {
  arguments <- as.list(expression)[-1]
  if (length(arguments) == 0L) return(arguments)
  arguments[!vapply(arguments, rlang::is_missing, logical(1))]
}

#' @noRd
audit_ast_deparse <- function(expression) {
  if (is.null(expression)) return(NA_character_)
  paste(deparse(expression, width.cutoff = 500L), collapse = " ")
}
