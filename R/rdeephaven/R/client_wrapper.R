#' @title The Deephaven Client
#' @description The Deephaven Client class is responsible for establishing and maintaining
#' a connection to a running Deephaven server and facilitating basic server requests.
#' @usage NULL
#' @format NULL
#' @docType class
#' @md
#'
#' @section Establishing a server connection with `dhConnect`:
#' Connections to a Deephaven server are established with a call to `dhConnect`,
#' which returns a `Client` object responsible for maintaining the connection and
#' providing an interface to basic server requests. Deephaven servers can be created
#' with many different configurations, so `dhConnect` has the following list of
#' arguments to support connections to servers with any configuration.
#' * `target`: A string denoting the URL hosting the server.
#' * `auth_type`: A string denoting the authentication type, can be `anonymous`,
#' `basic`, or any custom-built authenticator in the server, such as
#' `io.deephaven.authentication.psk.PskAuthenticationHandler`. Defaults to `anonymous`.
#' * `auth_token`: A string denoting the authentication token. When `auth_type`
#' is `anonymous`, it will be ignored; when `auth_type` is `basic`, it must be
#' `"user:password"`; when `auth_type` is a custom-built authenticator, it must
#' conform to the specific requirement of that authenticator.
#' * `session_type`: A string denoting the session type supported on the server.
#' Currently, `python` and `groovy` are supported. Defaults to `python`.
#' * `use_tls`: Whether or not to use a TLS connection.  Defaults to `FALSE`.
#' * `tls_root_certs`: PEM encoded root certificates to use for TLS connection,
#' or `""` to use system defaults. Only used if `use_tls == TRUE`. Defaults to system defaults.
#' * `int_options`: List of name-value pairs for int-valued options to the underlying
#' grpc channel creation.  Defaults to an empty list, which implies not using any channel options.
#' * `string_options`: List of name-value pairs for string-valued options to the underlying
#' grpc channel creation.  Defaults to an empty list, which implies not using any channel options.
#' * `extra_headers`: List of name-value pairs for additional headers and values
#' to add to server requests. Defaults to an empty list, which implies not using any extra headers.
#' 
#' @section Methods:
#' 
#' Once a server connection is established, the `Client` class supports the
#' following list of methods for facilitating server requests.
#'
#' - `open_table(client, name)`: Opens a table named `"name"` on the server, if
#' it exists. Returns a `TableHandle`.
#' - `empty_table(client, size)`: Creates an empty table on the server with no
#' columns and `size` rows. Thus, `size` must be a non-negative integer. Returns a `TableHandle`.
#' - `time_table(client, period, start_time)`:
#' - `as_dh_table(client, table_object)`:
#' - `run_script(client, script)`:
#' - `close(client)`:
#' 
#' When a `Client` instance is no longer in use, it should be closed with a call
#' to `close(client_instance)`.
#'
#' @rdname Client-class
#' @examples
#' my_array <- Array$create(1:10)
#' my_array$type
#' my_array$cast(int8())
#'
#' # Check if value is null; zero-indexed
#' na_array <- Array$create(c(1:5, NA))
#' na_array$IsNull(0)
#' na_array$IsNull(5)
#' na_array$IsValid(5)
#' na_array$null_count
#'
#' # zero-copy slicing; the offset of the new Array will be the same as the index passed to $Slice
#' new_array <- na_array$Slice(5)
#' new_array$offset
#'
#' # Compare 2 arrays
#' na_array2 <- na_array
#' na_array2 == na_array # element-wise comparison
#' na_array2$Equals(na_array) # overall comparison

#' @export
setClass(
  "Client",
  representation(
    .internal_rcpp_object = "Rcpp_INTERNAL_Client"
  )
)

setGeneric(
  "dhConnect",
  function(target, ...) {
    return(standardGeneric("dhConnect"))
  },
  signature = c("target")
)

#' @export
setMethod(
  "dhConnect",
  signature = c(target = "character"),
  function(target,
           auth_type = "anonymous",
           auth_token = "",
           session_type = "python",
           use_tls = FALSE,
           tls_root_certs = "",
           int_options = list(),
           string_options = list(),
           extra_headers = list()) {
    options <- new(INTERNAL_ClientOptions)

    verify_string("target", target, TRUE)
    verify_string("auth_type", auth_type, TRUE)
    if (auth_type == "") {
      stop("'auth_type' should be a non-empty string.")
    }
    verify_bool("use_tls", use_tls, TRUE)

    # check if auth_type needs to be changed and set credentials accordingly
    if (auth_type == "anonymous") {
      options$set_default_authentication()
    } else if (auth_type == "basic") {
      if (auth_token != "") {
        verify_string("auth_token", auth_token, TRUE)
        options$set_basic_authentication(auth_token)
      } else {
        stop("Basic authentication was requested, but no 'auth_token' was provided.")
      }
    } else {
      if (auth_token != "") {
        verify_string("auth_token", auth_token, TRUE)
        options$set_custom_authentication(auth_type, auth_token)
      } else {
        stop("Custom authentication was requested, but no 'auth_token' was provided.")
      }
    }

    # set session type if a valid session type is provided
    if ((session_type == "python") || (session_type == "groovy")) {
      options$set_session_type(session_type)
    } else {
      stop(paste0("'session_type' must be 'python' or 'groovy', but got ", session_type, "."))
    }

    # if tls is requested, set it and set the root_certs if provided
    if (use_tls == TRUE) {
      options$set_use_tls()
      if (tls_root_certs != "") {
        verify_string("tls_root_certs", tls_root_certs, TRUE)
        options$set_tls_root_certs(tls_root_certs)
      }
    }

    # set extra header options if they are provided
    if (length(int_options) != 0) {
      verify_list("int_options", int_options, TRUE)
      for (key in names(int_options)) {
        verify_string("key", key, TRUE)
        verify_int("value", int_options[[key]], TRUE)
        options$add_int_options(key, int_options[[key]])
      }
    }

    if (length(string_options) != 0) {
      verify_list("string_options", string_options, TRUE)
      for (key in names(string_options)) {
        verify_string("key", key, TRUE)
        verify_string("value", string_options[[key]], TRUE)
        options$add_string_options(key, string_options[[key]])
      }
    }

    if (length(extra_headers) != 0) {
      verify_list("extra_headers", extra_headers, TRUE)
      for (key in names(extra_headers)) {
        verify_string("key", key, TRUE)
        verify_string("value", extra_headers[[key]], TRUE)
        options$add_extra_headers(key, extra_headers[[key]])
      }
    }

    if ((auth_token != "") && (auth_type == "anonymous")) {
      warning("'auth_token' was set but it will not be used, as 'auth_type' is 'anonymous'.")
    }

    if ((tls_root_certs != "") && (use_tls == FALSE)) {
      warning("'tls_root_certs' was set but it will not be used, as 'use_tls is FALSE.")
    }

    internal_client <- new(INTERNAL_Client,
      target = target,
      client_options = options
    )
    return(new("Client", .internal_rcpp_object = internal_client))
  }
)

### HELPER FUNCTIONS ###

check_for_table <- function(client, name) {
  return(client@.internal_rcpp_object$check_for_table(name))
}

### USER-FACING METHODS ###

setGeneric(
  "open_table",
  function(client_instance, name) {
    return(standardGeneric("open_table"))
  },
  signature = c("client_instance", "name")
)

#' @export
setMethod(
  "open_table",
  signature = c(client_instance = "Client", name = "character"),
  function(client_instance, name) {
    verify_string("name", name, TRUE)
    if (!check_for_table(client_instance, name)) {
      stop(paste0("The table '", name, "' does not exist on the server."))
    }
    return(new("TableHandle", .internal_rcpp_object = client_instance@.internal_rcpp_object$open_table(name)))
  }
)

setGeneric(
  "empty_table",
  function(client_instance, size) {
    return(standardGeneric("empty_table"))
  },
  signature = c("client_instance", "size")
)

#' @export
setMethod(
  "empty_table",
  signature = c(client_instance = "Client", size = "numeric"),
  function(client_instance, size) {
    verify_nonnegative_int("size", size, TRUE)
    return(new("TableHandle", .internal_rcpp_object = client_instance@.internal_rcpp_object$empty_table(size)))
  }
)

setGeneric(
  "time_table",
  function(client_instance, period, ...) {
    return(standardGeneric("time_table"))
  },
  signature = c("client_instance", "period")
)

#' @export
setMethod(
  "time_table",
  signature = c(client_instance = "Client", period = "numeric"),
  function(client_instance, period, start_time = 0) {
    verify_any_int("period", period, TRUE)
    verify_any_int("start_time", start_time, TRUE)
    return(new("TableHandle", .internal_rcpp_object = client_instance@.internal_rcpp_object$time_table(start_time, period)))
  }
)

setGeneric(
  "as_dh_table",
  function(client_instance, table_object) {
    return(standardGeneric("as_dh_table"))
  },
  signature = c("client_instance", "table_object")
)

#' @export
setMethod(
  "as_dh_table",
  signature = c(client_instance = "Client", table_object = "RecordBatchReader"),
  function(client_instance, table_object) {
    ptr <- client_instance@.internal_rcpp_object$new_arrow_array_stream_ptr()
    table_object$export_to_c(ptr)
    return(
      new("TableHandle",
        .internal_rcpp_object = client_instance@.internal_rcpp_object$new_table_from_arrow_array_stream_ptr(ptr)
      )
    )
  }
)

#' @export
setMethod(
  "as_dh_table",
  signature = c(client_instance = "Client", table_object = "Table"),
  function(client_instance, table_object) {
    return(as_dh_table(client_instance, as_record_batch_reader(table_object)))
  }
)

#' @export
setMethod(
  "as_dh_table",
  signature = c(client_instance = "Client", table_object = "tbl_df"),
  function(client_instance, table_object) {
    return(as_dh_table(client_instance, arrow_table(table_object)))
  }
)

#' @export
setMethod(
  "as_dh_table",
  signature = c(client_instance = "Client", table_object = "data.frame"),
  function(client_instance, table_object) {
    return(as_dh_table(client_instance, arrow_table(table_object)))
  }
)

setGeneric(
  "run_script",
  function(client_instance, script) {
    return(standardGeneric("run_script"))
  },
  signature = c("client_instance", "script")
)

#' @export
setMethod(
  "run_script",
  signature = c(client_instance = "Client", script = "character"),
  function(client_instance, script) {
    verify_string("script", script, TRUE)
    client_instance@.internal_rcpp_object$run_script(script)
    return(NULL)
  }
)

# do not need to set generic for 'close', as it already exists as a generic
#' @export
setMethod(
  "close",
  signature = c(con = "Client"),
  function(con) {
    con@.internal_rcpp_object$close()
    return(NULL)
  }
)
