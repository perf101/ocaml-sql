(** The main module of ocaml-sql.
 *
 * @author Rok Strnisa
 **)

(** Use [debug_fn := f] to pass debugging messages to [f]. By default, these
 * messages are ignored. *)
val debug_fn : (string -> unit) option ref

(** Two modes of operation. SQL queries are executed only in [Live] mode. *)
type mode_ty = Live | Debug

(** Use [mode := Debug] to prevent SQL queries from being executed. Default
 * [mode] is [Live]. *)
val mode : mode_ty ref

(** Use [show_sql := true] to show SQL queries to be executed; they are shown
 * even in [Debug] mode. By default, SQL queries are not shown. *)
val show_sql : bool ref

(** An abstraction of the underlying database type. *)
module Type :
  sig
    type t =
        Boolean
      | Double
      | Integer
      | Varchar of int
      | VarcharUnknown
      | Text

    (** [of_pgtype fty] convert PostgreSQL database type [fty] to [t], or fails
     * if no suitable [t] exists. *)
    val of_pgtype : Postgresql.ftype -> t

    (** [string_of t] converts [t] to a string suitable for SQL table
     * generation. *)
    val string_of : t -> string

    (** [is_quoted t] is [true] iff a value of type [t] should be quoted in
     * SQL queries. *)
    val is_quoted : t -> bool
  end

(** The type of column names. *)
type col_name   = string

(** The type of column values. Note that all values of all types, e.g.
 * integers and strings, must be expressed using this type. *)
type col_value  = string

(** The type of column assignments, i.e. assigning a column value to a column
 * name. *)
type col_assign = col_name * col_value

(** The type of column declarations, i.e. a column name associated with an
 * SQL type. *)
type col_decl   = col_name * Type.t

(** The type of column initialisations, i.e. a column declaration together
 * with a column value. *)
type col_init   = col_name * col_value * Type.t

(** The type of the underlying database connection. *)
type conn       = Postgresql.connection

(** The type of the underlying database result. *)
type result     = Postgresql.result

(** [exec conn query] executes [query] using database connection [conn]. If
 * the command is successful, the result is returned; otherwise, None is
 * returned. *)
val exec : conn:conn -> query:string -> result option

(** [exec_exn conn query] executes [query] using database connection [conn].
 * The command returns the result without the option wrapper; however, if it
 * fails, an exception is thrown. All of the remaining SQL operations are
 * implemented using this function, and can so similarly cause an exception
 * to be thrown. *)
val exec_exn : conn:conn -> query:string -> result

(** [exec_ign_exn conn query] executes [query] using database connection
 * [conn], ignoring any result returned. *)
val exec_ign_exn : conn:conn -> query:string -> unit

(** [tbl_exists conn tbl] returns [true] if a table with name [tbl] exists
 * in the database. *)
val tbl_exists : conn:conn -> tbl:string -> bool

(** [get_col_types conn tbl] obtains a map from column names of table [tbl] to
 * corresponding SQL types. The results are cached. *)
val get_col_types : conn:conn -> tbl:string -> (col_name, Type.t) Hashtbl.t

(** [get_col_types_lst conn tbl] obtains an association list from column names
 * of table [tbl] to corresponding SQL types. The results are cached. *)
val get_col_types_lst : conn:conn -> tbl:string -> (col_name * Type.t) list

(** [get_col_type conn tbl col_name] obtains the SQL type corresponding to the
 * [col_name] of table [tbl]. The results are cached. *)
val get_col_type : conn:conn -> tbl:string -> col_name:col_name -> Type.t

(** [get_col_names conn tbl] obtains the list of column names in the table
 * named [tbl].*)
val get_col_names : conn:conn -> tbl:string -> col_name list

(** [update_entry conn tbl tuples_cond tuples_set] updates a row in table
 * [tbl] with column assignments [tuples_cond], modifying columns as specified
 * by [tuples_set]. The command throws an exception if no row satisfies
 * [tuples_cond]. *)
val update_entry : conn:conn -> tbl:string -> tuples_cond:col_assign list ->
  tuples_set:col_assign list -> unit

(** [get_first_entry result] obtains the value of the first column of the
 * first row within [result]. If there are no rows or no columns, [None] is
 * returned instead. *)
val get_first_entry : result:result -> col_value option

(** [get_first_entry_exn result] performs the same function as
 * [get_first_entry result], except that the value is unpacked, and where an
 * exception is thrown where [None] was returned. *)
val get_first_entry_exn : result:result -> col_value

(** [ensure_inserted_get_first_col conn tbl tuples] ensures that a row
 * specified with [tuples] exists within table [tbl], i.e. the row is inserted
 * if missing. The first column (often the generated unique identifier of the
 * row) is returned. Note that [tuples] only needs to contain non-optional
 * column assignments --- this is true for all similar functions below. *)
val ensure_inserted_get_first_col : conn:conn -> tbl:string ->
  tuples:col_assign list -> col_value

(** [ensure_inserted_get_id conn tbl tuples] performs the same function as
 * [ensure_inserted_get_first_col conn tbl tuples], except that it also
 * converts the returned value to an interger. *)
val ensure_inserted_get_id : conn:conn -> tbl:string ->
  tuples:col_assign list -> int

(** [ensure_inserted conn tbl tuples] performs the same function as
 * [ensure_inserted_get_first_col conn tbl tuples], except that it does not
 * return any values. *)
val ensure_inserted : conn:conn -> tbl:string -> tuples:col_assign list -> unit

(** [insert_or_update conn tbl tuples_cond tuples_set] performs the same
 * function as [update_entry conn tbl tuples_cond tuples_set], except that it
 * inserts the row specified with [tuples_cond] and [tuples_set] if no row
 * matches [tuples_cond]. *)
val insert_or_update : conn:conn -> tbl:string ->
  tuples_cond:col_assign list -> tuples_set:col_assign list -> unit

(** [get_col result col] returns the values of [col]-th column for all rows
 * in [result]. Counting starts with 0. *)
val get_col : result:result -> col:int -> col_value list
