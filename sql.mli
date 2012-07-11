val debug_fn : (string -> unit) option ref
type mode_ty = Live | Debug
val mode : mode_ty ref
val show_sql : bool ref
module Type :
  sig
    type t =
        Boolean
      | Double
      | Integer
      | Varchar of int
      | VarcharUnknown
      | Text
    val of_pgtype : Postgresql.ftype -> t
    val string_of : t -> string
    val is_quoted : t -> bool
  end
type col_name   = string
type col_value  = string
type col_assign = col_name * col_value
type col_decl   = col_name * Type.t
type col_init   = col_name * col_value * Type.t
type conn       = Postgresql.connection
type result     = Postgresql.result
val exec : conn:conn -> query:string -> result option
val exec_exn : conn:conn -> query:string -> result
val exec_ign_exn : conn:conn -> query:string -> unit
val tbl_exists : conn:conn -> tbl:string -> bool
val get_type : conn:conn -> tbl:string -> col_name:col_name -> Type.t
val update_entry : conn:conn -> tbl:string -> tuples_cond:col_assign list ->
  tuples_set:col_assign list -> unit
val get_first_entry : result:result -> col_value option
val get_first_entry_exn : result:result -> col_value
val ensure_inserted_get_first_col : conn:conn -> tbl:string ->
  tuples:col_assign list -> col_value
val ensure_inserted_get_id : conn:conn -> tbl:string ->
  tuples:col_assign list -> int
val ensure_inserted : conn:conn -> tbl:string -> tuples:col_assign list -> unit
val insert_or_update : conn:conn -> tbl:string ->
  tuples_cond:col_assign list -> tuples_set:col_assign list -> unit
val get_first_col : result:result -> col_value list
