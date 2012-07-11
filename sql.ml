open Core.Std
open Printf

let debug_fn : (string -> unit) option ref = ref None
let debug msg = match !debug_fn with None -> () | Some fn -> fn msg

type mode_ty = Live | Debug
let mode : mode_ty ref = ref Live

let show_sql : bool ref = ref false

module Type = struct
  type t =
    | Boolean
    | Double
    | Integer
    | Varchar of int
    | VarcharUnknown
    | Text

  let of_pgtype = function
    | Postgresql.BOOL -> Boolean
    | Postgresql.INT4 -> Integer
    | Postgresql.FLOAT8 -> Double
    | Postgresql.TEXT -> Text
    | Postgresql.VARCHAR -> VarcharUnknown
    | pgtype ->
      let pgtype_str = Postgresql.string_of_ftype pgtype in
      failwith (sprintf "sql_type_of_ftype: %s" pgtype_str)

  let string_of = function
    | Boolean -> "boolean"
    | Double -> "double"
    | Integer -> "integer"
    | Varchar sz -> sprintf "character varying(%d)" sz
    | VarcharUnknown -> failwith "string_of_sql_type: VarcharUnknown"
    | Text -> "text"

  let is_quoted = function
    | Boolean | Double | Integer -> false
    | _ -> true
end

type col_name   = string
type col_value  = string
type col_assign = col_name * col_value
type col_decl   = col_name * Type.t
type col_init   = col_name * col_value * Type.t
type conn       = Postgresql.connection
type result     = Postgresql.result

let exec ~(conn : conn) ~query =
  if !show_sql then debug query;
  let result = conn#exec query in
  match result#status with
    | Postgresql.Command_ok | Postgresql.Tuples_ok -> Some result
    | _ -> debug conn#error_message; None

let exec_exn ~conn ~query =
  match exec ~conn ~query with
  | None -> failwith ("Failed to execute query: " ^ query)
  | Some result -> result

let exec_ign_exn ~conn ~query =
  ignore (exec_exn ~conn ~query)

let tbl_exists ~conn ~tbl =
  let query = "SELECT * FROM pg_tables WHERE schemaname='public' AND " ^
    (sprintf "tablename='%s'" tbl) in
  (exec_exn ~conn ~query)#ntuples = 1

module PairKey = struct
  module T = struct
    type t = string * string with sexp
    type sexpable = t
    let compare = compare
    let equal = (=)
    let hash = Hashtbl.hash
  end
  include T
  include Hashable.Make (T)
end

let cached_types : Type.t PairKey.Table.t = PairKey.Table.create ()

let get_type ~conn ~tbl ~col_name =
  match PairKey.Table.find cached_types (tbl, col_name) with
  | Some ty -> ty
  | None ->
    let query = sprintf "SELECT * FROM %s LIMIT 1" tbl in
    let result = exec_exn ~conn ~query in
    for i = 0 to result#nfields - 1 do
      let key = tbl, result#fname i in
      let data = Type.of_pgtype (result#ftype i) in
      PairKey.Table.replace cached_types ~key ~data;
    done;
    PairKey.Table.find_exn cached_types (tbl, col_name)

let string_of_value ~conn ~tbl ~col_name ~value =
  let ty = get_type ~conn ~tbl ~col_name in
  match Type.is_quoted ty with
  | false -> value
  | true -> "'" ^ value ^ "'"

let sql_values_of_tuples ~conn ~tbl ~tuples =
  let value_string_of_tuple (col_name, value) =
    string_of_value ~conn ~tbl ~col_name ~value
  in String.concat ~sep:", " (List.map ~f:value_string_of_tuple tuples)

let sql_assign_of_tuples ~conn ~tbl ~tuples =
  let pair (col_name, value) =
    let val_str = string_of_value ~conn ~tbl ~col_name ~value in
    sprintf "%s=%s" col_name val_str
  in List.map ~f:pair tuples

let sql_cond_of_tuples ~conn ~tbl ~tuples =
  String.concat ~sep:" AND " (sql_assign_of_tuples ~conn ~tbl ~tuples)

let sql_set_of_tuples ~conn ~tbl ~tuples =
  String.concat ~sep:", " (sql_assign_of_tuples ~conn ~tbl ~tuples)

let update_entry ~conn ~tbl ~tuples_cond ~tuples_set =
  let cond = sql_cond_of_tuples ~conn ~tbl ~tuples:tuples_cond in
  let set = sql_set_of_tuples ~conn ~tbl ~tuples:tuples_set in
  let query = sprintf "UPDATE %s SET %s WHERE %s" tbl set cond in
  match !mode with
  | Debug -> debug query
  | Live -> exec_ign_exn ~conn ~query

let get_first_entry ~result =
  if result#nfields > 0 && result#ntuples > 0
  then Some (String.strip (result#getvalue 0 0))
  else None

let get_first_entry_exn ~result =
  match get_first_entry ~result with
  | None -> failwith "get_first_entry_exn"
  | Some v -> v

let sql_names_values_of_tuples ~conn ~tbl ~tuples =
  let names = String.concat ~sep:"," (List.map ~f:fst tuples) in
  let values = sql_values_of_tuples ~conn ~tbl ~tuples in
  sprintf "(%s) VALUES (%s)" names values

let ensure_inserted_get_first_col ~conn ~tbl ~tuples =
  let d = sprintf "%-25s: %s" tbl (sql_set_of_tuples ~conn ~tbl ~tuples) in
  match !mode with
  | Debug ->
    debug d; "-1"
  | Live ->
    let select = match tuples with
      | [] -> sprintf "SELECT * FROM %s" tbl
      | _ ->
          let cond = sql_cond_of_tuples ~conn ~tbl ~tuples in
          sprintf "SELECT * FROM %s WHERE %s" tbl cond
    in
    let result = exec_exn ~conn ~query:select in
    match result#ntuples with
    | 1 ->
      get_first_entry_exn result
    | 0 ->
      let insert = match tuples with
        | [] ->
            (* There's only one column in the table: the (serial) ID column. *)
            sprintf "INSERT INTO %s VALUES (DEFAULT)" tbl
        | _ ->
            let values = sql_names_values_of_tuples ~conn ~tbl ~tuples in
            sprintf "INSERT INTO %s %s" tbl values
      in
      exec_ign_exn ~conn ~query:insert;
      get_first_entry_exn (exec_exn ~conn ~query:select)
    | _ -> failwith "More than one row matches query."

let ensure_inserted_get_id ~conn ~tbl ~tuples =
  int_of_string (ensure_inserted_get_first_col ~conn ~tbl ~tuples)

let ensure_inserted ~conn ~tbl ~tuples =
  ignore (ensure_inserted_get_first_col ~conn ~tbl ~tuples)

let insert_or_update ~conn ~tbl ~tuples_cond ~tuples_set =
  let tuples = tuples_cond @ tuples_set in
  let d = sprintf "%-25s: %s" tbl (sql_set_of_tuples ~conn ~tbl ~tuples) in
  match !mode with
  | Debug ->
    debug d
  | Live ->
    let cond = sql_cond_of_tuples ~conn ~tbl ~tuples:tuples_cond in
    let select = sprintf "SELECT * FROM %s WHERE %s" tbl cond in
    let result = exec_exn ~conn ~query:select in
    match result#ntuples with
    | 1 ->
      update_entry ~conn ~tbl ~tuples_cond ~tuples_set
    | 0 ->
      ensure_inserted ~conn ~tbl ~tuples
    | _ -> failwith "More than one row matches query."

let get_first_col ~result =
  Array.to_list (Array.map ~f:(fun row -> row.(0)) result#get_all)
