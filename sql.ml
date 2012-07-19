(** @author Rok Strnisa *)

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
    | Double -> "double precision"
    | Integer -> "integer"
    | Varchar sz -> sprintf "character varying(%d)" sz
    | VarcharUnknown -> failwith "string_of_sql_type: VarcharUnknown"
    | Text -> "text"

  let is_quoted = function
    | Boolean | Double | Integer -> false
    | _ -> true
end

type tbl_name   = string
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

type col_type_cache_tbl = (col_name, Type.t) Hashtbl.t
type col_type_cache = (tbl_name, col_type_cache_tbl) Hashtbl.t

let cached_types : col_type_cache = Hashtbl.create 128

let get_col_types ~conn ~tbl =
  try Hashtbl.find cached_types tbl
  with Not_found ->
    let query = sprintf "SELECT * FROM %s LIMIT 0" tbl in
    let result = exec_exn ~conn ~query in
    let num_cols = result#nfields in
    let cached_types_tbl = Hashtbl.create num_cols in
    Hashtbl.add cached_types tbl cached_types_tbl;
    for i = 0 to num_cols - 1 do
      let col_name' = result#fname i in
      let ty = Type.of_pgtype (result#ftype i) in
      Hashtbl.replace cached_types_tbl col_name' ty;
    done;
    cached_types_tbl

let get_col_types_lst ~conn ~tbl =
  let col_types = get_col_types ~conn ~tbl in
  List.rev (Hashtbl.fold (fun n t acc -> (n, t)::acc) col_types [])

let get_col_type ~conn ~tbl ~col_name =
  try Hashtbl.find (get_col_types ~conn ~tbl) col_name
  with Not_found ->
    debug (sprintf "Column '%s' not found in table %s" col_name tbl);
    raise Not_found

let get_col_names ~conn ~tbl =
  List.map fst (get_col_types_lst ~conn ~tbl)

let string_of_value ~conn ~tbl ~col_name ~value =
  let ty = get_col_type ~conn ~tbl ~col_name in
  match Type.is_quoted ty with
  | false -> value
  | true -> "'" ^ value ^ "'"

let sql_values_of_tuples ~conn ~tbl ~tuples =
  let value_string_of_tuple (col_name, value) =
    string_of_value ~conn ~tbl ~col_name ~value
  in String.concat ", " (List.map value_string_of_tuple tuples)

let sql_assign_of_tuples ~conn ~tbl ~tuples =
  let pair (col_name, value) =
    let val_str = string_of_value ~conn ~tbl ~col_name ~value in
    sprintf "\"%s\"=%s" col_name val_str
  in List.map pair tuples

let sql_cond_of_tuples ~conn ~tbl ~tuples =
  String.concat " AND " (sql_assign_of_tuples ~conn ~tbl ~tuples)

let sql_set_of_tuples ~conn ~tbl ~tuples =
  String.concat ", " (sql_assign_of_tuples ~conn ~tbl ~tuples)

let update_entry ~conn ~tbl ~tuples_cond ~tuples_set =
  let cond = sql_cond_of_tuples ~conn ~tbl ~tuples:tuples_cond in
  let set = sql_set_of_tuples ~conn ~tbl ~tuples:tuples_set in
  let query = sprintf "UPDATE %s SET %s WHERE %s" tbl set cond in
  match !mode with
  | Debug -> debug query
  | Live -> exec_ign_exn ~conn ~query

let rstrip str =
  let len = String.length str in
  let rec aux i =
    if i <= 0 then len else
    if List.mem str.[i] [' '; '\t']
    then aux (i-1) else i+1
  in
  String.sub str 0 (aux (len-1))

let get_first_entry ~result =
  if result#nfields > 0 && result#ntuples > 0
  then Some (rstrip (result#getvalue 0 0))
  else None

let get_first_entry_exn ~result =
  match get_first_entry ~result with
  | None -> failwith "get_first_entry_exn"
  | Some v -> v

let sql_names_of_tuples ~tuples =
  String.concat "," (List.map (fun (n, _) -> "\"" ^ n ^ "\"") tuples)

let sql_names_values_of_tuples ~conn ~tbl ~tuples =
  let names = sql_names_of_tuples ~tuples in
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
      get_first_entry_exn ~result
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
      get_first_entry_exn ~result:(exec_exn ~conn ~query:select)
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

let get_col ~result ~col =
  Array.to_list (Array.map (fun row -> row.(col)) result#get_all)
