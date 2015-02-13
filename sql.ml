(** @author Rok Strnisa *)

open Printf

let debug_fn : (string -> unit) option ref = ref None
let debug msg = match !debug_fn with None -> () | Some fn -> fn msg

type mode_ty = Live | Debug
let mode : mode_ty ref = ref Live

let show_sql : bool ref = ref false

let time_queries : bool ref = ref false

let ignore_limit_0 : bool ref = ref false

module Type = struct
  type t =
    | Boolean
    | Double
    | Integer
    | LongInt
    | Varchar of int
    | VarcharUnknown
    | Text

  let of_pgtype = function
    | Postgresql.BOOL -> Boolean
    | Postgresql.INT4 -> Integer
    | Postgresql.INT8 -> LongInt
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
    | LongInt -> "bigint"
    | Varchar sz -> sprintf "character varying(%d)" sz
    | VarcharUnknown -> failwith "string_of_sql_type: VarcharUnknown"
    | Text -> "text"

  let is_quoted = function
    | Double | Integer | LongInt -> false
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

let limit_0 = " LIMIT 0"
let limit_0_len = String.length limit_0

let is_limit_0 ~query =
  limit_0 = String.sub query (String.length query - limit_0_len) limit_0_len

let output_timing ~start_time =
  let elapsed_time = Unix.gettimeofday () -. start_time in
  debug (sprintf "The query took %fs." elapsed_time)

let exec ~(conn : conn) ~query =
  let show = not (!ignore_limit_0 && is_limit_0 ~query) in
  if show && !show_sql then debug query;
  let start_time = if show && !time_queries then Unix.gettimeofday () else 0. in
  let result = conn#exec query in
  if show && !time_queries then output_timing ~start_time;
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

let escape_single_quotes str =
  Str.global_replace (Str.regexp "'") "''" str

let check_value ~ty ~value =
  match ty with
  | Type.Double ->
      if List.mem value ["inf";"-inf";"nan";"-nan"]
        then failwith (sprintf "Can't represent float value '%s'" value)
        else ()
  | _ -> ()

let string_of_value ~conn ~tbl ~col_name ~value =
  let ty = get_col_type ~conn ~tbl ~col_name in
  check_value ~ty ~value;
  match Type.is_quoted ty with
  | false -> if value="None" then "NULL" else value
  | true -> "'" ^ (escape_single_quotes value) ^ "'"

let sql_values_of_tuples ~conn ~tbl ~tuples =
  let value_string_of_tuple (col_name, value) =
    string_of_value ~conn ~tbl ~col_name ~value
  in String.concat ", " (List.map value_string_of_tuple tuples)

let sql_assign_of_tuples ~conn ~tbl ~tuples =
  let pair (col_name, value) =
    let val_str = string_of_value ~conn ~tbl ~col_name ~value in
    sprintf "\"%s\"%s%s" col_name (if val_str="NULL" then " is " else "=") val_str
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

let select_query ~conn ~tbl ~tuples =
  match tuples with
    | [] -> sprintf "SELECT * FROM %s" tbl
    | _ ->
      let cond = sql_cond_of_tuples ~conn ~tbl ~tuples in
      sprintf "SELECT * FROM %s WHERE %s" tbl cond

let select_exn ~conn ~tbl ~tuples =
  exec_exn ~conn ~query:(select_query ~conn ~tbl ~tuples)

let insert_get_first_col ~conn ~tbl ~tuples =
  let insert = match tuples with
    | [] ->
      (* There's only one column in the table: the (serial) ID column. *)
      sprintf "INSERT INTO %s VALUES (DEFAULT)" tbl
    | _ ->
      let values = sql_names_values_of_tuples ~conn ~tbl ~tuples in
      sprintf "INSERT INTO %s %s" tbl values
  in
  let select = select_query ~conn ~tbl ~tuples in
  let result = exec_exn ~conn ~query:(insert ^ ";" ^ select) in
  get_first_entry_exn ~result

let ensure_inserted_get_first_col ~conn ~tbl ~tuples =
  match !mode with
  | Debug ->
    debug (sprintf "%-25s: %s" tbl (sql_set_of_tuples ~conn ~tbl ~tuples));
    "-1"
  | Live ->
    let result = select_exn ~conn ~tbl ~tuples in
    match result#ntuples with
    | 0 -> insert_get_first_col ~conn ~tbl ~tuples
    | 1 -> get_first_entry_exn ~result
    | _ -> failwith "More than one row matches query."

let ensure_inserted_get_id ~conn ~tbl ~tuples =
  int_of_string (ensure_inserted_get_first_col ~conn ~tbl ~tuples)

let ensure_inserted ~conn ~tbl ~tuples =
  ignore (ensure_inserted_get_first_col ~conn ~tbl ~tuples)

let combine_cond_and_set_tuples ~tuples_cond ~tuples_set =
  let names_set = List.map fst tuples_set in
  let tuples_cond_filtered =
    List.filter (fun (n, _) -> not (List.mem n names_set)) tuples_cond in
  tuples_cond_filtered @ tuples_set

let insert_or_update ~conn ~tbl ~tuples_cond ~tuples_set =
  match !mode with
  | Debug ->
    let tuples = combine_cond_and_set_tuples ~tuples_cond ~tuples_set in
    debug (sprintf "%-25s: %s" tbl (sql_set_of_tuples ~conn ~tbl ~tuples))
  | Live ->
    let result = select_exn ~conn ~tbl ~tuples:tuples_cond in
    match result#ntuples with
    | 1 -> update_entry ~conn ~tbl ~tuples_cond ~tuples_set
    | 0 ->
      let tuples = combine_cond_and_set_tuples ~tuples_cond ~tuples_set in
      ensure_inserted ~conn ~tbl ~tuples
    | _ -> failwith "More than one row matches query."

let get_col ~result ~col =
  Array.to_list (Array.map (fun row -> row.(col)) result#get_all)
