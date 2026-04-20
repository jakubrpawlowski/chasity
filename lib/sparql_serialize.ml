open Fun_ext

let ind n = String.make (n * 2) ' '
let lines ~depth f = List.map (f >> ( ^ ) (ind depth)) >> String.concat "\n"

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | c -> Buffer.add_char buf c)
    s;
  Buffer.contents buf

let iri (Iri.Iri s) = "<" ^ s ^ ">"
let variable (Sparql.Var name) = "?" ^ name

let literal = function
  | Sparql.String s -> "\"" ^ escape s ^ "\""
  | Int n -> string_of_int n
  | Float f -> string_of_float f
  | Bool b -> if b then "true" else "false"
  | Typed { value; datatype } -> "\"" ^ escape value ^ "\"^^" ^ iri datatype

let data_object = function
  | Sparql.Data_iri i -> iri i
  | Data_literal l -> literal l

let data_triple (t : Sparql.data_triple) =
  Printf.sprintf "%s %s %s ." (iri t.subject) (iri t.predicate)
    (data_object t.object_)

let pattern_subject = function
  | Sparql.Sub_iri i -> iri i
  | Sub_var v -> variable v

let pattern_predicate = function
  | Sparql.Pred_iri i -> iri i
  | Pred_var v -> variable v

let pattern_object = function
  | Sparql.Obj_iri i -> iri i
  | Obj_literal l -> literal l
  | Obj_var v -> variable v

let triple_pattern (tp : Sparql.triple_pattern) =
  Printf.sprintf "%s %s %s ."
    (pattern_subject tp.subject)
    (pattern_predicate tp.predicate)
    (pattern_object tp.object_)

let filter_term = function
  | Sparql.Filt_var v -> variable v
  | Filt_iri i -> iri i
  | Filt_literal l -> literal l

let rec filter_expr = function
  | Sparql.Gt (a, b) -> Printf.sprintf "%s > %s" (filter_term a) (filter_term b)
  | Lt (a, b) -> Printf.sprintf "%s < %s" (filter_term a) (filter_term b)
  | Eq (a, b) -> Printf.sprintf "%s = %s" (filter_term a) (filter_term b)
  | Bound v -> Printf.sprintf "BOUND(%s)" (variable v)
  | Not e -> Printf.sprintf "!(%s)" (filter_expr e)
  | And (a, b) -> Printf.sprintf "(%s && %s)" (filter_expr a) (filter_expr b)
  | Or (a, b) -> Printf.sprintf "(%s || %s)" (filter_expr a) (filter_expr b)

let rec graph_pattern ~depth = function
  | Sparql.Triples tps -> lines ~depth triple_pattern tps
  | Optional gp ->
      Printf.sprintf "%sOPTIONAL {\n%s\n%s}" (ind depth)
        (graph_pattern ~depth:(depth + 1) gp)
        (ind depth)
  | Filter fe -> Printf.sprintf "%sFILTER(%s)" (ind depth) (filter_expr fe)
  | Values { variable = v; values = vs } ->
      Printf.sprintf "%sVALUES %s { %s }" (ind depth) (variable v)
        (List.map iri vs |> String.concat " ")
  | Graph { graph = g; pattern = gp } ->
      Printf.sprintf "%sGRAPH %s {\n%s\n%s}" (ind depth) (iri g)
        (graph_pattern ~depth:(depth + 1) gp)
        (ind depth)
  | Group gps -> gps |> List.map (graph_pattern ~depth) |> String.concat "\n"

let order_clause (v, dir) =
  let dir_s = match dir with Sparql.Asc -> "ASC" | Desc -> "DESC" in
  Printf.sprintf "%s(%s)" dir_s (variable v)

let select (s : Sparql.select) =
  let distinct = if s.distinct then "DISTINCT " else "" in
  let vars = List.map variable s.variables |> String.concat " " in
  let where = graph_pattern ~depth:1 s.where in
  let order =
    match s.order_by with
    | [] -> ""
    | clauses ->
        "\nORDER BY " ^ (List.map order_clause clauses |> String.concat " ")
  in
  let limit =
    match s.limit with None -> "" | Some n -> Printf.sprintf "\nLIMIT %d" n
  in
  Printf.sprintf "SELECT %s%s\nWHERE {\n%s\n}%s%s" distinct vars where order
    limit

let construct (c : Sparql.construct) =
  Printf.sprintf "CONSTRUCT {\n%s\n}\nWHERE {\n%s\n}"
    (lines ~depth:1 triple_pattern c.template)
    (graph_pattern ~depth:1 c.where)

let insert_data (q : Sparql.insert_data) =
  match q.graph with
  | None ->
      Printf.sprintf "INSERT DATA {\n%s\n}"
        (lines ~depth:1 data_triple q.triples)
  | Some g ->
      Printf.sprintf "INSERT DATA {\n%sGRAPH %s {\n%s\n%s}\n}" (ind 1) (iri g)
        (lines ~depth:2 data_triple q.triples)
        (ind 1)

let delete_data (q : Sparql.delete_data) =
  match q.graph with
  | None ->
      Printf.sprintf "DELETE DATA {\n%s\n}"
        (lines ~depth:1 data_triple q.triples)
  | Some g ->
      Printf.sprintf "DELETE DATA {\n%sGRAPH %s {\n%s\n%s}\n}" (ind 1) (iri g)
        (lines ~depth:2 data_triple q.triples)
        (ind 1)

let delete_insert (q : Sparql.delete_insert) =
  let wrap depth triples =
    match q.graph with
    | None -> lines ~depth triple_pattern triples
    | Some g ->
        Printf.sprintf "%sGRAPH %s {\n%s\n%s}" (ind depth) (iri g)
          (lines ~depth:(depth + 1) triple_pattern triples)
          (ind depth)
  in
  Printf.sprintf "DELETE {\n%s\n}\nINSERT {\n%s\n}\nWHERE {\n%s\n}"
    (wrap 1 q.delete) (wrap 1 q.insert)
    (graph_pattern ~depth:1 q.where)

let to_string = function
  | Sparql.Select s -> select s
  | Construct c -> construct c
  | Insert_data q -> insert_data q
  | Delete_data q -> delete_data q
  | Delete_insert q -> delete_insert q
