(* Lexer/Parser: shells out to riot, parses N-Triples into typed triples *)

type line = Line of string
type path = Path of string
type error = Riot_failed of { path : path; exit_code : int }

let parse_datatype rest =
  let rest = String.trim rest in
  match String.length rest >= 4 && rest.[0] = '^' && rest.[1] = '^' with
  | false -> None
  | true -> (
      let dt = String.trim (String.sub rest 2 (String.length rest - 2)) in
      match dt.[0] with
      | '<' -> Some (Term.Datatype.T (String.sub dt 1 (String.length dt - 2)))
      | _ -> None)

let parse_term s =
  let s = String.trim s in
  match s.[0] with
  | '<' -> Some (Term.Iri (String.sub s 1 (String.length s - 2)))
  | '_' -> Some (Term.Blank (String.sub s 2 (String.length s - 2)))
  | '"' ->
      let last_quote = String.rindex s '"' in
      if last_quote = 0 then None
      else
        let value = String.sub s 1 (last_quote - 1) in
        let rest =
          String.sub s (last_quote + 1) (String.length s - last_quote - 1)
        in
        Some (Term.Literal { value; datatype = parse_datatype rest })
  | _ -> None
  | exception Invalid_argument _ -> None

let split_at_space s =
  match String.index_opt s ' ' with
  | None -> None
  | Some i ->
      let left = String.sub s 0 i in
      let right =
        String.trim (String.sub s (i + 1) (String.length s - i - 1))
      in
      Some (left, right)

let parse_line (Line line) =
  let line = String.trim line in
  if String.length line = 0 || line.[0] = '#' then None
  else
    match String.rindex_opt line '.' with
    | None -> None
    | Some dot_pos -> (
        let content = String.trim (String.sub line 0 dot_pos) in
        match split_at_space content with
        | None -> None
        | Some (subject_str, rest) -> (
            match split_at_space rest with
            | None -> None
            | Some (predicate_str, object_str) -> (
                match
                  ( parse_term subject_str,
                    parse_term predicate_str,
                    parse_term object_str )
                with
                | Some subject, Some predicate, Some object_ ->
                    Some Term.{ subject; predicate; object_ }
                | _ -> None)))

let from_file (Path raw as path) =
  let cmd = Printf.sprintf "riot --output=ntriples %s" (Filename.quote raw) in
  let ic = Unix.open_process_in cmd in
  let lines = ref [] in
  (try
     while true do
       lines := Line (input_line ic) :: !lines
     done
   with End_of_file -> ());
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 -> Ok (List.filter_map parse_line (List.rev !lines))
  | Unix.WEXITED code -> Error (Riot_failed { path; exit_code = code })
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
      Error (Riot_failed { path; exit_code = -1 })

let pp_term fmt = function
  | Term.Iri uri -> Fmt.pf fmt "<%s>" uri
  | Term.Blank id -> Fmt.pf fmt "_:%s" id
  | Term.Literal { value; datatype = None } -> Fmt.pf fmt "%S" value
  | Term.Literal { value; datatype = Some (Term.Datatype.T dt) } ->
      Fmt.pf fmt "%S^^<%s>" value dt

let pp_triple fmt (t : Term.triple) =
  Fmt.pf fmt "%a %a %a ." pp_term t.subject pp_term t.predicate pp_term
    t.object_
