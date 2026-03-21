(* In-memory subject-indexed store for triple lookups *)

module TermMap = Map.Make (Term)

type t = (Term.t * Term.t) list TermMap.t

let of_triples triples =
  List.fold_left
    (fun store (triple : Term.triple) ->
      let existing =
        match TermMap.find_opt triple.subject store with
        | Some pairs -> pairs
        | None -> []
      in
      TermMap.add triple.subject
        ((triple.predicate, triple.object_) :: existing)
        store)
    TermMap.empty triples

let find_subject (subject : Term.t) (store : t) =
  match TermMap.find_opt subject store with Some pairs -> pairs | None -> []

let find_object predicate pairs =
  List.find_map
    (fun (pred, obj) ->
      if Term.compare pred predicate = 0 then Some obj else None)
    pairs

let find_all_objects predicate pairs =
  List.filter_map
    (fun (pred, obj) ->
      if Term.compare pred predicate = 0 then Some obj else None)
    pairs

let find_by_predicate (predicate : Term.t) (store : t) =
  TermMap.fold
    (fun subject pairs acc ->
      List.fold_left
        (fun acc (pred, obj) ->
          if Term.compare pred predicate = 0 then (subject, obj) :: acc else acc)
        acc pairs)
    store []
