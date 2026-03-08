(* AST: in-memory subject-indexed store of parsed triples *)

module TermMap = Map.Make (Ntriples.Term)

type t = (Ntriples.Term.t * Ntriples.Term.t) list TermMap.t

let of_triples triples =
  List.fold_left
    (fun store (triple : Ntriples.triple) ->
      let existing =
        match TermMap.find_opt triple.subject store with
        | Some pairs -> pairs
        | None -> []
      in
      TermMap.add triple.subject
        ((triple.predicate, triple.object_) :: existing)
        store)
    TermMap.empty triples

let find_subject (subject : Ntriples.Term.t) (store : t) =
  match TermMap.find_opt subject store with Some pairs -> pairs | None -> []

let find_by_predicate (predicate : Ntriples.Term.t) (store : t) =
  TermMap.fold
    (fun subject pairs acc ->
      List.fold_left
        (fun acc (pred, obj) ->
          if Ntriples.Term.compare pred predicate = 0 then (subject, obj) :: acc
          else acc)
        acc pairs)
    store []
