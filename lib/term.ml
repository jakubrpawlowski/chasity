(* RDF data model: terms (IRI, blank node, literal) and triples *)

module Datatype = struct
  type t = T of string

  let compare (T a) (T b) = String.compare a b
end

type t =
  | Iri of string
  | Blank of string
  | Literal of { value : string; datatype : Datatype.t option }

let compare a b =
  match (a, b) with
  | Iri a, Iri b -> String.compare a b
  | Blank a, Blank b -> String.compare a b
  | Literal a, Literal b ->
      let c = String.compare a.value b.value in
      if c <> 0 then c
      else Option.compare Datatype.compare a.datatype b.datatype
  | Iri _, _ -> -1
  | _, Iri _ -> 1
  | Blank _, _ -> -1
  | _, Blank _ -> 1

type triple = { subject : t; predicate : t; object_ : t }

let to_iri = function Iri s -> Some (Iri.Iri s) | _ -> None

let to_int = function
  | Literal { value; _ } -> int_of_string_opt value
  | _ -> None

let to_float = function
  | Literal { value; _ } -> float_of_string_opt value
  | _ -> None

let to_string = function Literal { value; _ } -> Some value | _ -> None
