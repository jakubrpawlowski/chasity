type variable = Var of string

type literal =
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Typed of { value : string; datatype : Iri.t }

(* Data triples: INSERT DATA / DELETE DATA — no variables *)

type data_object = Data_iri of Iri.t | Data_literal of literal
type data_triple = { subject : Iri.t; predicate : Iri.t; object_ : data_object }

(* Triple patterns: WHERE / CONSTRUCT / DELETE-INSERT — variables allowed *)

type pattern_subject = Sub_iri of Iri.t | Sub_var of variable
type pattern_predicate = Pred_iri of Iri.t | Pred_var of variable

type pattern_object =
  | Obj_iri of Iri.t
  | Obj_literal of literal
  | Obj_var of variable

type triple_pattern = {
  subject : pattern_subject;
  predicate : pattern_predicate;
  object_ : pattern_object;
}

(* Filter expressions *)

type filter_term =
  | Filt_var of variable
  | Filt_iri of Iri.t
  | Filt_literal of literal

type filter_expr =
  | Gt of filter_term * filter_term
  | Lt of filter_term * filter_term
  | Eq of filter_term * filter_term
  | Bound of variable
  | Not of filter_expr
  | And of filter_expr * filter_expr
  | Or of filter_expr * filter_expr

(* Graph patterns *)

type graph_pattern =
  | Triples of triple_pattern list
  | Optional of graph_pattern
  | Filter of filter_expr
  | Values of { variable : variable; values : Iri.t list }
  | Graph of { graph : Iri.t; pattern : graph_pattern }
  | Group of graph_pattern list

(* Query forms *)

type order_direction = Asc | Desc

type select = {
  distinct : bool;
  variables : variable list;
  where : graph_pattern;
  order_by : (variable * order_direction) list;
  limit : int option;
}

type construct = { template : triple_pattern list; where : graph_pattern }
type insert_data = { graph : Iri.t option; triples : data_triple list }
type delete_data = { graph : Iri.t option; triples : data_triple list }

type delete_insert = {
  graph : Iri.t option;
  delete : triple_pattern list;
  insert : triple_pattern list;
  where : graph_pattern;
}

type query =
  | Select of select
  | Construct of construct
  | Insert_data of insert_data
  | Delete_data of delete_data
  | Delete_insert of delete_insert
