(* Syntaxe abstraite Mini-ML *)

module VSet = Set.Make (String)

type typ =
  | TInt
  | TBool
  | TUnit
  | TFun of typ * typ
  | TStrct of string
  | TVar of string
  | TArray of typ

type schema = { vars : VSet.t; typ : typ }
type strct = (string * typ * bool) list

let rec string_of_type = function
  | TInt -> "int"
  | TBool -> "bool"
  | TUnit -> "unit"
  | TFun (typ1, typ2) -> (
      match typ1 with
      | TFun _ -> Printf.sprintf "(%s) -> %s" (string_of_type typ1) (string_of_type typ2)
      | _ -> Printf.sprintf "%s -> %s" (string_of_type typ1) (string_of_type typ2))
  | TStrct s -> s
  | TVar s -> s
  | TArray s -> Printf.sprintf "%s array" (string_of_type s)

let string_of_schema s = Printf.sprintf "%s.%s" (VSet.fold ( ^ ) s.vars "∀") (string_of_type s.typ)

type uop = Neg | Not

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Struct_Eq
  | Struct_Neq

type expr =
  | Int of int
  | Bool of bool
  | Array of expr list
  | Unit
  | Uop of uop * expr
  | Bop of bop * expr * expr
  | Var of string
  | Let of string * expr * expr
  | If of expr * expr * expr
  | Fun of string * expr
  | UnitFun of expr
  | App of expr * expr
  | Fix of string * expr
  | Strct of (string * expr) list
  | GetF of expr * string
  | SetF of expr * string * expr
  | Seq of expr * expr
  | GetArray of expr * expr
  | SetArray of expr * expr * expr

type prog = { types : (string * strct) list; code : expr }

(* Fonctions auxiliaires, utilisables pour gérer le sucre syntaxique
     let f (x1:t1) ... (xN:tN) = ...
   de définition d'une fonction à plusieurs arguments. *)
let rec mk_fun xs e = match xs with [] -> e | x :: xs -> Fun (x, mk_fun xs e)
let rec mk_fun_type xs t = match xs with [] -> t | (_, t') :: xs -> TFun (t', mk_fun_type xs t)

(* Interpretation *)

(* Valeurs *)
type value = VInt of int | VBool of bool | VUnit | VPtr of int

(* Environnement : associe des valeurs à des noms de variables *)
module Env = Map.Make (String)

(* Élements du tas *)
type heap_value =
  | VClos of string * expr * value Env.t
  | VStrct of (string, value) Hashtbl.t
  | VArray of value array
