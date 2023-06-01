(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type ty =
    TyBot
  | TyTop
  | TyId of string
  | TyVar of int * int
  | TyArr of ty * ty
  | TyRecord of (string * ty) list
  | TyVariant of (string * ty) list
  | TyRef of ty
  | TyString
  | TyUnit
  | TyBool
  | TySource of ty
  | TySink of ty
  | TyFloat
  | TyNat

type perset = 
  | Perset of string

(* type auAtom = 
    AuAtomUp of string
  | AuAtomDown of string *)

(* type auComp =
    AuComp of auth * auAtom *)

type auth =
    AuAtomUp of string
  | AuAtomDown of string
  | AuComp of auth * auth
  | AuArr of auth * auth

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * auth * term
  | TmApp of info * term * term
  | TmAscribe of info * term * ty
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term 
  | TmAssign of info * term * term
  | TmCase of info * term * (string * (string * term)) list
  | TmTag of info * string * term * ty
  | TmLet of info * string * term * term
  | TmFix of info * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty
  | TmAuas of info * term * auth
  | TmRequire of info * term * auth

type binding =
    NameBind 
  | VarBind of ty * auth
  | TmAbbBind of term * (ty option) * (auth option) * perset
  | TyVarBind
  | TyAbbBind of ty

type command =
    Import of string
  | Eval of info * perset * term
  | Bind of info * perset * string * binding
  | PersetRelDecl of perset * perset
  | PersetDecl of perset

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty

(* Perset *)
type persetTable
type edge
val emptyPersetTable : persetTable
val addPerset : persetTable -> perset -> perset -> persetTable
val prPerset : perset -> unit
val prPersetTab : persetTable -> unit
val prPersets : edge list -> unit
val makeAuthUp : perset -> auth
val makeAuthDown : perset -> auth

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

