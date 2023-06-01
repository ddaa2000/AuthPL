(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val typeof : context -> term -> ty
val subtype : context -> ty -> ty -> bool
type store
val emptystore : store
val shiftstore : int -> store -> store 
val eval : context -> store -> persetTable -> perset -> term -> term * store
val evalbinding : context -> store -> persetTable -> perset -> binding -> binding * store
val tyeqv : context -> ty -> ty -> bool
val simplifyty : context -> ty -> ty
val authOf : context -> persetTable -> perset -> term -> auth
