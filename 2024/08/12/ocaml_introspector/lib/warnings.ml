(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis && Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1998 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* When you change this, you need to update:
   - the list 'description' at the bottom of this file
   - man/ocamlc.m
*)
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
type loc = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}
             [@@deriving  yojson]

type field_usage_warning =
  | Unused
  | Not_read
  | Not_mutated
[@@deriving  yojson]

type constructor_usage_warning =
  | Unused
  | Not_constructed
  | Only_exported_private
[@@deriving  yojson]

type t =
  | Comment_start                           (*  1 *)
  | Comment_not_end                         (*  2 *)
(*| Deprecated --> alert "deprecated" *)    (*  3 *)
  | Fragile_match of string                 (*  4 *)
  | Ignored_partial_application             (*  5 *)
  | Labels_omitted of string list           (*  6 *)
  | Method_override of string list          (*  7 *)
  | Partial_match of string                 (*  8 *)
  | Missing_record_field_pattern of string  (*  9 *)
  | Non_unit_statement                      (* 10 *)
  | Redundant_case                          (* 11 *)
  | Redundant_subpat                        (* 12 *)
  | Instance_variable_override of string list (* 13 *)
  | Illegal_backslash                       (* 14 *)
  | Implicit_public_methods of string list  (* 15 *)
  | Unerasable_optional_argument            (* 16 *)
  | Undeclared_virtual_method of string     (* 17 *)
  | Not_principal of string                 (* 18 *)
  | Non_principal_labels of string          (* 19 *)
  | Ignored_extra_argument                  (* 20 *)
  | Nonreturning_statement                  (* 21 *)
  | Preprocessor of string                  (* 22 *)
  | Useless_record_with                     (* 23 *)
  | Bad_module_name of string               (* 24 *)
  | All_clauses_guarded                     (* 8, used to be 25 *)
  | Unused_var of string                    (* 26 *)
  | Unused_var_strict of string             (* 27 *)
  | Wildcard_arg_to_constant_constr         (* 28 *)
  | Eol_in_string                           (* 29 *)
  | Duplicate_definitions of string * string * string * string (*30 *)
  | Module_linked_twice of string * string * string (* 31 *)
  | Unused_value_declaration of string      (* 32 *)
  | Unused_open of string                   (* 33 *)
  | Unused_type_declaration of string       (* 34 *)
  | Unused_for_index of string              (* 35 *)
  | Unused_ancestor of string               (* 36 *)
  | Unused_constructor of string * constructor_usage_warning (* 37 *)
  | Unused_extension of string * bool * constructor_usage_warning (* 38 *)
  | Unused_rec_flag                         (* 39 *)
  | Name_out_of_scope of string * string list * bool (* 40 *)
  | Ambiguous_name of string list * string list *  bool * string (* 41 *)
  | Disambiguated_name of string            (* 42 *)
  | Nonoptional_label of string             (* 43 *)
  | Open_shadow_identifier of string * string (* 44 *)
  | Open_shadow_label_constructor of string * string (* 45 *)
  | Bad_env_variable of string * string     (* 46 *)
  | Attribute_payload of string * string    (* 47 *)
  | Eliminated_optional_arguments of string list (* 48 *)
  | No_cmi_file of string * string option   (* 49 *)
  | Unexpected_docstring of bool            (* 50 *)
  | Wrong_tailcall_expectation of bool      (* 51 *)
  | Fragile_literal_pattern                 (* 52 *)
  | Misplaced_attribute of string           (* 53 *)
  | Duplicated_attribute of string          (* 54 *)
  | Inlining_impossible of string           (* 55 *)
  | Unreachable_case                        (* 56 *)
  | Ambiguous_var_in_pattern_guard of string list (* 57 *)
  | No_cmx_file of string                   (* 58 *)
  | Flambda_assignment_to_non_mutable_value (* 59 *)
  | Unused_module of string                 (* 60 *)
  | Unboxable_type_in_prim_decl of string   (* 61 *)
  | Constraint_on_gadt                      (* 62 *)
  | Erroneous_printed_signature of string   (* 63 *)
  | Unsafe_array_syntax_without_parsing     (* 64 *)
  | Redefining_unit of string               (* 65 *)
  | Unused_open_bang of string              (* 66 *)
  | Unused_functor_parameter of string      (* 67 *)
  | Match_on_mutable_state_prevent_uncurry  (* 68 *)
  | Unused_field of string * field_usage_warning (* 69 *)
  | Missing_mli                             (* 70 *)
  | Unused_tmc_attribute                    (* 71 *)
  | Tmc_breaks_tailcall                     (* 72 *)
[@@deriving  yojson]

(* If you remove a warning, leave a hole in the numbering.  NEVER change
   the numbers of existing warnings.
   If you add a new warning, add it at the end with a new number;
   do NOT reuse one of the holes.
*)

type alert = {kind:string; message:string; def:loc; use:loc}
               [@@deriving  yojson]

let number = function
  | Comment_start -> 1
  | Comment_not_end -> 2
  | Fragile_match _ -> 4
  | Ignored_partial_application -> 5
  | Labels_omitted _ -> 6
  | Method_override _ -> 7
  | Partial_match _ -> 8
  | Missing_record_field_pattern _ -> 9
  | Non_unit_statement -> 10
  | Redundant_case -> 11
  | Redundant_subpat -> 12
  | Instance_variable_override _ -> 13
  | Illegal_backslash -> 14
  | Implicit_public_methods _ -> 15
  | Unerasable_optional_argument -> 16
  | Undeclared_virtual_method _ -> 17
  | Not_principal _ -> 18
  | Non_principal_labels _ -> 19
  | Ignored_extra_argument -> 20
  | Nonreturning_statement -> 21
  | Preprocessor _ -> 22
  | Useless_record_with -> 23
  | Bad_module_name _ -> 24
  | All_clauses_guarded -> 8 (* used to be 25 *)
  | Unused_var _ -> 26
  | Unused_var_strict _ -> 27
  | Wildcard_arg_to_constant_constr -> 28
  | Eol_in_string -> 29
  | Duplicate_definitions _ -> 30
  | Module_linked_twice _ -> 31
  | Unused_value_declaration _ -> 32
  | Unused_open _ -> 33
  | Unused_type_declaration _ -> 34
  | Unused_for_index _ -> 35
  | Unused_ancestor _ -> 36
  | Unused_constructor _ -> 37
  | Unused_extension _ -> 38
  | Unused_rec_flag -> 39
  | Name_out_of_scope _ -> 40
  | Ambiguous_name _ -> 41
  | Disambiguated_name _ -> 42
  | Nonoptional_label _ -> 43
  | Open_shadow_identifier _ -> 44
  | Open_shadow_label_constructor _ -> 45
  | Bad_env_variable _ -> 46
  | Attribute_payload _ -> 47
  | Eliminated_optional_arguments _ -> 48
  | No_cmi_file _ -> 49
  | Unexpected_docstring _ -> 50
  | Wrong_tailcall_expectation _ -> 51
  | Fragile_literal_pattern -> 52
  | Misplaced_attribute _ -> 53
  | Duplicated_attribute _ -> 54
  | Inlining_impossible _ -> 55
  | Unreachable_case -> 56
  | Ambiguous_var_in_pattern_guard _ -> 57
  | No_cmx_file _ -> 58
  | Flambda_assignment_to_non_mutable_value -> 59
  | Unused_module _ -> 60
  | Unboxable_type_in_prim_decl _ -> 61
  | Constraint_on_gadt -> 62
  | Erroneous_printed_signature _ -> 63
  | Unsafe_array_syntax_without_parsing -> 64
  | Redefining_unit _ -> 65
  | Unused_open_bang _ -> 66
  | Unused_functor_parameter _ -> 67
  | Match_on_mutable_state_prevent_uncurry -> 68
  | Unused_field _ -> 69
  | Missing_mli -> 70
  | Unused_tmc_attribute -> 71
  | Tmc_breaks_tailcall -> 72
;;

let last_warning_number = 72
;;

type description =
  { number : int;
    names : string list;
    (* The first element of the list is the current name, any following ones are
       deprecated. The current name should always be derived mechanically from
       the constructor name. *)
    description : string; }
    [@@deriving  yojson]

let descriptions = [
  { number = 1;
    names = ["comment-start"];
    description = "Suspicious-looking start-of-comment mark." };
  { number = 2;
    names =  ["comment-not-end"];
    description = "Suspicious-looking end-of-comment mark." };
  { number = 3;
    names = [];
    description = "Deprecated synonym for the 'deprecated' alert." };
  { number = 4;
    names = ["fragile-match"];
    description =
      "Fragile pattern matching: matching that will remain complete even\n\
      \    if additional constructors are added to one of the variant types\n\
      \    matched." };
  { number = 5;
    names = ["ignored-partial-application"];
    description =
      "Partially applied function: expression whose result has function\n\
      \    type and is ignored." };
  { number = 6;
    names = ["labels-omitted"];
    description = "Label omitted in function application." };
  { number = 7;
    names = ["method-override"];
    description = "Method overridden." };
  { number = 8;
    names = ["partial-match"];
    description = "Partial match: missing cases in pattern-matching." };
  { number = 9;
    names = ["missing-record-field-pattern"];
    description = "Missing fields in a record pattern." };
  { number = 10;
    names = ["non-unit-statement"];
    description =
      "Expression on the left-hand side of a sequence that doesn't have type\n\
      \    \"unit\" (and that is not a function, see warning number 5)." };
  { number = 11;
    names = ["redundant-case"];
    description =
      "Redundant case in a pattern matching (unused match case)." };
  { number = 12;
    names = ["redundant-subpat"];
    description = "Redundant sub-pattern in a pattern-matching." };
  { number = 13;
    names = ["instance-variable-override"];
    description = "Instance variable overridden." };
  { number = 14;
    names = ["illegal-backslash"];
    description = "Illegal backslash escape in a string constant." };
  { number = 15;
    names = ["implicit-public-methods"];
    description = "Private method made public implicitly." };
  { number = 16;
    names = ["unerasable-optional-argument"];
    description = "Unerasable optional argument." };
  { number = 17;
    names = ["undeclared-virtual-method"];
    description = "Undeclared virtual method." };
  { number = 18;
    names = ["not-principal"];
    description = "Non-principal type." };
  { number = 19;
    names = ["non-principal-labels"];
    description = "Type without principality." };
  { number = 20;
    names = ["ignored-extra-argument"];
    description = "Unused function argument." };
  { number = 21;
    names = ["nonreturning-statement"];
    description = "Non-returning statement." };
  { number = 22;
    names = ["preprocessor"];
    description = "Preprocessor warning." };
  { number = 23;
    names = ["useless-record-with"];
    description = "Useless record \"with\" clause." };
  { number = 24;
    names = ["bad-module-name"];
    description =
    "Bad module name: the source file name is not a valid OCaml module name."};
  { number = 25;
    names = [];
    description = "Ignored: now part of warning 8." };
  { number = 26;
    names = ["unused-var"];
    description =
    "Suspicious unused variable: unused variable that is bound\n\
    \    with \"let\" or \"as\", and doesn't start with an underscore (\"_\")\n\
    \    character." };
  { number = 27;
    names = ["unused-var-strict"];
    description =
    "Innocuous unused variable: unused variable that is not bound with\n\
    \    \"let\" nor \"as\", and doesn't start with an underscore (\"_\")\n\
    \    character." };
  { number = 28;
    names = ["wildcard-arg-to-constant-constr"];
    description =
      "Wildcard pattern given as argument to a constant constructor." };
  { number = 29;
    names = ["eol-in-string"];
    description =
      "Unescaped end-of-line in a string constant (non-portable code)." };
  { number = 30;
    names = ["duplicate-definitions"];
    description =
      "Two labels or constructors of the same name are defined in two\n\
      \    mutually recursive types." };
  { number = 31;
    names = ["module-linked-twice"];
    description = "A module is linked twice in the same executable." };
  { number = 32;
    names = ["unused-value-declaration"];
    description = "Unused value declaration." };
  { number = 33;
    names = ["unused-open"];
    description = "Unused open statement." };
  { number = 34;
    names = ["unused-type-declaration"];
    description = "Unused type declaration." };
  { number = 35;
    names = ["unused-for-index"];
    description = "Unused for-loop index." };
  { number = 36;
    names = ["unused-ancestor"];
    description = "Unused ancestor variable." };
  { number = 37;
    names = ["unused-constructor"];
    description = "Unused constructor." };
  { number = 38;
    names = ["unused-extension"];
    description = "Unused extension constructor." };
  { number = 39;
    names = ["unused-rec-flag"];
    description = "Unused rec flag." };
  { number = 40;
    names = ["name-out-of-scope"];
    description = "Constructor or label name used out of scope." };
  { number = 41;
    names = ["ambiguous-name"];
    description = "Ambiguous constructor or label name." };
  { number = 42;
    names = ["disambiguated-name"];
    description =
      "Disambiguated constructor or label name (compatibility warning)." };
  { number = 43;
    names = ["nonoptional-label"];
    description = "Nonoptional label applied as optional." };
  { number = 44;
    names = ["open-shadow-identifier"];
    description = "Open statement shadows an already defined identifier." };
  { number = 45;
    names = ["open-shadow-label-constructor"];
    description =
      "Open statement shadows an already defined label or constructor." };
  { number = 46;
    names = ["bad-env-variable"];
    description = "Error in environment variable." };
  { number = 47;
    names = ["attribute-payload"];
    description = "Illegal attribute payload." };
  { number = 48;
    names = ["eliminated-optional-arguments"];
    description = "Implicit elimination of optional arguments." };
  { number = 49;
    names = ["no-cmi-file"];
    description = "Absent cmi file when looking up module alias." };
  { number = 50;
    names = ["unexpected-docstring"];
    description = "Unexpected documentation comment." };
  { number = 51;
    names = ["wrong-tailcall-expectation"];
    description =
      "Function call annotated with an incorrect @tailcall attribute" };
  { number = 52;
    names = ["fragile-literal-pattern"];
    description = "Fragile constant pattern." };
  { number = 53;
    names = ["misplaced-attribute"];
    description = "Attribute cannot appear in this context." };
  { number = 54;
    names = ["duplicated-attribute"];
    description = "Attribute used more than once on an expression." };
  { number = 55;
    names = ["inlining-impossible"];
    description = "Inlining impossible." };
  { number = 56;
    names = ["unreachable-case"];
    description =
      "Unreachable case in a pattern-matching (based on type information)." };
  { number = 57;
    names = ["ambiguous-var-in-pattern-guard"];
    description = "Ambiguous or-pattern variables under guard." };
  { number = 58;
    names = ["no-cmx-file"];
    description = "Missing cmx file." };
  { number = 59;
    names = ["flambda-assignment-to-non-mutable-value"];
    description = "Assignment to non-mutable value." };
  { number = 60;
    names = ["unused-module"];
    description = "Unused module declaration." };
  { number = 61;
    names = ["unboxable-type-in-prim-decl"];
    description = "Unboxable type in primitive declaration." };
  { number = 62;
    names = ["constraint-on-gadt"];
    description = "Type constraint on GADT type declaration." };
  { number = 63;
    names = ["erroneous-printed-signature"];
    description = "Erroneous printed signature." };
  { number = 64;
    names = ["unsafe-array-syntax-without-parsing"];
    description =
      "-unsafe used with a preprocessor returning a syntax tree." };
  { number = 65;
    names = ["redefining-unit"];
    description = "Type declaration defining a new '()' constructor." };
  { number = 66;
    names = ["unused-open-bang"];
    description = "Unused open! statement." };
  { number = 67;
    names = ["unused-functor-parameter"];
    description = "Unused functor parameter." };
  { number = 68;
    names = ["match-on-mutable-state-prevent-uncurry"];
    description =
      "Pattern-matching depending on mutable state prevents the remaining \n\
      \    arguments from being uncurried." };
  { number = 69;
    names = ["unused-field"];
    description = "Unused record field." };
  { number = 70;
    names = ["missing-mli"];
    description = "Missing interface file." };
  { number = 71;
    names = ["unused-tmc-attribute"];
    description = "Unused @tail_mod_cons attribute" };
  { number = 72;
    names = ["tmc-breaks-tailcall"];
    description = "A tail call is turned into a non-tail call \
                   by the @tail_mod_cons transformation." };
]
;;

let name_to_number =
  let h = Hashtbl.create last_warning_number in
  List.iter (fun {number; names; _} ->
      List.iter (fun name -> Hashtbl.add h name number) names
    ) descriptions;
  fun s -> Hashtbl.find_opt h s
;;

(* Must be the max number returned by the [number] function. *)

let letter = function
  | 'a' ->
     let rec loop i = if i = 0 then [] else i :: loop (i - 1) in
     loop last_warning_number
  | 'b' -> []
  | 'c' -> [1; 2]
  | 'd' -> [3]
  | 'e' -> [4]
  | 'f' -> [5]
  | 'g' -> []
  | 'h' -> []
  | 'i' -> []
  | 'j' -> []
  | 'k' -> [32; 33; 34; 35; 36; 37; 38; 39]
  | 'l' -> [6]
  | 'm' -> [7]
  | 'n' -> []
  | 'o' -> []
  | 'p' -> [8]
  | 'q' -> []
  | 'r' -> [9]
  | 's' -> [10]
  | 't' -> []
  | 'u' -> [11; 12]
  | 'v' -> [13]
  | 'w' -> []
  | 'x' -> [14; 15; 16; 17; 18; 19; 20; 21; 22; 23; 24; 30]
  | 'y' -> [26]
  | 'z' -> [27]
  | _ -> assert false
;;

type state =
  {
    active: bool array;
    error: bool array;
    (* alerts: (Misc.Stdlib.String.Set.t * bool); (\* false:set complement *\) *)
    (* alert_errors: (Misc.Stdlib.String.Set.t * bool); (\* false:set complement *\) *)
  }
[@@deriving  yojson]
let current =
  ref
    {
      active = Array.make (last_warning_number + 1) true;
      error = Array.make (last_warning_number + 1) false;
      (* alerts = (Misc.Stdlib.String.Set.empty, false); (\* all enabled *\) *)
      (* alert_errors = (Misc.Stdlib.String.Set.empty, true); (\* all soft *\) *)
    }

let disabled = ref false

let without_warnings f =
  Misc.protect_refs [Misc.R(disabled, true)] f

let backup () = !current

let restore x = current := x

let is_active x =
  not !disabled && (!current).active.(number x)

let is_error x =
  not !disabled && (!current).error.(number x)

(* let alert_is_active {kind; _} = *)
(*   not !disabled && *)
(*   let (set, pos) = (!current).alerts in *)
(*   Misc.Stdlib.String.Set.mem kind set = pos *)

(* let alert_is_error {kind; _} = *)
(*   not !disabled && *)
(*   let (set, pos) = (!current).alert_errors in *)
(*   Misc.Stdlib.String.Set.mem kind set = pos *)

let with_state state f =
  let prev = backup () in
  restore state;
  try
    let r = f () in
    restore prev;
    r
  with exn ->
    restore prev;
    raise exn

let mk_lazy f =
  let state = backup () in
  lazy (with_state state f)

(* let set_alert ~error ~enable s = *)
(*   let upd = *)
(*     match s with *)
(*     | "all" -> *)
(*         (Misc.Stdlib.String.Set.empty, not enable) *)
(*     | s -> *)
(*         let (set, pos) = *)
(*           if error then (!current).alert_errors else (!current).alerts *)
(*         in *)
(*         let f = *)
(*           if enable = pos *)
(*           then Misc.Stdlib.String.Set.add *)
(*           else Misc.Stdlib.String.Set.remove *)
(*         in *)
(*         (f s set, pos) *)
(*   in *)
(*   if error then *)
(*     current := {(!current) with alert_errors=upd} *)
(*   else *)
(*     current := {(!current) with alerts=upd} *)

(* let parse_alert_option s = *)
(*   let n = String.length s in *)
(*   let id_char = function *)
(*     | 'a'..'z' | 'A'..'Z' | '_' | '\'' | '0'..'9' -> true *)
(*     | _ -> false *)
(*   in *)
(*   let rec parse_id i = *)
(*     if i < n && id_char s.[i] then parse_id (i + 1) else i *)
(*   in *)
(*   let rec scan i = *)
(*     if i = n then () *)
(*     else if i + 1 = n then raise (Arg.Bad "Ill-formed list of alert settings") *)
(*     else match s.[i], s.[i+1] with *)
(*       | '+', '+' -> id (set_alert ~error:true ~enable:true) (i + 2) *)
(*       | '+', _ -> id (set_alert ~error:false ~enable:true) (i + 1) *)
(*       | '-', '-' -> id (set_alert ~error:true ~enable:false) (i + 2) *)
(*       | '-', _ -> id (set_alert ~error:false ~enable:false) (i + 1) *)
(*       | '@', _ -> *)
(*           id (fun s -> *)
(*               set_alert ~error:true ~enable:true s; *)
(*               set_alert ~error:false ~enable:true s) *)
(*             (i + 1) *)
(*       | _ -> raise (Arg.Bad "Ill-formed list of alert settings") *)
(*   and id f i = *)
(*     let j = parse_id i in *)
(*     if j = i then raise (Arg.Bad "Ill-formed list of alert settings"); *)
(*     let id = String.sub s i (j - i) in *)
(*     f id; *)
(*     scan j *)
(*   in *)
(*   scan 0 *)

type modifier =
  | Set (** +a *)
  | Clear (** -a *)
  | Set_all (** @a *)
[@@deriving  yojson]

type token =
  | Letter of char * modifier option
  | Num of int * int * modifier
[@@deriving  yojson]
(* let letter_alert tokens = *)
(*   let print_warning_char ppf c = *)
(*     let lowercase = Char.lowercase_ascii c = c in *)
(*     Format.fprintf ppf "%c%c" *)
(*       (if lowercase then '-' else '+') c *)
(*   in *)
(*   let print_modifier ppf = function *)
(*     | Set_all -> Format.fprintf ppf "@" *)
(*     | Clear -> Format.fprintf ppf "-" *)
(*     | Set -> Format.fprintf ppf "+" *)
(*   in *)
(*   let print_token ppf = function *)
(*     | Num (a,b,m) -> if a = b then *)
(*           Format.fprintf ppf "%a%d" print_modifier m a *)
(*         else *)
(*           Format.fprintf ppf "%a%d..%d" print_modifier m a b *)
(*     | Letter(l,Some m) -> Format.fprintf ppf "%a%c" print_modifier m l *)
(*     | Letter(l,None) -> print_warning_char ppf l *)
(*   in *)
(*   let consecutive_letters = *)
(*     (\* we are tracking sequences of 2 or more consecutive unsigned letters *)
(*        in warning strings, for instance in '-w "not-principa"'. *\) *)
(*     let commit_chunk l = function *)
(*       | [] | [ _ ] -> l *)
(*       | _ :: _ :: _ as chunk -> List.rev chunk :: l *)
(*     in *)
(*     let group_consecutive_letters (l,current) = function *)
(*     | Letter (x, None) -> (l, x::current) *)
(*     | _ -> (commit_chunk l current, []) *)
(*     in *)
(*     let l, on_going = *)
(*       List.fold_left group_consecutive_letters ([],[]) tokens *)
(*     in *)
(*     commit_chunk l on_going *)
(*   in *)
(*   match consecutive_letters with *)
(*   | [] -> None *)
(*   | example :: _  -> *)
(*       let pos = { Lexing.dummy_pos with pos_fname = "_none_" } in *)
(*       let nowhere = { loc_start=pos; loc_end=pos; loc_ghost=true } in *)
(*       let spelling_hint ppf = *)
(*         let max_seq_len = *)
(*           List.fold_left (fun l x -> Int.max l (List.length x)) *)
(*             0 consecutive_letters *)
(*         in *)
(*         if max_seq_len >= 5 then *)
(*           Format.fprintf ppf *)
(*             "@ @[Hint: Did you make a spelling mistake \ *)
(*              when using a mnemonic name?@]" *)
(*         else *)
(*           () *)
(*       in *)
(*       let message = *)
(*         Format.asprintf *)
(*           "@[<v>@[Setting a warning with a sequence of lowercase \ *)
(*            or uppercase letters,@ like '%a',@ is deprecated.@]@ \ *)
(*            @[Use the equivalent signed form:@ %t.@]@ \ *)
(*            @[Hint: Enabling or disabling a warning by its mnemonic name \ *)
(*            requires a + or - prefix.@]\ *)
(*            %t@?@]" *)
(*           Format.(pp_print_list ~pp_sep:(fun _ -> ignore) pp_print_char) example *)
(*           (fun ppf -> List.iter (print_token ppf) tokens) *)
(*           spelling_hint *)
(*       in *)
(*       Some { *)
(*         kind="ocaml_deprecated_cli"; *)
(*         use=nowhere; def=nowhere; *)
(*         message *)
(*       } *)


let parse_warnings s =
  let error () = raise (Arg.Bad "Ill-formed list of warnings") in
  let rec get_num n i =
    if i >= String.length s then i, n
    else match s.[i] with
    | '0'..'9' -> get_num (10 * n + Char.code s.[i] - Char.code '0') (i + 1)
    | _ -> i, n
  in
  let get_range i =
    let i, n1 = get_num 0 i in
    if i + 2 < String.length s && s.[i] = '.' && s.[i + 1] = '.' then
      let i, n2 = get_num 0 (i + 2) in
      if n2 < n1 then error ();
      i, n1, n2
    else
      i, n1, n1
  in
  let rec loop tokens i =
    if i >= String.length s then List.rev tokens else
    match s.[i] with
    | 'A' .. 'Z' | 'a' .. 'z' ->
        loop (Letter(s.[i],None)::tokens) (i+1)
    | '+' -> loop_letter_num tokens Set (i+1)
    | '-' -> loop_letter_num tokens Clear (i+1)
    | '@' -> loop_letter_num tokens Set_all (i+1)
    | _ -> error ()
  and loop_letter_num tokens modifier i =
    if i >= String.length s then error () else
    match s.[i] with
    | '0' .. '9' ->
        let i, n1, n2 = get_range i in
        loop (Num(n1,n2,modifier)::tokens) i
    | 'A' .. 'Z' | 'a' .. 'z' ->
       loop (Letter(s.[i],Some modifier)::tokens) (i+1)
    | _ -> error ()
  in
  loop [] 0


type reporting_information =
  { id : string
  ; message : string
  ; is_error : bool
  ; sub_locs : (loc * string) list;
  }

let id_name w =
  let n = number w in
  match List.find_opt (fun {number; _} -> number = n) descriptions with
  | Some {names = s :: _; _} ->
      Printf.sprintf "%d [%s]" n s
  | _ ->
      string_of_int n

exception Errors;;

