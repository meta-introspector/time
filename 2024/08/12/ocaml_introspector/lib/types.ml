(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Representation of types and declarations *)

open Asttypes
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
(* Type expressions for the core language *)

type transient_expr =
  { mutable desc: type_desc;
    mutable level: int;
    mutable scope: int;
    id: int }
[@@deriving  yojson]
and type_expr = transient_expr
[@@deriving  yojson]
and type_desc =
    Tvar of string option
  | Tarrow of arg_label * type_expr * type_expr * commutable
  | Ttuple of type_expr list
  | Tconstr of Path.t * type_expr list * abbrev_memo ref
  | Tobject of type_expr * (Path.t * type_expr list) option ref
  | Tfield of string * field_kind * type_expr * type_expr
  | Tnil
  | Tlink of type_expr
  | Tsubst of type_expr * type_expr option
  (* | Tvariant of row_desc *)
  | Tunivar of string option
  | Tpoly of type_expr * type_expr list
  | Tpackage of Path.t * (Longident.t * type_expr) list
[@@deriving  yojson]
(* and row_desc = *)
(*     { row_fields: (label * row_field) list; *)
(*       row_more: type_expr; *)
(*       row_closed: bool; *)
(*       row_fixed: fixed_explanation option; *)
(*       row_name: (Path.t * type_expr list) option } *)
(* [@@deriving  yojson] *)
and fixed_explanation =
  | Univar of type_expr | Fixed_private | Reified of Path.t | Rigid
[@@deriving  yojson]
(* and row_field = [`some] row_field_gen *)
(* [@@deriving  yojson] *)
(* and _ row_field_gen = *)
(*     RFpresent : type_expr option -> [> `some] row_field_gen *)
(*   | RFeither : *)
(*       { no_arg: bool; *)
(*         arg_type: type_expr list; *)
(*         matched: bool; *)
(*         ext: [`some | `none] row_field_gen ref} -> [> `some] row_field_gen *)
(*   | RFabsent : [> `some] row_field_gen *)
(*   | RFnone : [> `none] row_field_gen *)
(* [@@deriving  yojson] *)
and abbrev_memo =
    Mnil
  | Mcons of private_flag * Path.t * type_expr * type_expr * abbrev_memo
  | Mlink of abbrev_memo ref
[@@deriving  yojson]
and any = [`some | `none | `var]
(* [@@deriving  yojson] *)
 and field_kind = [`some|`var] field_kind_gen
[@@deriving  yojson]
 and _ field_kind_gen =
   Fixme
     (*     FKvar : {mutable field_kind: any field_kind_gen} -> [> `var] field_kind_gen *)
     (* | FKprivate : [> `none] field_kind_gen  (\* private method; only under FKvar *\) *)
     (*   | FKpublic  : [> `some] field_kind_gen  (\* public method *\) *)
     (*   | FKabsent  : [> `some] field_kind_gen  (\* hidden private method *\) *)
     [@@deriving  yojson] 
and commutable = [`some|`var] commutable_gen
[@@deriving  yojson]
and _ commutable_gen =
  FixMecommutable_gen
  (*   Cok      : [> `some] commutable_gen *)
  (* | Cunknown : [> `none] commutable_gen *)
  (* | Cvar : {mutable commu: any commutable_gen} -> [> `var] commutable_gen *)
[@@deriving  yojson]
module TransientTypeOps = struct
  type t = type_expr
  [@@deriving  yojson]
  let compare t1 t2 = t1.id - t2.id
  let hash t = t.id
  let equal t1 t2 = t1 == t2
end

(* *)

module Uid = Shape.Uid

(* Maps of methods and instance variables *)

module MethSet = Misc.Stdlib.String.Set
module VarSet = Misc.Stdlib.String.Set

module Meths = Misc.Stdlib.String.Map
module Vars = Misc.Stdlib.String.Map


(* Value descriptions *)

module type ValueDescription = sig
  type attributes
       
  type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: attributes;
    val_uid: Uid.t;
  }
[@@deriving  yojson]
and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of
      class_signature * self_meths * Ident.t Vars.t * string
                                        (* Self *)
  | Val_anc of class_signature * Ident.t Meths.t * string
                                        (* Ancestor *)
[@@deriving  yojson]
and self_meths =
  | Self_concrete of Ident.t Meths.t
  | Self_virtual of Ident.t Meths.t ref
[@@deriving  yojson]
and class_signature =
  { csig_self: type_expr;
    mutable csig_self_row: type_expr;
    mutable csig_vars: (mutable_flag * virtual_flag * type_expr) Vars.t;
    mutable csig_meths: (method_privacy * virtual_flag * type_expr) Meths.t; }
[@@deriving  yojson]
and method_privacy =
  | Mpublic
  | Mprivate of field_kind
[@@deriving  yojson]
(* Variance *)
end

module Variance = struct
  type t = int
  type f = May_pos | May_neg | May_weak | Inj | Pos | Neg | Inv
  let single = function
    | May_pos -> 1
    | May_neg -> 2
    | May_weak -> 4
    | Inj -> 8
    | Pos -> 16
    | Neg -> 32
    | Inv -> 64
  let union v1 v2 = v1 lor v2
  let inter v1 v2 = v1 land v2
  let subset v1 v2 = (v1 land v2 = v1)
  let eq (v1 : t) v2 = (v1 = v2)
  let set x b v =
    if b then v lor single x else  v land (lnot (single x))
  let mem x = subset (single x)
  let null = 0
  let unknown = 7
  let full = 127
  let covariant = single May_pos lor single Pos lor single Inj
  let swap f1 f2 v =
    let v' = set f1 (mem f2 v) v in set f2 (mem f1 v) v'
  let conjugate v = swap May_pos May_neg (swap Pos Neg v)
  let get_upper v = (mem May_pos v, mem May_neg v)
  let get_lower v = (mem Pos v, mem Neg v, mem Inv v, mem Inj v)
  let unknown_signature ~injective ~arity =
    let v = if injective then set Inj true unknown else unknown in
    Misc.replicate_list v arity
end

module Separability = struct
  type t = Ind | Sep | Deepsep
  [@@deriving  yojson]
  type signature = t list
  [@@deriving  yojson]
  let eq (m1 : t) m2 = (m1 = m2)
  let rank = function
    | Ind -> 0
    | Sep -> 1
    | Deepsep -> 2
  let compare m1 m2 = compare (rank m1) (rank m2)
  let max m1 m2 = if rank m1 >= rank m2 then m1 else m2

  let print ppf = function
    | Ind -> Format.fprintf ppf "Ind"
    | Sep -> Format.fprintf ppf "Sep"
    | Deepsep -> Format.fprintf ppf "Deepsep"

  let print_signature ppf modes =
    let pp_sep ppf () = Format.fprintf ppf ",@," in
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep print) modes

  let default_signature ~arity =
    let default_mode = if Config.flat_float_array then Deepsep else Ind in
    Misc.replicate_list default_mode arity
end

(* Type definitions *)
module type TypeDeclaration = sig
  type attributes
type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: attributes;
    type_immediate: Type_immediacy.t;
    type_unboxed_default: bool;
    type_uid: Uid.t;
 }
[@@deriving  yojson]
and type_decl_kind = (label_declaration, constructor_declaration) type_kind
[@@deriving  yojson]
and ('lbl, 'cstr) type_kind =
    Type_abstract
  | Type_record of 'lbl list * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open
[@@deriving  yojson]
and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)
[@@deriving  yojson]
and variant_representation =
    Variant_regular          (* Constant or boxed constructors *)
  | Variant_unboxed          (* One unboxed single-field constructor *)
[@@deriving  yojson]
and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_type: type_expr;
    ld_loc: Location.t;
    ld_attributes: attributes;
    ld_uid: Uid.t;
  }
[@@deriving  yojson]
and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: attributes;
    cd_uid: Uid.t;
  }
[@@deriving  yojson]
and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list
end

module type ExtensionConstructor = sig
  type attributes
  type constructor_arguments
type extension_constructor =
  { ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: attributes;
    ext_uid: Uid.t;
  }
[@@deriving  yojson]
and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)
[@@deriving  yojson]
(* Type expressions for the class language *)
end

module type ClassType = sig
  type attributes
  type class_signature
  type constructor_arguments

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type
                                           [@@deriving  yojson]
end

module type ClassDecl = sig
  type attributes
  type class_signature
  type constructor_arguments
  type class_type
type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: attributes;
    cty_uid: Uid.t;
 }
end

module type ClassTypeDeclaration = sig
  type attributes
  type class_type
type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: attributes;
    clty_uid: Uid.t;
  }
[@@deriving  yojson]
(* Type expressions for the module language *)
end

type visibility =
  | Exported
  | Hidden
[@@deriving  yojson]

module type ModuleTypeDeclaration = sig
  type attributes
  type value_description
  type type_declaration
  type class_declaration
  type visibility
  type class_type_declaration 

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t
[@@deriving  yojson]
and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type
[@@deriving  yojson]
and module_presence =
  | Mp_present
  | Mp_absent
[@@deriving  yojson]
and signature = signature_item list
[@@deriving  yojson]
and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility
[@@deriving  yojson]
and module_declaration =
  {
    md_type: module_type;
    md_attributes: attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }
[@@deriving  yojson]
and modtype_declaration =
  {
    mtd_type: module_type option;  (* Note: abstract *)
    mtd_attributes: attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }
[@@deriving  yojson]
and rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)
[@@deriving  yojson]
and ext_status =
    Text_first                     (* first constructor of an extension *)
  | Text_next                      (* not first constructor of an extension *)
  | Text_exception                 (* an exception *)
[@@deriving  yojson]
end

(* Constructor and record label descriptions inserted held in typing
   environments *)

module type ConstructorDescription = sig
  type attributes
  type type_declaration
type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }
[@@deriving  yojson]
and  constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
[@@deriving  yojson]                                           true if a constant false if a block*)
end

module type LabelDescription = sig
  type attributes
  type record_representation
type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: attributes;
    lbl_uid: Uid.t;
  }
[@@deriving  yojson]
end

(**** Definitions for backtracking ****)

module type Change = sig
  type 'a row_field_gen
type change =
    Ctype of type_expr * type_desc
  | Ccompress of type_expr * type_desc * type_desc
  | Clevel of type_expr * int
  | Cscope of type_expr * int
  | Cname of
      (Path.t * type_expr list) option ref * (Path.t * type_expr list) option
  | Crow of [`none|`some] row_field_gen ref
  | Ckind of [`var] field_kind_gen
  | Ccommu of [`var] commutable_gen
  | Cuniv of type_expr option ref * type_expr option
                                      [@@deriving  yojson]
type changes =
    Change of change * changes ref
  | Unchanged
  | Invalid
[@@deriving  yojson]
type snapshot = changes ref * int
[@@deriving  yojson]

end


(* constructor and accessors for [field_kind] *)

type field_kind_view =
    Fprivate
  | Fpublic
  | Fabsent
[@@deriving  yojson]

(* Comparison for [type_expr]; cannot be used for functors *)

module type RowField = sig
  type row_field

type row_desc_repr =
    Row of { fields: (label * row_field) list;
             more:type_expr;
             closed:bool;
             fixed:fixed_explanation option;
             name:(Path.t * type_expr list) option }
[@@deriving  yojson]
end

type row_field_view =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent
[@@deriving  yojson]

