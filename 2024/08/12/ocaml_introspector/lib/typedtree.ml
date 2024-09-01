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

(* Abstract syntax tree after typing *)

open Asttypes
open Types

(* Value expressions for the core language *)

type partial = Partial | Total
                           [@@deriving  yojson]

(* type attribute = attribute *)
(*                    [@@deriving  yojson] *)
module type TypeTree = sig
  type arg_label
  type attribute
  type constructor_description
  type label_description
  type module_presence
  type mutable_flag
  type override_flag
  type partial
  type private_flag
  type record_representation
  type row_desc
  type type_expr
  type virtual_flag
  type attributes = attribute list
                      [@@deriving  yojson]
  type value = Value_pattern
                 [@@deriving  yojson]
type computation = Computation_pattern
[@@deriving  yojson]

type _ pattern_category =
| Value : value pattern_category
| Computation : computation pattern_category
                  [@@deriving  yojson]

type pattern = value general_pattern
                 [@@deriving  yojson]

and 'k general_pattern = 'k pattern_desc pattern_data
[@@deriving  yojson]

and 'a pattern_data =
  { pat_desc: 'a;
    pat_loc: Location.t;
    pat_extra : (pat_extra * Location.t * attribute list) list;
    pat_type: type_expr;
    pat_env: Env.t;
    pat_attributes: attribute list;
   }
[@@deriving  yojson]
and pat_extra =
  | Tpat_constraint of core_type
  | Tpat_type of Path.t * Longident.t loc
  | Tpat_open of Path.t * Longident.t loc * Env.t
  | Tpat_unpack
[@@deriving  yojson]
and 'k pattern_desc =
  (* value patterns *)
  | Tpat_any : value pattern_desc
  | Tpat_var : Ident.t * string loc -> value pattern_desc
  | Tpat_alias :
      value general_pattern * Ident.t * string loc -> value pattern_desc
  | Tpat_constant : constant -> value pattern_desc
  | Tpat_tuple : value general_pattern list -> value pattern_desc
  | Tpat_construct :
      Longident.t loc * constructor_description * value general_pattern list
      * (Ident.t loc list * core_type) option ->
      value pattern_desc
  | Tpat_variant :
      label * value general_pattern option * row_desc ref ->
      value pattern_desc
  | Tpat_record :
      (Longident.t loc * label_description * value general_pattern) list *
        closed_flag ->
      value pattern_desc
  | Tpat_array : value general_pattern list -> value pattern_desc
  | Tpat_lazy : value general_pattern -> value pattern_desc
  (* computation patterns *)
  | Tpat_value : tpat_value_argument -> computation pattern_desc
  | Tpat_exception : value general_pattern -> computation pattern_desc
  (* generic constructions *)
  | Tpat_or :
      'k general_pattern * 'k general_pattern * row_desc option ->
      'k pattern_desc
[@@deriving  yojson]
and tpat_value_argument = value general_pattern
[@@deriving  yojson]
and expression =
  { exp_desc: expression_desc;
    exp_loc: Location.t;
    exp_extra: (exp_extra * Location.t * attribute list) list;
    exp_type: type_expr;
    exp_env: Env.t;
    exp_attributes: attribute list;
   }
[@@deriving  yojson]
and exp_extra =
  | Texp_constraint of core_type
  | Texp_coerce of core_type option * core_type
  | Texp_poly of core_type option
  | Texp_newtype of string
[@@deriving  yojson]
and expression_desc =
    Texp_ident of Path.t * Longident.t loc * value_description
  | Texp_constant of constant
  | Texp_let of rec_flag * value_binding list * expression
  | Texp_function of { arg_label : arg_label; param : Ident.t;
      cases : value case list; partial : partial; }
  | Texp_apply of expression * (arg_label * expression option) list
  | Texp_match of expression * computation case list * partial
  | Texp_try of expression * value case list
  | Texp_tuple of expression list
  | Texp_construct of
      Longident.t loc * constructor_description * expression list
  | Texp_variant of label * expression option
  | Texp_record of {
      fields : ( label_description * record_label_definition ) array;
      representation : record_representation;
      extended_expression : expression option;
    }
  | Texp_field of expression * Longident.t loc * label_description
  | Texp_setfield of
      expression * Longident.t loc * label_description * expression
  | Texp_array of expression list
  | Texp_ifthenelse of expression * expression * expression option
  | Texp_sequence of expression * expression
  | Texp_while of expression * expression
  | Texp_for of
      Ident.t * pattern * expression * expression * direction_flag *
        expression
  | Texp_send of expression * meth
  | Texp_new of Path.t * Longident.t loc * class_declaration
  | Texp_instvar of Path.t * Path.t * string loc
  | Texp_setinstvar of Path.t * Path.t * string loc * expression
  | Texp_override of Path.t * (Ident.t * string loc * expression) list
  | Texp_letmodule of
      Ident.t option * string option loc * module_presence * module_expr *
        expression
  | Texp_letexception of extension_constructor * expression
  | Texp_assert of expression
  | Texp_lazy of expression
  | Texp_object of class_structure * string list
  | Texp_pack of module_expr
  | Texp_letop of {
      let_ : binding_op;
      ands : binding_op list;
      param : Ident.t;
      body : value case;
      partial : partial;
    }
  | Texp_unreachable
  | Texp_extension_constructor of Longident.t loc * Path.t
  | Texp_open of open_declaration * expression
[@@deriving  yojson]
and meth =
  | Tmeth_name of string
  | Tmeth_val of Ident.t
  | Tmeth_ancestor of Ident.t * Path.t
[@@deriving  yojson]
and 'k case =
    {
     c_lhs: 'k general_pattern;
     c_guard: expression option;
     c_rhs: expression;
    }
[@@deriving  yojson]
and record_label_definition =
  | Kept of type_expr
  | Overridden of Longident.t loc * expression
[@@deriving  yojson]
and binding_op =
  {
    bop_op_path : Path.t;
    bop_op_name : string loc;
    bop_op_val : value_description;
    bop_op_type : type_expr;
    bop_exp : expression;
    bop_loc : Location.t;
  }
[@@deriving  yojson]
(* Value expressions for the class language *)

and class_expr =
    {
     cl_desc: class_expr_desc;
     cl_loc: Location.t;
     cl_type: class_type;
     cl_env: Env.t;
     cl_attributes: attribute list;
    }
[@@deriving  yojson]
and class_expr_desc =
    Tcl_ident of Path.t * Longident.t loc * core_type list
  | Tcl_structure of class_structure
  | Tcl_fun of
      arg_label * pattern * (Ident.t * expression) list
      * class_expr * partial
  | Tcl_apply of class_expr * (arg_label * expression option) list
  | Tcl_let of rec_flag * value_binding list *
                  (Ident.t * expression) list * class_expr
  | Tcl_constraint of
      class_expr * class_type option * string list * string list * MethSet.t
    (* Visible instance variables, methods and concrete methods *)
  | Tcl_open of open_description * class_expr
[@@deriving  yojson]
and class_structure =
  {
   cstr_self: pattern;
   cstr_fields: class_field list;
   cstr_type: class_signature;
   cstr_meths: Ident.t Meths.t;
  }
[@@deriving  yojson]
and class_field =
   {
    cf_desc: class_field_desc;
    cf_loc: Location.t;
    cf_attributes: attribute list;
  }
[@@deriving  yojson]
and class_field_kind =
  | Tcfk_virtual of core_type
  | Tcfk_concrete of override_flag * expression
[@@deriving  yojson]
and class_field_desc =
    Tcf_inherit of
      override_flag * class_expr * string option * (string * Ident.t) list *
        (string * Ident.t) list
    (* Inherited instance variables and concrete methods *)
  | Tcf_val of string loc * mutable_flag * Ident.t * class_field_kind * bool
  | Tcf_method of string loc * private_flag * class_field_kind
  | Tcf_constraint of core_type * core_type
  | Tcf_initializer of expression
  | Tcf_attribute of attribute

(* Value expressions for the module language *)
[@@deriving  yojson]
and module_expr =
  { mod_desc: module_expr_desc;
    mod_loc: Location.t;
    mod_type: module_type;
    mod_env: Env.t;
    mod_attributes: attribute list;
   }
[@@deriving  yojson]
and module_type_constraint =
  Tmodtype_implicit
| Tmodtype_explicit of module_type
[@@deriving  yojson]
and functor_parameter =
  | Unit
  | Named of Ident.t option * string option loc * module_type
[@@deriving  yojson]
and module_expr_desc =
    Tmod_ident of Path.t * Longident.t loc
  | Tmod_structure of structure
  | Tmod_functor of functor_parameter * module_expr
  | Tmod_apply of module_expr * module_expr * module_coercion
  | Tmod_constraint of
      module_expr * module_type * module_type_constraint * module_coercion
  | Tmod_unpack of expression * module_type
[@@deriving  yojson]
and structure = {
  str_items : structure_item list;
  str_type : signature;
  str_final_env : Env.t;
}
[@@deriving  yojson]
and structure_item =
  { str_desc : structure_item_desc;
    str_loc : Location.t;
    str_env : Env.t
  }
[@@deriving  yojson]
and structure_item_desc =
    Tstr_eval of expression * attributes
  | Tstr_value of rec_flag * value_binding list
  | Tstr_primitive of value_description
  | Tstr_type of rec_flag * type_declaration list
  | Tstr_typext of type_extension
  | Tstr_exception of type_exception
  | Tstr_module of module_binding
  | Tstr_recmodule of module_binding list
  | Tstr_modtype of module_type_declaration
  | Tstr_open of open_declaration
  | Tstr_class of (class_declaration * string list) list
  | Tstr_class_type of (Ident.t * string loc * class_type_declaration) list
  | Tstr_include of include_declaration
  | Tstr_attribute of attribute
[@@deriving  yojson]
and module_binding =
    {
     mb_id: Ident.t option;
     mb_name: string option loc;
     mb_presence: module_presence;
     mb_expr: module_expr;
     mb_attributes: attribute list;
     mb_loc: Location.t;
    }
[@@deriving  yojson]
and value_binding =
  {
    vb_pat: pattern;
    vb_expr: expression;
    vb_attributes: attributes;
    vb_loc: Location.t;
  }
[@@deriving  yojson]
and module_coercion =
    Tcoerce_none
  | Tcoerce_structure of (int * module_coercion) list *
                         (Ident.t * int * module_coercion) list
  | Tcoerce_functor of module_coercion * module_coercion
  | Tcoerce_primitive of primitive_coercion
  | Tcoerce_alias of Env.t * Path.t * module_coercion
                                        [@@deriving  yojson]

and module_type =
  { mty_desc: module_type_desc;
    mty_type : module_type;
    mty_env : Env.t;
    mty_loc: Location.t;
    mty_attributes: attribute list;
   }
    [@@deriving  yojson]

and module_type_desc =
    Tmty_ident of Path.t * Longident.t loc
  | Tmty_signature of signature
  | Tmty_functor of functor_parameter * module_type
  | Tmty_with of module_type * (Path.t * Longident.t loc * with_constraint) list
  | Tmty_typeof of module_expr
  | Tmty_alias of Path.t * Longident.t loc
[@@deriving  yojson]
(* Keep primitive type information for type-based lambda-code specialization *)
and primitive_coercion =
  {
    pc_desc: Primitive.description;
    pc_type: type_expr;
    pc_env: Env.t;
    pc_loc : Location.t;
  }
[@@deriving  yojson]
and signature = {
  sig_items : signature_item list;
  sig_type : signature;
  sig_final_env : Env.t;
}
[@@deriving  yojson]
and signature_item =
  { sig_desc: signature_item_desc;
    sig_env : Env.t; (* BINANNOT ADDED *)
    sig_loc: Location.t }
[@@deriving  yojson]
and signature_item_desc =
    Tsig_value of value_description
  | Tsig_type of rec_flag * type_declaration list
  | Tsig_typesubst of type_declaration list
  | Tsig_typext of type_extension
  | Tsig_exception of type_exception
  | Tsig_module of module_declaration
  | Tsig_modsubst of module_substitution
  | Tsig_recmodule of module_declaration list
  | Tsig_modtype of module_type_declaration
  | Tsig_modtypesubst of module_type_declaration
  | Tsig_open of open_description
  | Tsig_include of include_description
  | Tsig_class of class_description list
  | Tsig_class_type of class_type_declaration list
  | Tsig_attribute of attribute
[@@deriving  yojson]
and module_declaration =
    {
     md_id: Ident.t option;
     md_name: string option loc;
     md_presence: module_presence;
     md_type: module_type;
     md_attributes: attribute list;
     md_loc: Location.t;
    }
[@@deriving  yojson]
and module_substitution =
    {
     ms_id: Ident.t;
     ms_name: string loc;
     ms_manifest: Path.t;
     ms_txt: Longident.t loc;
     ms_attributes: attributes;
     ms_loc: Location.t;
    }
[@@deriving  yojson]
and module_type_declaration =
    {
     mtd_id: Ident.t;
     mtd_name: string loc;
     mtd_type: module_type option;
     mtd_attributes: attribute list;
     mtd_loc: Location.t;
    }
[@@deriving  yojson]
and 'a open_infos =
    {
     open_expr: 'a;
     open_bound_items: signature;
     open_override: override_flag;
     open_env: Env.t;
     open_loc: Location.t;
     open_attributes: attribute list;
    }
[@@deriving  yojson]
and open_description = (Path.t * Longident.t loc) open_infos
[@@deriving  yojson]
and open_declaration = module_expr open_infos
[@@deriving  yojson]
and 'a include_infos =
    {
     incl_mod: 'a;
     incl_type: signature;
     incl_loc: Location.t;
     incl_attributes: attribute list;
    }
[@@deriving  yojson]
and include_description = module_type include_infos
[@@deriving  yojson]
and include_declaration = module_expr include_infos
[@@deriving  yojson]
and with_constraint =
    Twith_type of type_declaration
  | Twith_module of Path.t * Longident.t loc
  | Twith_modtype of module_type
  | Twith_typesubst of type_declaration
  | Twith_modsubst of Path.t * Longident.t loc
  | Twith_modtypesubst of module_type
[@@deriving  yojson]

and core_type =
(* mutable because of [Typeclass.declare_method] *)
  { mutable ctyp_desc : core_type_desc;
    mutable ctyp_type : type_expr;
    ctyp_env : Env.t; (* BINANNOT ADDED *)
    ctyp_loc : Location.t;
    ctyp_attributes: attribute list;
   }
[@@deriving  yojson]
and core_type_desc =
    Ttyp_any
  | Ttyp_var of string
  | Ttyp_arrow of arg_label * core_type * core_type
  | Ttyp_tuple of core_type list
  | Ttyp_constr of Path.t * Longident.t loc * core_type list
  | Ttyp_object of object_field list * closed_flag
  | Ttyp_class of Path.t * Longident.t loc * core_type list
  | Ttyp_alias of core_type * string
  | Ttyp_variant of row_field list * closed_flag * label list option
  | Ttyp_poly of string list * core_type
  | Ttyp_package of package_type
[@@deriving  yojson]
and package_type = {
  pack_path : Path.t;
  pack_fields : (Longident.t loc * core_type) list;
  pack_type : module_type;
  pack_txt : Longident.t loc;
}
[@@deriving  yojson]
and row_field = {
  rf_desc : row_field_desc;
  rf_loc : Location.t;
  rf_attributes : attributes;
}
[@@deriving  yojson]
and row_field_desc =
    Ttag of string loc * bool * core_type list
  | Tinherit of core_type
[@@deriving  yojson]
and object_field = {
  of_desc : object_field_desc;
  of_loc : Location.t;
  of_attributes : attributes;
}
[@@deriving  yojson]
and object_field_desc =
  | OTtag of string loc * core_type
  | OTinherit of core_type
[@@deriving  yojson]
and value_description =
  { val_id: Ident.t;
    val_name: string loc;
    val_desc: core_type;
    val_val: value_description;
    val_prim: string list;
    val_loc: Location.t;
    val_attributes: attribute list;
    }
[@@deriving  yojson]
and type_declaration =
  { typ_id: Ident.t;
    typ_name: string loc;
    typ_params: (core_type * (variance * injectivity)) list;
    typ_type: type_declaration;
    typ_cstrs: (core_type * core_type * Location.t) list;
    typ_kind: type_kind;
    typ_private: private_flag;
    typ_manifest: core_type option;
    typ_loc: Location.t;
    typ_attributes: attribute list;
   }
[@@deriving  yojson]
and type_kind =
    Ttype_abstract
  | Ttype_variant of constructor_declaration list
  | Ttype_record of label_declaration list
  | Ttype_open
[@@deriving  yojson]
and label_declaration =
    {
     ld_id: Ident.t;
     ld_name: string loc;
     ld_mutable: mutable_flag;
     ld_type: core_type;
     ld_loc: Location.t;
     ld_attributes: attribute list;
    }
[@@deriving  yojson]
and constructor_declaration =
    {
     cd_id: Ident.t;
     cd_name: string loc;
     cd_vars: string loc list;
     cd_args: constructor_arguments;
     cd_res: core_type option;
     cd_loc: Location.t;
     cd_attributes: attribute list;
    }
[@@deriving  yojson]
and constructor_arguments =
  | Cstr_tuple of core_type list
  | Cstr_record of label_declaration list
[@@deriving  yojson]
and type_extension =
  {
    tyext_path: Path.t;
    tyext_txt: Longident.t loc;
    tyext_params: (core_type * (variance * injectivity)) list;
    tyext_constructors: extension_constructor list;
    tyext_private: private_flag;
    tyext_loc: Location.t;
    tyext_attributes: attribute list;
  }
[@@deriving  yojson]
and type_exception =
  {
    tyexn_constructor: extension_constructor;
    tyexn_loc: Location.t;
    tyexn_attributes: attribute list;
  }
[@@deriving  yojson]
and extension_constructor =
  {
    ext_id: Ident.t;
    ext_name: string loc;
    ext_type: extension_constructor;
    ext_kind: extension_constructor_kind;
    ext_loc: Location.t;
    ext_attributes: attribute list;
  }
[@@deriving  yojson]
and extension_constructor_kind =
    Text_decl of string loc list * constructor_arguments * core_type option
  | Text_rebind of Path.t * Longident.t loc
[@@deriving  yojson]
and class_type =
    {
     cltyp_desc: class_type_desc;
     cltyp_type: class_type;
     cltyp_env: Env.t;
     cltyp_loc: Location.t;
     cltyp_attributes: attribute list;
    }
[@@deriving  yojson]
and class_type_desc =
    Tcty_constr of Path.t * Longident.t loc * core_type list
  | Tcty_signature of class_signature
  | Tcty_arrow of arg_label * core_type * class_type
  | Tcty_open of open_description * class_type
[@@deriving  yojson]
and class_signature = {
    csig_self: core_type;
    csig_fields: class_type_field list;
    csig_type: class_signature;
  }
[@@deriving  yojson]
and class_type_field = {
    ctf_desc: class_type_field_desc;
    ctf_loc: Location.t;
    ctf_attributes: attribute list;
  }
[@@deriving  yojson]
and class_type_field_desc =
  | Tctf_inherit of class_type
  | Tctf_val of (string * mutable_flag * virtual_flag * core_type)
  | Tctf_method of (string * private_flag * virtual_flag * core_type)
  | Tctf_constraint of (core_type * core_type)
  | Tctf_attribute of attribute
[@@deriving  yojson]
and class_declaration =
  class_expr class_infos
[@@deriving  yojson]
and class_description =
  class_type class_infos
[@@deriving  yojson]
and class_type_declaration =
  class_type class_infos
[@@deriving  yojson]
and 'a class_infos =
  { ci_virt: virtual_flag;
    ci_params: (core_type * (variance * injectivity)) list;
    ci_id_name: string loc;
    ci_id_class: Ident.t;
    ci_id_class_type: Ident.t;
    ci_id_object: Ident.t;
    ci_id_typehash: Ident.t;
    ci_expr: 'a;
    ci_decl: class_declaration;
    ci_type_decl: class_type_declaration;
    ci_loc: Location.t;
    ci_attributes: attribute list;
   }

type implementation = {
  structure: structure;
  coercion: module_coercion;
  signature: signature;
  shape: Shape.t;
}


(* Auxiliary functions over the a.s.t. *)

type pattern_action =
  { f : 'k . 'k general_pattern -> unit }
[@@deriving  yojson]

type pattern_transformation =
  { f : 'k . 'k general_pattern -> 'k general_pattern }
[@@deriving  yojson]

end
