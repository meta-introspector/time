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
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

(** Abstract syntax tree produced by parsing

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)



type constant =
  | Pconst_integer of string * char option
      (** Integer constants such as [3] [3l] [3L] [3n].

     Suffixes [[g-z][G-Z]] are accepted by the parser.
     Suffixes except ['l'], ['L'] and ['n'] are rejected by the typechecker
  *)
  | Pconst_char of char  (** Character such as ['c']. *)
  | Pconst_string of string * Location.t * string option
      (** Constant string such as ["constant"] or
          [{delim|other constant|delim}].

     The location span the content of the string, without the delimiters.
  *)
  | Pconst_float of string * char option
      (** Float constant such as [3.4], [2e5] or [1.4e-4].

     Suffixes [g-z][G-Z] are accepted by the parser.
     Suffixes are rejected by the typechecker.
  *)
                               [@@deriving  yojson]

                               (*
split out the remaining types into new files
                                *)

