(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of oangou: a content encryptor                                           *)
(* Copyright (C) 2024 Yves Ndiaye                                                             *)
(*                                                                                            *)
(* oangou is free software: you can redistribute it and/or modify it under the terms          *)
(* of the GNU General Public License as published by the Free Software Foundation,            *)
(* either version 3 of the License, or (at your option) any later version.                    *)
(*                                                                                            *)
(* oangou is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;        *)
(* without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR           *)
(* PURPOSE.  See the GNU General Public License for more details.                             *)
(* You should have received a copy of the GNU General Public License along with oangou.       *)
(* If not, see <http://www.gnu.org/licenses/>.                                                *)
(*                                                                                            *)
(**********************************************************************************************)

open Printf

let to_string = function
  | Error.AngouAlreadyConfigured ->
      sprintf "%s is already configured" Config.angou_name
  | AngouNotConfigured ->
      sprintf "%s is not configured" Config.angou_name
  | GetPasswordError ->
      "getpass(3) failed"
  | PasswordNotMatched ->
      "Passwords doesn't match"
  | UnknwonPeer string ->
      sprintf "Unknown peer \"%s\"" string
  | DecryptionOangouError ->
      "Decryption Error, you probabily mistyped your password"
  | DecryptMessageError ->
      "Unable to decrypt the message"
  | MirageCryptoError mirage_error ->
      Format.asprintf "%a" Mirage_crypto_ec.pp_error mirage_error

let string_of_color_oangou_error e =
  Printf.sprintf "%s : %s"
    (Util.Repr.sprintf Util.Repr.fg_red "error")
    (to_string e)

let register_oangou_error () =
  Printexc.register_printer (function
    | Error.AngouError e ->
        Option.some @@ Printf.sprintf "\n%s" @@ string_of_color_oangou_error e
    | _ ->
        None
    )
