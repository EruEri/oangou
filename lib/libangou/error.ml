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

type angou_error =
  | AngouAlreadyConfigured
  | AngouNotConfigured
  | CreateFileError of string
  | GetPasswordError
  | PasswordNotMatched
  | UnknwonPeer of string
  | DecryptionOangouError
  | DecryptMessageError
  | MirageCryptoError of Mirage_crypto_ec.error

exception AngouError of angou_error

module Exn = struct
  let angou_error e = AngouError e
  let angou_error_raise e = raise @@ angou_error e
  let create_file_error path = angou_error_raise @@ CreateFileError path
  let decryption_oangou () = angou_error_raise DecryptionOangouError
  let decrypt_message () = angou_error_raise DecryptMessageError
  let angou_not_configured () = angou_error_raise AngouNotConfigured
  let angou_already_configured () = angou_error_raise AngouAlreadyConfigured
  let getpass_error () = angou_error_raise GetPasswordError
  let password_not_matched () = angou_error_raise PasswordNotMatched
  let unknown_peer name = angou_error_raise @@ UnknwonPeer name
  let mirage_crypto_error e = angou_error_raise @@ MirageCryptoError e
end
