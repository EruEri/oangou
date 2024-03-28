(**********************************************************************************************)
(*                                                                                            *)
(* This file is part of oangou: a file encryptor                                              *)
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
  | GetPasswordError
  | PasswordNotMatched
  | UnknwonPeer of string
  | MirageCryptoError of Mirage_crypto_ec.error

exception AngouError of angou_error

module Exn = struct
  let angou_error e = AngouError e
  let angou_not_configured () = raise @@ angou_error AngouNotConfigured
  let angou_already_configured () = raise @@ angou_error AngouAlreadyConfigured
  let getpass_error () = raise @@ angou_error GetPasswordError
  let password_not_matched () = raise @@ angou_error PasswordNotMatched
  let unknown_peer name = raise @@ angou_error @@ UnknwonPeer name
  let mirage_crypto_error e = raise @@ angou_error @@ MirageCryptoError e
end
