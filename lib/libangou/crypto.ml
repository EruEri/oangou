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

module Make (AEAD : Mirage_crypto.AEAD) (Hash : Mirage_crypto.Hash.S) = struct
  let uint_8_max = 256
  let iv_size = 12

  let nonce =
    Cstruct.of_string
    @@ String.init AEAD.tag_size (fun i -> Char.chr (i mod 256))

  let encrypt ~key ?(nonce = nonce) data =
    let crypted_key = Hash.digest @@ Cstruct.of_string key in
    let key = AEAD.of_secret crypted_key in
    let data = Cstruct.of_string data in
    AEAD.authenticate_encrypt ~key ~nonce data

  let decrypt ~key ?(nonce = nonce) data =
    let crypted_key = Hash.digest @@ Cstruct.of_string key in
    let key = AEAD.of_secret crypted_key in
    let data = Cstruct.of_string data in
    AEAD.authenticate_decrypt ~key ~nonce data
end
