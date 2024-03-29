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

module AEAD = Mirage_crypto.Cipher_block.AES.GCM
module Dh_dsa = Mirage_crypto_ec.P256
module Hash = Mirage_crypto.Hash.SHA256
module Commandline = Commandline.Make (AEAD) (Dh_dsa) (Hash)

let () = Random.self_init ()
let seed = Util.Seed.seed ()
let g = Mirage_crypto_rng.create ~seed (module Mirage_crypto_rng.Fortuna)
let () = Mirage_crypto_rng.set_default_generator g
let () = exit @@ Commandline.eval ()
