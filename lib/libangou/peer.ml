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

module Make (Dh_dsa : Mirage_crypto_ec.Dh_dsa) = struct
  type pub = Dh_dsa.Dsa.pub
  type t = { name : string; pub : Dh_dsa.Dsa.pub; shared_secret : string }

  module Serialized = struct
    type s = { sname : string; spub : string; shared_secret : string }
    [@@deriving yojson]

    let serialize peer =
      let sname = peer.name in
      let spub = Cstruct.to_string @@ Dh_dsa.Dsa.pub_to_cstruct peer.pub in
      let shared_secret = peer.shared_secret in
      { sname; spub; shared_secret }

    let deserialize serialiazed =
      let ( let* ) = Result.bind in
      let name = serialiazed.sname in
      let* pub =
        Dh_dsa.Dsa.pub_of_cstruct @@ Cstruct.of_string serialiazed.spub
      in
      let shared_secret = serialiazed.shared_secret in
      Ok { name; pub; shared_secret }
  end
end
