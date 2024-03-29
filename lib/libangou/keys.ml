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

module Make (Dh_dsa : Mirage_crypto_ec.Dh_dsa) = struct
  type t = { priv : Dh_dsa.Dsa.priv; pub : Dh_dsa.Dsa.pub }

  let generate () =
    let priv, pub = Dh_dsa.Dsa.generate () in
    { priv; pub }

  module Serialized = struct
    type s = { spriv : string; spub : string } [@@deriving yojson]

    let serialize keys =
      let spriv = Cstruct.to_string @@ Dh_dsa.Dsa.priv_to_cstruct keys.priv in
      let spub = Cstruct.to_string @@ Dh_dsa.Dsa.pub_to_cstruct keys.pub in
      { spriv; spub }

    let deserialize serialiazed =
      let ( let* ) = Result.bind in
      let* priv =
        Dh_dsa.Dsa.priv_of_cstruct @@ Cstruct.of_string serialiazed.spriv
      in
      let* pub =
        Dh_dsa.Dsa.pub_of_cstruct @@ Cstruct.of_string serialiazed.spub
      in
      Ok { priv; pub }
  end
end
