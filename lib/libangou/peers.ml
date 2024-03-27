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

module Make
    (AEAD : Mirage_crypto.AEAD)
    (Dh_dsa : Mirage_crypto_ec.Dh_dsa)
    (Hash : Mirage_crypto.Hash.S) =
struct
  module PeersMap = Map.Make (String)
  module Peer = Peer.Make (Dh_dsa)
  module Keys = Keys.Make (Dh_dsa)
  module Crypto = Crypto.Make (AEAD) (Hash)

  type info = { public_keys : Peer.pub; shared_secret : string }
  type t = { keys : Keys.t; peers : info PeersMap.t }

  module Serialized = struct
    type s = {
      skeys : Keys.Serialized.s; [@key "keys"]
      speers : Peer.Serialized.s list; [@key "peers"]
    }
    [@@deriving yojson]

    let serialiaze peers =
      let skeys = Keys.Serialized.serialize peers.keys in
      let speers =
        peers.peers |> PeersMap.to_seq
        |> Seq.map (fun (name, info) ->
               let spub =
                 Cstruct.to_string @@ Dh_dsa.Dsa.pub_to_cstruct info.public_keys
               in
               let shared_secret = info.shared_secret in
               Peer.Serialized.{ sname = name; spub; shared_secret }
           )
        |> List.of_seq
      in
      { skeys; speers }

    let to_string_serialiaze t =
      Yojson.Safe.to_string @@ s_to_yojson @@ serialiaze t

    let deserialize serialiazed =
      let ( let* ) = Result.bind in
      let* keys = Keys.Serialized.deserialize serialiazed.skeys in
      let peers, _error =
        List.fold_left
          (fun (peers, errors) (peer : Peer.Serialized.s) ->
            let name = peer.sname in
            let shared_secret = peer.shared_secret in
            match Dh_dsa.Dsa.pub_of_cstruct @@ Cstruct.of_string peer.spub with
            | Ok public_keys ->
                let info = { public_keys; shared_secret } in
                let peers = PeersMap.add name info peers in
                (peers, errors)
            | Error e ->
                let errors = Some e in
                (peers, errors)
          )
          (PeersMap.empty, None) serialiazed.speers
      in
      Ok { keys; peers }

    let of_string string_serialiazed =
      let yojson = Yojson.Safe.from_string string_serialiazed in
      match s_of_yojson yojson with
      | Error e ->
          failwith e
      | Ok d ->
          deserialize d
  end

  let create () =
    let keys = Keys.generate () in
    let peers = PeersMap.empty in
    { keys; peers }

  let save ?(where = Config.angou_keys_file) ~key t =
    let data = Serialized.to_string_serialiaze t in
    let data = Crypto.encrypt ~key data in
    let encrypted_data = Cstruct.to_string data in
    Out_channel.with_open_bin where (fun oc ->
        Out_channel.output_string oc encrypted_data
    )

  let load ?(from = Config.angou_keys_file) ~key () =
    let content = Util.Io.readall_filename from in
    let data =
      match Crypto.decrypt ~key content with
      | None ->
          failwith "Is None todo"
      | Some data ->
          Cstruct.to_string data
    in
    let peers =
      match Serialized.of_string data with
      | Ok d ->
          d
      | Error e ->
          failwith @@ Format.asprintf "%a" Mirage_crypto_ec.pp_error e
    in
    peers

  let shared_secret peer peers =
    peers.peers |> PeersMap.find_opt peer
    |> Option.map (fun info -> info.shared_secret)
end
