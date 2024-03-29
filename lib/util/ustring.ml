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

let spaces n = String.init n (fun _ -> ' ')

let line ~first ~last length s =
  String.init length (function
    | 0 ->
        first
    | n when n - 1 = length ->
        last
    | _ ->
        s
    )

let truncate n s =
  let length = String.length s in
  if length < n then
    s
  else
    let subs = String.sub s 0 n in
    Printf.sprintf "%s..." subs

let of_cstruct ?(hexa = false) cstruct =
  match hexa with
  | true ->
      Cstruct.to_hex_string cstruct
  | false ->
      Cstruct.to_string cstruct

let of_string ?(hexa = false) string =
  match hexa with
  | true ->
      Cstruct.to_string @@ Cstruct.of_hex string
  | false ->
      string
