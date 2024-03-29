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

let read_file ch = really_input_string ch (in_channel_length ch)
let readall_filename string = In_channel.with_open_bin string read_file

(**
  [read_content ?file ()] read the content of [file] if present otherwise read from [stdin]
*)
let read_content ?file () =
  match file with
  | None ->
      In_channel.input_all stdin
  | Some file ->
      readall_filename file

let write_content ?file content =
  match file with
  | None ->
      Out_channel.output_string stdout content
  | Some file ->
      Out_channel.with_open_bin file (fun oc ->
          Out_channel.output_string oc content
      )
