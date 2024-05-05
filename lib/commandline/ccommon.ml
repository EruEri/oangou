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

open Cmdliner

let exits =
  [
    Cmd.Exit.info ~doc:"on success." Libangou.ExitCode.angou_success;
    Cmd.Exit.info ~doc:"on $(mname) errors." Libangou.ExitCode.angou_error;
    Cmd.Exit.info ~doc:"on internal errors." Libangou.ExitCode.any_other_error;
    Cmd.Exit.info ~doc:"on command line parsing errors."
      Libangou.ExitCode.angou_parsing_options_error;
  ]
