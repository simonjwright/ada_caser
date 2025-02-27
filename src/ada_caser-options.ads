--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Libadalang.Common;

private package Ada_Caser.Options is

   type Dictionary_Reporter
   is access procedure (Dictionary_Filename : String);

   procedure Process_Options (Report_Dictionaries_To : Dictionary_Reporter);

   function Character_Set return String;

   function Language return Libadalang.Common.Language_Version;

   function Pipe return Boolean;

   function Project return String;

   function Verbose return Boolean;

   function Report_Diagnostics return Boolean;

end Ada_Caser.Options;
