--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

private package Ada_Caser.Options is

   type Dictionary_Reporter
   is access procedure (Dictionary_Filename : String);

   procedure Process_Options (Report_Dictionaries_To : Dictionary_Reporter);

   function Character_Set return String;

   function Is_Verbose return Boolean;

end Ada_Caser.Options;
