--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: Copyright (C) 2024 Simon Wright <simon@pushface.org>

private package Ada_Caser.Dictionaries is

   Invalid_Name : exception;

   procedure Add_Dictionary (From_File : String);

   function Normalize (Id : Wide_Wide_String) return Wide_Wide_String;

end Ada_Caser.Dictionaries;
