--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada.Text_IO;
with Ada.Command_Line;
with Ada_Caser.Options;

package body Ada_Caser.Messages is

   use Ada.Text_IO;

   Errors : Natural := 0;

   procedure Info (Message : String) is
   begin
      if Options.Is_Verbose then
         Put_Line (Standard_Error, "Info: " & Message);
      end if;
   end Info;

   procedure Warning (Message : String) is
   begin
      Put_Line (Standard_Error, "Warning: " & Message);
   end Warning;

   procedure Error (Message : String; Quit : Boolean := False) is
   begin
      Errors := Errors + 1;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line (Standard_Error, "Error: " & Message);
      if Quit then
         raise Notified_Error;
      end if;
   end Error;

   function Number_Of_Errors return Natural is (Errors);

end Ada_Caser.Messages;
