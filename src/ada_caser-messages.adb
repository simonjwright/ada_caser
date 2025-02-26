--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;
with Ada.Command_Line;
with Ada_Caser.Options;
with Ada.Calendar;

package body Ada_Caser.Messages is

   Errors : Natural := 0;

   Start_Time : constant Ada.Calendar.Time := Ada.Calendar.Clock;

   procedure Info (Message : String) is
      use Ada.Text_IO;
      package Duration_IO is new Fixed_IO (Duration);
      use type Ada.Calendar.Time;
   begin
      if Options.Verbose then
         Put (Standard_Error, "Info: ");
         Duration_IO.Put (Standard_Error,
                          Ada.Calendar.Clock - Start_Time,
                          Fore => 2,
                          Aft  => 3,
                          Exp  => 0);
         Put (Standard_Error, ": ");
         Put (Standard_Error, Message);
         New_Line;
      end if;
   end Info;

   procedure Info (Message : Wide_Wide_String) is
      use Ada.Wide_Wide_Text_IO;
      package Duration_IO is new Fixed_IO (Duration);
      use type Ada.Calendar.Time;
   begin
      if Options.Verbose then
         Put (Standard_Error, "Info: ");
         Duration_IO.Put (Standard_Error,
                          Ada.Calendar.Clock - Start_Time,
                          Fore => 2,
                          Aft  => 3,
                          Exp  => 0);
         Put (Standard_Error, ": ");
         Put (Standard_Error, Message);
         New_Line;
      end if;
   end Info;

   procedure Warning (Message : String) is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "Warning: " & Message);
   end Warning;

   procedure Warning (Message : Wide_Wide_String) is
      use Ada.Wide_Wide_Text_IO;
   begin
      Put_Line (Standard_Error, "Warning: " & Message);
   end Warning;

   procedure Error (Message : String; Quit : Boolean := False) is
      use Ada.Text_IO;
   begin
      Errors := Errors + 1;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line (Standard_Error, "Error: " & Message);
      if Quit then
         raise Notified_Error;
      end if;
   end Error;

   procedure Error (Message : Wide_Wide_String; Quit : Boolean := False) is
      use Ada.Wide_Wide_Text_IO;
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
