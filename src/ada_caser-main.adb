--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada_Caser.Dictionaries;
with Ada_Caser.Messages;
with Ada_Caser.Options;
with Ada_Caser.Processing;
with GNAT.Command_Line;
with Ada.Strings.Fixed;
with Libadalang.Analysis;
with Langkit_Support.Diagnostics;

procedure Ada_Caser.Main is
   use Libadalang;
   Context : Analysis.Analysis_Context;
begin

   Options.Process_Options
     (Report_Dictionaries_To => Dictionaries.Add_Dictionary'Access);

   Context := Analysis.Create_Context (Charset => Options.Character_Set);

   Arguments :
   loop
      declare
         Arg : constant String :=
           GNAT.Command_Line.Get_Argument (Do_Expansion => True);
      begin
         exit Arguments when Arg'Length = 0;
         Messages.Info ("file to process: " & Arg);

         Process_File :
         declare
            Unit : constant Analysis.Analysis_Unit :=
              Context.Get_From_File (Arg);
         begin
            if Unit.Has_Diagnostics then
               Process_Diagnostics :
               declare
                  --  Once you get one skipped token, most of the
                  --  tokens in the rest of the unit be skipped. This
                  --  assumes that the first one will be the
                  --  significant one.
                  First_Skipped_Token : Boolean := True;
               begin
                  for D of Unit.Diagnostics loop
                     Process_One_Diagnostic :
                     declare
                        Message : constant String :=
                          Langkit_Support.Diagnostics.To_Pretty_String (D);
                        use Ada.Strings.Fixed;
                        Skipped_Token : constant Boolean :=
                          Index (Message, "Skipped token") in Message'Range;
                     begin
                        if Skipped_Token then
                           --  Only output the first "skipped token" message.
                           if First_Skipped_Token then
                              First_Skipped_Token := False;
                              Messages.Error (Message);
                           end if;
                        else
                           Messages.Error (Message);
                        end if;
                     end Process_One_Diagnostic;
                  end loop;
               end Process_Diagnostics;
               Messages.Error ("quitting because of errors", Quit => True);
            else
               Processing.Process (Unit);
            end if;
         end Process_File;
      end;
   end loop Arguments;

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null; -- This is only raised after -h/--help, so nothing to do
   when others =>
      if Messages.Number_Of_Errors = 0 then
         --  If non-sero, we've already output an Error message and set
         --  exit status to Failure.
         raise;
      end if;
end Ada_Caser.Main;
