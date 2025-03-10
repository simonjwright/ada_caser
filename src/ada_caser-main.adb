--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada_Caser.Dictionaries;
with Ada_Caser.Messages;
with Ada_Caser.Options;
with Ada_Caser.Processing;
with Ada_Caser.Utilities;
with Ada_Caser.Version;
with GNAT.Command_Line;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Libadalang.Analysis;
with Libadalang.Project_Provider;
with Langkit_Support.Diagnostics;

--  Supports symbolic traceback on macOS.
with Ada_Caser.Exception_Reporting;
pragma Unreferenced (Ada_Caser.Exception_Reporting);

procedure Ada_Caser.Main is
   use Libadalang;
   Context : Analysis.Analysis_Context;
begin

   Options.Process_Options
     (Report_Dictionaries_To => Dictionaries.Add_Dictionary'Access);

   if Options.Version then
      Ada.Text_IO.Put_Line (Ada_Caser.Version);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
      return;
   end if;

   Set_Up_Provider :
   declare
      Project_File : constant String :=
        (if Options.Project /= ""
         then Options.Project
         else Utilities.Default_Project);

      Env      : GNATCOLL.Projects.Project_Environment_Access;
      Project  : constant GNATCOLL.Projects.Project_Tree_Access
        := new GNATCOLL.Projects.Project_Tree;
      Provider : Analysis.Unit_Provider_Reference;

      function To_Virtual_File (Name : String)
         return GNATCOLL.VFS.Virtual_File;
      function To_Virtual_File (Name : String)
         return GNATCOLL.VFS.Virtual_File
      is
         use GNATCOLL.VFS;
      begin
         return Create_From_Base (Filesystem_String (Name));
      end To_Virtual_File;
   begin
      Messages.Info ("using project file: " & Project_File);

      GNATCOLL.Projects.Initialize (Env);

      begin
         Project.Load (To_Virtual_File (Project_File), Env);
      exception
         when GNATCOLL.Projects.Invalid_Project =>
            Messages.Error ("invalid project " & Project_File, Quit => True);
      end;

      Provider := Project_Provider.Create_Project_Unit_Provider
          (Tree => Project, Env => Env);

      Context := Analysis.Create_Context
          (Unit_Provider => Provider, Charset => Options.Character_Set);
   end Set_Up_Provider;

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
            if not Unit.Has_Diagnostics then
               Processing.Process (Unit);
            else
               Messages.Error ("in " & Unit.Get_Filename);
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
            end if;
         end Process_File;
      end;
   end loop Arguments;

exception
   when GNAT.Command_Line.Exit_From_Command_Line
     | GNAT.Command_Line.Invalid_Switch =>
      --  These are only raised after information already output,
      --  so nothing to do
      null;

   when GNAT.Command_Line.Invalid_Parameter =>
      Ada.Text_IO.Put_Line ("invalid command line switch parameter");

   when Notified_Error =>
      if Messages.Number_Of_Errors = 0 then
         --  If non-sero, we've already output an Error message and set
         --  exit status to Failure.
         raise;
      end if;
end Ada_Caser.Main;
