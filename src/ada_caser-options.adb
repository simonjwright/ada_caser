--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with GNAT.Command_Line;
with Ada_Caser.Messages;

package body Ada_Caser.Options is

   use Libadalang.Common;

   package Dictionary_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   Dicts : Dictionary_Vectors.Vector;

   Charset : Ada.Strings.Unbounded.Unbounded_String
     := Ada.Strings.Unbounded.To_Unbounded_String ("utf-8");
   function Character_Set return String is
     (Ada.Strings.Unbounded.To_String (Charset));

   The_Language : Language_Version := Ada_2022;
   function Language return Language_Version is
     (The_Language);

   Pipe_Output : aliased Boolean := True;
   function Pipe return Boolean is (Pipe_Output);

   The_Project : Ada.Strings.Unbounded.Unbounded_String;
   function Project return String is
     (Ada.Strings.Unbounded.To_String (The_Project));

   Is_Verbose : aliased Boolean := False;
   function Verbose return Boolean is (Is_Verbose);

   Version_Required : aliased Boolean := False;
   function Version return Boolean is (Version_Required);

   Diagnostics : aliased Boolean := False;
   function Report_Diagnostics return Boolean is (Diagnostics);

   procedure Process_Options
     (Report_Dictionaries_To : Dictionary_Reporter)
   is

      Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;

      procedure Charset_Callback (Unused : String; Value : String);

      procedure Language_Version_Callback (Unused : String; Value : String);

      procedure Project_Callback (Unused : String; Value : String);

      procedure Dictionary_Callback (Unused : String; Value : String);

      procedure Charset_Callback (Unused : String; Value : String) is
      begin
         Charset := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      end Charset_Callback;

      procedure Language_Version_Callback (Unused : String; Value : String)
      is
         Result : Language_Version;
      begin
         Result       := Language_Version'Value (Value);
         The_Language := Result;
      exception
         when Constraint_Error =>
            Messages.Error ("invalid language " & Value, Quit => True);
      end Language_Version_Callback;

      procedure Project_Callback (Unused : String; Value : String) is
      begin
         The_Project := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      end Project_Callback;

      procedure Dictionary_Callback (Unused : String; Value : String) is
      begin
         Dicts.Append (Value);
      end Dictionary_Callback;

   begin

      GNAT.Command_Line.Set_Usage
        (Command_Line_Config,
         Usage => "[switches] files",
         Help  => "Adjust casing in the source files");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Output      => Is_Verbose'Access,
         Switch      => "-v",
         Long_Switch => "--verbose",
         Help        => "Report progress");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Callback    => Charset_Callback'Unrestricted_Access,
         Switch      => "-c=",
         Long_Switch => "--charset=",
         Help        => "Specify the character set (default ""utf-8"")");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Callback    => Language_Version_Callback'Unrestricted_Access,
         Switch      => "-l=",
         Long_Switch => "--language=",
         Help        => "Specify the language version (default ""Ada_2022"")");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Output      => Pipe_Output'Access,
         Switch      => "-p",
         Long_Switch => "--pipe",
         Help        => "Output to standard output");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Callback    => Project_Callback'Unrestricted_Access,
         Switch      => "-P=",
         Long_Switch => "--project=",
         Help        => "Specify the project (no default)");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Callback    => Dictionary_Callback'Unrestricted_Access,
         Switch      => "-D=",
         Long_Switch => "--dictionary=",
         Help        => "Add casing dictionary");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Output      => Version_Required'Access,
         Switch      => "-V",
         Long_Switch => "--version",
         Help        => "Report ada_caser's version");

      --  This is for diagnostic use only
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Output      => Diagnostics'Access,
         Long_Switch => "--diagnostics",
         Help        => "Report diagnostics in source file");

      GNAT.Command_Line.Getopt (Command_Line_Config);

      for Dict of Dicts loop
         Report_Dictionaries_To (Dict);
      end loop;

   end Process_Options;

end Ada_Caser.Options;
