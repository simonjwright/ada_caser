--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada_Caser.Dictionaries;
with Ada_Caser.Messages;
with Ada_Caser.Options;

with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Libadalang.Common;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Ada_Caser.Processing is

   procedure Process_Without_Project
     (Unit : Libadalang.Analysis.Analysis_Unit; File : File_Type)
   is
      use Libadalang;

      Token : Common.Token_Reference := Analysis.First_Token (Unit);
      use type Common.Token_Reference;
      use type Common.Token_Kind;
      use type Common.Language_Version;

      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
   begin
      Messages.Info ("processing " & Unit.Get_Filename);
      while Token /= Common.No_Token loop

         if not Options.Report_Diagnostics then
            case Common.Kind (Common.Data (Token)) is
               when Common.Ada_Identifier =>
                  if Analysis.Is_Keyword (Token, Options.Language) then
                     --  Keywords for language versions > 83 are
                     --  parsed as indentifiers.
                     Put
                       (File,
                        Translate (Common.Text (Token), Lower_Case_Map));
                  elsif Options.Language > Common.Ada_83
                    and then Translate (Common.Text (Token), Lower_Case_Map)
                         = "aliased"
                  then
                     --  "aliased" isn't recognised as a keyword
                     --  (libadalang #971).
                     Put
                       (File,
                        Translate (Common.Text (Token), Lower_Case_Map));
                  else
                     Put
                       (File,
                        Dictionaries.Normalize (Common.Text (Token)));
                  end if;
               when Common.Ada_Char |
                 Common.Ada_Comment |
                 Common.Ada_Integer |
                 Common.Ada_String =>
                  Put
                    (File,
                     Common.Text (Token));
               when Common.Ada_Whitespace =>
                  for Ch of Common.Text (Token) loop
                     --  output an LF by calling New_Line, so
                     --  [Wide_Wide_]Text_IO realises that the end of
                     --  the file doesn't need an extra blank line.

                     if Ch = Wide_Wide_Character'Val (16#0000_000a#) then
                        New_Line (File);
                     else
                        Put (File, Ch);
                     end if;
                  end loop;
               when others =>
                  Put
                    (File,
                     Translate (Common.Text (Token), Lower_Case_Map));
            end case;
         else
            Put (Common.Text (Token));
            Put (" is ");
            Put (Common.Kind (Common.Data (Token))'Wide_Wide_Image);
            New_Line;
         end if;

         Token := Common.Next (Token);
      end loop;
      Messages.Info (Unit.Get_Filename & " done.");
   end Process_Without_Project;

   procedure Process_With_Project
     (Unit : Libadalang.Analysis.Analysis_Unit; File : File_Type)
   is
      use Libadalang;
      use Libadalang.Analysis;
      use Libadalang.Common;

      --  Holds the Defining Name of those tokens that have one.
      Xrefs : array (1 .. Token_Count (Unit)) of Analysis.Defining_Name
        := (others => No_Defining_Name);

      function Find_Defining_Names (Node : Analysis.Ada_Node'Class)
         return Common.Visit_Status
      is
      begin
         if Node.Kind in Ada_Identifier
         then

            declare
               --  This is an identifier, so there's better be only one
               --  token
               pragma Assert
                 (Node.As_Single_Tok_Node.Token_Start
                    = Node.As_Single_Tok_Node.Token_End);
               Token : constant Token_Reference :=
                 Node.As_Single_Tok_Node.Token_Start;
               Idx   : constant Natural         := Natural (Index (Token));
               Decl  : Defining_Name renames Xrefs (Idx);
            begin
               Decl := Node.As_Name.P_Referenced_Defining_Name;
               if Options.Report_Diagnostics
                 and then Decl /= No_Defining_Name
               then
                  Put_Line
                    (Common.Text (Token) &
                     " => " &
                     Decl.Text &
                     " at " &
                     Decl.Full_Sloc_Image);
               end if;
            exception
               when Property_Error =>
                  null;
            end;

         end if;
         return Into;
      end Find_Defining_Names;

      procedure Replace_Defined_Names is
         Token : Common.Token_Reference := Analysis.First_Token (Unit);

         use Ada.Strings.Wide_Wide_Fixed;
         use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
      begin
         while Token /= Common.No_Token loop

            case Common.Kind (Common.Data (Token)) is
               when Common.Ada_Identifier =>

                  if Analysis.Is_Keyword (Token, Options.Language)
                  then
                     --  Keywords for language versions > 83 are
                     --  parsed as indentifiers.
                     Put
                       (File,
                        Translate (Common.Text (Token), Lower_Case_Map));
                  elsif Options.Language > Ada_83
                    and then Translate (Common.Text (Token), Lower_Case_Map)
                         = "aliased"
                  then
                     --  "aliased" isn't recognised as a keyword
                     --  (libadalang #971).
                     Put
                       (File,
                        Translate (Common.Text (Token), Lower_Case_Map));
                  elsif Xrefs (Positive (Index (Token)))
                            /= No_Defining_Name
                  then
                     Extract_Wanted_Part :
                     declare
                        Defining_Name : constant Wide_Wide_String :=
                          Xrefs (Positive (Index (Token)))
                            .P_Relative_Name.Text;
                     begin
                        Put (File, Defining_Name);
                     end Extract_Wanted_Part;
                  else
                     --  Do we want to normalize? would then require
                     --  (at least) two passes, because the recorded
                     --  (unnormalized) defining name from the first
                     --  pass would be used for references in that
                     --  pass.
                     Put
                       (File,
                        Dictionaries.Normalize (Common.Text (Token)));
                  end if;
               when Common.Ada_Char |
                 Common.Ada_Comment |
                 Common.Ada_Integer |
                 Common.Ada_String =>
                  Put (File,
                    Common.Text (Token));
               when Common.Ada_Whitespace =>
                  for Ch of Common.Text (Token) loop
                     --  output an LF by calling New_Line, so
                     --  [Wide_Wide_]Text_IO realises that the end of
                     --  the file doesn't need an extra blank line.

                     if Ch = Wide_Wide_Character'Val (16#0000_000a#) then
                        New_Line (File);
                     else
                        Put (File, Ch);
                     end if;
                  end loop;
               when others =>
                  Put (File,
                     Translate (Common.Text (Token), Lower_Case_Map));
            end case;

            Token := Common.Next (Token);
         end loop;
      end Replace_Defined_Names;

   begin
      Messages.Info ("setting up for " & Unit.Get_Filename);

      --  Fill the Xref information
      Unit.Root.Traverse (Find_Defining_Names'Access);

      Messages.Info ("processing " & Unit.Get_Filename);
      Replace_Defined_Names;

      Messages.Info (Unit.Get_Filename & " done.");
   end Process_With_Project;

   procedure Process (Unit : Libadalang.Analysis.Analysis_Unit) is
      Result_File : File_Type;
   begin
      Create (Result_File, Name => "", Mode => Out_File);

      if Options.Project = "" then
         Process_Without_Project (Unit, Result_File);
      else
         Process_With_Project (Unit, Result_File);
      end if;

      Reset (Result_File, Mode => In_File);

      if Options.Pipe then

         begin
            loop
               declare
                  Line : constant Wide_Wide_String := Get_Line (Result_File);
               begin
                  Put_Line (Line);
               end;
            end loop;
         exception
            when others =>
               null;
         end;

      else
         Compare_Files :
         declare
            package Strings is new Ada.Containers.Indefinite_Vectors
              (Element_Type => Wide_Wide_String,
               Index_Type   => Positive);

            Input_File : constant String := Unit.Get_Filename;

            Input_Text  : Strings.Vector;
            Result_Text : Strings.Vector;

            use type Strings.Vector;
         begin

            Read_The_Input :
            declare
               File : File_Type;
            begin
               Open (File, Name => Input_File, Mode => In_File);
               loop
                  Input_Text.Append (Get_Line (File));
               end loop;
            exception
               when End_Error =>
                  Close (File);
            end Read_The_Input;

            Read_The_Result :
            begin
               loop
                  Result_Text.Append (Get_Line (Result_File));
               end loop;
            exception
               when End_Error =>
                  null;
            end Read_The_Result;

            if Result_Text = Input_Text then
               Messages.Info (Input_File & " unchanged");
            else
               Messages.Info (Input_File & " changed");
               declare
                  use Ada.Directories;
                  Backup : constant String := Input_File & "~";
               begin
                  if Exists (Backup) then
                     Delete_File (Backup);
                  end if;
                  Rename (Input_File, Backup);
                  Copy_File (Name (Result_File), Input_File);
               end;
            end if;
         end Compare_Files;

      end if;
   end Process;

end Ada_Caser.Processing;
