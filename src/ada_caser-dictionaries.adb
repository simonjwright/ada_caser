--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Characters.Wide_Wide_Latin_9;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Text_IO;
with Ada_Caser.Messages;
with Ada_Caser.Utilities;

package body Ada_Caser.Dictionaries is

   function Casing_Equals (L, R : Wide_Wide_String) return Boolean;
   function Casing_Less_Than (L, R : Wide_Wide_String) return Boolean;

   package Lowercase_Wide_Wide_String_Maps is new
   Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => Wide_Wide_String,
      Element_Type => Wide_Wide_String,
      "<"          => Casing_Less_Than,
      "="          => Casing_Equals);

   --  For Case_Exceptions, the whole identifier has to match
   Case_Exceptions     : Lowercase_Wide_Wide_String_Maps.Map;
   --  For Sub_Case_Exceptions, each 'word' in the identifier matches
   --  separately.
   Sub_Case_Exceptions : Lowercase_Wide_Wide_String_Maps.Map;

   function Casing_Equals (L, R : Wide_Wide_String) return Boolean is
      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
   begin
      return Translate (L, Lower_Case_Map) = Translate (R, Lower_Case_Map);
   end Casing_Equals;

   function Casing_Less_Than (L, R : Wide_Wide_String) return Boolean is
      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
   begin
      return Translate (L, Lower_Case_Map) < Translate (R, Lower_Case_Map);
   end Casing_Less_Than;

   procedure Add_Exception (From_Line : Wide_Wide_String);

   procedure Add_Dictionary (From_File : String) is
      Exceptions : Ada.Wide_Wide_Text_IO.File_Type;
      use Ada.Wide_Wide_Text_IO;
   begin
      Messages.Info ("Reading case exceptions from '" & From_File & "'");

      Open (Exceptions, Mode => In_File, Name => From_File);

      while not End_Of_File (Exceptions) loop
         Add_Exception (Get_Line (Exceptions));
      end loop;

      Close (Exceptions);

   exception
      when Mode_Error | Name_Error =>
         Messages.Error
           ("Unable to open case exceptions file '" & From_File & "'");
         raise Notified_Error;
   end Add_Dictionary;

   procedure Add_Exception (From_Line : Wide_Wide_String) is
      use Ada.Strings;
      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps;

      --  We only want the first word on the line.
      The_Line : constant Wide_Wide_String := Trim (From_Line, Both);
      --  Find_Spans splits on [Wide_Wide_]Character, so translate any
      --  tabs to spaces before splitting.
      Spans    : constant Utilities.Spans  :=
        Utilities.Find_Spans
          (Translate
             (The_Line,
              To_Mapping ((1 => Ada.Characters.Wide_Wide_Latin_9.HT), " ")),
           Splitting_At => Ada.Characters.Wide_Wide_Latin_9.Space);

      function Is_Caseable (Str : Wide_Wide_String) return Boolean
      is
         use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
      begin
         return (for all Ch of Str =>
              Ch = '_' or else Is_In (Ch, Alphanumeric_Set));
      --  for J in Str'Range loop
      --     if not Is_In (str (j), Alphanumeric_Set)
      --     then
      --        return False;
      --     end if;
      --  end loop;
      --  return True;
      end Is_Caseable;

   begin
      if Spans'Length > 0 then
         if The_Line (Spans (1).L) /= '#' then
            --  Not a comment.
            if The_Line (Spans (1).L) = '*' then
               Sub_Case_Exception :
               declare
                  Word : constant Wide_Wide_String :=
                    The_Line (Spans (1).L + 1 .. Spans (1).U);
               begin
                  if Sub_Case_Exceptions.Contains (Word) then
                     if Sub_Case_Exceptions.Element (Word) /= Word then
                        Messages.Error
                          ("Sub-case exception (*"
                          & Sub_Case_Exceptions.Element (Word)
                             & ") already found for '*"
                           & Word
                           & "'");
                     end if;
                  else
                     Messages.Info ("added part case exception " & Word);
                     if not Is_Caseable (Word) then
                        Messages.Warning ("part case exception *"
                          & Word
                           & " contains uncaseable characters");
                     end if;
                     Sub_Case_Exceptions.Insert (Word, Word);
                  end if;
               end Sub_Case_Exception;
            else
               Whole_Word_Exception :
               declare
                  Word : constant Wide_Wide_String :=
                    The_Line (Spans (1).L .. Spans (1).U);
               begin
                  if Case_Exceptions.Contains (Word) then
                     if Case_Exceptions.Element (Word) /= Word then
                        Messages.Error
                          ("Case exception ("
                          & Case_Exceptions.Element (Word)
                             & ") already found for '"
                           & Word
                           & "'");
                     end if;
                  else
                     Messages.Info ("added full case exception " & Word);
                     if not Is_Caseable (Word) then
                        Messages.Warning ("full case exception "
                          & Word
                           & " contains uncaseable characters");
                     end if;
                     Case_Exceptions.Insert (Word, Word);
                  end if;
               end Whole_Word_Exception;
            end if;
         end if;
      end if;
   end Add_Exception;

   function Normalize (Id : Wide_Wide_String) return Wide_Wide_String is
      use Ada.Strings;
      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps;
      use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
      use Ada.Strings.Wide_Wide_Unbounded;

      package Wide_Wide_String_Vectors is new
      Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Unbounded_Wide_Wide_String);

      function "+" (R : Wide_Wide_String) return Unbounded_Wide_Wide_String
         renames To_Unbounded_Wide_Wide_String;
      function "+" (R : Unbounded_Wide_Wide_String) return Wide_Wide_String
         renames To_Wide_Wide_String;

      --  A Component consists of lower-case space-separated Words,
      --  each of which is subjected to Sub_Case_Exceptions (such as
      --  "IO" in "Text_IO", or, if no exception is found, the first
      --  character is capitalized.
      procedure Process_Component (S : in out Unbounded_Wide_Wide_String);
      procedure Process_Word (S : in out Unbounded_Wide_Wide_String);

      procedure Process_Component (S : in out Unbounded_Wide_Wide_String) is
         Str   : constant Wide_Wide_String := +S;
         Words : Wide_Wide_String_Vectors.Vector;
      begin
         --  Split into words.

         declare
            Spans : constant Utilities.Spans :=
              Utilities.Find_Spans (Str, ' ');
         begin
            for W in Spans'Range loop
               --  Merge runs of spaces

               if Spans (W).U >= Spans (W).L then
                  Words.Append (+(Str (Spans (W).L .. Spans (W).U)));
               end if;
            end loop;
         end;

         for W of Words loop
            Process_Word (W);
         end loop;

         --  Merge the Words back into the Component, joining with '_'.
         S := Words (1);
         for J in 2 .. Natural (Words.Length) loop
            Append (S, '_');
            Append (S, Words (J));
         end loop;

         --  Finally, look for whole-component exceptions (such as
         --  "unsigned_short").
         if Case_Exceptions.Contains (+S) then
            S := +Case_Exceptions.Element (+S);
         end if;
      end Process_Component;

      procedure Process_Word (S : in out Unbounded_Wide_Wide_String) is
      begin
         if Sub_Case_Exceptions.Contains (+S) then
            S := +Sub_Case_Exceptions.Element (+S);
         else
            Replace_Slice
              (S,
               Low  => 1,
               High => 1,
               By   =>
                 Translate (Slice (S, Low => 1, High => 1), Upper_Case_Map));
         end if;
      end Process_Word;

      --  The dot-separated components
      Components : Wide_Wide_String_Vectors.Vector;

      Result : Unbounded_Wide_Wide_String;
   begin
      --  It's sometimes legal (e.g. in state transitions) for the
      --  identifier to be empty.

      if Id'Length = 0 then
         return "";
      end if;

      --  Split the Id into components at the '.'s. For each
      --  component, translate underscores into spaces, and trim the
      --  result.
      declare
         Spans : constant Utilities.Spans := Utilities.Find_Spans (Id, '.');
      begin
         for C in Spans'Range loop
            Components.Append
              (+(Trim
                  (Translate
                     (Id (Spans (C).L .. Spans (C).U), To_Mapping ("_", " ")),
                   Both)));
         end loop;
      end;

      --  Check that none of the components is empty.
      if (for some C of Components => Length (C) = 0) then
         Messages.Error ("empty component in """ & Id & """");
         raise Notified_Error;
      end if;

      for C of Components loop
         Process_Component (C);
      end loop;

      --  Merge the Components back into the Identifier, joining with '.'.
      Result := Components (1);
      for J in 2 .. Natural (Components.Length) loop
         Append (Result, '.');
         Append (Result, Components (J));
      end loop;

      return +Result;
   end Normalize;

end Ada_Caser.Dictionaries;
