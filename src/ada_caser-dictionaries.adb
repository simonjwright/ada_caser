with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada_Caser.Messages;
with Ada_Caser.Utilities;

package body Ada_Caser.Dictionaries is

   function Casing_Equals (L, R : String) return Boolean;
   function Casing_Less_Than (L, R : String) return Boolean;

   package Lowercase_String_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (Key_Type     => String,
        Element_Type => String,
        "<"          => Casing_Less_Than,
        "="          => Casing_Equals);

   --  For Case_Exceptions, the whole identifier has to match
   Case_Exceptions     : Lowercase_String_Maps.Map;
   --  For Sub_Case_Exceptions, each 'word' in the identifier matches
   --  separately.
   Sub_Case_Exceptions : Lowercase_String_Maps.Map;

   package Lowercase_String_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets
       (Element_Type => String,
        "<"          => Casing_Less_Than,
        "="          => Casing_Equals);

   Reserved : Lowercase_String_Sets.Set;

   function Casing_Equals (L, R : String) return Boolean is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
   begin
      return Translate (L, Lower_Case_Map) = Translate (R, Lower_Case_Map);
   end Casing_Equals;

   function Casing_Less_Than (L, R : String) return Boolean is
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps.Constants;
   begin
      return Translate (L, Lower_Case_Map) < Translate (R, Lower_Case_Map);
   end Casing_Less_Than;

   procedure Add_Exception (From_Line : String);

   procedure Add_Dictionary (From_File : String) is
      Exceptions : Ada.Text_IO.File_Type;
      use Ada.Text_IO;
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

   procedure Add_Exception (From_Line : String) is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;

      --  We only want the first word on the line.
      L     : constant String := Trim (From_Line, Both);
      --  Find_Spans splits on Character, so translate any tabs to
      --  spaces before splitting.
      Words : constant Utilities.Spans :=
        Utilities.Find_Spans
          (Translate (L, To_Mapping ((1 => ASCII.HT), " ")),
           Splitting_At => ' ');

   begin
      if Words'Length > 0 then
         if L (Words (1).L) /= '#' then
            --  Not a comment.
            if L (Words (1).L) = '*' then
               --  It's a sub-case exception.
               declare
                  Word : constant String := L (Words (1).L + 1 .. Words (1).U);
               begin
                  if Sub_Case_Exceptions.Contains (Word) then
                     Messages.Error
                       ("Sub-case exception already found for '" & Word & "'");
                  else
                     Sub_Case_Exceptions.Insert (Word, Word);
                  end if;
               end;
            else
               --  It's a whole word exception.
               declare
                  Word : constant String := L (Words (1).L .. Words (1).U);
               begin
                  if Case_Exceptions.Contains (Word) then
                     Messages.Error
                       ("Case exception already found for '" & Word & "'");
                  else
                     Case_Exceptions.Insert (Word, Word);
                  end if;
               end;
            end if;
         end if;
      end if;
   end Add_Exception;

   function Normalize (Id : String) return String is
      use Ada.Strings;
      use Ada.Strings.Fixed;
      use Ada.Strings.Maps;
      use Ada.Strings.Maps.Constants;
      use Ada.Strings.Unbounded;

      package String_Vectors is new
        Ada.Containers.Vectors
          (Index_Type   => Positive,
           Element_Type => Unbounded_String);

      function "+" (R : String) return Unbounded_String
      renames To_Unbounded_String;
      function "+" (R : Unbounded_String) return String renames To_String;

      --  A Component consists of lower-case space-separated Words,
      --  each of which is subjected to Sub_Case_Exceptions (such as
      --  "IO" in "Text_IO", or, if no exception is found, the first
      --  character is capitalized.
      procedure Process_Component (S : in out Unbounded_String);
      procedure Process_Word (S : in out Unbounded_String);

      procedure Process_Component (S : in out Unbounded_String) is
         Str   : constant String := +S;
         Words : String_Vectors.Vector;
      begin
         --  Check for reserved words, which don't contain underscores
         --  (spaces by now) and must therefore be whole components.
         if Reserved.Contains (Str) then
            raise Invalid_Name with "reserved word '" & Str & "' not allowed";
         end if;

         --  Split into words.
         declare
            Ws : constant Utilities.Spans := Utilities.Find_Spans (Str, ' ');
         begin
            for W in Ws'Range loop
               --  Merge runs of spaces
               if Ws (W).U >= Ws (W).L then
                  Words.Append (+(Str (Ws (W).L .. Ws (W).U)));
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

      procedure Process_Word (S : in out Unbounded_String) is
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
      Components : String_Vectors.Vector;

      Result : Unbounded_String;
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
         Cs : constant Utilities.Spans := Utilities.Find_Spans (Id, '.');
      begin
         for C in Cs'Range loop
            Components.Append
              (+(Trim
                   (Translate
                      (Id (Cs (C).L .. Cs (C).U), To_Mapping ("_", " ")),
                    Both)));
         end loop;
      end;

      --  Check that none of the components is empty.
      if (for some C of Components => Length (C) = 0) then
         raise Invalid_Name with "empty component in """ & Id & """";
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

begin

   --  Save the reserved words (of Ada 95).
   Reserved.Insert ("abort");
   Reserved.Insert ("abs");
   Reserved.Insert ("abstract");
   Reserved.Insert ("accept");
   Reserved.Insert ("access");
   Reserved.Insert ("aliased");
   Reserved.Insert ("all");
   Reserved.Insert ("and");
   Reserved.Insert ("array");
   Reserved.Insert ("at");
   Reserved.Insert ("begin");
   Reserved.Insert ("body");
   Reserved.Insert ("case");
   Reserved.Insert ("constant");
   Reserved.Insert ("declare");
   Reserved.Insert ("delay");
   Reserved.Insert ("delta");
   Reserved.Insert ("digits");
   Reserved.Insert ("do");
   Reserved.Insert ("else");
   Reserved.Insert ("elsif");
   Reserved.Insert ("end");
   Reserved.Insert ("entry");
   Reserved.Insert ("exception");
   Reserved.Insert ("exit");
   Reserved.Insert ("for");
   Reserved.Insert ("function");
   Reserved.Insert ("generic");
   Reserved.Insert ("goto");
   Reserved.Insert ("if");
   Reserved.Insert ("in");
   Reserved.Insert ("is");
   Reserved.Insert ("limited");
   Reserved.Insert ("loop");
   Reserved.Insert ("mod");
   Reserved.Insert ("new");
   Reserved.Insert ("not");
   Reserved.Insert ("null");
   Reserved.Insert ("of");
   Reserved.Insert ("or");
   Reserved.Insert ("others");
   Reserved.Insert ("out");
   Reserved.Insert ("package");
   Reserved.Insert ("pragma");
   Reserved.Insert ("private");
   Reserved.Insert ("procedure");
   Reserved.Insert ("protected");
   Reserved.Insert ("raise");
   Reserved.Insert ("range");
   Reserved.Insert ("record");
   Reserved.Insert ("rem");
   Reserved.Insert ("renames");
   Reserved.Insert ("requeue");
   Reserved.Insert ("return");
   Reserved.Insert ("reverse");
   Reserved.Insert ("select");
   Reserved.Insert ("separate");
   Reserved.Insert ("subtype");
   Reserved.Insert ("tagged");
   Reserved.Insert ("task");
   Reserved.Insert ("terminate");
   Reserved.Insert ("then");
   Reserved.Insert ("type");
   Reserved.Insert ("until");
   Reserved.Insert ("use");
   Reserved.Insert ("when");
   Reserved.Insert ("while");
   Reserved.Insert ("with");
   Reserved.Insert ("xor");

   --  Include the reserved words new in Ada 2005.
   Reserved.Insert ("interface");
   Reserved.Insert ("overriding");
   Reserved.Insert ("synchronized");

   --  Include the reserved words new in Ada 2012.
   Reserved.Insert ("some");

   --  Include the reserved words new in Ada 2022.
   Reserved.Insert ("parallel");

end Ada_Caser.Dictionaries;
