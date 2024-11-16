with Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with GNAT.Command_Line;

package body Ada_Caser.Options is

   package Dictionary_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   Dicts : Dictionary_Vectors.Vector;

   Verbose : aliased Boolean := False;

   Charset : Ada.Strings.Unbounded.Unbounded_String
     := Ada.Strings.Unbounded.To_Unbounded_String ("utf-8");

   function Character_Set return String is
     (Ada.Strings.Unbounded.To_String (Charset));

   function Is_Verbose return Boolean is (Verbose);

   procedure Process_Options
     (Report_Dictionaries_To : Dictionary_Reporter)
   is

      Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;

      procedure Charset_Callback (Unused : String; Value : String);

      procedure Dictionary_Callback (Unused : String; Value : String);

      procedure Charset_Callback (Unused : String; Value : String) is
      begin
         Charset := Ada.Strings.Unbounded.To_Unbounded_String (Value);
      end Charset_Callback;

      procedure Dictionary_Callback (Unused : String; Value : String) is
      begin
         Dicts.Append (Value);
      end Dictionary_Callback;

   begin

      GNAT.Command_Line.Set_Usage
        (Command_Line_Config,
         Usage => "[switches] sourcefile",
         Help  => "Adjust casing in the source file");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Verbose'Access,
         "-v",
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
         Callback    => Dictionary_Callback'Unrestricted_Access,
         Switch      => "-D=",
         Long_Switch => "--dictionary=",
         Help        => "Add casing dictionary");

      GNAT.Command_Line.Getopt (Command_Line_Config);

      for Dict of Dicts loop
         Report_Dictionaries_To (Dict);
      end loop;

   end Process_Options;

end Ada_Caser.Options;
