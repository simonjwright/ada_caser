with GNAT.Command_Line;

package body Ada_Caser.Options is

   Verbose : aliased Boolean := False;

   function Is_Verbose return Boolean is (Verbose);

   Command_Line_Config : GNAT.Command_Line.Command_Line_Configuration;

   procedure Process_Options
     (Report_Dictionaries_To : Dictionary_Reporter)
   is

      procedure Dictionary_Callback (Unused : String; Value : String);

      procedure Dictionary_Callback (Unused : String; Value : String) is
      begin
         Report_Dictionaries_To (Value);
      end Dictionary_Callback;

   begin

      GNAT.Command_Line.Set_Usage
        (Command_Line_Config,
         Usage => "[switches] sourcefile",
         Help  => "Format the source file according to the switches");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Verbose'Access,
         "-v",
         Long_Switch => "--verbose",
         Help        => "Report progress");
      GNAT.Command_Line.Define_Switch
        (Command_Line_Config,
         Callback    => Dictionary_Callback'Unrestricted_Access,
         Switch      => "-D=",
         Long_Switch => "--dictionary=",
         Help        => "Add casing dictionary");

      GNAT.Command_Line.Getopt (Command_Line_Config);

   end Process_Options;

end Ada_Caser.Options;
