with Ada_Caser.Dictionaries;
with Ada_Caser.Messages;
with Ada_Caser.Options;
with GNAT.Command_Line;

procedure Ada_Caser.Main is

begin

   Options.Process_Options
     (Report_Dictionaries_To => Dictionaries.Add_Dictionary'Access);

exception
   when GNAT.Command_Line.Exit_From_Command_Line =>
      null; -- This is only raised after -h/--help, so nothing to do
   when others                                   =>
      if Messages.Number_Of_Errors = 0 then
         --  If non-sero, we've already output an Error message and set
         --  exit status to Failure.
         raise;
      end if;
end Ada_Caser.Main;
