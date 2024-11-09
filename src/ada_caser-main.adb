with Ada_Caser.Dictionaries;
with Ada_Caser.Messages;
with Ada_Caser.Options;
with Ada_Caser.Processing;
with GNAT.Command_Line;
with Libadalang.Analysis;

procedure Ada_Caser.Main is
   use Libadalang;
   Context : constant Analysis.Analysis_Context := Analysis.Create_Context;
begin

   Options.Process_Options
     (Report_Dictionaries_To => Dictionaries.Add_Dictionary'Access);

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
               for D of Unit.Diagnostics loop
                  Messages.Error (Unit.Format_GNU_Diagnostic (D));
               end loop;
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
   when others                                   =>
      if Messages.Number_Of_Errors = 0 then
         --  If non-sero, we've already output an Error message and set
         --  exit status to Failure.
         raise;
      end if;
end Ada_Caser.Main;
