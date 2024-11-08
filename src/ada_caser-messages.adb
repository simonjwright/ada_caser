with Ada.Text_IO;
with Ada.Command_Line;
with Ada_Caser.Options;

package body Ada_Caser.Messages is

   use Ada.Text_IO;

   Errors : Natural := 0;

   procedure Info (Message : String) is
   begin
      if Options.Is_Verbose then
         Put_Line ("Info: " & Message);
      end if;
   end Info;

   procedure Warning (Message : String) is
   begin
      Put_Line (Standard_Error, "Warning: " & Message);
   end Warning;

   procedure Error (Message : String) is
   begin
      Errors := Errors + 1;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      Put_Line (Standard_Error, "Error: " & Message);
   end Error;

   function Number_Of_Errors return Natural is (Errors);

end Ada_Caser.Messages;
