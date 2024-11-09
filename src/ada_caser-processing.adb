with Ada_Caser.Messages;

package body Ada_Caser.Processing is

   procedure Process (Unit : Libadalang.Analysis.Analysis_Unit) is
   begin
      Messages.Info ("processing " & Unit.Get_Filename);
   end Process;

end Ada_Caser.Processing;
