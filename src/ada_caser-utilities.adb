with Ada.Strings.Fixed;

package body Ada_Caser.Utilities is

   function Find_Spans (S : String; Splitting_At : Character) return Spans is
      use Ada.Strings.Fixed;
      Result : Spans (1 .. Count (S, String'(1 => Splitting_At)) + 1);
      J      : Positive := 1;
   begin
      Result (J).L := S'First;
      for K in S'Range loop
         if S (K) = Splitting_At then
            Result (J).U := K - 1;
            J            := J + 1;
            Result (J).L := K + 1;
         elsif K = S'Last then
            Result (J).U := K;
         end if;
      end loop;
      return Result;
   end Find_Spans;

end Ada_Caser.Utilities;
