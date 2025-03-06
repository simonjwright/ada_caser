--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada.Command_Line;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Fixed;
with GNAT.OS_Lib;

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

   function Find_Spans
     (S : Wide_Wide_String; Splitting_At : Wide_Wide_Character)
      return Spans is
      use Ada.Strings.Wide_Wide_Fixed;
      Result : Spans
        (1 .. Count (S, Wide_Wide_String'(1 => Splitting_At)) + 1);
      J      : Positive := 1;
   begin
      if S'Length = 0 then
         --  This can only happen when (mis)using the function to get
         --  case exceptions; an identifier can never have length
         --  zero.
         return (1 .. 0 => <>);
      end if;

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

   function Default_Project return String is

      --  Append uses Ada.Directories to append path components to an
      --  input directory so as to handle Windows path separators. It has
      --  to do it via recursion because Ada_Directories.Compose will
      --  only append a simple component.
      function Append (Path : String; Addition : String) return String with
        Pre => Path'Length > 0 and then Addition'Length > 0
      is
         Separator    : constant String  := "/";
         Separator_At : constant Natural :=
           Ada.Strings.Fixed.Index (Addition, Separator);
      begin
         if Separator_At = 0 then
            return Ada.Directories.Compose (Path, Addition);
         end if;
         return
           Append
             (Path     =>
                Ada.Directories.Compose
                  (Path, Addition (Addition'First .. Separator_At - 1)),
              Addition =>
                Addition (Separator_At + Separator'Length .. Addition'Last));
      end Append;

      Location_P : GNAT.OS_Lib.String_Access
        := GNAT.OS_Lib.Locate_Exec_On_Path (Ada.Command_Line.Command_Name);
      Location   : constant String := Location_P.all;
   begin
      GNAT.OS_Lib.Free (Location_P);
      return Append
          (Path     =>
             Ada.Directories.Containing_Directory
               (Ada.Directories.Full_Name (Location)),
           Addition => "../share/ada_caser/default.gpr");
   end Default_Project;

end Ada_Caser.Utilities;
