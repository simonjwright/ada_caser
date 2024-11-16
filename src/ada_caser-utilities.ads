--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

private package Ada_Caser.Utilities is

   --  The function Find_Spans is a utility to help in processing
   --  strings containing multiple components separated by a single
   --  separator character; for example a qualified name, Ada.Text_IO,
   --  split at '.', would contain two spans:
   --
   --  Ada.Text_IO
   --  L-U L-----U
   --
   --  Consecutive occurrences of the separator character result in an
   --  empty Span (U < L).
   type Span is record
      L : Natural;
      U : Natural := 0;
   end record;
   type Spans is array (Natural range <>) of Span;

   function Find_Spans (S : String; Splitting_At : Character) return Spans;

   function Find_Spans
     (S : Wide_Wide_String; Splitting_At : Wide_Wide_Character)
      return Spans;

end Ada_Caser.Utilities;
