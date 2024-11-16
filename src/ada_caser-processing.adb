--  SPDX-License-Identifier: Apache-2.0
--  SPDX-FileCopyrightText: Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada_Caser.Messages;
with Ada_Caser.Dictionaries;
with Libadalang.Common;
with Langkit_Support.Text;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Ada_Caser.Processing is

   procedure Process (Unit : Libadalang.Analysis.Analysis_Unit) is
      use Libadalang;

      Token : Common.Token_Reference := Analysis.First_Token (Unit);
      use type Common.Token_Reference;
      use type Common.Token_Kind;
   begin
      Messages.Info ("processing " & Unit.Get_Filename);
      while Token /= Common.No_Token loop
         case Common.Kind (Common.Data (Token)) is
            when Common.Ada_Identifier =>
               Put (Dictionaries.Normalize (Common.Text (Token)));
            when others =>
               Put (Common.Text (Token));
         end case;
         Token := Common.Next (Token);
      end loop;
      Messages.Info (Unit.Get_Filename & " done.");
   end Process;

end Ada_Caser.Processing;
