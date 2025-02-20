--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada.Strings;
with Ada_Caser.Messages;
with Ada_Caser.Dictionaries;
with Ada_Caser.Options;
with Libadalang.Common;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Ada_Caser.Processing is

   procedure Process (Unit : Libadalang.Analysis.Analysis_Unit) is
      use Libadalang;

      Token : Common.Token_Reference := Analysis.First_Token (Unit);
      use type Common.Token_Reference;
      use type Common.Token_Kind;

      use Ada.Strings.Wide_Wide_Fixed;
      use Ada.Strings.Wide_Wide_Maps.Wide_Wide_Constants;
   begin
      Messages.Info ("processing " & Unit.Get_Filename);
      while Token /= Common.No_Token loop

         if not Options.Report_Tokens then
            case Common.Kind (Common.Data (Token)) is
               when Common.Ada_Identifier =>
                  Put (Dictionaries.Normalize (Common.Text (Token)));
               when Common.Ada_Char |
                 Common.Ada_Comment |
                 Common.Ada_Integer |
                 Common.Ada_String =>
                  Put (Common.Text (Token));
               when Common.Ada_Whitespace =>
                  for Ch of Common.Text (Token) loop
                     --  output an LF by calling New_Line, so
                     --  [Wide_Wide_]Text_IO realises that the end of
                     --  the file doesn't need an extra blank line.
                     if Ch = Wide_Wide_Character'Val (16#0000_000a#) then
                        New_Line;
                     else
                        Put (Ch);
                     end if;
                  end loop;
               when others =>
                  Put (Translate (Common.Text (Token), Lower_Case_Map));
            end case;
         else
            Put (Common.Text (Token));
            Put (" is ");
            Put (Common.Kind (Common.Data (Token))'Wide_Wide_Image);
            New_Line;
         end if;

         Token := Common.Next (Token);
      end loop;
      Messages.Info (Unit.Get_Filename & " done.");
   end Process;

end Ada_Caser.Processing;
