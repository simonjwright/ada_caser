--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

private package Ada_Caser.Messages is

   procedure Info (Message : String);
   procedure Info (Message : Wide_Wide_String);

   procedure Warning (Message : String);
   procedure Warning (Message : Wide_Wide_String);

   procedure Error (Message : String; Quit : Boolean := False);
   procedure Error (Message : Wide_Wide_String; Quit : Boolean := False);
   --  Sets exit status to Failure.
   --  If Quit is True, raises Notified_Error

   function Number_Of_Errors return Natural;

end Ada_Caser.Messages;
