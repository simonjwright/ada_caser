private package Ada_Caser.Messages is

   procedure Info (Message : String);

   procedure Warning (Message : String);

   procedure Error (Message : String);

   function Number_Of_Errors return Natural;

end Ada_Caser.Messages;
