private package Ada_Caser.Dictionaries is

   Invalid_Name : exception;

   procedure Add_Dictionary (From_File : String);

   function Normalize (Id : String) return String;

end Ada_Caser.Dictionaries;
