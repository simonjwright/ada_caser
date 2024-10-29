private package Ada_Caser.Options is

   type Dictionary_Reporter
   is access procedure (Dictionary_Filename : String);

   procedure Process_Options (Report_Dictionaries_To : Dictionary_Reporter);

   function Is_Verbose return Boolean;

end Ada_Caser.Options;
