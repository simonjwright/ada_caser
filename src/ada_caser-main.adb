with Ada_Caser.Dictionaries;
with Ada_Caser.Options;

procedure Ada_Caser.Main is

begin

   Options.Process_Options
     (Report_Dictionaries_To => Dictionaries.Add_Dictionary'Access);

end Ada_Caser.Main;
