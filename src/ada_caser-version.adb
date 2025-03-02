--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Ada_Caser_Config; use Ada_Caser_Config;
function Ada_Caser.Version return String is
begin
   return Crate_Name & ": " & Crate_Version;
end Ada_Caser.Version;
