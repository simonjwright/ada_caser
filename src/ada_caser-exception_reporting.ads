--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

package Ada_Caser.Exception_Reporting
with Elaborate_Body
is

--  On macOS, the body (in ada_caser-exception_reporting--macos.adb)
--  uses the crate exsytrawo to decode exceptions.
--
--  On other OSs, the body (ada_caser-exception_reporting--other.adb)
--  is null.
end Ada_Caser.Exception_Reporting;
