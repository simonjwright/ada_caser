--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with Excep_Sym_Trace_Workaround;
with GNAT.Exception_Traces;

package body Ada_Caser.Exception_Reporting is

--  This is the macOS version of this package. It uses the crate
--  exsytrawo to decode exceptions.

begin

   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator
     (Excep_Sym_Trace_Workaround.Symbolic_Traceback'Access);

end Ada_Caser.Exception_Reporting;
