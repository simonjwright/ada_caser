--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

with GNAT.Exception_Traces;      use GNAT.Exception_Traces;
with Excep_Sym_Trace_Workaround; use Excep_Sym_Trace_Workaround;

separate (Ada_Caser.Main)
procedure Initialize_Exception_Handling is
begin
   Trace_On (Unhandled_Raise);
   Set_Trace_Decorator (Symbolic_Traceback'Access);
end Initialize_Exception_Handling;
