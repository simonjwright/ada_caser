--  SPDX-License-Identifier: Apache-2.0
--  Copyright (C) 2024 Simon Wright <simon@pushface.org>

package Ada_Caser is

   --  Raised if an error has already been reported; to be caught in the
   --  main program by a null exception handler.
   Notified_Error : exception;

end Ada_Caser;
