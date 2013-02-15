-------------------------------------------------------------------------------
--                                                                           --
--                     Copyright (C) 2012-, AdaHeads K/S                     --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

package AMI.Unique_ID is

   type Instance is tagged record
      Timestamp : Integer;
      Sequence  : Integer;
   end record;

   function Value (Item : in String) return Instance;
   --  Constructor. Raises Constraint_Error on parse errors.
   function Create (Item : in String) return Instance renames Value;
   pragma Obsolescent (Create, "Please use Value instead");

   function Image (Item : in Instance) return String;
   --  Debug-friendly represenation of the call ID.
   function To_String (Item : in Instance) return String renames Image;
   pragma Obsolescent (To_String, "Please use Image instead");

   function "<" (Left  : in Instance;
                 Right : in Instance) return Boolean;

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean;

   Null_Instance : constant Instance := (-1, -1);

   function Validate (Item : in String) return Boolean;
   --  Non-exception-raising way of checking whether a string can be
   --  converted into a Call_ID
end AMI.Unique_ID;
