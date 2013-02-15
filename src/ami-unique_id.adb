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

with Ada.Strings.Fixed;

package body AMI.Unique_ID is

   function "<" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      if Left.Timestamp = Right.Timestamp then
         return Left.Sequence < Right.Sequence;
      else
         return Left.Timestamp < Right.Timestamp;
      end if;
   end "<";

   function "=" (Left  : in Instance;
                 Right : in Instance) return Boolean is
   begin
      return (Left.Timestamp = Right.Timestamp) and
        (Left.Sequence = Right.Sequence);
   end  "=";

   function Image (Item : in Instance) return String is
   begin
      if Item = Null_Instance then
         return "<Null>";
      else
         return Ada.Strings.Fixed.Trim
           (Integer'Image (Item.Timestamp),
            Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim
           (Integer'Image (Item.Sequence),
            Ada.Strings.Left);
      end if;
   end Image;

   function Validate (Item : in String) return Boolean is
      Dummy_Call_ID : Instance := Null_Instance;
      pragma Unreferenced (Dummy_Call_ID);
   begin
      if Item'Length < 3 then
         return False;
      end if;

      Dummy_Call_ID := Value (Item);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Validate;

   -------------
   --  Value  --
   -------------

   function Value (Item : String) return Instance is
      Offset : constant Natural := Ada.Strings.Fixed.Index
        (Source => Item, Pattern => ".");
   begin
      return
        (Timestamp => Integer'Value
           (Item (Item'First .. Item'First + Offset - 2)),
         Sequence  => Integer'Value
           (Item (Item'First + Offset .. Item'Last)));

   exception
      when Constraint_Error =>
         raise Constraint_Error with "Bad value: """ & Item & """";

   end Value;
end AMI.Unique_ID;
