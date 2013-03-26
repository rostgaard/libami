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

with Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed; -- For Index
with Ada.Characters.Latin_1;

with AMI.Trace;

package body AMI.Parser is

   -----------------
   --  Action_ID  --
   -----------------

   function Action_ID (Packet : in Packet_Type) return Action_ID_Type is
   begin
      return Action_ID_Type'Value (Packet.Get_Value (ActionID));
   end Action_ID;

   -----------------
   --  Get_Value  --
   -----------------

   function Get_Value (Packet   : in Packet_Type;
                       Key      : in AMI.Packet_Keys.Events;
                       Required : in Boolean := True) return String is
   begin
      return To_String (Packet.Get_Value (Key, Required));
   end Get_Value;

   -----------------
   --  Get_Value  --
   -----------------

   function Get_Value (Packet   : in Packet_Type;
                       Key      : in AMI.Packet_Keys.Events;
                       Required : in Boolean := True)
                       return Unbounded_String is
   begin
      if Required then
         return Packet.Fields.Element (Key);
      else
         if not Packet.Has_Value (Key) then
            return Null_Unbounded_String;
         else
            return Packet.Fields.Element (Key);
         end if;
      end if;
   exception
      when Constraint_Error =>
         raise Key_Not_In_Packet with "No pair with key " & Key'Img;
   end Get_Value;

   -----------------
   --  Has_Value  --
   -----------------

   function Has_Value (Packet   : in Packet_Type;
                       Key      : in AMI.Packet_Keys.Events) return Boolean is
   begin
      return Packet.Fields.Contains (Key);
   end Has_Value;

   ----------------------------
   --  Hash_Equivalent_Keys  --
   ----------------------------

   function Hash_Equivalent_Keys (Left, Right : in AMI.Packet_Keys.Events)
                                  return Boolean is
   begin
      return Left = Right;
   end Hash_Equivalent_Keys;

   ---------------------
   --  Hash_Function  --
   ---------------------

   function Hash_Function (Key : in AMI.Packet_Keys.Events)
                           return Ada.Containers.Hash_Type
   is
   begin
      return AMI.Packet_Keys.Events'Pos (Key);
   end Hash_Function;

   --------------
   --  Header  --
   --------------

   function Header (Packet : in Packet_Type) return Pair_Type is
   begin
      return Packet.Header;
   end Header;

   --------------
   --  Header  --
   --------------

   procedure Header (Packet : out Packet_Type; Header : in Pair_Type) is
   begin
      Packet.Header := Header;
   end Header;

   --------------------
   --  Header_Value  --
   --------------------

   function Header_Value (Packet : in Packet_Type) return String is
   begin
      return To_String (Packet.Header.Value);
   end Header_Value;

   -------------
   --  Image  --
   -------------

   function Image (Packet : in Packet_Type) return String is
   begin
      return "Header:" & Image (Packet.Header) & Image (Packet.Fields);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (List : in Pair_List_Type.Map) return String is
      package Latin_1 renames Ada.Characters.Latin_1;
      Buffer : Unbounded_String;
   begin

      for Cursor in List.Iterate loop
         Append (Buffer,
                 Latin_1.LF &
                   "[" & Pair_List_Type.Key (Cursor)'Img &
                   "] => [" &
                   Pair_List_Type.Element (Cursor) &
                   "]");
      end loop;

      return To_String (Buffer);
   end Image;

   -------------
   --  Image  --
   -------------

   function Image (Item : in Pair_Type) return String is
   begin
      return "[" & AMI.Packet_Keys.Events'Image (Item.Key) &
        "] => [" & To_String (Item.Value) & "]";
   end Image;

   ------------------
   --  Parse_Line  --
   ------------------

   function Parse_Line (Line : in String) return Pair_Type is
      Context : constant String := Package_Name & ".Parse_Line";

      Underscore_Map : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping ("-", "_");

      Seperator_Index : Natural := Index
        (Source  => Line,
         Pattern => Key_Value_Seperator);
      Key_Length     : Natural;
      Key            : AMI.Packet_Keys.Events;
   begin
      --  Special cases
      if Line'Length = 0 then
         return Empty_Line;
      elsif Seperator_Index = 0 then
         raise BAD_LINE_FORMAT;
      end if;

      --  Sometimes we get string slice instead of a "real" string.
      if Line'First /= 1 then
         Seperator_Index := Seperator_Index - Line'First + 1;
      end if;

      --  This one really isn't needed, but improves readability of
      --  the source code - hopefully.
      Key_Length := Seperator_Index - Key_Value_Seperator'Length - 1;
      Key        := AMI.Packet_Keys.Events'Value
        (Translate (Source  => Line (Line'First .. Line'First + Key_Length),
                    Mapping => Underscore_Map));

      --  Return the anonymous object
      return (Key   => Key,
              Value => To_Unbounded_String
                (Line (Line'First + Seperator_Index + 1 .. Line'Last)));
   exception
      when Constraint_Error =>
         AMI.Trace.Error ("Unkown line """ & Line & """", Context);
         return Bad_Line;

      when BAD_LINE_FORMAT =>
         AMI.Trace.Error ("Malformatted line """ & Line & """", Context);
         return Bad_Line;
   end Parse_Line;

   ------------
   --  Push  --
   ------------

   procedure Push (Packet : out Packet_Type; Pair : in Pair_Type) is
   begin
      Packet.Fields.Insert (Key      => Pair.Key,
                            New_Item => Pair.Value);
   end Push;

   -------------------
   --  Read_Packet  --
   -------------------

   function Read_Packet (Get_Line : Get_Line_Procedure)
                         return Packet_Type is
      Context        : constant String := Package_Name & ".Read_Packet";
      Current_Pair   : Pair_Type       := Null_Pair;
      Current_Packet : Packet_Type     := New_Packet;
   begin
      loop
         Current_Pair := AMI.Parser.Parse_Line
           (Line => Get_Line.all);

         --  We return on an empty line, meaning the packet is complete
         if Current_Pair = Empty_Line then
            return Current_Packet;

            --  Fill the header
         elsif Current_Packet.Header = Null_Pair then
            Current_Packet.Header := Current_Pair;

            --  Or simply insert a key/value pair
         elsif Current_Pair.Key /= Null_Key then
            Current_Packet.Fields.Insert
              (Key      => Current_Pair.Key,
               New_Item => Current_Pair.Value);
         else
            AMI.Trace.Debug ("Read_Packet: Skipping bad line", Context);
         end if;
      end loop;
   end Read_Packet;

end AMI.Parser;
