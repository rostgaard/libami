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
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;

with AMI.Packet_Keys;
package AMI.Parser is
   use AMI.Packet_Keys;
   use Ada.Strings.Unbounded;

   Package_Name : constant String := "AMI.Parser";

   type Get_Line_Procedure is not null access function return String;

   Key_Not_In_Packet : exception;

   subtype AMI_Header_Key_Type is Events range Event .. Response;
   --  Only these are allowed as headers

   BAD_LINE_FORMAT : exception;
   BAD_PACKET_FORMAT : exception;
   --  Raised when a malformatted line is encountered by the parser

   type Pair_Type is record
      Key   : AMI.Packet_Keys.Events;
      Value : Unbounded_String;
   end record;

   type Header_Type is record
      Key   : AMI_Header_Key_Type;
      Value : Unbounded_String;
   end record;

   Empty_Line : constant Pair_Type :=
     (Key   => Null_Key,
      Value => To_Unbounded_String (""));

   Bad_Line : constant Pair_Type :=
     (Key   => Null_Key,
      Value => To_Unbounded_String ("Bad Line"));

   Null_Pair : constant Pair_Type :=
     (Key   => Null_Key,
      Value => Null_Unbounded_String);

   function Hash_Function
     (Key  : in AMI.Packet_Keys.Events)
      return Ada.Containers.Hash_Type;
   function Hash_Equivalent_Keys
     (Left, Right : in AMI.Packet_Keys.Events)
      return        Boolean;

   package Pair_List_Type is new Ada.Containers.Hashed_Maps (
      Key_Type        => AMI.Packet_Keys.Events,
      Element_Type    => Unbounded_String,
      Hash            => Hash_Function,
      Equivalent_Keys => Hash_Equivalent_Keys);

   type Packet_Type is tagged private;
   --  Every AMI event/response has the same basic format ... not counting
   --  The ones with "text" format.

   function Action_ID (Packet : in Packet_Type) return Action_ID_Type;
   --  Conveniently return the Action_ID of a packet without the cast.

   function Get_Value (Packet   : in Packet_Type;
                       Key      : in AMI.Packet_Keys.Events;
                       Required : in Boolean := True) return String;
   --  Extracts a value from a packet. Raises exception when the Required flag
   --  is set and a value is not found for the key.

   function Get_Value (Packet   : in Packet_Type;
                       Key      : in AMI.Packet_Keys.Events;
                       Required : in Boolean := True) return Unbounded_String;

   function Has_Value (Packet   : in Packet_Type;
                       Key      : in AMI.Packet_Keys.Events) return Boolean;

   function Header (Packet : in Packet_Type) return Pair_Type;
   --  returns the entire header key/value pair.

   procedure Header (Packet : out Packet_Type; Header : in Pair_Type);

   procedure Push (Packet : out Packet_Type; Pair : in Pair_Type);

   function Header_Value (Packet : in Packet_Type) return String;
      --  Returns the value of the header.

   function Image (Packet : in Packet_Type) return String;

   function Read_Packet (Get_Line : Get_Line_Procedure)
                         return Packet_Type;
   --  Continously calls Read_Line and Parse_Line untill a complete packet has
   --  been assembled.

   New_Packet : constant Packet_Type;
   --  Fresh new packet without any data

   function Parse_Line (Line : in String) return Pair_Type;
   --  Tokenizes a line into a key-value Pair_Type.
   --  Takes a line of text, with key-value pairs structured:
   --  Key: Value<CRLF>

   function Image (List : in Pair_List_Type.Map) return String;

   function Image (Item : in Pair_Type) return String;

private
   type Packet_Type is tagged record
      Header : Pair_Type := Null_Pair;
      Fields : Pair_List_Type.Map;
   end record;

      New_Packet : constant Packet_Type :=
     (Header => Null_Pair,
      Fields => Pair_List_Type.Empty_Map);
end AMI.Parser;
