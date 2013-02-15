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

with Ada.Calendar;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash_Case_Insensitive;

with AMI.Channel_ID;
with AMI.Parser;
with AMI.Unique_ID;

package AMI.Channel is
   use AMI.Unique_ID;

   Package_Name : constant String := "AMI.Channel";

   package US renames Ada.Strings.Unbounded;

   subtype Channel_Key is US.Unbounded_String;

   subtype Variable_Key is US.Unbounded_String;

   subtype Variable_Value is US.Unbounded_String;

   package Variable_Storage is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type       => Variable_Key,
        Element_Type   => Variable_Value,
        Hash           => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded."=",
        "="             => Ada.Strings.Unbounded."=");

   type Valid_State is (Down, Reserved, Off_Hook,
                        Dialed, Ringing, Receiver_Ringing, Up, Busy, Unknown);

   type Instance (Is_Null : Boolean) is tagged record
      case Is_Null is
         when False =>
            ID                    : Channel_ID.Instance (Is_Null => False);
            Bridged_With          : Channel_ID.Instance (Is_Null => False);
            State                 : Valid_State;
            Priority              : Natural;
            Unique_ID             : AMI.Unique_ID.Instance;
            Caller_ID_Number      : US.Unbounded_String;
            Caller_ID_Name        : US.Unbounded_String;
            Connected_Line_Number : US.Unbounded_String;
            Connected_Line_Name   : US.Unbounded_String;
            Account_Code          : US.Unbounded_String;
            Extension             : US.Unbounded_String;
            Application           : US.Unbounded_String;
            Application_Data      : US.Unbounded_String;
            Context               : US.Unbounded_String;
            Bridged_Unique_ID     : AMI.Unique_ID.Instance;
            Created_At            : Ada.Calendar.Time;
            Variable              : Variable_Storage.Map;
         when True =>
            null;
      end case;
   end record;
   --  Representation of a channel.

   procedure Add_Variable (Channel :    out Instance;
                           Key     : in     US.Unbounded_String;
                           Value   : in     US.Unbounded_String);
   --  Add a variable to a channel.

   function Create (Packet : in AMI.Parser.Packet_Type) return Instance;
   --  Robust constructor that ignores, non-important, missing fields.

   procedure Change_State (Channel :    out Instance;
                           Packet  : in     AMI.Parser.Packet_Type);
   --  Updates the state of a channel with the values from the given
   --  Packet.

   --     function To_JSON (Channel : in Instance)
   --                       return GNATCOLL.JSON.JSON_Value;

   function To_String (Channel : in Instance) return String;

   Null_Object  : constant Instance;
   Empty_Object : constant Instance;

   function To_Channel_State (Item : in String) return Valid_State;
   function To_Channel_State (Item : in Integer) return Valid_State;
   --  Conversion functions.

   Not_Found     : exception;
   Duplicate_Key : exception;

   type Channel_Process_Type is not null access
     procedure (C : in AMI.Channel.Instance);

   function "=" (Left : Instance; Right : Instance) return Boolean;

   package Channel_List_Type is new
     Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type       => Channel_Key,
        Element_Type   => AMI.Channel.Instance,
        Hash           => Ada.Strings.Unbounded.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Unbounded."=");

   protected type Protected_Channel_List_Type is
      function Contains (Key : in Channel_Key) return Boolean;
      procedure Insert (Key  : in Channel_Key;
                        Item : in AMI.Channel.Instance);
      procedure Remove (Key : in Channel_Key);
      procedure Rename (Old_Name : in Channel_Key;
                        New_Name : in Channel_Key);

      function Get (Key : in Channel_Key) return AMI.Channel.Instance;
      function Length return Natural;
      procedure Put (Key  : in Channel_Key;
                     Item : in AMI.Channel.Instance);
      --  function To_JSON return GNATCOLL.JSON.JSON_Value;
      function To_String return String;
      procedure Update (Key  : in Channel_Key;
                        Item : in AMI.Channel.Instance);
   private
      Protected_List : Channel_List_Type.Map;
   end Protected_Channel_List_Type;

   List : Protected_Channel_List_Type;
   --  Package-visible singleton.

   Transition_List : Protected_Channel_List_Type;
   --  Package-visible singleton of the channels currently in transition.
private

   Null_Object : constant Instance := (Is_Null => True);
   Empty_Object : constant Instance :=
                    (Is_Null               => False,
                     ID                    => Channel_ID.Empty_Channel,
                     Bridged_With          => Channel_ID.Empty_Channel,
                     State                 => Unknown,
                     Priority              => 0,
                     Caller_ID_Number      => US.Null_Unbounded_String,
                     Caller_ID_Name        => US.Null_Unbounded_String,
                     Connected_Line_Number => US.Null_Unbounded_String,
                     Connected_Line_Name   => US.Null_Unbounded_String,
                     Account_Code          => US.Null_Unbounded_String,
                     Application           => US.Null_Unbounded_String,
                     Application_Data      => US.Null_Unbounded_String,
                     Unique_ID             => AMI.Unique_ID.Null_Instance,
                     Bridged_Unique_ID     => AMI.Unique_ID.Null_Instance,
                     Extension             => US.Null_Unbounded_String,
                     Context               => US.Null_Unbounded_String,
                     Created_At            => Ada.Calendar.Clock,
                     Variable              => Variable_Storage.Empty_Map);

   --  TODO: Implement these
   --     package Channel_Storage is new
   --       Ada.Containers.Indefinite_Ordered_Maps
   --         (Key_Type     => Channel_ID.Instance,
   --          Element_Type => Channel.Instance,
   --          "<"          => Channel_ID."<",
   --          "="          => Channel."=");
   --
   --     Channel_List : Channel_Storage.Map := Channel_Storage.Empty_Map;

end AMI.Channel;
