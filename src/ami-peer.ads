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
with Ada.Calendar;
with Ada.Strings.Unbounded;

with AMI.Peer_ID;

package AMI.Peer is
   use Ada.Containers;
   use Ada.Strings.Unbounded;
   use AMI.Peer_ID;

   Peer_Not_Found : exception;
   --  When a peer is not found in the list, something is terribly wrong.
   --  It means we have an inconsistant state between Agent and Peer, and
   --  thus, we raise an exception.

   type SIP_Peer_Status_Type is (Unknown, Unregistered, Idle, Busy, Paused);

   type Conditional_Time (Never : Boolean := True) is record
      case Never is
         when True =>
            null;
         when False =>
            Time : Ada.Calendar.Time := Ada.Calendar.Clock;
      end case;
   end record;

   function To_String (Item : in Conditional_Time) return String;

   type Peer_Type is tagged
      record
         ID           : Peer_ID_Type; --  Was 'peer'
         State        : SIP_Peer_Status_Type := Unregistered;
         Last_State   : SIP_Peer_Status_Type := Unknown;
         Port         : Unbounded_String;
         Address      : Unbounded_String;
         Last_Seen    : Conditional_Time;
      end record;

   function Available (Peer : in Peer_Type) return Boolean;

   procedure Seen (Peer : out Peer_Type);
   --  Bump the timestamp for the peer to the current_time.

   function To_String (Peer : in Peer_Type) return String;

   function Hash (Peer_ID : Peer_ID_Type) return Hash_Type;

   package Peer_List_Storage is new Ada.Containers.Hashed_Maps
     (Key_Type        => Peer_ID_Type,
      Element_Type    => Peer_Type,
      Hash            => Hash,
      Equivalent_Keys => "=");

   protected type Peer_List_Type is
      function Contains (Peer_ID : in Peer_ID_Type) return Boolean;
      function Count return Natural;
      function Get (Peer_ID : in Peer_ID_Type) return Peer_Type;
      procedure Put (Peer : in Peer_Type);
      function To_String return String;
   private
      List : Peer_List_Storage.Map;
   end Peer_List_Type;

   Null_Peer : constant Peer_Type;

   List : Peer_List_Type;
   --  Package-visisble singleton.

private
      Null_Peer : constant Peer_Type :=
     (ID           => Null_Peer_ID,
      State        => Unregistered,
      Last_State   => Unknown,
      Port         => Null_Unbounded_String,
      Address      => Null_Unbounded_String,
      Last_Seen    => (Never => True));
end AMI.Peer;
