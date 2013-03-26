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

with Ada.Containers.Vectors;

with AMI.Event;
with AMI.Parser;

package AMI.Observers is

   Package_Name : constant String := "AMI.Observers";

   package Callback_Collections is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => AMI.Event.Event_Callback,
                                 "="          => AMI.Event."=");

   type Event_Listeners is array (AMI.Event.Event_Type)
     of Callback_Collections.Vector;

   procedure Register (Event   : in     AMI.Event.Event_Type;
                       Handler : in     AMI.Event.Event_Callback);
   --  Global event listeners.

   procedure Register (Listeners :    out Event_Listeners;
                       Event     : in     AMI.Event.Event_Type;
                       Handler   : in     AMI.Event.Event_Callback);

   procedure Notify (Event  : in     AMI.Event.Event_Type;
                     Packet : in     AMI.Parser.Packet_Type);

   procedure Notify (Listeners : in Event_Listeners;
                     Event     : in AMI.Event.Event_Type;
                     Packet    : in AMI.Parser.Packet_Type);

   Empty_List : constant Event_Listeners :=
     (others => Callback_Collections.Empty_Vector);

end AMI.Observers;
