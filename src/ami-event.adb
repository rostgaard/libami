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

with Ada.Strings.Unbounded;

with AMI.Trace;

package body AMI.Event is

   procedure Dispatch (Callback_Table : in Event_Callback_Table;
                       Packet         : in Packet_Type) is
      use Ada.Strings.Unbounded;
      Context : constant String := Package_Name & ".Dispatch";

      Event    : Event_Type;
   begin
      Event := Event_Type'Value
        (To_String (Packet.Header.Value));

      --  Launch the callback.
      Callback_Table (Event) (Packet => Packet);

   exception
      when others =>
         AMI.Trace.Error ("AMI.Event.Dispatch failed for: " &
                          (To_String (Packet.Header.Value)),
                          Context);
         raise;
   end Dispatch;

   procedure Null_Callback (Packet : in AMI.Parser.Packet_Type) is
   begin
      null;
   end Null_Callback;

end AMI.Event;
