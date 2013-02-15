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

with AMI.Trace;

package body AMI.Observers is
   use AMI.Trace;

   Global_Callbacks : Event_Listeners;

   --------------
   --  Notify  --
   --------------

   procedure Notify (Listeners : in Event_Listeners;
                     Event     : in AMI.Event.Event_Type;
                     Packet    : in AMI.Parser.Packet_Type) is
      Context : constant String := Package_Name & ".Notify";
      use Callback_Collections;

      procedure Call (C : Cursor);

      procedure Call (C : Cursor) is
      begin
         Element (C) (Packet);
      end Call;

   begin
      if Listeners (Event).Is_Empty then
         AMI.Trace.Debug ("Nobody cared about event " & Event'Img, Context);
      end if;

      Listeners (Event).Iterate (Process => Call'Access);
   end Notify;

   procedure Notify (Event  : in AMI.Event.Event_Type;
                     Packet : in AMI.Parser.Packet_Type) is
   begin
      Notify (Global_Callbacks, Event, Packet);
   end Notify;

   ----------------
   --  Register  --
   ----------------

   procedure Register (Listeners :    out Event_Listeners;
                       Event     : in     AMI.Event.Event_Type;
                       Handler   : in     AMI.Event.Event_Callback) is
      Context : constant String := Package_Name & ".Register";
   begin
      AMI.Trace.Debug (Message => "Registering handler for " & Event'Img,
                       Context => Context);
      if not Listeners (Event).Contains (Handler) then
         Listeners (Event).Append (Handler);
      end if;
   end Register;

   ----------------
   --  Register  --
   ----------------

   procedure Register (Event   : in AMI.Event.Event_Type;
                       Handler : in AMI.Event.Event_Callback) is
   begin
      Register (Listeners => Global_Callbacks,
                Event     => Event,
                Handler   => Handler);
   end Register;

end AMI.Observers;
