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

with AMI.Parser;
with AMI.Event;
with AMI.Observers;
with AMI.Trace;

package body AMI.Channel.Event_Handlers is
   use AMI;
   use AMI.Trace;

   procedure Masquerade (Packet : in Parser.Packet_Type);

   procedure New_Account_Code (Packet : in Parser.Packet_Type);
   --  Occurs whenever a channel changes its account code.

   procedure New_Caller_ID (Packet : in Parser.Packet_Type);
   --  Occurs whenever a channel changes its caller ID (common name).

   procedure New_State (Packet : in Parser.Packet_Type);
   --  Update the state of a channel.

   procedure Attach_Variable (Packet : in Parser.Packet_Type);
   --  Adds a variable to a given channel.

   procedure New_Channel (Packet : in Parser.Packet_Type);
   --  A Newchannel event represents any channel created within asterisk.
   --  We collect every channel into a channel list and distribute them
   --  from there to either a call list or a peer channel list.

   procedure New_Extension (Packet : in Parser.Packet_Type);
   --  From the point of view of a channel, a new extension event changes the
   --  application of the channel.

   procedure Rename (Packet : in Parser.Packet_Type);

   -----------------------
   --  Attach_Variable  --
   -----------------------

   procedure Attach_Variable (Packet : in Parser.Packet_Type) is
      Context        : constant String  := Package_Name & ".Attach_Variable";
      Channel_Key    : US.Unbounded_String renames
                         Packet.Get_Value (Parser.Channel);
      Target_Channel : Channel.Instance := Channel.Empty_Object;
   begin
      Target_Channel := Channel.List.Get (Channel_Key);

      Target_Channel.Add_Variable (Packet.Get_Value (Parser.Variable),
                                   Packet.Get_Value (Parser.Value));

      Channel.List.Update (Key  => Channel_Key,
                           Item => Target_Channel);

--        AMI.Trace.Log (AMI.Trace.Debug, Context & ": Adding [" &
--                         Packet.Get_Value (Parser.Variable) & "] => [" &
--                         Packet.Get_Value (Parser.Value)    & "]" &
--                         " to channel " & US.To_String (Channel_Key));
   exception
      when others =>
         AMI.Trace.Error
           ("Failed to add [" &
              Packet.Get_Value (Parser.Variable) & "] => [" &
              Packet.Get_Value (Parser.Value)    & "]" &
              " to channel " & US.To_String (Channel_Key), Context);
         raise;
   end Attach_Variable;

   ------------------
   --  Masquerade  --
   ------------------

   procedure Masquerade (Packet : in Parser.Packet_Type) is
      Context      : constant String  := Package_Name & ".Masquerade";

      Clone_Key    : US.Unbounded_String renames
                       Packet.Get_Value (Parser.Clone);
      Original_Key : US.Unbounded_String renames
                       Packet.Get_Value (Parser.Original);
      Clone        : Channel.Instance := Channel.Empty_Object;
      Original     : Channel.Instance := Channel.Empty_Object;

   begin
      Clone    := Channel.List.Get (Clone_Key);
      Original := Channel.List.Get (Original_Key);

      Clone.ID := Original.ID;

      Channel.List.Update (Key  => Original_Key,
                           Item => Clone);
      AMI.Trace.Debug ("Cloning channel " &
                       Packet.Get_Value (Parser.Clone) & " to " &
                         Packet.Get_Value (Parser.Original),
                       Context);
   end Masquerade;

   ------------------------
   --  New_Account_Code  --
   ------------------------

   procedure New_Account_Code (Packet : in Parser.Packet_Type) is
      Context          : constant String := Package_Name & ".New_Account_Code";
      Target_Channel   : Channel.Instance := Channel.Empty_Object;
      New_Account_Code : String renames
                           Packet.Get_Value (Parser.AccountCode);
      Old_Account_Code : String renames
                           Packet.Get_Value (Parser.OldAccountCode);
      Channel_Key      : US.Unbounded_String renames
                           Packet.Get_Value (Parser.Channel);
   begin
      if New_Account_Code /= Old_Account_Code then
         AMI.Trace.Debug ("No value changed - ignoring", Context);
      else
         AMI.Trace.Debug ("Changing account code (" &
                          Old_Account_Code & " => " & New_Account_Code & ")"
                        , Context);
         Target_Channel := Channel.List.Get (Channel_Key);

         Target_Channel.Account_Code :=
           US.To_Unbounded_String (New_Account_Code);

         Channel.List.Update (Key  => Channel_Key,
                              Item => Target_Channel);
      end if;
   exception
      when Channel.Not_Found =>
         AMI.Trace.Error ("Channel not found: " &
                            Packet.Get_Value (Parser.Channel),
                          Context);
         raise;
   end New_Account_Code;

   ---------------------
   --  New_Caller_ID  --
   ---------------------

   procedure New_Caller_ID (Packet : in Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Context              : constant String  :=
                               Package_Name & ".New_Caller_ID";
      New_Caller_ID_Name   : Unbounded_String renames
                               Packet.Get_Value (Parser.CallerIDName);
      New_Caller_ID_Number : Unbounded_String renames
                               Packet.Get_Value (Parser.CallerIDNum);
      Channel_Key          : Unbounded_String renames
                               Packet.Get_Value (Parser.Channel);
      Target_Channel       : Channel.Instance := Channel.Empty_Object;
   begin
      Target_Channel := Channel.List.Get (Channel_Key);

      if
        Target_Channel.Caller_ID_Number /= New_Caller_ID_Number or
        Target_Channel.Caller_ID_Name   /= New_Caller_ID_Name
      then
         Target_Channel.Caller_ID_Number := New_Caller_ID_Number;
         Target_Channel.Caller_ID_Name   := New_Caller_ID_Name;

         Channel.List.Update (Channel_Key,
                              Target_Channel);

         AMI.Trace.Debug ("Channel" &
                          To_String (Channel_Key) &
                          " new values : Caller_ID_Name => " &
                          To_String (New_Caller_ID_Name) &
                          " new values : Caller_ID_Num => " &
                            To_String (New_Caller_ID_Number),
                         Context);
      end if;
   end New_Caller_ID;

   -------------------
   --  New_Channel  --
   -------------------

   procedure New_Channel (Packet : in Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Context              : constant String  :=
                               Package_Name & ".New_Channel";
      Channel_Key          : Unbounded_String renames
                               Packet.Get_Value (Parser.Channel);
   begin
      AMI.Trace.Debug
        (To_String (Channel_Key), Context);
      AMI.Trace.Debug
        ("Inserting " &
           To_String (Channel_Key), Context);
      Channel.List.Insert (Packet.Get_Value (Parser.Channel),
            Channel.Create (Packet => Packet));
   end New_Channel;

   ---------------------
   --  New_Extension  --
   ---------------------

   procedure New_Extension (Packet : in Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Context          : constant String := Package_Name & ".New_Extension";
      Target_Channel   : Channel.Instance := Channel.Empty_Object;
      Application      : US.Unbounded_String renames
                           Packet.Get_Value (Parser.Application);
      Application_Data : US.Unbounded_String renames
                           Packet.Get_Value (Parser.AppData);
      Channel_Key      : US.Unbounded_String renames
                           Packet.Get_Value (Parser.Channel);
      New_Extension    : US.Unbounded_String renames
                           Packet.Get_Value (Parser.Extension);
   begin
      Target_Channel := Channel.List.Get (Channel_Key);

      Target_Channel.Application := Application;

      if Application_Data = "(NULL)" then
         Target_Channel.Application_Data := Null_Unbounded_String;
      else
         Target_Channel.Application_Data := Application_Data;
      end if;

      Target_Channel.Extension := New_Extension;

      Channel.List.Update (Key  => Channel_Key,
                           Item => Target_Channel);

      AMI.Trace.Debug (To_String (Channel_Key) &
                         " enters queue " & To_String (Application_Data),
                       Context);
   exception
      when Channel.Not_Found =>
         AMI.Trace.Error ("Channel not found " &
                          To_String (Channel_Key), Context);
         raise;
   end New_Extension;

   -----------------
   --  New_State  --
   -----------------

   procedure New_State (Packet : in Parser.Packet_Type) is
      use Ada.Strings.Unbounded;
      Context           : constant String  := Package_Name & ".New_State";
      Channel_To_Change : Channel.Instance := Channel.Empty_Object;
      Channel_Key       : US.Unbounded_String renames
                            Packet.Get_Value (Parser.Channel);
   begin

      AMI.Trace.Debug
        ("Updating channel "
         & To_String (Channel_Key) & " with new state "
         & Packet.Get_Value (Parser.ChannelStateDesc), Context);

      Channel_To_Change := Channel.List.Get (Channel_Key);

      Channel.Change_State (Channel_To_Change, Packet);

      Channel.List.Update (Channel_Key,
                           Channel_To_Change);

   exception
      when others =>
         AMI.Trace.Error ("failed to update channel " &
                          To_String (Channel_Key), Context);
         raise;
   end New_State;

   --------------------------------
   --  Register_Global_Handlers  --
   --------------------------------

   procedure Register_Global_Handlers is
   begin
      AMI.Observers.Register
        (Event   => AMI.Event.Masquerade,
         Handler => Masquerade'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Newexten,
         Handler => New_Extension'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.NewAccountCode,
         Handler => New_Account_Code'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Newcallerid,
         Handler => New_Caller_ID'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.VarSet,
         Handler => Attach_Variable'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Newchannel,
         Handler => New_Channel'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Newstate,
         Handler => New_State'Access);

      AMI.Observers.Register
        (Event   => AMI.Event.Rename,
         Handler => Rename'Access);
   end Register_Global_Handlers;

   procedure Rename (Packet : in Parser.Packet_Type) is
   begin
      Channel.List.Rename (Old_Name => Packet.Get_Value (Parser.Channel),
                           New_Name => Packet.Get_Value (Parser.Newname));
   end Rename;
end AMI.Channel.Event_Handlers;
