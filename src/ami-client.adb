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

with AMI.Response;
with AMI.Trace;
with Ada.Exceptions;

package body AMI.Client is
   use Ada.Strings.Unbounded;
   use AMI.Trace;
   use GNAT.Sockets;

   ---------------
   --  Connect  --
   ---------------

   procedure Connect (Client   : access Instance;
                      Hostname : in     String;
                      Port     : in     Natural) is
      use Ada.Exceptions;

      Context : constant String := Package_Name & ".Connect";
      Address : Sock_Addr_Type (Family_Inet);
      Socket  : Socket_Type;
   begin
      Create_Socket (Socket);

      Client.Socket := Socket;

      Client.Connected := False;
      Client.Authenticated := False;

      Address.Addr   := Addresses (Get_Host_By_Name (Hostname));
      Address.Port   := Port_Type (Port);

      AMI.Trace.Information ("Connecting to " &
                         Hostname & ":" &
                         Positive'Image (Port),
                       Context);

      Connect_Socket (Socket => Client.Socket,
                      Server => Address);
      Client.Channel := Stream (Client.Socket);

      Client.Connected := True;
      Client.Server_Greeting := To_Unbounded_String (Client.Get_Line);

      AMI.Trace.Information ("Connected to " &
                               Hostname & ":" &
                               Positive'Image (Port),
                             Context);
   exception
      when E : GNAT.Sockets.Socket_Error =>
         --  Synchronize the state
         Client.Connected := False;
         Client.Authenticated := False;
         Client.On_Disconnect_Handler.all;
         AMI.Trace.Error (Context => Context, Message =>
                          "Failed to connect: " & Exception_Message (E));
   end Connect;

   function Create return Reference is
   begin
      return new Instance;
   end Create;

   -----------------
   --  Connected  --
   -----------------

   function Connected (Client : in Instance) return Boolean is
   begin
      return Client.Connected;
   end Connected;

   ------------------
   --  Disconnect  --
   ------------------

   procedure Disconnect (Client : in out Instance) is
   begin
      Shutdown_Socket (Client.Socket);
   end Disconnect;

   ----------------
   --  Finalize  --
   ----------------

   procedure Finalize (Obj : in out Instance) is
   begin
      Obj.Disconnect;
      AMI.Trace.Debug ("Finalize (instance) called for Client ");
   end Finalize;

   ----------------
   --  Get_Line  --
   ----------------

   function Get_Line (Client : in Instance) return String is
      Char   : Character := Ada.Characters.Latin_1.NUL;
      Buffer : String (1 .. 2048);
      Offset : Natural := 0;
   begin
      loop
         exit when Offset >= Buffer'Last or Char = ASCII.LF;

         Char := Character'Input (Client.Channel);
         case Char is
            when ASCII.CR | ASCII.LF =>
               null;
            when others =>
               Offset := Offset + 1;
               Buffer (Offset) := Char;
         end case;
      end loop;

      return Buffer (Buffer'First .. Buffer'First + Offset - 1);
   end Get_Line;

   ------------------
   --  Initialize  --
   ------------------

   procedure Initialize (Obj : in out Instance) is
   begin
      AMI.Trace.Debug ("Initialize (instance) called for new client");
   end Initialize;
   --------------------
   --  Is_Connected  --
   --------------------

   function Is_Connected (Client  : in Instance) return Boolean is
   begin
      raise Program_Error with "Not supported";
      return False;
   end Is_Connected;

   -------------------
   --  Read_Packet  --
   -------------------

   function Read_Packet (Client : access AMI.Client.Instance)
                         return AMI.Parser.Packet_Type is
      use AMI.Parser;
      Context        : constant String := Package_Name & ".Read_Packet";
      Current_Pair   : Pair_Type       := Null_Pair;
      Current_Packet : Packet_Type     := New_Packet;
   begin
      loop
         Current_Pair := AMI.Parser.Parse_Line (Line => Client.Get_Line);

         --  We return on an empty line, meaning the packet is complete
         if Current_Pair = Empty_Line then
            return Current_Packet;

            --  Fill the header
         elsif Current_Packet.Header = Null_Pair then
            Current_Packet.Header (Current_Pair);

            --  Or simply insert a key/value pair
         elsif Current_Pair.Key /= Null_Key then
            Current_Packet.Push (Current_Pair);
         else
            AMI.Trace.Debug ("Read_Packet: Skipping bad line", Context);
         end if;
      end loop;
   end Read_Packet;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : access Instance;
                   Item   : in     String) is
   begin
      Client.Wait_For_Connection;
      String'Write (Client.Channel, Item);
   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : access Instance;
                   Item   : in AMI.Packet.Action.Request) is
   begin
      AMI.Response.Subscribe (Item);

      Client.Send (String (Item.To_AMI_Packet));
   end Send;

   ------------
   --  Send  --
   ------------

   procedure Send (Client : access Instance;
                   Item   : in AMI.AMI_Packet) is
   begin
      Client.Send (String (Item));
   end Send;

   ------------
   --  Send  --
   ------------

   function Send (Client : access Instance;
                  Item   : in AMI.Packet.Action.Request)
                  return AMI.Parser.Packet_Type is
   begin
      AMI.Response.Subscribe (Item);
      Send (Client => Client,
            Item   => String (Item.To_AMI_Packet));

      return AMI.Response.Claim (Ticket => Item.Action_ID);
   end Send;

   ---------------------------
   --  Wait_For_Connection  --
   ---------------------------

   procedure Wait_For_Connection (Client  : access Instance;
                                  Timeout : in     Duration := 3.0) is
      use Ada.Calendar;
      Absolute_Timeout : constant Time := Clock + Timeout;
   begin
      loop
         exit when Client.Connected or Clock > Absolute_Timeout;
         delay 0.05;

      end loop;

      if not Client.Connected then
         raise AMI.Client.Connection_Timeout;
      end if;
   end Wait_For_Connection;
end AMI.Client;
