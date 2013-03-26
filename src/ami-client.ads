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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Sockets;

with AMI.Packet.Action;
with AMI.Parser;

package AMI.Client is

   Package_Name       : constant String := "AMI.Client";

   Connection_Timeout : exception;

   type Connection_Event_Handler is not null access procedure;
   --  Parameterless procedure to execute when connection state changes.

   Ignore_Event : constant Connection_Event_Handler;
   --  Silently ignore connection state changes.

   type Instance is tagged limited private;
   --  This is the actual client instance.

   procedure Connect (Client   : in out Instance;
                      Hostname : in     String;
                      Port     : in     Natural);

   procedure Disconnect (Client : in out Instance);

   function Get_Line (Client : in Instance) return String;

   procedure Send (Client : in Instance;
                   Item   : in String);
   --  Send an abitrary string. Use this as a last resort, as most should be
   --  available through AMI.Packet.Action.

   procedure Send (Client : in Instance;
                   Item   : in AMI.Packet.Action.Request);
   --  Primary send function.

   function Send (Client : in Instance;
                  Item   : in AMI.Packet.Action.Request)
                     return AMI.Parser.Packet_Type;
   --  Synchronous version of send operation. Uses an internal buffer to
   --  achieve synchronous operation.

   procedure Send (Client : in Instance;
                   Item   : in AMI.AMI_Packet);
   --  Alternate Send operation - marked for removal.
   pragma Obsolescent (Send, "Please use the primary send instead");

   function Is_Connected (Client : in Instance) return Boolean;
   pragma Obsolescent (Is_Connected, "Not supported by GNAT.Sockets.");

   function Connected (Client : in Instance) return Boolean;

   procedure Wait_For_Connection (Client  : in Instance;
                                  Timeout : in     Duration := 3.0);
   --  Waits for a client to establish a connection for duration.
   --  Raises TIMEOUT if the connection is not established within the
   --  given duration.

      type Reference is access Instance;

private

   function Read_Packet (Client : access AMI.Client.Instance)
                         return AMI.Parser.Packet_Type;

   procedure Ignore is null;
   Ignore_Event : constant Connection_Event_Handler := Ignore'Access;

   type Instance_Handle is new Natural range 1 .. 10;

   function Create return Reference;

   type Instance is new Ada.Finalization.Limited_Controlled with
      record
         Initialized           : Boolean := False;
         Connected             : Boolean := False;
         Server_Greeting       : Ada.Strings.Unbounded.Unbounded_String;
         Authenticated         : Boolean := False;
         Shutdown              : Boolean := False;
         Socket                : GNAT.Sockets.Socket_Type :=
                                   GNAT.Sockets.No_Socket;
         Channel               : GNAT.Sockets.Stream_Access := null;
         On_Connect_Handler    : Connection_Event_Handler := Ignore_Event;
         On_Disconnect_Handler : Connection_Event_Handler := Ignore_Event;
      end record;

   overriding procedure Initialize (Obj : in out Instance);
   overriding procedure Finalize (Obj : in out Instance);

   procedure Deallocate is new Ada.Unchecked_Deallocation
     (Object => Instance,
      Name   => Reference);

end AMI.Client;
