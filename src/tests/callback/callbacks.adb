with Ada.Text_IO; use Ada.Text_IO;
with AMI.Packet;

package body Callbacks is
   procedure Test1 (Client : in AMI.Client.Reference;
                       Packet : in AMI.Parser.Packet_Type) is
   begin
      Put_Line ("Test1 callback!");
   end Test1;
end Callbacks;
