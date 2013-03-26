with AMI.Parser; use AMI.Parser;
with AMI.Client; use AMI.Client;

package Callbacks is
   procedure Test1 (Client : in AMI.Client.Reference;
                       Packet : in AMI.Parser.Packet_Type);
end Callbacks;
