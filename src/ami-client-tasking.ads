with AMI.Packet.Action;
with AMI.Event;
with AMI.Parser;

package AMI.Client.Tasking is
   task type Instance;

   function Create return Instance;

   type Client_Event_Callback is
     access procedure (Client : in AMI.Client.Reference;
                       Packet : in AMI.Parser.Packet_Type);

   procedure Subscribe (Obj     : in Instance;
                        Event   : in AMI.Event.Event_Type;
                        Handler : in Client_Event_Callback);

   procedure Unsubscribe is null;

   procedure Send (Obj    : in Instance;
                   Packet : in AMI.Packet.Action.Request);
   --  function Send (Packet : AMI.Packet.Action.Request) return AMI.Packet.Reponse;

   procedure Connect (Obj      : in Instance;
                      Hostname : in String;
                      Port     : in Natural);

   procedure Disconnect (Obj : in Instance);

   procedure Shutdown (Obj : in Instance);

private
   Recheck_Connection_Delay : constant Duration := 2.0;
   --  How long we should wait between connection polling.
end AMI.Client.Tasking;
