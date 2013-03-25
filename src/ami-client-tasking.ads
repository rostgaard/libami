with AMI.Packet.Action;

package AMI.Client.Tasking is
   task type Instance;

   function Create return Instance;

   procedure Subscribe is null;

   procedure Unsubscribe is null;

   procedure Send (Packet : AMI.Packet.Action.Request) is null;
   --function Send (Packet : AMI.Packet.Action.Request) return AMI.Packet.Reponse;

   procedure Connect (Obj      : in Instance;
                      Hostname : in String;
                      Port     : in Natural);

   procedure Disconnect (Obj : in Instance);
end AMI.Client.Tasking;
