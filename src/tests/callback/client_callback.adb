with Ada.Text_IO; use Ada.Text_IO;

with AMI.Event;
with AMI.Packet.Action;

with Callbacks;

with AMI.Client.Tasking; use AMI.Client.Tasking;

procedure Client_Callback is
   use AMI.Client;

   procedure Test_Scope;

   procedure Test_Scope is
      Test : Tasking.Instance := Create;
   begin
      Connect (Test, "asterisk2.adaheads.com", 5038);

      Subscribe (Test, AMI.Event.FullyBooted, Callbacks.Test1'Access);
      AMI.Client.Tasking.Send (Obj    => Test,
                               Packet => AMI.Packet.Action.Login
                                 (Username    => "test",
                                  Secret      => "test"));
      delay 5.0;
      Disconnect (Test);
      Shutdown (Test);
      Put_Line ("Leaving Test_Scope");
   end Test_Scope;
begin
   Put_Line ("Entering Test_Scope");
   Test_Scope;
   Put_Line ("Left Test_Scope");
end Client_Callback;
