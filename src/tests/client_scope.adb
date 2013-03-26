with Ada.Text_IO; use Ada.Text_IO;

with AMI.Client.Tasking; use AMI.Client.Tasking;

procedure Client_Scope is
   use AMI.Client;

   procedure Test_Scope;

   procedure Test_Scope is
      Test : Tasking.Instance := Create;
   begin
      Connect (Test, "asterisk2.adaheads.com", 5038);
      Put_Line ("Leaving test scope");
      Disconnect (Test);
      Shutdown (Test);
   end Test_Scope;
begin
   Put_Line ("Entering test scope");
   Test_Scope;
   Put_Line ("Left test scope");
end Client_Scope;
