with Ada.Text_IO; use Ada.Text_IO;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Ada.Strings.Unbounded;

with AMI.Trace;
with AMI.Event;
with AMI.Observers;
package body AMI.Client.Tasking is
   use Ada.Strings.Unbounded;

   package Client_Attribute is new Ada.Task_Attributes
     (Attribute => Client.Reference, Initial_Value => null);


   function Client_Reference return AMI.Client.Reference;

   procedure Dispatch (C : in Client.Reference;
                       Packet : in AMI.Parser.Packet_Type);

   procedure Connect (Obj      : in Instance;
                      Hostname : in String;
                      Port     : in Natural) is
   begin
      Client_Attribute.Value (Obj'Identity).Connect (Hostname, Port);
   end Connect;

   procedure Disconnect (Obj : in Instance) is
   begin
      Client_Attribute.Value (Obj'Identity).Disconnect;
   end Disconnect;

   ----------------
   --  Dispatch  --
   ----------------

   procedure Dispatch (C : in Client.Reference;
                       Packet : in AMI.Parser.Packet_Type) is
      Context : constant String := Package_Name & ".Dispatch";
      pragma Unreferenced (Context);
      use AMI.Parser;
   begin
      if Packet.Header.Key = AMI.Parser.Event then
         --  Notify the local observers.
         AMI.Observers.Notify (C.Event_Observers,
                               AMI.Event.Event_Type'Value
                                 (To_String (Packet.Header.Value)),
                               Packet);
         --  Notify the global observers.
         AMI.Observers.Notify (AMI.Event.Event_Type'Value
                               (To_String (Packet.Header.Value)),
                               Packet);
      end if;
   end Dispatch;


   function Client_Reference return AMI.Client.Reference is
   begin
      return Client_Attribute.Value;
   end Client_Reference;

   protected Shutdown_Handler is

      procedure Termination_Finalizer
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
   end Shutdown_Handler;

   protected body Shutdown_Handler is

      procedure Termination_Finalizer
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence)
      is
         use Ada.Task_Termination;
         use Ada.Task_Identification;
         use Ada.Exceptions;

         Context : constant String :=
           Package_Name & ".Shutdown_Handler.Shutdown_Handler";
         Ref     : Client.Reference := Client_Attribute.Value;

      begin

         case Cause is
         when Normal =>
            AMI.Trace.Debug
              (Context => Context,
               Message => "Task " & Image (T => T) & " terminated normally");
         when Abnormal =>
            AMI.Trace.Error
              (Context => Context,
               Message => "Task " & Image (T => T) &
                 " terminated abnormally.");
         when Unhandled_Exception =>
            AMI.Trace.Error
              (Context => Context,
               Message => "Task " & Image (T => T) &
                 " terminated with exception: " & Exception_Information (X));
         end case;

         Put_Line ("Deallocating the client");
         AMI.Client.Deallocate (Ref);

      end Termination_Finalizer;

   end Shutdown_Handler;

   task body Instance is
   begin
      while not Client_Reference.Shutdown loop
         Client_Reference.Wait_For_Connection (Timeout => 3.0);
         --  TODO dispatcher code.
         Dispatch (C      => Client_Reference,
                   Packet => Client_Reference.Read_Packet);
      end loop;

      Put_Line ("instance: Got object ");
      delay 2.0;

   end Instance;

   function Create return Instance is
   begin
      return Obj : Instance do
         Client_Attribute.Set_Value (Client.Create, Obj'Identity);
         Ada.Task_Termination.Set_Specific_Handler
           (T       => Obj'Identity,
            Handler => Shutdown_Handler.Termination_Finalizer'Access);
         Put_Line ("starting task with attributes");
      end return;
   end Create;

end AMI.Client.Tasking;
