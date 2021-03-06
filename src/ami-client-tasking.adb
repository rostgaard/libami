with Ada.Calendar;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with AMI.Trace;
with AMI.Packet_Keys;
with AMI.Observers;

package body AMI.Client.Tasking is
   use Ada.Strings.Unbounded;

   package Client_Callback_Collections is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Client_Event_Callback,
                                 "="          => "=");

   type Client_Event_Listeners is array (AMI.Event.Event_Type)
     of Client_Callback_Collections.Vector;

   type Client_Data is
      record
         Client_Ref      : Client.Reference;
         Event_Observers : access Client_Event_Listeners;
      end record;

   package Client_Attribute is new Ada.Task_Attributes
     (Attribute => Client_Data, Initial_Value =>
        (null, null));

   procedure Notify (Event  : in AMI.Event.Event_Type;
                        Packet : in AMI.Parser.Packet_Type);

   protected Shutdown_Handler is
      procedure Termination_Finalizer
        (Cause : in Ada.Task_Termination.Cause_Of_Termination;
         T     : in Ada.Task_Identification.Task_Id;
         X     : in Ada.Exceptions.Exception_Occurrence);
   end Shutdown_Handler;

   procedure Dispatch (Ref    : in Client.Reference;
                       Packet : in AMI.Parser.Packet_Type);

   procedure Connect (Obj      : in Instance;
                      Hostname : in String;
                      Port     : in Natural) is
      Attr : Client_Data renames Client_Attribute.Value (T => Obj'Identity);

   begin
      Attr.Client_Ref.Connect (Hostname, Port);
   end Connect;

   procedure Disconnect (Obj : in Instance) is
      Attr : Client_Data renames Client_Attribute.Value (T => Obj'Identity);

   begin
      Attr.Client_Ref.Disconnect;
   end Disconnect;

   ----------------
   --  Dispatch  --
   ----------------

   procedure Dispatch (Ref    : in Client.Reference;
                       Packet : in AMI.Parser.Packet_Type) is
      Context : constant String := Package_Name & ".Dispatch";
      pragma Unreferenced (Context);
      use AMI.Packet_Keys;

      Attr : Client_Data renames Client_Attribute.Value;

   begin

      if Packet.Header.Key = AMI.Packet_Keys.Event then
         --  Notify the local observers.
         Notify (Event     => AMI.Event.Event_Type'Value
                 (To_String (Packet.Header.Value)),
                 Packet    => Packet);
         --  Notify the global observers.
         AMI.Observers.Notify (AMI.Event.Event_Type'Value
                               (To_String (Packet.Header.Value)),
                               Packet);
      end if;
   end Dispatch;

   ------------------------
   --  Shutdown_Handler  --
   ------------------------

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
           Package_Name & ".Shutdown_Handler.Termination_Finalizer";
         Ref     : Client.Reference := Client_Attribute.Value.Client_Ref;

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

         AMI.Client.Deallocate (Ref);

      end Termination_Finalizer;

   end Shutdown_Handler;

   ---------------------
   --  Instance Task  --
   ---------------------

   task body Instance is
      use Ada.Calendar;

      function Current_Time return Time renames Clock;
      procedure Reader_Loop;

      Next_Attempt    : Time := Current_Time;
      Event_Observers : Client_Event_Listeners;

      use Ada.Task_Identification;

      Client : Reference renames Client_Attribute.Value.Client_Ref;

      Context : constant String :=
        Package_Name & ".Instance(" & Image (Current_Task) & ")";

      procedure Reader_Loop is
      begin
         Client.Wait_For_Connection (Timeout => 3.0);

         Dispatch (Ref    => Client,
                   Packet => Client.Read_Packet);
      exception
         when Ada.IO_Exceptions.End_Error =>
            AMI.Trace.Debug (Context => Context,
                             Message => "Reader operated on closed socket");
            Client.Connected := False;
         when Connection_Timeout =>
            AMI.Trace.Debug (Context => Context,
                             Message => "Timeout reached for reader");
      end Reader_Loop;

   begin
      while not Client.Shutdown loop
         delay until Next_Attempt;
         Next_Attempt := Next_Attempt + Recheck_Connection_Delay;
         Reader_Loop;
      end loop;
   end Instance;

   --------------
   --  Notify  --
   --------------

   procedure Notify (Event  : in AMI.Event.Event_Type;
                     Packet : in AMI.Parser.Packet_Type) is
      Context : constant String := Package_Name & ".Notify ";
      use Client_Callback_Collections;

      procedure Call (C : Cursor);

      Attr : Client_Data renames
        Client_Attribute.Value;

      procedure Call (C : Cursor) is
      begin
         Element (C) (Attr.Client_Ref, Packet);
      end Call;

   begin
      if Attr.Event_Observers (Event).Is_Empty then
         AMI.Trace.Debug ("Nobody cared about event " & Event'Img, Context);
      end if;

      Attr.Event_Observers (Event).Iterate (Process => Call'Access);
   end Notify;

   --------------
   --  Create  --
   --------------

   function Create return Instance is

   begin
      return Obj : Instance do
         Client_Attribute.Set_Value
           (Val => (Client_Ref      => Client.Create,
                    Event_Observers => new Client_Event_Listeners),
            T   => Obj'Identity);
         Ada.Task_Termination.Set_Specific_Handler
           (T       => Obj'Identity,
            Handler => Shutdown_Handler.Termination_Finalizer'Access);
      end return;
   end Create;

   procedure Send (Obj    : in Instance;
                   Packet : AMI.Packet.Action.Request) is
   begin
      Client_Attribute.Value (Obj'Identity).Client_Ref.Send (Packet);
   end Send;

   ---------------
   --  Shutdown --
   ---------------

   procedure Shutdown (Obj : in Instance) is
      Client : Reference renames
        Client_Attribute.Value (T => Obj'Identity).Client_Ref;
   begin
      Client.Shutdown := True;
      Client.Disconnect;
   end Shutdown;

   procedure Subscribe (Obj     : in Instance;
                        Event   : in AMI.Event.Event_Type;
                        Handler : in Client_Event_Callback) is
      Attr : Client_Data renames
        Client_Attribute.Value (T => Obj'Identity);
   begin
      if not Attr.Event_Observers (Event).Contains (Handler) then
         Attr.Event_Observers (Event).Append (Handler);
      end if;
   end Subscribe;

end AMI.Client.Tasking;
