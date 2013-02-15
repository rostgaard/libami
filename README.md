# Ada library for communicating with Asterisk PBX'es over AMI[1]

This library is meant as a more-or-less high level abstraction on the
AMI interface to Asterisk.
It tries to hide away most of the horrors and pains of doing synchronization
and keeping the communication/state clean.

It is currently in flux, however. And if a relatively stable version is 
needed - it would be recommendable to checkout the adaheads/Alice project
on GitHub and scrap the ami*.ad[sb] from there instead.

This project aims to be able to have multiple connections to multiple servers
dynamically (or statically, if preferred).

## Installation
Just running "make" and "make install" (as superuser) should do the trick.
If you whish to change the default location, you can do so by editing
makefile.setup.

By then you should be able to include AMI(.gpr) in your project.

## Usage
Though still in flux, you should be able to use the AMI library by creating a
handle.
See the test case cases for more information.
````ada
with AMI.Client;
with AMI.Packet.Action;

procedure Full_Test is
   use AMI.Client;
   Client   : access AMI.Client.Instance := Get(Create);

   
begin
   Client.Connect (Hostname => "Somehost",
                   Port     => 5038);
   Client.Send (AMI.Packet.Action.Login (Username    => "someuser",
                                         Secret      => "somepassword"));
  
   --  Just hang around.
   loop
      delay 1.0;
   end loop;
end Full_Test;
````

[1] Asterisk Management interface.
