unit ThreadManager;
{$MESSAGE '*******************COMPILING ThreadManager.pas'}
{$INLINE AUTO}
//3. One-shot background task

//tackling each of these scenarios:
//Fire-Forget
//1.  first off, I use commands for this nowadays, they select their own
//    threads from pools and offer more and better services.
//... so in short.  Inherit your command from TCommand and override
//    DoExecute.  Its much less painful.
//BACKground Engine
//1. Loop through a functino until terminated
//2. Once terminated, either free self if an unowned engine
//-- or once terminated enter self into pool

//Some other concepts I remember
//1. Poolable threads must be suspendable
//2. Pools will return only suspended threads.  this is so you can set some params before resuming them.

interface

 uses
  system.rtlconsts,
  system.Types,
  sysutils, classes,
{$IFDEF WINDOWS}
  winapi.windows,
{$ENDIF}
  tickcount,
  sharedobject;


implementation

uses ManagedThread;



end.
