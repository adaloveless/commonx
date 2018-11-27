# commonx
Delphi/Object Pascal Classes and Tools for just about everything and everyone.

## Latest News: 

### ThreadsCommandsAndQueues demo is up!  
This demo also demonstrates BigBrainUltra, and TFastBitmap for multi-threaded image processing.

Clone/watch https://github.com/adaloveless/commonx_demos for the latest demos of this class library.  Be sure to clone the sub repository.  

## Where to Start

This document will be continually updated over time.  This library contains a ton of stuff.  It really could be about 25-30 separate projects, however, my coding style has always heavily reusable, and building reusable classes, also means that there's heavy dependency among all the things contained in here.   Therefore I'll start at the bottom, and work upwards.   If you're staring into this abyss wondering where to look first, check out, for starters...  

### BetterObject.pas

I basically inherit everything I build from BetterObject.pas if it is at all possible.  BetterObject implements some extra useful things that you don't get from TObject... like... for example, a Virtual Contstructor (required if you are going to build ClassFactories using something like what is in ClassFactory.pas).  There's a few other things in there that are self-explainatory, but I'm trying to be brief.

Another interesting thing you'll find in BetterObject.pas is THolder<> and IHolder<> generics.  This generic is super useful for implementing reference counting on windows.   All it does it hold onto any object, but exposes the IHolder<T> interface.  I use it when I'm passing JSON documents around to zillions of functions and arrays and caches.   
 [code]
  function GetJsonWithHolder(someJsondocument: TJSONDocument): IHolder<TJsonDocument>;
  var
    h : IHolder<TJSONDocument>;
  begin
    h := THolder<TJsonDocument>.create;
    h.o := someJsonDocument;// will be destroyed when references to h hit 0
    result := h;//
  end
  [/code]
  
  Hopfully you get the picture.
  
  ### SharedObject.pas
  
  Is the starting point for the most basic sharable objects.   As a really basic starting point, I use TSharedObject for a lot of things because it implements Lock and Unlock.  You can argue whether or not it is a good idea to do that... but it is there if you need it.   There are other paradigms of locking if you look around in the code, NamedLock.pas, for example allows you to control access via a lock string, it can be the basis for implementing things like range, Area, and 3d-volume locks if you have a use for that kind of thing.  There are also Queued locks that enforce a "wait your turn" approach to locking (much more expensive).  In addition there are Queues in Simplequeue.pas that execute items in order VERY EFFICIENTLY in a private thread... If you want to use multiple processors, check out the MultiQueue, or TCommandProcessor, which is a bit more expensive but allows you to define resource intensity for a list of commands to maximize concurrency while thread counts are maintained automatically for you.  
    
  ### More to come in good time

I'm just getting started here.  There's lots to cover, and I've only begun to upload what I have in the last few hours.  If you look around you'll find lots of goodies.

- SimpleReliableUDP: UDP replacement for TCP
- RDTP: An RPC call framework for making remote function calls against a server (with a code generator) 
- SoundTools: Engines for real-time audio synthesis.  Integrates with Port Audio, DirectSound, and is even android compatible.
- advancedgraphics_dx: Tired of your 2D graphics being super slow?  advancedgraphics_dx.pas makes it simple to accellerate your 2D draws.  Its a bit old though and runs DirectX9  (however I have a DirectX 11 Engine that mimicks Unity which is not currently checked-in)
- Btree.pas  Implements not-quite an AVLTree generic.  It is actually better than an AVLTree, arguably the holy-grail of sorting.   This Modified AVLTree includes the ability to store duplicate keys, is pointer-perfect, auto-balancing (except for duplicates), and totally Generic, so you can track basically anything with it!  I have used it in the aforementioned DirectX11 3D engine to manage my Alpha-Sorting, as well as to manage caches in my iSCSI server.  It is actually possible to track the same object by reference in multiple trees and linked lists at the same time... so I used, for example, one tree to manage cached blocks by block number, another by dirty time, and another by last time used.

Happy Coding.
