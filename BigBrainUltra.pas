unit BigBrainUltra;
{$MESSAGE '*********************************************************************'}
{$MESSAGE 'BigBrain-------------------------------------------------------------'}
{$MESSAGE '*********************************************************************'}
{$DEFINE DISABLE_INTERFACE}
{$DEFINE DISABLE_IMPLEMENTATION}
{$DEFINE DISABLE_INITIALIZATION}
{$DEFINE DISABLE_FINALIZATION}

{VERSION UPDATES}
//log 3.0.0 - Restructured with $I for includes
//log 2.6.5 - Compiles (disabled) for OSX
//          -
//log 2.6.4 - Stable for Firemonkey Apps
//          - Stable For 64-Bit Apps
//          - Conforms to OS memory size limit for performance increase
//          - Initial implementation (non functioning) of measures to
//            waste less memory when blocks are large.
//
//log 2.6.3 - Block Alignment to 16 bytes.  20-30% performance boost
//            HOLD_MEMORY implemented
//            DONT_LINK_ALLOCATED implemented
//            COUNT_HEAP_OPERATIONS implemented
//            INFLATE_TO_32 fixed.  should work with 32-bit 64-bit and STATS/NOSTATS
//            ALIGN_MEMORY_OF_HEAP_OBJECTS implemented
//
//log 2.6.2 - Fix: tiny blocks (<8 bytes) in 64-bit apps could potentially trash memory during Realloc functions
//
//log 2.6.1 - Delphi XE2 32-bit and 64-bit support
//
//log 2.5.7 - fixed a few typos and clarifies some comments, no real code changes
//          - altered compiler directive placement to properly allow the
//            directives "DISABLE_ALL" and "ENABLE_ON_SINGLE_CPU" to result
//            in proper compileration
//log 2.5.6 - Log started

(*
BigBrain Commercial License Agreement
--------------------------------------
Commercial customers who purchased BigBrainUltra from Digital Tundra LLC are
granted license to distribute unlimited quantities of COMPILED applications from
BigBrainUltra.pas and to redistribute the BigBrainShareMem.dll included in the
package with certain limitations.

LIMITATIONS
1) The license applies only to a single title/product line developed by a single company,
organization, or institution.  The license is transferable from consultant to client
so long as the consultant forfeits all license rights to the client.

2) The product/title must NOT be a memory manager product or competing product with
any Digital Tundra LLC products. In the event that Digital Tundra LLC releases
a competing product after you received this license you will retain all rights
mentioned in this agreement.

3) The product must not contain readable source code for any .pas files packaged with BigBrain.

LIABILITY
Although BigBrain is stressed tested and in use by hundreds of companies,
use of BigBrain in your application is entirely at your own risk.  Digital Tundra
LLC is not responsibile if BigBrain fails to perform within the environment of your
application.

Digital Tundra LLC will work to support licensed customers at its discretion,
however, this is not a guarantee of unlimited support.

Digital Tundra LLC is in no way responsible for loss of profits resulting from
the use of this memory manager.

Digital Tundra LLC may change this license agreement upon the release of
updated versions of BigBrain.  If your company chooses not to accept the updated
license terms, you may choose not to use the upgraded BigBrain source and retain
all rights mentioned in the earlier license agreement.

TERMINATION OF RIGHTS
Digital Tundra LLC may terminate this agreement with public notice for any
reason.  Upon termination of rights, Digital Tundra LLC may at its
discretion offer refunds to licensed customers who bought licenses within 1 year
of the termination. Digital Tundra LLC is not responsible for loss of profits
due to the termination of this agreement beyond the initial license fee.

*)
{$IFDEF DO_MEM_CHART}
{$DEFINE BBDEBUG}
{$ENDIF}
{$Q-}//no overflow checking
{$R-}//no range checking
{$IFDEF BBDEBUG}
  {$DEFINE MESSAGES}
  {x$MESSAGE warn 'BB optmization is disabled'}
  {$O-}               //Optimization Setting
  {$D+}               //Force debug information on ... if set to +, your code
                      // will be harder to debug
   {$DEFINE STATS}
  {$DEFINE INLINE_DEBUG_DISABLE}
{$ELSE}
  {$MESSAGE 'BB setting O+ D-'}
  {$O+}               //Force compiler optimization on
  {$D-}               //Force debug information off ... if set to +, your code
                      // will be harder to debug
  {x$DEFINE STATS}
{$ENDIF}



//NEW! TWEAKABLES!
//Please note that NOT all conditional defines defined within this PAnsiChar will
//yield stable results.  Please use care when changing these settings.
//Defines beginining with "$DEFINE" are enabled... "xDEFINE" means disabled
{x$DEFINE STATS}
{x$DEFINE ATOMICLOCKS}
{$DEFINE TRY_BEFORE_BUY}
{$IFDEF TRY_BEFORE_BUY}{$MESSAGE 'DEFINED: TRY_BEFORE_BUY'}{$ENDIF}

{$DEFINE USE_MULTIPLE_THREAD_BLOCK_HOLD_TIMES}
{$IFDEF USE_MULTIPLE_THREAD_BLOCK_HOLD_TIMES}{$MESSAGE 'DEFINED: USE_MULTIPLE_THREAD_BLOCK_HOLD_TIMES'}{$ENDIF}

{$DEFINE COUNT_HEAP_OPERATIONS}
{$IFDEF COUNT_HEAP_OPERATIONS}{$MESSAGE 'DEFINED: COUNT_HEAP_OPERATIONS'}{$ENDIF}

{$DEFINE HOLD_MEMORY}
{$IFDEF HOLD_MEMORY}{$MESSAGE 'DEFINED: HOLD_MEMORY'}{$ENDIF}

//new for x64/v4.0
{$IFNDEF STATS}
{$DEFINE DONT_LINK_ALLOCATED}  //20% speed boost, not compatible with $STATS
{$IFDEF DONT_LINK_ALLOCATED}{$MESSAGE 'DEFINED: DONT_LINK_ALLOCATED'}{$ENDIF}
{$ENDIF}

{$DEFINE ALIGN_MEMORY_OF_HEAP_OBJECTS}
{$IFDEF ALIGN_MEMORY_OF_HEAP_OBJECTS}{$MESSAGE 'DEFINED: ALIGN_MEMORY_OF_HEAP_OBJECTS'}{$ENDIF}

{x$DEFINE ALIGN_MEMORY} //!EXPERIMENTAL/alternate method of aligning memory- DO NOT USE
{$IFDEF ALIGN_MEMORY}{$MESSAGE warn 'EXPERIMENTAL: ALIGN_MEMORY'}{$ENDIF}

{x$DEFINE INFLATE_to_24}
{$IFDEF INFLATE_to_24}{$MESSAGE 'DEFINED: INFLATE_to_24'}{$ENDIF}

{$DEFINE INFLATE_to_32} //recommended- header will be on 16-byte boundry as well as memory block returned
{$IFDEF INFLATE_to_32}{$MESSAGE 'DEFINED: INFLATE_to_32'}{$ENDIF}


{x$DEFINE NARROW_SIZE_INDEXES}  //!EXPERIMENTAL DO NOT USE
{$IFDEF NARROW_SIZE_INDEXES}{$MESSAGE warn 'EXPERIMENTAL: NARROW_SIZE_INDEXES'}{$ENDIF}

{x$DEFINE NO_REMANAGE_TRAP}  //!EXPERIMENTAL DO NOT USE
{$IFDEF NO_REMANAGE_TRAP}{$MESSAGE warn 'EXPERIMENTAL: NO_REMANAGE_TRAP'}{$ENDIF}

//{x$DEFINE DONT_CACHE_LARGE_BLOCKS} //!EXPERIMENTAL DO NOT USE
{$IFDEF DONT_CACHE_LARGE_BLOCKS}{$MESSAGE warn 'EXPERIMENTAL: DONT_CACHE_LARGE_BLOCKS'}{$ENDIF}

//{$IFDEF DONT_CACHE_LARGE_BLOCKS}const MAX_CACHEABLE_BLOCK_SIZE = 1023;{$ENDIF}

{x$DEFINE USE_LARGER_BOOLEANS}
{$IFDEF USE_LARGER_BOOLEANS}{$MESSAGE 'DEFINED: USE_LARGER_BOOLEANS'}{$ENDIF}

{x$DEFINE ALLOW_ZERO_LENGTH} //if defined will allow the memory manger to serve
                            //out blocks that have 0 length.  This requires
                            //an extra check which may be unnecessary on most
                            //apps
{$IFDEF ALLOW_ZERO_LENGTH}{$MESSAGE 'DEFINED: ALLOW_ZERO_LENGTH'}{$ENDIF}

{$DEFINE PACK_HEADERS}
{$IFDEF PACK_HEADERS}{$MESSAGE 'DEFINED: PACK_HEADERS'}{$ENDIF}

{$DEFINE WINDOWS2000_COMPATIBLE}
{$IFDEF WINDOWS2000_COMPATIBLE}{$MESSAGE 'DEFINED: WINDOWS2000_COMPATIBLE'}{$ENDIF}
                        //if you require windows 2000(SP3 and earlier)/NT compatability enable this
                        //--note using this option nulls the effect of
                        //  low-frag and high performance heaps, but BigBrain
                        //  does quite well without these features.

{$DEFINE FREEMAN}       //It is recommended that you always use
{$DEFINE USEDEADLIST}   //the FREEMAN and USEDEADLIST directives together.
                        //Both of these directives allow Big Brain to
                        //delete unused block managers from unused terminated
                        //threads, thereby keeping the system running smoothly
                        //even after 250-300 threads were running prior.
{$IFDEF FREEMAN}{$MESSAGE 'DEFINED: FREEMAN'}{$ENDIF}
{$IFDEF USEDEADLIST}{$MESSAGE 'DEFINED: USEDEADLIST'}{$ENDIF}


{$DEFINE LOCKLISTS}     //Should always be on.  If off (untested) it will
                        //behave more like the non-Pro BigBrain.  The "off" setting
                        //however has not been tested recently and there is no reason to.
{$DEFINE USELISTCLASS}  //Should always be on.  If off (untested) some classes
                        //will be compiled as records instead.  The manager was
                        //originally built with records then upgraded to classes
                        //which have shown no performance difference.
{x$DEFINE LOCKMAN}       //Should always be off.  If on (untested) the entire
                        //thread-memory-manager will be locked during memory
                        //operations, whereas locks on the block-lists are
                        //really all thats necessary and perform much better
{$DEFINE LESSWASTE}     //recommended ON --
                        //  [on] - Blocks are more widely distributed across
                        //  the linked lists because the header size is NOT taken
                        //  into consideration when choosing the size index.
                        //  [off] - Header is considered as part of the payload
                        //  when choosing the size index which means that
                        //  no blocks are allocated in the smallest indexes.
                        //  and blocks are reused more often, but chance for contention
                        //  is higher.

{$IF CompilerVersion >= 17.0}        //  "NOINLINE" must be defined for PRE-Delphi2005/2006
{$DEFINE SUPPORTS_INLINE}  //  if [ON] disables use of the inline directive in Delphi
{$IFDEF SUPPORTS_INLINE}{$MESSAGE 'DEFINED: SUPPORTS_INLINE'}{$ENDIF}

{$ENDIF}                //  2005.  Turn [ON] (for Delphi2005) only if you
                        //  experience problems.
                        //  !!Must be [ON] (NOINLINE=true) for earlier Delphi versions!!
{$IF CompilerVersion >= 23.0}
{$DEFINE DELPHI64}
{$ENDIF}

{$IFDEF DELPHI64}{$MESSAGE 'DEFINED: DELPHI64'}{$ENDIF}

{$IFDEF DELPHI64}{$DEFINE USE_ASM_INTERLOCKED_FUNCTIONS}{$ENDIF}
{$IFDEF USE_ASM_INTERLOCKED_FUNCTIONS}{$MESSAGE 'DEFINED: USE_ASM_INTERLOCKED_FUNCTIONS'}{$ENDIF}


{$IFDEF CPUX86}{$DEFINE XE2}{$ENDIF}
{$IFDEF CPUX64}{$DEFINE XE2}{$ENDIF}
{$IFDEF MACOS}{$DEFINE XE2}{$ENDIF}
{$IFDEF XE2}{$MESSAGE 'DEFINED: XE2'}{$ENDIF}

{$IFDEF MACOS}{$DEFINE USE_ALTERNATE_DANGER_LOCK}{$DEFINE NO_INIT}{$ENDIF}

{$IFDEF USE_ALTERNATE_DANGER_LOCK}{$MESSAGE 'DEFINED: USE_ALTERNATE_DANGER_LOCK'}{$ENDIF}
{$IFDEF NO_INIT}{$MESSAGE 'DEFINED: NO_INIT'}{$ENDIF}

{$IFNDEF INLINE_DEBUG_DISABLE}
  {$IFDEF SUPPORTS_INLINE}
    {$DEFINE ALLOW_INLINE}
  {$ENDIF}
{$ENDIF}
{$IFDEF ALLOW_INLINE}{$MESSAGE 'DEFINED: ALLOW_INLINE'}{$ENDIF}

//use the ASM version of MoveMem
//if we're on x86 (including on earlier compilers
//before the CPUX86 directive was introduced
{$IFNDEF XE2}{$DEFINE USE_ASM_MOVEMEM}{$ENDIF}
{$IFDEF CPUX86}{$DEFINE USE_ASM_MOVEMEM}{$ENDIF}
{$IFDEF CPUX64}{$DEFINE USE_ASM_MOVEMEM}{$ENDIF}
//--------------------------------------------
{$IFDEF USE_ASM_MOVEMEM}{$MESSAGE 'DEFINED: USE_ASM_MOVEMEM'}{$ENDIF}


{$IFNDEF ALLOW_INLINE}
{$DEFINE NOINLINE}
{$ENDIF}

{$IFDEF NOINLINE}{$MESSAGE 'DEFINED: NOINLINE'}{$ENDIF}

{$DEFINE ENABLE_ON_SINGLE_CPU}
                        //  Enables the memory manager even if the machine only
                        //  has 1 CPU.  BigBrain is designed only for multi-
                        //  processor/multi-core machines.  Enabling on a single
                        //  CPU will actually hurt performance
{$IFDEF ENABLE_ON_SINGLE_CPU}{$MESSAGE 'DEFINED: ENABLE_ON_SINGLE_CPU'}{$ENDIF}

{x$DEFINE LITEMODE}      // Don't enable this.  Disables most memory-manager
                        // operation in favor of a simple round-robin heap
                        // system.  This "Lite" mode however, can be used
                        // as an underlying layer, which is useful.
{$IFDEF LITEMODE}{$MESSAGE 'DEFINED: LITEMODE'}{$ENDIF}

{$DEFINE USELITEASOS}   // WARNING* Requires Windows 2000 SP4 or later!
                        // Uses the above mentioned "Lite mode" to supply
                        // new blocks of memory.  This layer sits BELOW the bulk
                        // of the Big Brain functionality.  And replaces operations
                        // that were left to the Delphi standard memory maanger
                        // in version 1.0
{$IFDEF USELITEASOS}{$MESSAGE 'DEFINED: USELITEASOS'}{$ENDIF}

//NOTE! Not all of these combinations are valid.  Use at your own risk.
//Some valid combinations
//  USELITEASOS+LOOK_ASIDE
//  USELITEASOS+LOOK_ASIDE+LITE_EXTERNAL_LOCKS
//  USELITEASOS+LOW_FRAG



{$DEFINE LOOK_ASIDE}    // [on] Faster than LOW_FRAG (LOW_FRAG must be disabled
                        // when this is enabled).  But doesn't provide
                        // extra support for reducing memory fragmentation.
{$IFDEF LOOK_ASIDE}{$MESSAGE 'DEFINED: LOOK_ASIDE'}{$ENDIF}

{xDEFINE LOW_FRAG}      // [on] Uses slightly slower OS Heaps that can reduce
                        // fragmentation.  You MUST disable LOOK_ASIDE
                        // when enabling this
{$IFDEF LOW_FRAG}{$MESSAGE 'DEFINED: LOW_FRAG'}{$ENDIF}


{x$DEFINE LITE_EXTERNAL_LOCKS}
                        // If [on] BigBrain manages the locks on windows heap
                        // allocations in the "Lite" layer.  If [off] the heaps
                        // are created with
                        // the OS serialization flags which are faster according
                        // to my tests.
                        // Do NOT enable this in conjunction with LOW_FRAG
{$IFDEF LITE_EXTERNAL_LOCKS}{$MESSAGE 'DEFINED: LITE_EXTERNAL_LOCKS'}{$ENDIF}

{x$DEFINE DISABLE_ALL}   // If [on] the memory manager will not be installed.
                        // useful for comparing against the native Borland MM.
                        // Although note that the cleanup thread will still run.
{$IFDEF DISABLE_ALL}{$MESSAGE 'DEFINED: DISABLE_ALL'}{$ENDIF}

{xDEFINE EXTRA_ERR_CHECKING}
                        //Reenables try..finally blocks that are removed in
                        //many locations.  It turns out that try..finally
                        //adds additional instructions can really add up when
                        //it is used as often as in a memory manager
                        //recommend DISABLED.  When disabled, performance boost
                        //will result from the removing try..finally blocks in
                        //low-risk functions.
{$IFDEF EXTRA_ERR_CHECKING}{$MESSAGE 'DEFINED: EXTRA_ERR_CHECKING'}{$ENDIF}

{x$DEFINE USE_SPINLOCK}
{$IFDEF USE_SPINLOCK}
{$DEFINE SpinLock}
{$DEFINE ASM}
{$IFDEF SpinLock}{$MESSAGE 'DEFINED: SpinLock'}{$ENDIF}

{x$DEFINE DynamicSleep}
{$IFDEF DynamicSleep}{$MESSAGE 'DEFINED: DynamicSleep'}{$ENDIF}

{$ENDIF}
{$IFDEF MACOS}
  {$DEFINE LINUX}
{$ENDIF}

{$IFDEF LINUX}{$MESSAGE 'DEFINED: LINUX'}{$ENDIF}

interface


{$DEFINE DISABLE_MASTER_KEYWORDS}
{$UNDEF DISABLE_INTERFACE}
  {$I 'BigBrainUltra2013.pas'}
{$DEFINE DISABLE_INTERFACE}


implementation

{$UNDEF DISABLE_IMPLEMENTATION}
  {$I 'BigBrainUltra2013.pas'}
{$DEFINE DISABLE_IMPLEMENTATION}

initialization
{$UNDEF DISABLE_INITIALIZATION}
  {$I 'BigBrainUltra2013.pas'}
{$DEFINE DISABLE_INITIALIZATION}

finalization
{$UNDEF DISABLE_FINALIZATION}
  {$I 'BigBrainUltra2013.pas'}
{$DEFINE DISABLE_FINALIZATION}


{$MESSAGE '*********************************************************************'}
{$MESSAGE 'BigBrain-------------------------------------------------------------'}
{$MESSAGE 'End of BigBrain compilation -----------------------------------------'}
{$MESSAGE '*********************************************************************'}

end.




