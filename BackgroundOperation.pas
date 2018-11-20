unit BackgroundOperation;

interface

uses
  typex,
{x$DEFINE USE_ANON_THREAD}
{$IFDEF USE_ANON_THREAD}
  anonthread,
{$ELSE}
  anoncommand,
{$ENDIF}
  systemx;

type
  {$IFDEF USE_ANON_THREAD}
    TAnonBackground<T> = class(TAnonymousThread<T>);
  {$ELSE}
    TAnonBackground<T> = class(TAnonymousCommand<T>);
  {$ENDIF}


implementation

end.
