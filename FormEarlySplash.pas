unit FormEarlySplash;

interface

uses
  orderlyinit, classes;


implementation

uses
  FormSplash;

procedure oinit;
begin
  Showsplash;
end;

procedure oafterinit;
begin
  HideSplash;
end;


initialization
  init.registerprocs('FormEarlySplash', oinit, oafterinit, nil, nil, nil, '');





end.
