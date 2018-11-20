unit html_feeder_compiler;

interface

uses compiler_feeder, namevaluepair, sysutils, stringx, stringx.ansi;

type
  THTMLContext =
    (
      htmlcWhiteSpace,
      htmlcComment,
      htmlcJavaScript,
      htmlcMothershipScript,
      htmlcTagString,
      htmlcJavascriptString,
      htmlcTag
    );

  TTagINfo = class(TObject)
  private
    FName: ansistring;
    FAttributes: TNameValuePairList;
    FAutoClosed: boolean;
    procedure SetName(const Value: ansistring);
  public
    constructor create; reintroduce; virtual;
    destructor destroy;override;
    property Name: ansistring read FName write SetName;
    property Attributes: TNameValuePairList read FAttributes;
    function TagText: ansistring;
    property AutoClosed: boolean read FAutoClosed write FAutoClosed;
    procedure Init;
  end;


  THTMLPromoter = class(TCompilerFeeder)
  private
    FLastTag: TTagInfo;

  //promotes stupid lame fixes to bad IE code
  public
    constructor create;override;
    destructor destroy;override;
    procedure Process;
    procedure ProcessTag(parentcontext: THTMLContext);
    procedure ProcessStyleTag(parentcontext: THTMLContext);
    procedure ProcessHTMLComment(parentcontext: THTMLContext);
    procedure ProcessXMLComment(parentcontext: THTMLContext);
    property LastTag: TTagInfo read FLastTag;
    procedure OnPromoteTag(var bHandled: boolean);virtual;
    procedure PromoteTag;

  end;



implementation

{ TTagINfo }

constructor TTagINfo.create;
begin
  FAttributes := TNameValuePairList.create;


end;

destructor TTagINfo.destroy;
begin
  FAttributes.free;
  inherited;
end;



procedure TTagINfo.Init;
begin
  Name := '';
  AutoClosed := false;
  Attributes.clear;
end;

procedure TTagINfo.SetName(const Value: ansistring);
begin
  FName := Value;
end;

function TTagINfo.TagText: ansistring;
var
  t: integer;
  nvp: TNameValuePair;
begin
  result := '<'+Name;

  for t:= 0 to Attributes.count-1 do begin
    nvp := Attributes.itemsbyindex[t];
    if nvp.Value = '*FLAG*' then begin
      result := result + ' '+ nvp.Name;
    end else begin
      if (nvp.value = '') or (nvp.value[1]<>'"') then begin
        nvp.value := '"'+nvp.value+'"';
      end;
      result := result+' '+nvp.Name+'='+nvp.Value;
    end;
  end;

  if AutoClosed then
    result := result +' />'
  else
    result := result + '>';


end;

{ THTMLPromoter }

constructor THTMLPromoter.create;
begin
  inherited;
  FLastTag := TTagInfo.create;

  symbols := ['<','>','=',' ',#13, #10];
  identifiers := ['a'..'z','A'..'Z','0'..'9','_'];
  whitespaces := [' ',#13, #10, #9];

end;

destructor THTMLPromoter.destroy;
begin
  FLastTag.free;
  inherited;
end;

procedure THTMLPromoter.OnPromoteTag(var bHandled: boolean);
begin

//TODO -cunimplemented: unimplemented block
end;

procedure THTMLPromoter.Process;
begin
  //scan through html
  while not eof do begin
    //if <!-- then parse as html comment
    if self.GetSurrounding(3) = '<!--' then begin
      self.ProcessHTMLComment(htmlcWhiteSpace);
    end else
    //if <!-- then parse as html comment
    if self.GetSurrounding(1) = '<!' then begin
      self.ProcessXMLComment(htmlcWhiteSpace);
    end else
    //if < then parse tag and attributes (subroutine)
    if self.GetSurrounding(0) = '<' then begin
      self.ProcessTag(htmlcWhiteSpace);
    end else
    //else passthrough
    begin
      self.AddToResult(GetSurrounding(0));
      self.Move(1);
    end;
  end;


end;

procedure THTMLPromoter.ProcessHTMLComment(parentcontext: THTMLContext);
begin
  while (not eof) and (GetSurrounding(-2) <> '-->') do begin
     AddToREsult(GetSurrounding(0));
     move(1);
  end;
end;

procedure THTMLPromoter.ProcessStyleTag(parentcontext: THTMLContext);
begin
  AddToResult(LastTag.TagText);
  repeat
     AddToREsult(GetSurrounding(0));
     move(1);
  until eof or (lowercase(GetSurrounding(-7)) = '</style>');

end;

procedure THTMLPromoter.ProcessTag(parentcontext: THTMLContext);
var
  sThis: ansistring;
  istep: integer;
  s: ansistring;
  sTagName, sAttributeName, sAttributeVAlue: ansistring;
  poo: ansistring;
  bInQuotes: boolean;
const
  step_before_tag_name = 0;
  step_tag_name = 1;
  step_attribute_name = 2;
  step_attribute_value = 3;
  step_attribute_expect_equal = 4;
begin
  lastTag.Init();
  lastTag.Attributes.Clear;
  istep := 0;
  //scan through Html until > is found in tag context
  while not eof do begin
    case iStep of
      //---------------------
      step_before_tag_name:
      begin
        move(1);
        iStep := step_tag_name;
      end;
      //---------------------
      step_tag_name:
      begin
        sTagName := self.GetCurrentWord;
        LastTag.Name := sTagName;
        iStep := step_attribute_name;

      end;
      //---------------------
      step_attribute_name:
      begin
        MoveThroughWhiteSpace();
        poo := GetSurrounding(0);
        if poo='>' then begin
          move(1);
          break;

        end;

        if poo='/' then begin
          LastTag.AutoClosed := true;
        end;

        sAttributeName := self.GetCurrentWord;
        iStep := step_attribute_expect_equal;
      end;

      //---------------------
      step_attribute_value:
      begin
        sAttributeValue := GetcurrentWord;
        LastTag.Attributes.Add(sAttributeName, unquote(sAttributeValue));
        iStep := step_attribute_name;
      end;

      //---------------------
      step_attribute_expect_equal:
      begin
        poo := GetSurrounding(0);
//        if poo='>' then begin
//          move(1);
//          break;
//
//        end;

        if poo='/' then begin
          LastTag.AutoClosed := true;
          move(1);
        end;

        if  poo = '=' then begin
          move(1);
          iStep := step_attribute_value;
        end else
        if poo = ' ' then begin
          move(1);
        end else
        if (poo[1] in ['>','<', 'a'..'z', 'A'..'Z', '_', '-', '0'..'9']) then begin
          sAttributeValue:='*FLAG*';
          move(1);
          LastTag.Attributes.Add(sAttributeName, sAttributeValue);
          iStep := step_attribute_name;
          if poo='>' then break;

        end else begin
          move(1);
        end;
      end
    end;

  end;

  if (lowercase(LastTag.Name)='style') and (not LastTag.AutoClosed) then
    ProcessStyleTag(htmlcWhiteSpace)
  else
    PromoteTag;

end;

procedure THTMLPromoter.ProcessXMLComment(parentcontext: THTMLContext);
begin
  repeat
     AddToREsult(GetSurrounding(0));
     move(1);
  until (not eof) and (GetSurrounding(0) <> '>');


end;

procedure THTMLPromoter.PromoteTag;
var
  bHandled: boolean;
begin
  OnPromoteTag(bHandled);
  if not bHandled then
    self.AddToResult(LastTag.TagText);

end;

end.
