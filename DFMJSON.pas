unit DFMJSON;

interface
uses
   Classes,qjson,encoding;


function Dfm2JSON(dfm: TStream): tqjson; overload;
function Dfm2JSON(const filename: string): tqjson; overload;
function DfmBin2JSON(dfm: TStream): tqjson; overload;
function DfmBin2JSON(const filename: string): tqjson; overload;

procedure SaveJSON2Dfm(json: tqjson; const filename: string);
function JSON2Dfm(json: tqjson): string;

implementation
uses
  SysUtils,
  StrUtils,
  RTLConsts,
  Clipbrd,Variants;

function ConvertOrderModifier(parser: TParser): Integer;
begin
  Result := -1;
  if Parser.Token = '[' then
  begin
    Parser.NextToken;
    Parser.CheckToken(toInteger);
    Result := Parser.TokenInt;
    Parser.NextToken;
    Parser.CheckToken(']');
    Parser.NextToken;
  end;
end;

function ConvertHeader(parser: TParser; IsInherited, IsInline: Boolean): tqjson;
var
  ClassName, ObjectName: string;
  Flags: TFilerFlags;
  Position: Integer;
begin
  Parser.CheckToken(toSymbol);
  ClassName := Parser.TokenString;
  ObjectName := '';
  if Parser.NextToken = ':' then
  begin
    Parser.NextToken;
    Parser.CheckToken(toSymbol);
    ObjectName := ClassName;
    ClassName := Parser.TokenString;
    Parser.NextToken;
  end;
  Flags := [];
  Position := ConvertOrderModifier(parser);
  result := tqjson.Create;
  try
    if IsInherited then
      result.add('$Inherited', true);
    if IsInline then
      result.add('$Inline', true);
    if Position >= 0 then
      result.add('$ChildPos', position);
    result.add('$Class', ClassName);
    if ObjectName <> '' then
      result.add('$Name', ObjectName);
  except
    result.Free;
    raise;
  end;
end;

procedure ConvertProperty(parser: TParser; obj: tqjson); forward;

procedure  ConvertValue(JObj:TQJson ;aname:string;parser: TParser);
var
  Order: Integer;
  arr,ajobj: tQJson;
  sub: tQJson;
  TokenStr: string;


  function CombineString: String;
  begin
    Result := Parser.TokenWideString;
    while Parser.NextToken = '+' do
    begin
      Parser.NextToken;
      if not (Parser.Token in  [Classes.toString, toWString]) then
        Parser.CheckToken(Classes.toString);
      Result := Result + Parser.TokenWideString;
    end;
  end;

begin
  if Parser.Token in  [Classes.toString, toWString] then
  begin
    JObj.AddVariant(aname, QuotedStr(CombineString) );
  end
  else
  begin
    case Parser.Token of
      toSymbol:
      begin
        tokenStr := Parser.TokenComponentIdent;
        if tokenStr = 'True' then
          JObj.Add(aname,true)
        else if tokenStr = 'False' then
          JObj.Add(aname,false)
        else JObj.Add(aname, Parser.TokenComponentIdent);
      end;
      toInteger:
      begin
       JObj.Add(aname, Parser.TokenInt)
      end;
      toFloat:
      begin
        ajobj := TQJson.create;
        if parser.FloatType = #0 then
           TQJson(ajobj).add('$float', 0) //null
        else  TQJson(ajobj).add('$float', Parser.FloatType);
         TQJson(ajobj).add('value', Parser.TokenFloat);
        JObj.Add(aname,ajobj)
      end;
      '[':
      begin
        ajobj := tqjson.Create;
        tqjson(ajobj).add('$set', true);
        arr := tqjson(ajobj).AddArray('value');
        Parser.NextToken;
        if Parser.Token <> ']' then
          while True do
          begin
            TokenStr := Parser.TokenString;
            case Parser.Token of
              toInteger: begin end;
              toString,toWString: TokenStr := '#' + IntToStr(Ord(TokenStr[1]));
            else
              Parser.CheckToken(toSymbol);
            end;
            arr.Add('',TokenStr);
            if Parser.NextToken = ']' then Break;
            Parser.CheckToken(',');
            Parser.NextToken;
          end;
        JObj.Add(aname,ajobj);
      end;
      '(':
     begin
        Parser.NextToken;
        ajobj := tqjson.Create;
        ajobj.datatype:=jdtArray;
        while Parser.Token <> ')' do
          ConvertValue(ajobj,'',parser);
        JObj.Add(aname,ajobj)
//           ConvertValue(parser);
      end;
      '{':
      begin
        Parser.NextToken;
        ajobj := tqjson.Create;

        tqjson(ajobj).add('$hex', true);
        tokenStr := '';
        while Parser.Token <> '}' do
        begin
           tokenStr := tokenStr + parser.TokenString;
           parser.NextToken;
        end;
        tqjson(ajobj).add('value', tokenStr);
        JObj.Add(aname,ajobj) ;
      end;
      '<':
      begin
        Parser.NextToken;
        ajobj := tqjson.Create;
        tqjson(ajobj).add('$collection', true);
        arr := ajobj.addArray('values');
        while Parser.Token <> '>' do
        begin
          Parser.CheckTokenSymbol('item');
          Parser.NextToken;
          Order := ConvertOrderModifier(parser);
          sub := tqjson.Create;
          if Order <> -1 then
             sub.add('$order',order);
          while not Parser.TokenSymbolIs('end') do
             ConvertProperty(parser, sub);
          arr.Add(sub);
          Parser.NextToken;
        end;
        JObj.Add(aname,ajobj)

      end;
      else begin
        Parser.Error(SInvalidProperty);

      end;
    end;
    Parser.NextToken;
  end;
end;

procedure ConvertProperty(parser: TParser; obj: tqjson);
var
  PropName: string;
begin
  Parser.CheckToken(toSymbol);
  PropName := Parser.TokenString;
  Parser.NextToken;
  while Parser.Token = '.' do
  begin
    Parser.NextToken;
    Parser.CheckToken(toSymbol);
    PropName := PropName + '.' + Parser.TokenString;
    Parser.NextToken;
  end;
  Parser.CheckToken('=');
  Parser.NextToken;

  ConvertValue(obj,propName,parser);
end;

function ConvertObject(parser: TParser): tqjson;
var
  InheritedObject: Boolean;
  InlineObject: Boolean;
  children: tqjson;
begin
  InheritedObject := False;
  InlineObject := False;
  if Parser.TokenSymbolIs('INHERITED') then
    InheritedObject := True
  else if Parser.TokenSymbolIs('INLINE') then
    InlineObject := True
  else
    Parser.CheckTokenSymbol('OBJECT');
  Parser.NextToken;
  result := ConvertHeader(parser, InheritedObject, InlineObject);
  while not Parser.TokenSymbolIs('END') and
    not Parser.TokenSymbolIs('OBJECT') and
    not Parser.TokenSymbolIs('INHERITED') and
    not Parser.TokenSymbolIs('INLINE') do
    ConvertProperty(parser, result);
  children := result.AddArray('$Children');
  while not Parser.TokenSymbolIs('END') do
     children.Add(ConvertObject(parser));

  Parser.NextToken;
end;

function Dfm2JSON(dfm: TStream): tqjson;
var
  parser: TParser;
begin
  parser := TParser.Create(dfm);
  try
    result := ConvertObject(parser);
  finally
    parser.Free;
  end;
end;

function Dfm2JSON(const filename: string): tqjson;
var
  stream: tMemorystream;
begin
  stream := tMemorystream.Create;
  stream.LoadFromFile(fileName);
  stream.position:=0;
  try
    result := Dfm2JSON(stream);
  finally
    stream.Free;
  end;
end;

function DfmBin2JSON(dfm: TStream): TQJson;
var
   outStream: TStringStream;
begin
  outStream := TStringStream.Create('');
  try
    Classes.ObjectBinaryToText(dfm, outStream);
    result := Dfm2JSON(outStream);
  finally
    outStream.Free;
  end;
end;

function DfmBin2JSON(const filename: string): tqjson;
var
  stream: tMemorystream;
begin
  stream := tMemorystream.Create;
  stream.LoadFromFile(fileName);
  try
    result := DfmBin2JSON(stream);
  finally
    stream.Free;
  end;
end;

procedure SaveJSON2Dfm(json: tqjson; const filename: string);

begin
  savetoFile(JSON2DFM(json),fileName);


end;

//------------------------------------------------------------------------------

function IndentStr(depth: integer): string;
begin
  result := StringOfChar(' ', depth * 2);
end;

procedure WriteJSONObject(json: TQJson; sl: TStringList; indent: integer); forward;
procedure WriteJSONProperty(const name: string; value: tqjson; sl: TStringList; indent: integer); forward;

function capitalize(value: string): string;
begin
  value[1] := UpCase(value[1]);
  result := value;
end;

procedure WriteJSONArrProperty(const name: string; value: tqjson; sl: TStringList; indent: integer);

  function GetString(value: variant): string;
  begin

    if (value=true ) or (value =False ) then
      result := capitalize(value.Value)
    else
      result := value.Value;
  end;

var
  i: integer;
  line: string;
begin
  sl.Add(IndentStr(indent) + format('%s = (', [name]));
  for i := 0 to value.Count - 1 do
  begin

    line := IndentStr(indent + 1) + value.Items[i].asstring;
    if i = value.Count - 1 then
    line := line + ')';
    sl.Add(line);
  end;
end;

procedure WriteFloatProperty(const name: string; value: TQJson; sl: TStringList; indent: integer);
var
   fValue: double;
  strFloat:string;
  num: double;
  numVal: string;
begin
 
  fValue :=value.ItemByName('value').asfloat;
  num := fvalue;
  strFLoat:=lowercase(value.ItemByName('value').AsString);
  if (frac(num) = 0.0) and  (pos('e',strFloat)=0) then  // (IndexOfAny(['e', 'E']) = -1) then
    numVal := strFloat + '.000000000000000000'
  else numval :=strFloat;
  { TODO -oLG : Have not yet discerned the purpose of this code, presently running fine without it }
(*
  if float.ValueType = jvtUndefined then
    sl.Add(IndentStr(indent) + format('%s = %s', [name, numVal]))
  else *)sl.Add(IndentStr(indent) + format('%s = %s', [name, numVal + strFLoat]));
end;

procedure WriteSetProperty(const name: string; value: tqjson; sl: TStringList; indent: integer);
var
  i: integer;
  line: string;
  sub: tqjson;
begin
  line := '';
  sub := value.itembyname('value');
  for i := 0 to sub.Count - 1 do
  begin
    if line = '' then
      line := sub.Items[i].Value
    else line := format('%s, %s', [line, sub.Items[i].Value]);
  end;
  sl.Add(IndentStr(indent) + format('%s = [%s]', [name, line]));
end;

procedure WriteHexProperty(const name: string; value: TQJson; sl: TStringList; indent: integer);
var
  hex, line: string;
begin
  sl.Add(IndentStr(indent) + format('%s = {', [name]));
  hex := value.itembyname('value').asstring;  { malkovich.Malkovich['Malkovich'].Malkovich }
  while hex <> '' do
  begin
    line := Copy(hex, 0, 64);
    delete(hex, 1, 64);
    if hex = '' then
      line := line + '}';
    sl.Add(IndentStr(indent + 1) + line);
  end;
end;

procedure WriteCollectionItem(value: tqjson; sl: TStringList; indent: integer);
var
  i: integer;
  name : string;
  
begin
  if Assigned(value.itembyname('$order')) then
    sl.Add(IndentStr(indent) + format('item [%d]', [value.itembyname('$order').AsInteger]))
  else sl.Add(IndentStr(indent) + 'item');

  for i := 0 to value.Count - 1 do begin
    name := value[i].Name;
    if not (name[1]='$') then
      WriteJSONProperty(name, value[i].ItemByName(name), sl, indent + 1);
  end;
  sl.Add(IndentStr(indent) + 'end');
end;

procedure WriteCollection(const name: string; value: tqjson; sl: TStringList; indent: integer);
var
  values: tqjson;
  sub: tqjson;
begin
  sl.Add(IndentStr(indent) + format('%s = <', [name]));
  values := value.itembyname('values');
  for sub in values do
    WriteCollectionItem(sub, sl, indent + 1);
  sl[sl.Count - 1] := sl[sl.Count - 1] + '>';
end;

procedure WriteJSONObjProperty(const name: string; value: TQJson; sl: TStringList; indent: integer);
begin
  if assigned(value.itembyname('$float')) then
    WriteFloatProperty(name, value, sl, indent)
  else if assigned(value.itembyname('$set')) then
    WriteSetProperty(name, value, sl, indent)
  else if assigned(value.itembyname('$hex')) then
    WriteHexProperty(name, value, sl, indent)
  else if assigned(value.itembyname('$collection')) then
    WriteCollection(name, value, sl, indent)
  else
    asm int 3 end;
end;

function StringNeedsWork(const str: string): boolean;
begin
  result := (Length(str) > 66) or ( (Pos(#13,str)>0) or (Pos(#10,str)>0));
end;

function DfmQuotedStr(const value: string): string;
const separators: array[0..2] of string = (#13#10, #13, #10);
var
  lines: tstringList;
  i: integer;
begin
  try
    lines:=tstringlist.create;
    lines.text :=value;
    result:='';
    for i := 0 to lines.Count-1 do
    begin
      if lines.strings[i] <> '' then
      begin
        lines.strings[i] := QuotedStr(lines[i]);
        result:=result+lines.Strings[i]+'#13'
      end;
    end;
    if Result<>'' then
      result:=Copy(Result,1,length(result)-1);
  finally
    if Assigned(lines) then
    begin
      lines.clear;
      lines.free;
    end;
  end;
end;

procedure WriteStringProperty(const name: string; value: variant; sl: TStringList; indent: integer);
var
  str, sub: string;
begin
  str := value.Value;
  if (str[1]='''')  and StringNeedsWork(str) then //66 = 64 limit + 2 quotes
  begin
    str := AnsiDequotedStr(str, '''');
    sl.Add(IndentStr(indent) + format('%s = ', [name]));
    while length(str )> 0 do
    begin
      sub := DfmQuotedStr(Copy(str, 0, 64));
      delete(str, 1, 64);
      if str <> '' then
        sub := sub + ' +';
      sl.Add(IndentStr(indent + 1) + sub);
    end;
  end
  else sl.Add(IndentStr(indent) + format('%s = %s', [name, str]));
end;

procedure WriteJSONProperty(const name: string; value: tqjson; sl: TStringList; indent: integer);
begin

    if  (TQJson(value).datatype=jdtarray) then
      WriteJSONArrProperty(name, value, sl, indent)
    else if value.DataType=jdtObject then
      WriteJSONObjProperty(name, tqjson(value), sl, indent)

  else if value.DataType <>jdtBoolean  then
    WritestringProperty(name, value.ItemByName(name).asstring, sl, indent)
  else if (value.DataType=jdtInteger) or (value.DataType=jdtfloat) then
    sl.Add(IndentStr(indent) + format('%s = %s', [name, value.ItemByName(name).asstring]))
  else if value.DataType=jdtBoolean then
    sl.Add(IndentStr(indent) + format('%s = %s', [name, capitalize(booltostr(value.ItemByName(name).AsBoolean,true))]))
  else
    assert(False);
end;

procedure WriteJSONProperties(json: tqjson; sl: TStringList; indent: integer);
var
  i: integer;
  name: string;
  children : tqjson;

begin
  for i := 0 to json.Count - 1 do
  begin
    name := json[i].name;
    if not (name[1]='$') then
      WriteJSONProperty(name, json.ItemByName(name), sl, indent);
  end;

  children := json.itembyname('$Children') ;
  if Assigned(children) then
    for  i:=0 to children.count do
    begin
      WriteJSONObject(children[i], sl, indent);
    end;
end;

procedure WriteJSONObject(json: tqjson; sl: TStringList; indent: integer);
var
  dfmType, name, cls, header: string;
begin
  if Assigned(json.itembyname('$Inherited')) then
    dfmType := 'inherited'
  else if Assigned(json.itembyname('$Inline')) then
    dfmType := 'inline'
  else dfmType := 'object';
  if Assigned(json.itembyname('$Name')) then
     name := json.itembyname('$Name').asstring
  else name := '';
  cls := json.itembyname('$Class').asstring;
  if name = '' then
    header := format('%s %s', [dfmType, cls])
  else header := format('%s %s: %s', [dfmType, name, cls]);
  if Assigned(json.itembyname('$ChildPos')) then
    header := format('%s [%d]', [header, json.itembyname('$ChildPos').AsInteger]);
  sl.Add(indentStr(indent) + header);
  WriteJSONProperties(json, sl, indent + 1);
  sl.add(indentStr(indent) + 'end');
end;

function JSON2Dfm(json: tqjson): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create();
  try
    WriteJSONObject(json, sl, 0);
    result := sl.Text;
  finally
    sl.free;
  end;
end;

end.
