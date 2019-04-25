unit encoding;
interface
 uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
 Controls, Forms, Dialogs, ComCtrls, Menus, StdCtrls;
Type
TEncodeFlags = (efUnknown, efAnsi, efUnicode, efUncodeBigEn, efUTF8);
TTextFormat=(tfAnsi,tfUnicode,tfUnicodeBigEndian,tfUtf8);
TUTF8Falg = packed record
   EF, BB, BF: Byte;
 end;
const
TextFormatFlag:array[tfAnsi..tfUtf8] of word=($0000,$FFFE,$FEFF,$EFBB);
Encode: TUTF8Falg = (EF: $EF; BB: $BB; BF: $BF);
   function ChWideToAnsi(const StrW: WideString): AnsiString;
   function ChAnsiToWide(const StrA: AnsiString): WideString;
   function UTF8ToWideString(const Stream: TStream): WideString;
   procedure TextToUTF8Stream(const Text: string; var Stream: TStream);

   function GetEncodeFromStream(const Stream: TStream): TEncodeFlags;
    
   function loadFromFile(const fName :string) :String;
   procedure savetoFile(const txt, fName :String);
   procedure wideTextToUTF8Stream(const Text: WideString; var Stream: TStream);
implementation
function loadFromFile(const fName :String) :String;
var
  FStream :TStream;
begin
    FStream := TMemoryStream.Create;  
 TMemoryStream(FStream).LoadFromFile(fName);
 if GetEncodeFromStream(FStream) = efUTF8 then
 begin
   result := ChWideToAnsi(UTF8ToWideString(FStream));
 end;
end;
procedure savetoFile(const txt, fName :String);
var
  FStream :TStream;
begin
    FStream := TMemoryStream.Create;  
  try
    TextToUTF8Stream(txt, FStream);
    TMemoryStream(FStream).SaveToFile(fName);
  finally
    FStream.Size := 0;
  end;
end;
procedure WordLoHiExchange(var w:Word);
var
b:Byte;
begin
  b:=WordRec(w).Lo;
  WordRec(w).Lo:=WordRec(w).Hi;
  WordRec(w).Hi:=b;
end;
    
function ChWideToAnsi(const StrW: WideString): AnsiString;
var
 nLen: integer;
begin
 Result := StrW;
 if Result <> '' then
 begin
   nLen := WideCharToMultiByte(936, 624, @StrW[1], -1, nil, 0, nil, nil);
   SetLength(Result, nLen - 1);
   if nLen > 1 then
     WideCharToMultiByte(936, 624, @StrW[1], -1, @Result[1], nLen - 1, nil, nil);
 end;
end;
function ChAnsiToWide(const StrA: AnsiString): WideString;
var
 nLen: integer;
begin
 Result := StrA;
 if Result <> '' then
 begin
   nLen := MultiByteToWideChar(936, 1, PChar(@StrA[1]), -1, nil, 0);
   SetLength(Result, nLen - 1);
   if nLen > 1 then
     MultiByteToWideChar(936, 1, PChar(@StrA[1]), -1, PWideChar(@Result[1]), nLen - 1);
 end;
end;
function UTF8ToWideString(const Stream: TStream): WideString;
var
 nLen: Cardinal;
begin
 try
   SetLength(Result, Stream.Size div SizeOf(WideChar) * 3);
   nLen := Utf8ToUnicode(@Result[1], Length(Result),
     Pointer(DWord(TMemoryStream(Stream).Memory) + Stream.Position),
     Stream.Size - Stream.Position);
   SetLength(Result, nLen);
 except
   SetLength(Result, 0);
 end;
end;

procedure TextToUTF8Stream(const Text: string; var Stream: TStream);
var
 StringW, StrW: WideString;
 nLen: Cardinal;
begin
 try
   if Text <> '' then
   begin
     StrW := ChAnsiToWide(Text);
     nLen := Length(StrW) * 3;
     SetLength(StringW, nLen);
     nLen := UnicodeToUtf8(@StringW[1], nLen, @StrW[1], Length(StrW));
     SetLength(StringW, nLen);
     Stream.Write(Encode, SizeOf(Encode));
     Stream.Write(StringW[1], Length(StringW));
   end
   else
     Stream.Write(Encode, SizeOf(Encode));
 except
   SetLength(StrW, 0);
   SetLength(StringW, 0);
 end;
end;
procedure wideTextToUTF8Stream(const Text: WideString; var Stream: TStream);
var
 StringW, StrW: WideString;
 nLen: Cardinal;
begin
 try
   if Text <> '' then
   begin
     StrW := Text;
     nLen := Length(StrW) * 3;
     SetLength(StringW, nLen);
     nLen := UnicodeToUtf8(@StringW[1], nLen, @StrW[1], Length(StrW));
     SetLength(StringW, nLen);
     Stream.Write(Encode, SizeOf(Encode));
     Stream.Write(StringW[1], Length(StringW));
   end
   else
     Stream.Write(Encode, SizeOf(Encode));
 except
   SetLength(StrW, 0);
   SetLength(StringW, 0);
 end;
end;
function GetEncodeFromStream(const Stream: TStream): TEncodeFlags;
var
 FEncode: TUTF8Falg;
begin
 Result := efUnknown;
 Stream.Read(FEncode, SizeOf(FEncode));
 if (FEncode.EF = Encode.EF) and (FEncode.BB = Encode.BB)
   and (FEncode.BF = Encode.BF) then Result := efUTF8;
end;
end.
