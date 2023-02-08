unit HSV2RGB;

interface

Function RFromHSV(intH,intS,intV:integer):integer;
Function GFromHSV(intH,intS,intV:integer):integer;
Function BFromHSV(intH,intS,intV:integer):integer;

implementation

Function RFromHSV(intH,intS,intV:integer):integer;
var
   intHi,intMin,intMax,intReturn:integer;
begin
{Функция для получения красной компоненты из HSV цвета}
{intH - тон. [0..255]}
{intS - насыщенность. [0..255]}
{intV - яркость. [0..255]}
{ProgrammerForever (C) 2012}
{Автор - Боев Григорий}

      intHi:=(intH/60) mod 6;
      intMin:=intV*(255-intS)/255;
      intMax:=intV;

      if (intHi=0) Then intReturn:=intMax;
      if (intHi=1) Then intReturn:=intMax-(intMax-intMin)*(intH mod 60)/60;
      if (intHi=2) Then intReturn:=intMin;
      if (intHi=3) Then intReturn:=intMin;
      if (intHi=4) Then intReturn:=intMin+(intMax-intMin)*(intH mod 60)/60;
      if (intHi=5) Then intReturn:=intMax;

      if (intReturn>255) then intReturn:=255;
      if (intReturn<0)   then intReturn:=0;
      RFromHSV:=intReturn;
end;


Function GFromHSV(intH,intS,intV:integer):integer;
var
   intHi,intMin,intMax,intReturn:integer;
begin
{Функция для получения зелёной компоненты из HSV цвета}
{intH - тон. [0..255]}
{intS - насыщенность. [0..255]}
{intV - яркость. [0..255]}
{ProgrammerForever (C) 2012}
{Автор - Боев Григорий}

      intHi:=(intH/60) mod 6;
      intMin:=intV*(255-intS)/255;
      intMax:=intV;

      if (intHi=0) Then intReturn:=intMin+(intMax-intMin)*(intH mod 60)/60;
      if (intHi=1) Then intReturn:=intMax;
      if (intHi=2) Then intReturn:=intMax;
      if (intHi=3) Then intReturn:=intMax-(intMax-intMin)*(intH mod 60)/60;
      if (intHi=4) Then intReturn:=intMin;
      if (intHi=5) Then intReturn:=intMin;

      if (intReturn>255) then intReturn:=255;
      if (intReturn<0)   then intReturn:=0;
      GFromHSV:=intReturn;
end;


Function BFromHSV(intH,intS,intV:integer):integer;
var
   intHi,intMin,intMax,intReturn:integer;
begin
{Функция для получения синей компоненты из HSV цвета}
{intH - тон. [0..255]}
{intS - насыщенность. [0..255]}
{intV - яркость. [0..255]}
{ProgrammerForever (C) 2012}
{Автор - Боев Григорий}

      intHi:=(intH/60) mod 6;
      intMin:=intV*(255-intS)/255;
      intMax:=intV;

      if (intHi=0) Then intReturn:=intMin;
      if (intHi=1) Then intReturn:=intMin;
      if (intHi=2) Then intReturn:=intMin+(intMax-intMin)*(intH mod 60)/60;
      if (intHi=3) Then intReturn:=intMax;
      if (intHi=4) Then intReturn:=intMax;
      if (intHi=5) Then intReturn:=intMax-(intMax-intMin)*(intH mod 60)/60;

      if (intReturn>255) then intReturn:=255;
      if (intReturn<0)   then intReturn:=0;
      BFromHSV:=intReturn;
end;

end.