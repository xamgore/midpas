unit Sens;

interface
	{ add public declarations here }

type
     Touch=record
     x,y:integer;
     end;

//загрузка сенсора
procedure InitSens;

//новое отпускание нажатия
Function NewTouchR(p:Touch):boolean;

//возвращает нажатую кнопку
Function Touched(p:touch):boolean;

procedure UPD;

//обновление экрана с написанием fps
function repaintFPS:integer;

implementation
	{ add unit functions & procedures here }
uses sensor;

var  R:Touch;       //отпуcтили
     D:Touch;       //нажато в данный момент

//новое отпускание нажатия
Function NewTouchR(p:Touch):boolean;
begin
NewTouchR:=false;
if (pointer_released_x<>R.x) or (pointer_released_y<>R.y) then
  begin
  NewTouchR:=true;
  R.x:=pointer_released_x;
  R.y:=pointer_released_y;
  end;
P.x:=R.x;
P.y:=R.y;
end;

//возвращает нажатую кнопку
Function Touched(p:touch):boolean;
begin
Touched:=false;
if (pointer_dragged_x<>D.x) or (pointer_dragged_y<>D.y) then
  begin
  Touched:=true;
  D.x:=pointer_dragged_x;
  D.y:=pointer_dragged_y;
  end;
P.x:=D.x;
P.y:=D.y;
end;

procedure UPD;
begin
R.x:=pointer_released_x;
R.y:=pointer_released_y;
D.x:=pointer_dragged_x;
D.y:=pointer_dragged_y;
end;

procedure InitSens;
begin
Sensor.Init;
end;


var FP,n,t,TOld:integer;

function repaintFPS:integer;
var Rel:integer;
begin
Rel:=GetRelativeTimeMS;
if Rel-t>1000 then
  begin
  t:=Rel;
  Fp:=n;
  n:=-1;
  end;
n:=n+1;
repaintFPS:=Rel-Told;
Told:=Rel;
//setcolor(0,0,0);
//drawtext(IntegerToString(FP),0,0);
repaint;
end;

initialization
	{ add initialization code here }
t:=GetRelativeTimeMS;		
TOld:=t;
end.
 