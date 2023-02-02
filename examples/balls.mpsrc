program Balls;

uses sens;
uses picker;



var GameResult:integer;

//_______________________________________________________________________________
//Работа с шариками!
//*******************************************************************************
type
				Ball=record
				tip:integer;
				pos:real;
				x,y:real;
				end;

				Point2i=record
				x,y:integer;
				end;

				Point2d=record
				x,y:real;
				end;
const
			  maxWay2=200;
			  maxBalls=200;
			  BallR=12;
			  BAllD=24;
var
		way:array [1..maxWay2] of point2d;      //путевые точки
    wayL:array [1..maxWay2] of Real;        //расстояния по точкам
		maxWay:integer;

    Balls:array [1..maxBalls] of Ball;
    BallsNum:integer;

    StringBalls:string;

function GetPointD(L:real):point2D;                                 //PUBLIC!!
var up,down,pos:integer;
    k:real;
begin
if (L>0) and (L<wayL[maxWay]) then
  begin
  up:=maxWay;
  down:=1;
  while (up>down+1) do
    begin
    pos:=(up+down) div 2;
    if wayL[pos]<L then
      down:=pos;
    else
      up:=pos;
    end;
  K:=L-wayL[down];
  K:=K/(wayL[up]-wayL[down]);
  GetPointD.x:=way[down].x+k*(way[up].x-way[down].x);
  GetPointD.y:=way[down].y+k*(way[up].y-way[down].y);
  end
else
  begin                             //TEST!!!!
  GetPointD.x:=0; //-D
  GetPointD.y:=0; //-D
  end;
end;

//меняет местами a и b
Procedure BallsSamena(a,b:integer);
var tip:integer;
    pos:real;
begin
tip:=Balls[a].tip; Balls[a].tip:=Balls[b].tip; Balls[b].tip:=tip;
pos:=Balls[a].pos; Balls[a].pos:=BAlls[b].pos; Balls[b].pos:=pos;
pos:=Balls[a].y; Balls[a].y:=BAlls[b].y; Balls[b].y:=pos;
pos:=Balls[a].x; Balls[a].x:=BAlls[b].x; Balls[b].x:=pos;
end;

//добавляет шар на позицию "num" типа "tip" на позицию pos
Procedure AddBall(num,tip:integer;pos:real);                       //PUBLIC!!
var i:integer;
		p:Point2d;
begin
if ((Num>0) and (Num<maxBalls)) then
  begin
  BallsNum:=BallsNum+1;
  for i:=BallsNum downto Num+1 do
    BallsSamena(i,i-1);
  Balls[num].tip:=tip;
  Balls[num].pos:=pos;
  p:=GetPointD(pos);
	Balls[num].x:=p.x;
  Balls[num].y:=p.y;
  end;
end;

//берёт из стека значение нового шара
Function NewBall:integer;                                           //PUBLIC!!
begin
if StringBalls<>'' then
  begin
  //NewBall:=StringToInteger(copy(StringBalls,0,1));
  if random(4)=1 then
    NewBall:=Balls[1].tip
  else
    NewBall:=Random(4);
  StringBalls:=copy(StringBalls,1,Length(StringBalls));
  end;
end;

//удаляет num шариков начиная с позиции pos
Procedure DeleteBalls(pos,num:integer);
var i:integer;
begin
for i:=pos to BallsNum-num do
  BallsSamena(i,i+num);
BallsNum:=BallsNum-num;
end;

//удаляет далеко ушедшие шары
Procedure DeleteLast;
begin
if BallsNum>0 then
  begin
  if Balls[BallsNum].pos>=wayL[maxWay] then
    begin
    BallsNum:=BallsNum-1;
    DeleteLast;
    GameResult:=-1;     //проиграли!!
    end
  end
else GameResult:=1;  // выиграли!!
end;

//True если шарики взрываются
function BallBoom(tip,pos:integer):boolean;                          //PUBLIC!!
var up,down:integer;
begin
if Balls[pos].tip=tip then
  begin
	for up:=pos to BallsNum-1 do
		if Balls[up+1].tip<>tip then
			break;
	for down:=pos downto 2 do
		if Balls[down-1].tip<>tip then
			break;
	if up>down then
    begin
		BallBoom:=true;
		DeleteBalls(down,up-down+1);
		end
	else
    BallBoom:=false;
	end
else
  BallBoom:=False;
end;

//двигает шары начиная с шара pos на dx
procedure MoveBallsForward(pos:integer;dx:real);                    //PUBLIC!!
var i:integer;
    p:Point2D;
begin
if (pos>0) and (pos<maxBalls) then
  begin
  Balls[pos].pos:=Balls[pos].pos+dx;
    p:=GetPointD(Balls[pos].pos);
    Balls[pos].x:=p.x;
    Balls[pos].y:=p.y;

  for i:=pos+1 to BallsNum do
		begin

    if (Balls[i].pos<=Balls[i-1].pos+24) then
			begin
      Balls[i].pos:=Balls[i-1].pos+24;
      p:=GetPointD(Balls[i].pos);
      Balls[i].x:=p.x;
      Balls[i].y:=p.y;
      end
    else
      break;

		end;

  if pos=1 then
    begin
    if BallsNum=0 then
			begin
      if (StringBalls<>'') then
        AddBall(1,NewBall,0.1);
      end
    else
      while (StringBalls<>'') and (Balls[1].pos>BallD) do
        AddBall(1,NewBall,Balls[1].pos-BallD)                   //тут может быть ошибка
    end;
  end;
DeleteLast;
end;

//_______________________________________________________________________________
//FlyBalls
//*******************************************************************************
type
        FlyBall=record     //летящий шар
        tip:integer;
        x,y,vx,vy:real;
        end;
const
      maxFly=20;
var                        //летящие шары
    Fly:array [1..maxFly] of FlyBall;
    NumFly:integer;

//меняем ячейки местами
Procedure FlySamena(n,m:integer);
var int:integer;
    re:real;
begin
int:=Fly[n].tip; Fly[n].tip:=Fly[m].tip; Fly[m].tip:=int;
re:=Fly[n].x; Fly[n].x:=Fly[m].x; Fly[m].x:=re;
re:=Fly[n].y; Fly[n].y:=Fly[m].y; Fly[m].y:=re;
re:=Fly[n].vx; Fly[n].vx:=Fly[m].vx; Fly[m].vx:=re;
re:=Fly[n].vy; Fly[n].vy:=Fly[m].vy; Fly[m].vy:=re;
end;
//Удаляем улетевшие шарики
procedure DeletingLast;
var i:integer;
begin
For i:=NumFly downto 1 do
  if (Fly[i].x<-BallR) or (Fly[i].y<-BallR) or
  (Fly[i].x>getWidth+BallR) or (Fly[i].y>getHeight+BallR) then
    begin
    FlySamena(i,numFly);
    if NumFly>0 then
      NumFly:=NumFly-1;
    end;
end;

//Костыль!
function Palliativ(F:FlyBall;px,py:real):real;
var i:integer;
begin
while ((i<=20) and ( (px-F.x-0.05*F.vx*i)*(px-F.x-0.05*F.vx*i) +(py-F.y-0.05*F.vy*i)*(py-F.y-0.05*F.vy*i) > 625.0 ) ) do
  i:=i+1;
Palliativ:=0.05*sqrt(F.vx*F.vx+F.vy*F.vy)*i;
end;


function mustAddForward(F:FlyBall;pos:integer):boolean;
var ax,ay,bx,by,K:real;
    p:point2d;
begin
Ax:=Balls[pos].x-F.x;
Ay:=Balls[pos].y-F.y;
Bx:=F.vx; By:=F.vy;
K:=sqrt((Ax*Ax+Ay*Ay)/(Bx*Bx+By*By));
Bx:=Bx*K-Ax;
By:=By*K-Ay;
p:=GetPointD(Balls[pos].pos+4);   //почти производная
Ax:=P.x-Balls[pos].x;
Ay:=P.y-Balls[pos].y;
if Ax*Bx+Ay*By>0 then
  mustAddForward:=true
else
  mustAddForward:=false;
end;

//передаются:шар, шар в который попали, запас перемещения
procedure popadanie(F:FlyBall;Pos:integer);
var Vect:real;
begin
if BallBoom(F.tip,pos) then
  begin
  F.x:=-100;
  end
else
	begin                                                              //ПРИДУМАТЬ НОРМАЛНЬНЫЙ СПОСОБ!!
	if mustAddForward(F,pos) then
	  begin                               //добавляем вперёд
	  if not BallBoom(F.tip,pos+1) then
	    begin
			AddBall(pos+1,F.tip,Balls[pos].pos);
			MoveBallsForward(pos+1,BallD);
			end;
	  end
	else
    begin                               //добавляем назад
		if pos>1 then//не рассмотрен частный случай
		  begin
		  if not BallBoom(F.tip,pos-1) then
				begin
				if Balls[pos].pos-Balls[pos-1].pos>2*BallD then
					AddBall(pos,F.tip,Balls[pos].pos-BallD)
				else
					begin
					AddBall(pos,F.tip,Balls[pos-1].pos);
					MoveBallsForward(pos,BallD);
					end;
				end;
			end
		else
      AddBall(1,F.tip,Balls[1].pos-BallD);
		end;
	end;
F.x:=-100; 	//удаляем шарик
end;

//двигает конкретные шарики
//Где-то сидит та же ошибка что мешала мне!!Fuu!! ИСПРАВИТЬ!!
procedure MoveOne(F:FlyBall;v:real);
var R:real;
    i,BoomPos:integer;
    way,waymin:real;
begin
R:=sqrt(F.vx*F.vx+F.vy*F.vy);
r:=V/R;
F.vx:=F.vx*r;
F.vy:=F.vy*r;
waymin:=V;
for i:=1 to BallsNum do
  begin
  if abs(trunc(Balls[i].x-F.x-F.vx))<35 then
    if abs(trunc(Balls[i].y-F.y-F.vy))<35 then
      begin                                 //КОСТЫЛЬ!!
      way:=Palliativ(F,Balls[i].x,Balls[i].y);             //КОСТЫЛЬ!!
      if way<waymin then
        begin
        waymin:=way;
        BoomPos:=i;
        end;
      end;
  end;
if Waymin>=0 then
    begin
    R:=waymin/V;
    F.x:=F.x+F.vx*R;
    F.y:=F.y+F.vy*R;
    if BoomPos<>0 then
      Popadanie(F,BoomPos);// передаются Fly шар и номер шара для обработки пересечения
    end;
end;
//двигает все шарики на v                          //PUBLIC!!
Procedure MoveFly(v:real);
var i:integer;
begin
for i:=1 to NumFly do
  MoveOne(Fly[i],v);
DeletingLast;
end;
//добавляет новый шарик
Procedure AddFlying(tip,x,y,vx,vy:integer);      //PUBLIC!!
begin
If NumFly<maxFly then
  begin
  numFly:=NumFly+1;
  Fly[numFly].tip:=tip;
  Fly[numFly].vx:=vx;
  Fly[numFly].vy:=vy;
  Fly[numFly].x:=x;
  Fly[numFly].y:=y;
  end;
end;



//_______________________________________________________________________________
//Station!
//*******************************************************************************
var StationX,StationY,Station_Ball:integer;
    StationX2,StationY2:integer;

//выдаёт только те шарики, цвет которых есть на экране
function StationNewBall:integer;                //PUBLIC!!
begin
StationNewBall:=Station_Ball;
if (BallsNum>0) then
Station_Ball:=Balls[random(BallsNum)+1].tip;
//Station_Ball:=random(4);
end;

Function NewBallSpecial:integer;
var i,x:integer;
begin
NewBallSpecial:=Station_Ball;
if (BallsNum>0) then
  for i:=1 to 10 do
		begin
    x:=Balls[random(BallsNum)+1].tip;
    if x<>Station_Ball then
      break;
    end;
Station_Ball:=x;
{x:=Random(3);
if Station_Ball<=x then
  x:=x+1;
Station_Ball:=x;}
end;
//обрабатывает нажатия
procedure TouchUPD;                             //PUBLIC!!
var p:Sens.Touch;
     c:integer;
begin
if NewTouchR(p) then
  {if sqr(p.x-StationX)+sqr(p.y-StationY)>sqr(BallD) then
    AddFlying(StationNewBall,StationX,StationY,p.x-StationX,p.y-StationY)
  else
    p.x:=NewBallSpecial;}
  begin
  if sqr(p.x-StationX)+sqr(p.y-StationY)<2000 then
    p.x:=NewBallSpecial
  else
    if sqr(p.x-StationX2)+sqr(p.y-StationY2)<2000 then
      begin
		  C:=stationX2; StationX2:=StationX; StationX:=c;
		  C:=stationY2; StationY2:=StationY; StationY:=c;
      end
    else
      AddFlying(StationNewBall,StationX,StationY,p.x-StationX,p.y-StationY)
  end;
end;


//_______________________________________________________________________________
//Рисование
//*******************************************************************************
var
      Im_balls:array [0..3] of image;
      Im_Fon:Image;

procedure DrawScene;                           //PUBLIC!!
var i:integer;
    p:point2d;
begin
DrawImage(Im_Fon,0,0);
//Line
For i:=1 to BallsNum do
  drawImage(Im_Balls[Balls[i].tip],trunc(Balls[i].x)-12,trunc(Balls[i].y)-12);
//Flying
for i:=1 to NumFly do
  drawImage(Im_Balls[Fly[i].tip],trunc(Fly[i].x)-12,trunc(Fly[i].y)-12); //TEST!!Alarm!!
//Station
{setcolor(30,100,30);
fillEllipse(StationX-BallD,StationY-BallD,48,48);}
DrawImage(Im_Balls[Station_Ball],StationX-BallR,StationY-BallR);
end;


Procedure DrawWay;
var i:integer;
begin
drawImage(Im_Fon,0,0);
setcolor(0,0,0);
for i:=2 to maxWay do
  drawLine(trunc(way[i-1].x),trunc(way[i-1].y),trunc(way[i].x),trunc(way[i].y));
setcolor(255,0,0);
for i:=1 to maxWay do
  Fillrect(trunc(Way[i].x)-1,trunc(way[i].y)-1,3,3);
setcolor(255,0,255);
i:=1;
While i<=MaxWay do
   begin
   Fillrect(trunc(Way[i].x)-1,trunc(way[i].y)-1,4,4);
   i:=i+4;
   end;
repaint;
end;

//_______________________________________________________________________________
//ФУНКЦИИ ИГРЫ
//*******************************************************************************

procedure Game;
var BallsSpeed,FlySpeed,k:real;
    deltaTime:integer;
begin
DeltaTime:=RepaintFPS;
BallsSpeed:=0.028;
FlySpeed:=0.34;
deltaTime:=20;
repeat
MoveBallsForward(1,DeltaTime*BallsSpeed);
TouchUPD;
MoveFly(DeltaTime*FlySpeed);
DrawScene;
DeltaTime:=RepaintFPS;
until GameResult<>0;
if GameResult=1 then
  DrawImage(LoadIMage('/Win.png'),190,110)
else
  DrawImage(LoadImage('/End.png'),160,120);
repaint;
Delay(3000);
end;

//_______________________________________________________________________________
//ЗАГРУЗОЧНЫЕ ФУНКЦИИ
//*******************************************************************************

Procedure InputWay;
var i:integer;
    p:Sens.Touch;
begin
setcolor(0,200,0);
fillrect(0,0,GetWidth,GetHeight);
repaint;
setcolor(0,0,0);
Sens.UPD;
for i:=1 to MaxWay2 do
  begin
    repeat
    Delay(20);
    until Touched(p);
  way[i].x:=p.x;
  way[i].y:=p.y;
  if (i>10) and NewTouchR(p) then
    break;
  if i>1 then
    begin
    drawLine(trunc(way[i-1].x),trunc(way[i-1].y),trunc(way[i].x),trunc(way[i].y));
    wayL[i]:=wayL[i-1]+sqrt(pow(way[i].x-way[i-1].x,2)+pow(way[i].y-way[i-1].y,2));
    repaint;
    end;
  end;
MaxWay:=i-1;
end;

function mediana(p1,p2,p3:point2d):point2d;
begin
mediana.x:=(p1.x+p2.x+p3.x)/3;
mediana.y:=(p1.y+p2.y+p3.y)/3;
end;

//Сглаживание пути
Procedure Smooth;
var i:integer;
begin
i:=5;
While i<=MaxWay do                         //добавление середин отрезков
  begin
  way[i-2].x:=(way[i-4].x+way[i].x)/2;
  way[i-2].y:=(way[i-4].y+way[i].y)/2;
  i:=i+4;
  end;
i:=7;
while i<=maxWay do                        //первые медианы
  begin
  way[i-2]:=mediana(way[i-4],way[i-2],way[i]);
  i:=i+4;
  end;
i:=3;
while i<=maxWay do                         //Добавление середин отрезков
  begin
	way[i-1].x:=(way[i-2].x+way[i].x)/2;
	way[i-1].y:=(way[i-2].y+way[i].y)/2;
	i:=i+2;
	end;
i:=4;
while i<maxWay do                           //Медианы
  begin
  way[i-1]:=mediana(way[i-2],way[i-1],way[i]);
  i:=i+2;
	end;
for i:=2 to MaxWay do
   wayL[i]:=wayL[i-1]+sqrt(pow(way[i].x-way[i-1].x,2)+pow(way[i].y-way[i-1].y,2));
maxWay:=maxWay;
end;

procedure AddWayPoint(x,y:real);
begin
maxWay:=maxWay+4;
Way[maxWay].x:=x;
Way[maxWay].y:=y;
end;

procedure SpecialLoad;
begin
maxWay:=-3;
AddWayPoint(394,196);

AddWayPoint(275,-27);
AddWayPoint(45,50);
AddWayPoint(21,332);
AddWayPoint(36,534);

AddWayPoint(120,588);
AddWayPoint(225,620);
AddWayPoint(328,360);
AddWayPoint(285,225);

AddWayPoint(209,120);
AddWayPoint(116,200);
AddWayPoint(100,304);
AddWayPoint(115,410);

AddWayPoint(170,450);
AddWayPoint(211,429);
AddWayPoint(216,371);

Smooth;
StationX2:=334;
StationY2:=615;
StationX:=25;
StationY:=25;
//DrawWay;
//Delay(1000000);
end;

//Создание пути для сглаживания
Procedure InputWay2;
var i:integer;
    P:sens.Touch;
begin
setcolor(0,200,0);
fillrect(0,0,GetWidth,GetHeight);
repaint;
setcolor(0,0,0);
Sens.UPD;
i:=1;
while i<maxWay2 do
  begin
    repeat
    Delay(200);
    until Touched(p);
  way[i].x:=p.x;
  way[i].y:=p.y;
  if (i>20) and NewTouchR(p) then
    break;
	if i>1 then
    begin
    drawLine(trunc(way[i-4].x),trunc(way[i-4].y),trunc(way[i].x),trunc(way[i].y));
    repaint;
    end;
  i:=i+4;
  end;
maxWay:=i;
end;

//ставит позицию стрелялки
procedure InputStation;
var i:integer;
    p:sens.Touch;
begin
setcolor(0,200,0);
Fillrect(0,0,GetWidth,GetHeight);
setcolor(0,0,0);
for i:=2 to MaxWay do
  drawLine(trunc(way[i-1].x),trunc(way[i-1].y),trunc(way[i].x),trunc(way[i].y));
drawtext('InputStation!!',0,0);
repaint;
for i:=1 to 2 do
  repeat
  Delay(50);
  until NewTouchR(p);
StationX:=p.x;
StationY:=p.y;
end;

procedure Loading;
var i:integer;
begin
randomize;
Sens.InitSens;
Load(LoadImage('/balls1.png'));
for i:=0 to 3 do
  Im_balls[i]:=getImage(i);
picker.Reset;
Im_Fon:=Loadimage('/f3.png');
{InputWay2;
Smooth;
InputStation;}
SpecialLoad;
StringBalls:='2101112200330112300031312121201021020100012112222121012213232323233321212213012131230102031001203010230010301203012001202012003001202100012211121';
end;

begin
Loading;
Game;
end.
�
