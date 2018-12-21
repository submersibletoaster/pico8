pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
show=true
score=0
function _init()

end

function _update60()
  if btn(4) then
    score+=1
  end
  if btnp(5) then
    show=show==false and true or false
  end
  
end

function _draw()
  cls()
  pal()
  spr(128,64,96,8,4)
  if show then
    prright(tostr(score),120,120,15)
  end
  firedecay(64,96,128,128)
  grabspr(64,96,64,32,0)
  
  cls()
  setpal(firepal)
  spr(128,64,96,8,4)
  pal()

  prright(tostr(score),120,119,1)
  prright(tostr(score),120,121,1)
  prright(tostr(score),120,120,7)
  
  
end

-->8
-- fx
function setpal(t)
  for i=0,15 do
    pal(i,t[i+1])
  end
end

-- poking into memory
local scr_st=0x6000
local scr_wd=64
local spr_st=0x1000


-- grab x,y [w-h] from display
-- and write to sprite@ sp
function grabspr(x,y,w,h,sp)
  local lgth=flr(w/2)
  local offset=scr_st+(x/2)+(y*scr_wd)
  for oh=0,h-1 do
    local src=offset+(oh*scr_wd)
    local dst=spr_st+(oh*scr_wd)
    memcpy(dst,src,lgth)
  end
end

firemap={
 0,0,1,2,
 2,2,5,10,
 2,7,9,9,
 1,5,2,10 }
firepal={
 0,0,5,13,
 2,8,2,8,
 9,8,9,10,
 9,10,10,7
}
function firedecay(x1,y1,x2,y2)

  local yvel=-1
  local depth=5
  for x=x1,x2 do
   for y=y1,y2 do
    local p=pget(x,y)
    local xoff=rnd(depth) -(depth/2)
 
    local col=p>0 and p-1 or 0
    
    pset(x+xoff,y+yvel,col)
    --circfill(x+xoff,y+yvel,2,col)
   end
  end
  

end
-->8
-- formatting
function prright(s,x,y,col)
  col=col or 7
  local ox=x-(#s*4)
  print(s,ox,y,col)
end
