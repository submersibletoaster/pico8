pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- main
local cx=0
local cy=0
local dx=0
local dy=0
function _init()

end

function _update60()
  if btn(0) then
   cx-=1
  elseif btn(1) then
   cx+=1
  end
  if btn(2) then
   cy-=1
  elseif btn(3) then
   cy+=1
  end
  
  if btnp(4) then
    dx = -cx
    dy = -cy
  end
  
  cx+= flr(dx*0.1)
  cy+= flr(dy*0.1)
  
  if cx<0 then cx=0 end
  if cy<0 then cy=0 end
  
end

function _draw()
 cls()
 camera(cx,cy)
 draw_stars(cx,cy,4478,128)
 camera(cx*1.25,cy*1.25)
 draw_stars(cx*1.25,cy*1.25,8086,256)
 camera(cx*1.5,cy*1.5)
 draw_stars(cx*1.5,cy*1.5,16,512)
end
-->8
-- background
local star_cols={1,5,6,7,13}
function xy2starcell(x,y)
  local cx=flr(x/128)
  local cy=flr(y/128)
  return {cx,cy}
end

function draw_stars(x,y,base,ds)
 local grps={
   {x-128,y-128},
   {x,y-128},
   {x+128,y-128},
   {x-128,y},
   {x,y},
   {x+128,y},
   {x-128,y+128},
   {x,y+128},
   {x+128,y+128},
 }
 local mn=0
 local mx=128*128
 for p in all(grps) do
  repeat
  if p[1]<mn or p[1]>mx then break end
  if p[2]<mn or p[2]>mx then break end
  
  local cell=xy2starcell(p[1],p[2])
  local clx=cell[1]*128
  local cly=cell[2]*128
  local seed=shl(13,cell[1])
            +cell[2]
  seed=bxor(base,seed)
  srand(seed)


  
  local sz=128*128
  local density=ds or 256
  local nxt=0
  while nxt<sz do
    local step=flr(rnd(density))
    nxt+=step
    local col=star_cols[
      flr(rnd(#star_cols))+1
      ]
    local x=nxt%128
    local y=flr((nxt-x)/128)
    --printh(x .. "," .. y)
    
    pset(clx+x,cly+y,col) 
  end
  until true -- loop break next
 end
end
