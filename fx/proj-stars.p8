pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
offset=0
function _init()

end

function _update60()
  offset+=1
end

function pr(x,y)
  local xo=x-64
  
  local yf=sqrt(1+(y/64))
  return {xo*yf,y}
end

function _draw()
  cls()
  camera(0,offset)
  draw_stars(0,offset,
    4478,1024,2,
    pr
  )
end
-->8
-- stars

function xy2starcell(x,y)
  local cx=flr(x/128)
  local cy=flr(y/128)
  return {cx,cy}
end

-- draw stars for camera(x,y)
-- random seed of base
-- ds - distribution bigger is sparser
-- p - palette
-- projection - function transforms x,y
function draw_stars(x,y,base,ds,pl,project)
 pl=pl or 1
 project=project or function(x,y)
   return {x,y}
 end
 apply_pal(star_pal[pl])
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
    
    local r=project(x,y)
    pset(clx+r[1],cly+r[2],col)
  end
  until true -- loop break next
 end
end


-->8
-- palette
star_cols={
 0,1,2,3,4,5,6,7,
 8,9,10,11,12,13,14,15
}
star_pal={
 {1,1,1,1,1,1,1,1,
  5,5,5,5,5,6,6,13} ,
 {1,1,1,1,1,5,5,5,
  5,5,6,10,13,13,12,13},
 {1,1,5,5,5,6,6,9,
  13,12,15,13,7,7,7,7}
}


function apply_pal(p)
  for i=0,15 do
    pal(i,p[i+1])
  end
end

