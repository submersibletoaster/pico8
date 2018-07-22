pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local l1={x=0,y=0}
local l2={x=0,y=0}
local l3={x=0,y=0}
local l4={x=0,y=0}

local d_pal=dark1
local hz=50
local coll
local player

local debug=true
local debug_coll=false
local debug_flash=14
local profile={}


function _init()
 music(0,30)
 player=pl.new()
end


function _update()

 profile.coll = stat(1)
 update_coll()
 profile.coll = stat(1) -profile.coll
 
 profile.update = stat(1)
 player:get_input()
 
 player:update()
 
 spawn_wave()
 
 for w in all(weps) do
  w:update()
 end

 for p in all(puffs) do
  p:update() 
 end
 
 for n in all(nmes) do
  n:update()
 end
 
 for w in all(wrks) do
  w:update()
 end
 
 for p in all(pups) do
  p:update()
 end
 

 -- layer scrolling
 l1.x+=0.1
 l2.x+=1
 l3.x+=2
 l4.x+=3
 
 
 if l1.x> shl(1,10) then
  l1.x=l1.x % shl(1,10)
 end
 if l2.x> shl(1,10) then
  l2.x=0
 end
 if l3.x> shl(1,10) then
  l3.x=0
 end
 if l4.x> 127 then
  l4.x=1
 end
 
 profile.update=stat(1) - profile.update
 
end

function _draw()
 cls()
 fillp()
 
 set_pal(ref_pal,0)
 camera()
 draw_ref_scene()
 
 --reflection
 reflect(hz+32,127-(hz+32),4) 


 pal()
 camera()
 draw_scene()
 draw_stats()

 if (debug) draw_debug()
end

local dbg_col={
 scene=4,
 ref_scene=12,
 update=10,
 coll=8
}
function draw_debug()
 
	camera()
	pal()
 color(4)
 print(stat(7),10,0)
 print(stat(1),10,10)
 print(stat(0),10,20)
 
 print(#nmes,10,30,14)
 print(#wrks,10,40,8)
 print(#weps,10,50,11)
 print(#puffs,10,60,7)
 
 if (debug_coll) coll:draw_debug()

 debug_flash=debug_flash%10 +1
 pset(player.pos.x,player.pos.y,debug_flash)
 for n in all(nmes) do
  pset(n.pos.x,n.pos.y,debug_flash)
 end
 
 for p in all(pups) do
  pset(p.pos.x,p.pos.y,debug_flash)
 end

 -- computer 'meter'
 local px=0
 local pwid=50
 for k,v in pairs(profile) do
  local dist=flr(v*pwid)
  rectfill(px,110,px+dist,114,
   dbg_col[k]
  )
  px+= dist
 end
 rect(0,109,pwid,115,0)
 
end

function draw_stats()
 camera(-110,-118)
 
 rectfill(0,0,20,10,3)
 print(tostr(player.score),
  1,1,11
 )
end

function draw_sky(h)
 rectfill(0,0,128,h,12)
end

function draw_grass(h,off)
 off=off or 32
 rectfill(0,h,127,h+off,3)
end

function draw_l4(h,off)
 off = off or 16
 
 
 camera(l1.x,0)
 mapdraw(0,0,0,h-off,128,2)
 mapdraw(0,0,128*8,h-off,128,2)

 camera()
end

function draw_l3(h,off)
 off = off or 3
 camera(l2.x,0)
 mapdraw(0,2,0,h-off,128,2)
 mapdraw(0,2,128*8,h-off,128,2)
 
end

function draw_l2(h,off)
 off=off or 10
 camera(l3.x,0)
 palt(0,false)
 palt(8,true)
 
 mapdraw(0,4,0,h+off,128,3)
 mapdraw(0,4,128*8,h+off,128,3)
 
 palt(0,true)
 palt(8,false)
 camera()
end

function draw_l1(h,off)
 off = off or 28
 camera(l4.x,0)
 mapdraw(0,7,0,h+off,16,1)
 mapdraw(0,7,128,h+off,16,1)
end

function draw_scene()

 profile.scene = stat(1)
 -- sky
 camera()
 draw_sky(hz)

 -- grass from horizon to dirt
 draw_grass(hz)
 
 -- paralax - uses camera
 -- mountains
 draw_l4(hz)
 
 -- trees
 draw_l3(hz)
 
 --dirt

 draw_l2(hz)

 -- pier
 draw_l1(hz)
 
 -- end paralax
 camera()
 player:draw()
 
 for w in all(weps) do
  w:draw()
 end
 
 for p in all(pups) do
  p:draw()
 end
 
 for p in all(puffs) do
  p:draw()
 end

 for w in all(wrks) do
  w:draw()
 end
 
 for n in all(nmes) do
  n:draw()
 end
 
 profile.scene = stat(1) - profile.scene
end

function draw_ref_scene()
 profile.ref_scene = stat(1)
 
 camera()
 local rh=60
 draw_sky(rh)
 draw_grass(rh,15)
 draw_l4(rh)
 draw_l3(rh)
 -- layer 2 is 'flat' unseen
 -- by reflection
 draw_l1(rh,8)
 
 
 player:draw()


 for n in all(nmes) do
  n:draw()
 end
 
  
 for p in all(puffs) do
  p:draw()
 end

 -- brighter pal for wrecks
 set_pal(ref_exp_pal) 
 for w in all(wrks) do
  w:draw()
 end

 
 -- brighter pal for weps
 pal()
 for w in all(weps) do
  w:draw()
 end
 
 profile.ref_scene = stat(1) - profile.ref_scene


end
-->8
-- visual fx

--reflection
dark1={
 {0,0},
 {1,1},
 {2,2},
 {3,2},
 {4,2},
 {5,1},
 {6,13},
 {7,6},
 {8,4},
 {9,4},
 {10,9},
 {11,3},
 {12,13},
 {13,5},
 {14,2},
 {15,14}
}
ref_pal={
   {0,0},
   {1,0},
   {2,0},
   {3,3},
   {4,5},
   {5,1},
   {6,13},
   {7,6},
   {8,2},
   {9,4},
   {10,13},
   {11,5},
   {12,1},
   {13,5},
   {14,4},
   {15,14}
}
ref_exp_pal={
   {0,0},
   {1,0},
   {2,0},
   {3,3},
   {4,5},
   {5,1},
   {6,13},
   {7,15}, --keep white
   {8,4},
   {9,4},
   {10,9}, --
   {11,5},
   {12,1},
   {13,5},
   {14,4},
   {15,14}
}

function set_pal(pt,p)
 p = p  or 0
 for t in all(pt) do
  pal(t[1],t[2],p)
 end
end


local gfx=0x6000
local mod={3,2,2,2,2,1,1,1,0,0,
	-1,-1,-1,-2,-2,-2,-2,-3,
 -2,-2,-2,-2,-1,-1,-1,0,
 1,1,1,2,2,2,2 }
local mod_step=1

function reflect(y,depth,offset)
 --reflection
 offset = offset or 0
 local roff=y
 local soff=y-offset
 local bound = min(127,roff+depth)
 
 mod_step+=1
 if (mod_step > #mod) mod_step=1
 
 for i=0,depth-1 do
   local ms=1+ ((i+mod_step) % #mod)
   local msx=1+(i%#mod)
 
   memcpy(
   	gfx+ ( (roff+i)*64 ),
   	gfx+ ( (soff-(flr(i*1.5))+mod[ms] )*64 ) + mod[msx],
   	64
   )
 end


end


-->8
-- collision
local qt={}
qt={
 new=function(pos,sz,cap)
  local t={
   cap=cap or 3,
   ob={},
   pos=pos or vec2d.new(64,64),
   sz=sz or 64,
   nw=nil,ne=nil,sw=nil,se=nil
  }
  setmetatable(t,qt)
  return t
 end,
 has_point=function(t,pos)
  if t.pos.x-t.sz < pos.x 
      and
     t.pos.x+t.sz > pos.x
      and
     t.pos.y-t.sz < pos.y
      and
     t.pos.y+t.sz > pos.y then
     return true
  else
   return false
  end
 end,
 intersect=function(t,x1,y1,x2,y2)
  if t.pos.x - t.sz < x1
     or
     t.pos.x + t.sz > x2
     or
     t.pos.y - t.sz < y1
     or
     t.pos.y + t.sz > y2
     then
     return true
  else
   return false
  end
 end,
 add=function(t,o)
  if not t:has_point(o.pos) then
   return false
  end
  if count(t.ob) < t.cap then
   add(t.ob,o)
   return true
  end
  
  if (not t.nw) t:subdivide()
  
  if (t.nw:add(o,r)) return true
  if (t.ne:add(o,r)) return true
  if (t.sw:add(o,r)) return true
  if (t.se:add(o,r)) return true
  
  return false
 end,
 subdivide=function(t)
  --[[
  printh("subdiv:["
   ..tostr(t.pos.x) .. ","
   ..tostr(t.pos.y) .. "] - "
   ..tostr(t.sz) .. " cap:"
   ..tostr(t.cap) .. " #obj:"
   ..tostr(count(t.ob))
  )
  ]]--
  
  local half= t.sz/2
  t.nw=qt.new(
    t.pos + vec2d.new(-half,-half),
    half
  )
  t.ne=qt.new(
    t.pos + vec2d.new(half,-half),
    half
  )
  t.sw=qt.new(
   t.pos + vec2d.new(-half,half),
   half
  )
  t.se=qt.new(
   t.pos + vec2d.new(half,half),
   half
  )

  local q={t.nw,t.ne,t.sw,t.se}  
  for o in all(t.ob) do
   for c in all(q) do
    if c:add(o)==true then
     break
    end
   end
   del(t.ob,o)
  end

  
 end,
 query=function(t,pos,r)
  local m={}
  
  if not t:intersect(pos.x-r,pos.y-r,pos.x+r,pos.y+r) then
   return m
  end

  local range=qt.new(pos,r)  
  for o in all(t.ob) do
   if range:has_point(o.pos) then
    add(m,o)
   end
  end
  
  if t.nw==nil then
   return m
  end
  
  local q={t.nw,t.ne,t.se,t.sw}
  for c in all(q) do
   local g=c:query(pos,r)
   for e in all(g) do
    add(m,e)
   end
  end
  
  return m
 end,
 draw_debug=function(t,col,d,n)
  camera()
  d = d or 0
  n = n or 1
	 d+=1
  col=col or 3
  col=1+ ((col+1)%16)
  if (t.nw!=nil) then
   t.nw:draw_debug(col,d,1)
   t.ne:draw_debug(col,d,2)
   t.sw:draw_debug(col,d,3)
   t.se:draw_debug(col,d,4)
  end
  print(tostr(count(t.ob)),
  	90 + (n*8),
  	20+(d*8),
  	col
  )
  
  fillp(0b1010010110100101)
  line(t.pos.x,t.pos.y,
   t.pos.x-t.sz,t.pos.y-t.sz,col
  )
  rect(t.pos.x-t.sz,t.pos.y-t.sz,
   t.pos.x+t.sz,t.pos.y+t.sz, col )
  fillp()
  pset(t.pos.x,t.pos.y,10)
   
 end
}
qt.__index=qt


function update_coll()
 coll=qt.new()

 
 for n in all(nmes) do
  coll:add(n)
 end

 for w in all(weps) do
  local close = coll:query(w.pos,4)
  for n in all(close) do
   n:hit(w)
   w.die=true
   break
  end 
 end

 for p in all(pups) do
  if p:near(player,5) == true then
   p:hit(player)
  end
 end
 
 local close = coll:query(player.pos,6)
 for n in all(close) do
  player:hit(n)
 end
 
 

end

-->8
-- player
pl={}
pl={
 new=function()
  local player={
   pos=vec2d.new(25,40),
   dv=vec2d.new(0,0),
   turn=3,
   tframe={68,66,64,72,74},
   tdecay=0,tdamp=0,
   gun=guns.laser,
   score=0,
   nrg=10,
  }
  setmetatable(player,pl)
  return player
 end,
 set_gun=function(t,name)
  t.gun = guns[name]
 end,
 get_input=function(t)
  -- shoot
  if btn(4) then
   t.shot=1
  end 
 
  -- rolling up/down 
  if btn(2) then
   t.turn=max(t.turn-1,1)
   t.tdamp=15
   t.tdecay=1
   t.dv.y-=1.5
  elseif btn(3) then
   t.turn=min(t.turn+1,5)
   t.tdamp=10
   t.tdecay=-1
   t.dv.y+=1.5
  end
  
  -- forward / backwards
  if btn(0) then
   t.dv.x-=1.5
  elseif btn(1) then
   t.dv.x+=1.5
  end
 
 end,
 
 update=function(t)
  -- turn damping
  if t.tdamp != 0 then
   t.tdamp-=1
   
   if t.tdamp%5 == 0 then
    t.turn+= t.tdecay
   end
    
   if t.turn==3 then
    t.tdamp=0
   end
  end
  
  t.gun:update()
  -- shooting
  if t.shot==1 then
   t.gun:fire(t.pos)
   t.shot=0
  end
  
  t.pos = t.pos + t.dv
  
  -- damping
  t.dv *= vec2d.new(0.5,0.5)
   
 end,
 draw=function(t)
 camera()
 spr( t.tframe[ t.turn ],
  t.pos.x-8, t.pos.y-8,
  2,2
 ) 
 end,
 hit=function(t,o)
  wrk.new(
   t.pos + vec2d.new(2,2),
   t.dv
  )
  wrk.new(
   t.pos -vec2d.new(2,2),
   t.dv
  )
 end
}
pl.__index=pl

 
guns={
}

gun={}
gun={
 new=function(n)
 n=n or {}
  local t={
   shot=10,
   decay=0
  }
  setmetatable(t,gun)
  for k,v in pairs(n) do
   t[k] = v
  end
  return t
 end,
 update=function(t)
  t.decay -= 1
 end,
 ready=function(t)
  if (t.decay <=0) then
   return true
  else
   return false
  end
 end, 
 fire=function(t,pos)
  if t:ready() then
   wep.new({pos=pos})
   sfx(0)
   t.decay=t.shot
  end
 end,
 random=function()
  local p={}
  for k,v in pairs(guns) do
   add(p,k)
  end
 end
}
gun.__index=gun

guns.default=gun.new()

guns.laser=gun.new({
 shot=12,
 fire=function(t,pos)
  if (not t:ready()) return
  wep.new({
   pos=pos,
   dv=vec2d.new(7,0),
   draw=function(t)
    fillp(0b0101010101010101)
    line(t.pos.x-7,t.pos.y,t.pos.x,t.pos.y,
     shl(11,4)+3
     )
    fillp()
    pset(t.pos.x-1,t.pos.y,11)
    pset(t.pos.x,t.pos.y,7)
   end
  })
  sfx(6)
  t.decay=t.shot
 end
})



-->8
-- weapons
weps={}
wep={}
wep={
 new=function(n)
  local t={
   pos=vec2d.new(0,64),
   dv=vec2d.new(5,0)
  }
  for k,v in pairs(n) do
  -- printh("wep override:"..k.."="..type(v))
   
   t[k] = v
  end
  setmetatable(t,wep)
  add(weps,t)
  return t
 end,
 update=function(t)
--  printh("wep update pos=".. type(t.pos))
  t.last = t.pos:copy()
  
  t.pos += t.dv
  if t.pos.x > 128 then
   del(weps,t)
  end
 end,
 draw=function(t)
  pset(t.pos.x,t.pos.y,10)
  pset(t.pos.x-1,t.pos.y,8+ t.pos.x%2)
 end
}
wep.__index=wep



-->8
-- enemies
nextwave=100
wavetick=0
function spawn_wave()
 wavetick+=1
 
 if wavetick > nextwave then
  local sp=0
  for i=3,3+flr(rnd(5)) do
   nme.new()
   sp+=1
  end
  nextwave+=15
  nextwave+=flr(rnd(100))
 end
 
end

nmes={}
nme={}
nme={
 new=function(pos,dv)
  local t={
   pos=pos or vec2d.new(130,10+flr(rnd(80))),
   dv=dv or vec2d.new(-1,0)
  }
  setmetatable(t,nme)
  add(nmes,t)
  return t
 end,
 update=function(t)
  -- move ahead and chase player
  t.pos += t.dv
  
  --[[
  pl_dy = player.y - t.y
  if pl_dy > 0 then
   t.y+=0.5
  elseif pl_dy < 0 then
   t.y-=0.5
  end
  ]]--
  
  -- off screen extinction
  if t.pos.x < -4 or t.pos.x > 130 then
   t.die=true
  end
  
 end,
 draw=function(t)
  spr(76,t.pos.x-4,t.pos.y-4)
  if t.die then
   del(nmes,t)
  end
 end,
 hit=function(t,w)
  t.hits=t.hits or 0
  t.hits+=1
  sfx(1,1)
  wrk.new(t.pos,t.pos)
  player.score += 10
  if player.score%50 == 0 then
   pup.new(t.pos,"score",60)
  end
  if player.score%40==0 then
   pup.new(t.pos+vec2d.new(4,4),"gun","default")
   pup.new(t.pos+vec2d.new(-8,-8),"rate", nil)
  end
  del(nmes,t)
 end

}
nme.__index=nme

wrks={}
wrk={}
wrk_ani={
 {96,3},{98,4},{100,5},
 {102,4},
 {104,3},
 {106,4},
 {108,3}
}
debris_col={4,5,6,8,9,10}

wrk={}
wrk={
 new=function(pos,dv)
  local p={}
  -- debris
  for i=5,5+flr(rnd(5)) do
   local dir = (i%2)-0.5
   local dpos=vec2d.new(
     pos.x+(rnd(8)-4),
     pos.y+(rnd(8)-4)
   )
   local d={
    pos=dpos,
    dv=vec2d.new(rnd(3)*dir,-rnd(2)),
    lt=120+rnd(200)
   }
   puff.new(
    d.pos:copy(),
    d.dv:copy()
   )
   add(p,d)
  end
  local t={
   pos=pos,dv=dv,
   p=p, f=hz+abs(hz-pos.y),
   c=0,a=0,
   flp=pos.y%2==0 and true or false,
   flp2=pos.x%2==0 and true or false
  }
  setmetatable(t,wrk)
  add(wrks,t)
  return t
 end,
 update=function(t)
  for p in all(t.p) do
   p.dv.x *= 0.95 --damping
   --p.dy *= 0.95
   p.dv.y += 0.09 -- grav
   p.pos += p.dv
  
   -- bounce
   if p.pos.y > t.f then
    p.dv *= vec2d.new(1,0.4)
    p.lt/=2
   end
   p.lt-=1
   if p.lt < 0 then
    del(t.p,p)
   end
  end
  
  if t.die and count(t.p)==0 then
   del(wrks,t)
   return
  elseif t.die then
   return
  else
   if t.c <= 0 then
    t.a=t.a+1
    if t.a > #wrk_ani then
     t.die=true
    else
     t.c=wrk_ani[t.a][2]
    end
   end
  end
  
  t.c-=1
  
 end,
 draw=function(t)
  if not t.die then
  spr(wrk_ani[t.a][1],
   t.pos.x-8,t.pos.y-8,
   2,2,
   t.flp,t.flp2
  )
  end
 
 
  for p in all(t.p) do
   circfill(p.pos.x,p.pos.y,1,
   8+(p.lt%2)
   )
  end
 end
}
wrk.__index=wrk


puff_ptn={
 {0b1111101111111101,shl(10,4)+5},
 {0b1010111101011111,shl(8,4)+5},
 {0b1010010110100101,shl(9,4)+5},
 {0b0000101000000101,shl(0,4)+5},
 {0b0100000000100000,shl(0,4)+5},

 {0b1111101111111101,shl(0,4)+13},
 {0b1010111101011111,shl(0,4)+13},
 {0b1010010110100101.1,shl(13,4)+5},
 {0b0000101000000101,shl(5,4)+13},
 {0b0100000000100000.1,shl(5,4)+13},

 {0b1010010110100101.1,5},
 {0b0101101101001101.1,13},
 {0b1010010110100101.1,5},
 {0b1011111110111111.1,13},
 {0b1111101111110111.1,13},
}
puffs={}
puff={}
puff={
 new=function(pos,dv,sz)
  local t={
   pos=pos,
   dv=dv,
   sz=sz or 2,
   prev={},
   lt=40,   
  }
  setmetatable(t,puff)
  add(puffs,t)
  return t
 end,
 update=function(t)
  if t.lt < 0 then
   del(puffs,t)
   return
  end
  t.pos+= t.dv
  t.dv *= vec2d.new(0.95,0.95)
  t.dv += vec2d.new(0,0.06)
  add(t.prev,t.pos:copy())
  
  t.lt-=1
 end,
 draw=function(t)
  local lst=max(1,#t.prev-#puff_ptn)
  for i=lst,#t.prev do
   local pos=t.prev[i]
   local age=lst+#t.prev - i
   local ptn = flr(lerp(age,1,30,1,#puff_ptn))
   
   --fillp(puff_ptn[1+i%#puff_ptn])
   --fillp(puff_ptn[1+age%#puff_ptn])
   fillp(puff_ptn[ptn][1])
   circfill(
    pos.x,pos.y,
    t.sz,
//    shl(5,4)+13  
    puff_ptn[ptn][2]
   ) 
  end
  fillp()
 end 
}
puff.__index=puff
-->8
-- helper functions
lerp=function(n,mn,mx,sl,sh)
 if (n<mn) return sl
 if (n>mx) return sh
 local rng=mx-mn
 local dist=n-mn
-- printh("range: "..tostr(rng))
 local norm = dist/rng

 local srng=sh-sl
 return sl+ ( norm*srng )

end

vec2d={}
vec2d={

 new=function(x,y)
  local t={x=x or 0,y=y or 0}
  setmetatable(t,vec2d)
  return t
 end,
 __add=function(a,b)
  return vec2d.new(a.x+b.x,a.y+b.y)
 end,
 __sub=function(a,b)
  return vec2d.new(a.x-b.x,a.y-b.y)
 end,
 __mul=function(a,b)
  return vec2d.new(a.x*b.x,a.y*b.y)
 end,
 __div=function(a,b)
  return vec2d.new(a.x/b.x,a.y/b.y)
 end,
 copy=function(t)
  return vec2d.new(t.x,t.y)
 end
}
vec2d.__index=vec2d
-->8
-- power ups
pup_type={
 gun={
  sp=111,
  apply=function(t)
   player:set_gun(t.v)
  end
 },
 rate={
  sp=126,
  apply=function(t)
   if player.gun.shot > 4 then
    player.gun.shot = player.gun.shot-2
   end
  end
 },
 score={
  sp=110,
  apply=function(t)
   player.score+= t.v
   sfx(4,3)
  end
 }
}
pups={}
pup={}
pup={
 new=function(pos,k,v)
  local t={pos=pos,
   k=k,
   v=v,
   lt=90
  }
  setmetatable(t,pup)
  add(pups,t)
  return pup
 end,
 near=function(t,ob,r)
  if (abs(t.pos.x-ob.pos.x) < r) and (abs(t.pos.y-ob.pos.y) < r ) then
     return true
  end
  return false
 end,
 hit=function(t,pl)
  pup_type[t.k].apply(t)
  del(pups,t)
 end,
 update=function(t)
  t.pos += vec2d.new(-0.5,0.0)
  
  t.lt-=1
  if t.lt< 0 then
   del(pups,t)
   sfx(5,3)
  end
 end,
 draw=function(t)
  pal()
  if t.lt < 30 then
   pal(12,8 + t.lt%3)
   pal(11,8 + t.lt%3)
  end
  spr( pup_type[ t.k ].sp,
   t.pos.x-4,t.pos.y-4
  )
 end
}
pup.__index=pup
__gfx__
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000004444444444444444446666d4
00000000000000000000000000000000000000000000000700000000000000000000000000000000000000000d6666600d6666604444444444466444465555d4
0000000000000000000000000000000000000000000000770000000000000000000000000000000000000000059dddd9059dddd94ddd44444d66664445111154
000000000000007700000000000000000000000000000077700000000000000000000000000000000000000099ddddd099ddddd04444444446666dd441111114
000000000000007770000000000000000000000000000777700000000000000000000000000000000000000005ddddd005ddddd04444444d4666ddd444111144
000000000000077777000000000000000000000000000767700000000000000000000000000000000000000001ddddd001d4dd4044444444566ddd5444444444
0000000000007776777000000000000006660000000066767600000000000000000000000000000000000000dddddddddd54dd4d444dd4444444444444444444
000000000007766666700000dd50000066666000000767666600000000000000000006000000000000000000ddddddddddd5dddd444444444444444444444444
0000000000666666666600dddd5000066666660000d66666666000000000000000066600000000000000066000000000000000008888888888888888888888b8
000000000666666666666ddddd550066666666600d66666666600000000000dd0006666000000005500066660d6666600d6666608b88888888888888b88888b8
000000006666666666d666ddddd500666666666dd6666666666600dddd000ddd556666d00000005555006666059dddd9059dddd9b88b8383888883888b838b88
00000000666666666d56666dddd5566666666ddd6666666666666dddddd55ddd5666666dd600055555006666059dddd099ddd7d0b83b8383838b388b8b883b88
000000006666666ddd66666dddddd66666666d66666666666666666ddd55dddd66666666ddd655666556666605d9ddd005ddedd03333343d33333333333333b3
00000000666666ddd566666ddddd6666666dd66666666666666666666ddddd666666666666d6666666666666019dddd001deedd0444444444444444443345434
0000000066666dd556666666ddd6666666666666666666666666666666666666666666666666666666666666dddddddddddddddd44d44444444444d444444444
000000006666666666666666d666666666666666666666666666666666666666666666666666666666666666dddddddddddddd7d44444dd44444444444444444
0000330000b3003bb0000000000003300000000000000000000000000000000000000000000000000000000000000000000000005d0000d55d0000d500000040
000b33300b333bb3330000300330b3330000000000000000000000000000000000000000000000000000000000000000000000000d5d5d000d5d5d0000000040
00b333333333b3333330b333333b3333000000033330000330000000000000000000000000000000000000000000000000000000d5d00d5dd5d00d5d00000004
303333333333b333333b333333b3333330300333333300b3330000000000000000000000000000000000000099000009000000000d55d0d50d55d0d500000000
3333333b333333333333333333b333333333333333333333330333300000000000000000000000000000000000999990000000005d00d5d05d00d5d000000000
3333333b333333333333333333333333333333b33b33333333b3333300000000000000000000000000000000000000000000000000d5d0000077777000000000
3333333b333b333333333b333333333333333b33333303333333333300000000000000000000000000000000dddddddddddddddd5d0000d5577777d500000000
333333533333b3333333b333333333b333333333333d03333305433000000000000000000000000000000000dddddddddddddddd0d5d0d5d0d5d0d5d00000000
53335533b33335d3333333333333333bb3333335054d005300054d00000000000000000000000000000000aaaa000000000000005d0000d54404444400000000
03333933b33334d59333333333333333d9433305054d005d0005400000000000000000449900000000000a9aaaa00000000000000d5d5d040d44544000000000
0d339933333354d59933333433333333d094500505400050000040006000000000000044449999000099999aaaaaaaa000000000d5d00d4d44d0445d00000000
0d333954bd9454d099454d9459333335d094500005400000000000006000000499aa444444499990aaa9aaa99999aaaa004990000d55d4d44d4440d500000000
0d5339540d9404d099454d9459405055004450000040000000000000660004444994444444999999aaaaa9999999999aa55999905d0044d4d444d5d000000000
0d5099500d9400d099404d94594050050044000000000000000000006666444499999944499449999aa9999999949999554499990044d4444445d00000000000
000099000d9400009900409409400000004400000000000000000000666666999999999999444999999999999449999444449999544044d44d0000d500000000
000099000094000000000094000000000000000000000000000000006666666699999999999999999999999999994444444999994444045d4d5d0d5d00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ef000001e8810000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111f0001e22210000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000151ee1011e888810000000000000000
6600000000000000000000000000000000000000000000006600000000000000000000000000000000000000000000000111e610885d82100000000000000000
6660000000000000660000000000000000007000000000006660000000000000660000000000000000007000000000001eeee6e1885582100000000000000000
66660000444000006660000000000000626077000000000066660000444000006660000000000000660077700000000016666e2111888ee10000000000000000
66666004444400006226077444440000622677770000000066666004444400006666077044400000666077799444000001ee2220001822210000000000000000
d22266666666660062226777766666006222777776666660d222666666666600d222666444446600d22266644444660000111100000188810000000000000000
6dd66777777666666ddd6777777666666dd66777777666666dd66777777666666226677777766666622667777776666600011000000000000000000000000000
6666ddddd66666606666ddddd66666d0dd6dddddd666ddd06666ddddd66666606666d777766666600266777776666660001dd100000111600000000000000000
006ddddddddd6600006ddddddddddd00006ddddddddddd00006ddddddddd6600006dd77dd666000000007777d60000000122dd10001333310000000000000000
0000000000000000000006600000000000006666000000000000000000000000000000000000000000007770000000001dddddd1114333410000000000000000
00000000000000000000000000000000000066000000000000000000000000000000000000000000000070000000000066666666333443440000000000000000
00000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000001d55551455555530000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001665551056d6d6d50000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111100055555500000000000000000
0000000000000000000000000550000000000000000011000000000011000000000100015511000011000000000111000000000000000000001cc100005dd600
000000000000000000000022222510000000015255599510000100015550000011050015242410001500001522000110000000000000000001c77c1005dd66d0
00000000000000000000524982982200001115222d98491011550005d52500000125121124420000000012115022001000001102200000001cc77cc15ddbbddd
00000000000000000152289a99a2892001552558d4822d510525121249820000022451241242200000005122151520000005210000000000c777777c5dbbbbd5
0000028255000000052849a7a77a9a9015428d599a482951522451242492500000412412152dd01000012210152110100001000001210000c777777cdd6bbd55
00002998282500001289d99a9a79a9821522922022559910054584121545d0100001510052542150000150000255215000000000021100001cc77cc106ddd550
0005879998895000529aa5899994a8250d292dd2829ad911005451005251215000245000224415000102500005020500000000000000000001c77c1000dd5500
000289a7a999200029a77a99aad94250128ad89229a9d9510029510528451500140211022442012110001000221000000000000000000000001cc10000000000
000899a77aa2200022a7a99aa9aa9820012a99211d8998950248115249925121140024155220001110000005511000010010000000010000005dd60000000000
000289aaa7a92000029a9496a877a9921289da99182da225025424155412051101502422125001201000002111500001000000000005100005dd66d000000000
0000289999a95000042488a78a77a982592898a828d98285005224221250012000111242210102001000055521010010000000511000000055b6bdbd00000000
000052298922000000528a77aaaa98205a2d4a998450995115111242212152000000011122110500001100155511000000010011000000005b6bdbd500000000
00000002255000000028a7aa9899821059429a54425111100105511122110500000011001100000000011100110000000001100000100000ddbdbdb500000000
0000000000000000002898a8888210001a89582228100000000055001100000010001000000100000001000000000000000000000011000006ddd55000000000
00000000000000000002824252200000159d101551000000000010000000000000000000000000000000100000000510000000000000000000dd550000000000
00000000000000000000011000000000011000000000000000000000000000000000000011000000000500000000010000000000000000000000000000000000
09000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
04000000000000000000000000000000000000000fffff4000000000000000000000000000000000000000000000000000000000000000000000000000000000
666d000000000000000000000000000000000000fffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
676d00000000000000000000ffff000000000000fffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d0000000000666d000000ffff000000000000fffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
676d0000000000666d000000ffff000000000000fdfdfdf400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d00666666d0676d000004ffff4d0000000000fffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
676d00676676d0666d0000454ff454d00006666dfdfdfdf400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d00666666d0676d0000454ff454d00006666dfffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
6d6d00676676d0666d000044444444d00006666dfdfdfdf400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d00666666d0676d0004445555444d0006666dfffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
676d00666666d0666d5554445115444d5556666dfdfdfdf400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d00666666d0666d5754445115444d5756666dfffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
676d55555555555555555555555555555556666dfdfdfdf400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d57575757575757575757575757575756666dfffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
666d000000000000000000000000000000000000fffffff400000000000000000000000000000000000000000000000000000000000000000000000000000000
__label__
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccefcccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc1111fccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc151eeeccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc111e6eccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccceeeee6eecccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccce6666e2ecccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccceee222ccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccceeeeeeeecccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccce6666e2ecccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccceee222ccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccceeeecccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc66cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc666ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc6666cccc444ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc11cccccccccccccccccccccccccccccccccccccc
cccccc66666cc44444cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccd2226666666666cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc1cccccc1ccc1cccccccccccccccccccccccccccccc
cccccc6dd6677777766666ccccccccccccccccccccccccccccccc8acccccccccccccccccccccccccccc8accc11051159ccccccccccccccccc8accccccccccccc
cccccc6666ddddd666666ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc5c1122111aaa9ccccccccccccccccccccccccccccccc
cccccccc6ddddddddd66cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc2c1c12242111accccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc21cc5212242a51acccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc11ccc2255142aa41cccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc121c244220112a41cccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc514422ccc542accccccccccc7ccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc5124525cc158cccccccccccc77ccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77cccccc1cdd251214888ccccccccccc777cccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc777ccccccc224214215822ccccccccc7777cccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc77777ccccccc24421121521ccccccccc7677cccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc7776777ccccc1424251cc5611ccccccc667676ccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccc6ccccccccccccccccccccc77666667cccccd11551cc61666cccccc7676666ccccccccccccccccc
cccccccccccaaaaccccccccccccccccccccccccccccccccc666ccccccccccccccc66ccc6666666666ccdddd5cccc6666666ccccd66666666cccccdd5cccc6666
cccccccccca9aaaaccccccccccc4499ccccccccccccddccc6666cccccccc55ccc6666c666666666666ddddd55cc666666666ccd666666666cccccdd55cc66666
999cccc99999aaaaaaaaccccccc44449999ccddcccddd556666dccccccc5555cc66666666666666d666ddddd5cc666666666dd66666666666ccddddd5cc66666
9999caaa9aaa99999aaaa99aa44444449999cddd55ddd5666666dd6ccc55555cc6666666666666d56666dddd5566666666ddd6666666666666dddddd55666666
99999aaaaa9999999999a4994444444999999dd55dddd66666666ddd65566655666666666666ddd66666dddddd66666666d66666666666666666dddddd666666
449999aa999999994999999b3993bb99449996dddd3366666336666b3663bb66666666666efddd5b3663bbddd6666666dd336b666666666666336dddd336666b
4499999999999944999949b333bb3334449396336b333666b33366b333bb333666636661111f55b333bb333d666366336b333336666366336b333dddb33366b3
9999999999993333944433333b3333339b333333b333366b333333333b3333336b3336151eee33333b3333336b333333b33333336b333333b3333d6b33333333
3333333333333333333b33333b333333b333333b33333333333333333b333333b33333111e6e33333b333333b333333b33333333b333333b3333333333333333
333333333333333333333333333333333333333b333333333333b3333333333333333eeeee6ee333333333333333333b333333333333333b333333333333b333
33333333333b33b3333333333333333333333333333333333333b3333333333333333e6666e2e33333333333333333333333333333333333333333333333b333
3333333333b3333333333333b333333333b33333333333333333b333b333333333b333eee222b333b333333333b333333333333333b33333333333333333b333
5433333333333333d33333333b3333333b333333333b3333333533333b3333333b33333eeee533333b3333333b333333333b33333b333333333b333333353333
54d33b3333335354d3353b33335d3333333333333333b53335533b33335d33333333353335533b33335d3333333333333333b333333333333333b53335533b33
54333d9433335354d335db33334d5933333333333333333333933b33334d59333333333333933b33334d59333333333333333933333333333333333333933b33
34333d394533535433353333354d599333334333333333d339933333354d5993333343d339933333354d5993333343333333399333334333333333d339933333
33333d394533335433333bd9454d399454d94593333353d333954bd9454d399454d943d333954bd9454d399454d945933333599454d94593333353d333954bd9
3333333445333334333333d9434d399454d94594353553d5339543d9434d399454d943d5339543d9434d399454d945943535599454d94594353553d5339543d9
3333333443333333333333d9433d399434d94594353353d5399533d9433d399434d943d5399533d9433d399434d945943533599434d94594353353d5399533d9
3b333334433333333b3333d9433339933b394394333333333b9333d9433339933b394333399333d94b333993343943943b333993343943943b333333399333d9
b33b333333333333b33b333343333333b33b433333333333b99b333343333333b33b433339933339b33b333333394333b33b333333394333b33b333339933339
b33b3333333b333bb33b3333333b333bb33b3333333b333bb33b3333333b333bb33b3333333b333bb33b3333333b333bb33b3333333b333bb33b3333333b333b
3333343d333333333333343d333333333333343d333333333333343d333333333333343d333333333333343d333333333333343d333333333333343d33333333
44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
44d44444444444d444d44444444444d444d44444444444d444d44444444444d444d44444444444d444d44444444444d444d44444444444d444d44444444444d4
44444dd44444444444444dd44444444444444dd44444444444444dd44444444444444dd44444444444444dd44444444444444dd44444444444444dd444444444
5d0000d55d0000d55d0000d55d0000d5444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
0d5d5d000d5d5d000d5d5d000d5d5d04444444444446644444444444444444444444444444444444444444444444444444444444444444444444444444444444
d5d00d5dd5d00d5dd5d00d5dd5d00d4d4ddd44444d6666444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd4444
0d55d0d50d55d0d50d55d0d50d55d4d44444444446666dd444444444444444444444444444444444444444444444444444444444444444444444444444444444
5d00d5d05d00d5d05d00d5d05d0044d44444444d4666ddd44444444d4444444d4444444d4444444d4444444d4444444d4444444d4444444d4444444d4444444d
0077777000d5d00000d5d0000044d44444444444566ddd5444444444444444444444444444444444444444444444444444444444444444444444444444444444
577777d55d0000d55d0000d5544044d4444dd44444444444444dd444444dd444444dd444444dd444444dd444444dd444444dd444444dd444444dd444444dd444
0d5d0d5d0d5d0d5d0d5d0d5d4444045d444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
5d0000d55d0000d55d0000d544444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
0d5d5d000d5d5d000d5d5d0444466444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
d5d00d5dd5d00d5dd5d00d4d4d6666444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd44444ddd4444
d66666d5d66666d5d66666d4d66666d4d6666644d6666644d6666644d6666644d6666644d6666644d6666644d6666644d6666644d6666644d6666644d6666644
59dddd9059dddd9059dddd9459dddd9459dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d59dddd9d
9ddddd099ddddd099ddddd499ddd7d599ddddd499ddddd499ddddd499ddddd4459dddd499ddddd499ddddd499ddddd499ddddd499ddddd499ddddd499ddddd49
5dddddd55dddddd55dddddd45ddedd445ddddd445ddddd445ddddd445ddddd445d9ddd445ddddd445ddddd445ddddd445ddddd445ddddd445ddddd445ddddd44
1d4dd45d1ddddd5d1ddddd5d1deedd441ddddd441ddddd441ddddd441d4dd44419dddd441ddddd441ddddd441ddddd441ddddd441ddddd441ddddd441ddddd44
d54dd4ddddddddddddddddddddddddddddddddddddddddddddddddddd54dd4ddddddddddddddddddddddddddddddddddddddddddddddddddddddddefdddddddd
dd5dddddddddddddddddddddddddd7dddddddddddddddddddddddddddd5ddddddddddddddddddddddddddddddddddddddddddddddddddddddddd1111fddddddd
3534513333153333355451553445155451433333135333415554515534451554535333415554515534451554514333331445155451433333135151eee5545ddd
3335453333131553315533335514333333333333333333334335333355143333333333334335333355143333333333333334333333333333333111e6e3353335
533533333313155331353333153333333333333333513331133533331533333333313331133533331533333333333333335333333333333333eeeee6ee353315
333333335333333333333353333333335333333333333333335333533333333353333333335333533333333353333333333333335333333333e6666e2e533315
3333333333333333333333333333333333333335333333333333533333333333333333333333533333333333333333353333333333333335333eee2223335333
33333333333333333335333335333333533333353333333333333333353333335333333333333333353333335333333533333333533333353333eeee33333333
53444444444444333345553333353333334533333353333dd53333333335333333d5333dd53333333335333333d533333353333333d5333333533335d5333333
dd554444dd44444444544444453443554455444d555533ddddd33dddd53dd355ddddddddddd33555153dd355555ddddddd5533d5dddddddddddd33d555533ddd
dd44441ddd4ddd44444dddd44dd555555544441555115551dddddd55d1111111111ddddddddddddd51dddd555511dddddddd555ddddddddddddd55555511dddd
dddd444111144444dddddddd111111155554444115511155511dddd51111111111111dddddd4444dddd5ddd55555111ddddddddd55ddddddddddd1155555111d
111111111111111dddd111111111111111111111111111111111ddd111111111111111dd14dddd404dddd11555511111ddddddd11115dddddddd111115511111
111111111111111111111111111111111111111111111111111111111111111111111111110004d4d66611111111111111ddd11111111dd6d6d1111111111111
1111111111111111111111111111111111111111111111111111111111111111111111111110000e661111111111111111111111111111666611111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111114e66111111111111111111111111111111666111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111050501011110011111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111005005001400111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111105500111150d11111111111111111111111111111
111111111111111111111111111111111111111111111111111111111111111111111111111111111111001110011050dd501111111111111111111111111111
1111111111d555555555dd1111111111111111111111111111111111111111111111111111111111111111010100050000d11111111111111111111111111111
1111115000dddddddddd111111111111111111111111111111111111111111111111111111111111111111011111101110111111111111111111111111111111
1111dddd111155511111111111111111111111111111111111111111111111111111111111111111111111001111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114444111111111111
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114dddd4041111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144400011111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144444d44111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000e11111111111
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114e111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__map__
01020304050607080901020304050607082728292a000028292a0000000a0102030407080708090a01020304050600000000280000000007070009000000000000000708090a010203040506070102030405060708090a0708090102030408040708090a0102030405060708090102030405060708090a090102070405020308
11121314151617181911121314151617183738393a3b3c38393a3b3c3c1a1112131414181718191a11121314151637383b3c38393a3b383938393c3a3b38393c3a161314191a111213141516171112131415161718191a171819111213141112171819191112131415161a181911121314151617181912131112171415161318
2021222320212223202122232021222320218081828384808184852425212223202122202122232223202122232021242526242525202124252621222320202122232425262021222324212021222324232021222320212224252624252021222320212223202122232021222422212323202122242221222323242526252223
3031323330313233303132333031323330319091929394909194953435313233303132303132333233303132333031343536343535303134353631323330303132333435363031323334313031323334333031323330313234353634353031323330313233303132333031323432313333303132343231323333343536353233
1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1e1d1e1d1e1f1e1d1e1d1f1d1e1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1e
0d3e2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d3d0d0e0d0d0d0d0d0d0d0d0d0d0f0d0d0d0d0d0d0d0d0d0d0e0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d
3e2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d3d0e0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0f0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0e0d0d0d0d0d0d0d
2b0b2b0c2b0b2c1b2b0b2b0c2b0b2b1c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000200000f350356501b4503565026450000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200003b65037950316501b92025920376501d9203265028650139201f6500e920169202c650386502565035c5016650326500fc5027650339502265028950326501b950256500f6300f6400e6300c63000020
0001000039556305562b5562757623576205761b55618556135560f5760e5760d5760c5560b5560a5560857608576085760855608556000000000000000000000000000000000000000000000000000000000000
00011b1f3c6763c1523c6563c1523c6563c1523c6563c1523c6563c1523c6563c1423c1423c6463c1423c1423c6333c1323c1323c6333c1323c1223c1223c6233c1223c1223c1123c6133c1123c1123c1123c112
0002000004410084300b240164501e350214502c45037210044200642009330104301233017440204403445039250392503f4502945023450194500e4500c0500a45009250074300642006420064100000000000
0008000012570125700b5700757007570075700007000020000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000100003555032350355203235035520323203552032350355503232035550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000f00200c15500155001550015503155001550015500157051550015500155001550c1550015500155001550c155001550315500155051550015507155051550315500155001550015505155001550715500155
000f00000015500145001410c15500145001450014103155001510514500145071450015105145001550c145001550c755001550c755001550c75500155051550c155031550c155051550c155031550c15505155
010f002000000000003c603000003c653000003c603000003c603000003c625000003c6533c6253c6350000000000000003c605000003c650000003c6253c6333c000000003c6073c6373c6633c6253c6333c657
001e00000c0533c0050c053000000c053000000c053000000c053000000c053000000c053000000c0530c0230c053000000c053000000c053000000c053000000c0533c6230c053000000c0533c6230c05300000
011e00203c7353c7303c5003c5003c5003c5050000000000335303353033530335352e5302e5302e535000003c7353c730000000000000000000000000000000337303373033730337302e7302e7302e7302e730
00140018002550025500355002550035500355002550345500255005550525500255002550745500555004550c35500455003550f4550c3551835500000000000000000000000000000000000000000000000000
__music__
01 094b4d44
00 0a4b4d44
00 0a0c4b44
01 0a0c4b44
00 090c0b44
02 090c0d44

