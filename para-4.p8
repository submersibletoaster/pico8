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

function _init()
 music(0,30)
end


function _update()

 update_coll()
 player_input()
 
 player_update()
 
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
 

 -- layer scrolling
 l1.x+=0.333333
 l2.x+=1
 l3.x+=2
 l3.x+=2.5
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
 set_pal(ref_pal,0)

-- draw_debug()
end

function draw_debug()
 
	camera()
 color(4)
 print(stat(7),10,0)
 print(stat(1),10,10)
 print(stat(0),10,20)
 
 print(#nmes,10,30,8)
 print(#wrks,10,40,5)
 
 coll:draw_debug()

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
 draw_player()
 
 for w in all(weps) do
  w:draw()
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
 
 




end

function draw_ref_scene()
 camera()
 local rh=60
 draw_sky(rh)
 draw_grass(rh,15)
 draw_l4(rh)
 draw_l3(rh)
 -- layer 2 is 'flat' unseen
 -- by reflection
 draw_l1(rh,8)
 
 
 draw_player()


 for n in all(nmes) do
  n:draw()
 end
 
  
 for p in all(puffs) do
  p:draw()
 end

 set_pal(ref_exp_pal) 
 for w in all(wrks) do
  w:draw()
 end

 
 -- brighter pal for weps
 pal()
 for w in all(weps) do
  w:draw()
 end
 


end
-->8
-- reflection fx
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
   	gfx+ ( (soff-(i*2)+mod[ms] )*64 ) + mod[msx],
   	64
   )
 end


end
-->8
-- collision
local qt={}
qt={
 new=function(bx,by,sz,cap)
  local t={
   cap=cap or 4,
   ob={},
   bx=bx or 64,
   by=by or 64,
   sz=sz or 64,
   nw=nil,ne=nil,sw=nil,se=nil
  }
  setmetatable(t,qt)
  return t
 end,
 has_point=function(t,x,y)
  if t.bx-t.sz < x 
      and
     t.bx+t.sz > x
      and
     t.by-t.sz < y
      and
     t.by+t.sz > y then
     return true
  else
   return false
  end
 end,
 intersect=function(t,x1,y1,x2,y2)
  if t.bx - t.sz < x1
     or
     t.bx + t.sz > x2
     or
     t.by - t.sz < y1
     or
     t.by + t.sz > y2
     then
     return true
  else
   return false
  end
 end,
 add=function(t,o)
  if not t:has_point(o.x,o.y) then
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
--[[  printh("subdiv:["
   ..tostr(t.bx) .. ","
   ..tostr(t.by) .. "] - "
   ..tostr(t.sz)
  )
  ]]--
  local half= t.sz/2
  t.nw=qt.new(
    t.bx-half,
    t.by-half,
    half
   )
  t.ne=qt.new(t.by+half,t.by+half,half)
  t.sw=qt.new(t.bx-half,t.by-half,half)
  t.se=qt.new(t.bx+half,t.by-half,half)

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
 query=function(t,x,y,r)
  local m={}
  
  if not t:intersect(x-r,y-r,x+r,y+r) then
   return m
  end

  local range=qt.new(x,y,r)  
  for o in all(t.ob) do
   if range:has_point(o.x,o.y) then
    add(m,o)
   end
  end
  
  if t.nw==nil then
   return m
  end
  
  local q={t.nw,t.ne,t.se,t.sw}
  for c in all(q) do
   local g=c:query(x,y,r)
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
  line(t.bx,t.by,t.bx-t.sz,t.by-t.sz,col)
  rect(t.bx-t.sz,t.by-t.sz,
   t.bx+t.sz,t.by+t.sz, col )
  fillp()
  pset(t.bx,t.by,10)
  

  
  
 end
}
qt.__index=qt


function update_coll()
 coll=qt.new()
 
 for n in all(nmes) do
  coll:add(n)
 end
 
 for w in all(weps) do
  local close = coll:query(w.x,w.y,4)
  for n in all(close) do
   n:hit(w)
   w.die=true
   break
  end 
 end
 
 

end

-->8
-- player
local player={x=25,y=40,turn=3,
 tframe={68,66,64,72,74},
 tdecay=0,tdamp=0,
 shot=0,shotdecay=0,
 score=0
}

function player_input()

 -- shoot
 if btn(4) then
  player.shot=1
 end 

 -- rolling up/down 
 if btn(2) then
  player.turn=max(player.turn-1,1)
  player.tdamp=15
  player.tdecay=1
  player.y-=1.5
 elseif btn(3) then
  player.turn=min(player.turn+1,5)
  player.tdamp=10
  player.tdecay=-1
  player.y+=1.5
 end
 
 -- forward / backwards
 if btn(0) then
  player.x-=1.5
 elseif btn(1) then
  player.x+=1.5
 end
 
end

function player_update()
 -- turn damping
 if player.tdamp != 0 then
  player.tdamp-=1
  
  if player.tdamp%5 == 0 then
   player.turn+= player.tdecay
  end
   
  if player.turn==3 then
   player.tdamp=0
  end
 end
 
 if player.shot !=0 then
  if player.shotdecay==0 then
   wep.new(player.x,player.y)
   player.shotdecay=5
   sfx(0)
  else
   player.shotdecay-=1
   player.shot=0
  end
 end
end

function draw_player()
 camera()
 spr( player.tframe[ player.turn ],
  player.x-8, player.y-8,
  2,2
 )
 
end
 
 
-->8
-- weapons
weps={}
wep={}
wep={
 new=function(x,y)
  local t={x=x or 0,y=y or 64}
  t.dx=5
  setmetatable(t,wep)
  add(weps,t)
  return t
 end,
 update=function(t)
  t.x+= t.dx
  if t.x > 128 then
   t.die=true
  end
 end,
 draw=function(t)
  pset(t.x,t.y,10)
  pset(t.x-1,t.y,8+ t.x%2)
  if t.die then
   del(weps,t)
  end
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
 new=function(x,y,dx)
  local t={
   x=x or 130,
   y=y or (10+flr(rnd(80)) ),
   dx=dx or -1
  }
  setmetatable(t,nme)
  add(nmes,t)
  return t
 end,
 update=function(t)
  -- move ahead and chase player
  t.x += t.dx
  
  --[[
  pl_dy = player.y - t.y
  if pl_dy > 0 then
   t.y+=0.5
  elseif pl_dy < 0 then
   t.y-=0.5
  end
  ]]--
  
  -- off screen extinction
  if t.x < -4 or t.x > 130 then
   t.die=true
  end
  
 end,
 draw=function(t)
  spr(76,t.x-4,t.y-4)
  if t.die then
   del(nmes,t)
  end
 end,
 hit=function(t,w)
  t.hits=t.hits or 0
  t.hits+=1
  sfx(1,1)
  wrk.new(t.x,t.y)
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
wrk={
 new=function(x,y)
  local p={}
  -- debris
  for i=5,5+flr(rnd(5)) do
   local dir = (i%2)-0.5
   local d={
    x=x+(rnd(8)-4),
    y=y+(rnd(8)-4),
    dx=rnd(3)*dir,
    dy=-rnd(2),
    lt=120+rnd(200)
   }
   puff.new(d.x,d.y,d.dx,d.dy)
   add(p,d)
  end
  local t={x=x, y=y,
   p=p, f=hz+abs(hz-y),
   c=0,a=0,
   flp=y%2==0 and true or false,
   flp2=x%2==0 and true or false
  }
  setmetatable(t,wrk)
  add(wrks,t)
  return t
 end,
 update=function(t)
  for p in all(t.p) do
   p.dx *= 0.95 --damping
   --p.dy *= 0.95
   p.dy += 0.09 -- grav
   p.x+= p.dx
   p.y+= p.dy
  
   -- bounce
   if p.y > t.f then
    p.dy = -p.dy*0.4
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
   t.x-8,t.y-8,
   2,2,
   t.flp,t.flp2
  )
  end
 
 
  for p in all(t.p) do
   circfill(p.x,p.y,1,
   8+(p.lt%2)
--    debris_col[     1+((p.lt+p.y) %#debris_col] ]
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
 new=function(x,y,dx,dy,sz)
  local t={x=x,y=y,
   dx=dx or 0,
   dy=dy or 0,
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
  t.x+= t.dx
  t.y+= t.dy
  
  t.dx *= 0.95
  t.dy *= 0.95
  t.dy += 0.06
  add(t.prev,{t.x,t.y})
  
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
    pos[1],pos[2],
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
303333333333b333333b333333b3333330300333333300b3330000000000000000000000000000000000000000000000000000000d55d0d50d55d0d500000000
3333333b333333333333333333b333333333333333333333330333300000000000000000000000000000000000000000000000005d00d5d05d00d5d000000000
3333333b333333333333333333333333333333b33b33333333b3333300000000000000000000000000000000000000000000000000d5d0000077777000000000
3333333b333b333333333b333333333333333b3333330333333333330000000000000000000000000000000000000000000000005d0000d5577777d500000000
333333533333b3333333b333333333b333333333333d0333330543300000000000000000000000000000000000000000000000000d5d0d5d0d5d0d5d00000000
53335533b33335d3333333333333333bb3333335054d005300054d00000000000000000000000000000000aaaa000000000000005d0000d54404444400000000
03333933b33334d59333333333333333d9433305054d005d0005400000000000000000449900000000000a9aaaa00000000000000d5d5d040d44544000000000
0d339933333354d59933333433333333d094500505400050000040006000000000000044449999000099999aaaaaaaa000000000d5d00d4d44d0445d00000000
0d333954bd9454d099454d9459333335d094500005400000000000006000000499aa444444499990aaa9aaa99999aaaa004990000d55d4d44d4440d500000000
0d5339540d9404d099454d9459405055004450000040000000000000660004444994444444999999aaaaa9999999999aa55999905d0044d4d444d5d000000000
0d5099500d9400d099404d94594050050044000000000000000000006666444499999944499449999aa9999999949999554499990044d4444445d00000000000
000099000d9400009900409409400000004400000000000000000000666666999999999999444999999999999449999444449999544044d44d0000d500000000
000099000094000000000094000000000000000000000000000000006666666699999999999999999999999999994444444999994444045d4d5d0d5d00000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ef00000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001111f0000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000151eee0000000000000000000000000
6600000000000000000000000000000000000000000000006600000000000000000000000000000000000000000000000111e6e0000000000000000000000000
666000000000000066000000000000000000700000000000666000000000000066000000000000000000700000000000eeeee6ee000000000000000000000000
666600004440000066600000000000006260770000000000666600004440000066600000000000006600777000000000e6666e2e000000000000000000000000
6666600444440000622607744444000062267777000000006666600444440000666607704440000066607779944400000eee2220000000000000000000000000
d22266666666660062226777766666006222777776666660d222666666666600d222666444446600d22266644444660000eeee00000000000000000000000000
6dd66777777666666ddd6777777666666dd66777777666666dd66777777666666226677777766666622667777776666600000000000000000000000000000000
6666ddddd66666606666ddddd66666d0dd6dddddd666ddd06666ddddd66666606666d77776666660026677777666666000000000000000000000000000000000
006ddddddddd6600006ddddddddddd00006ddddddddddd00006ddddddddd6600006dd77dd666000000007777d600000000000000000000000000000000000000
00000000000000000000066000000000000066660000000000000000000000000000000000000000000077700000000000000000000000000000000000000000
00000000000000000000000000000000000066000000000000000000000000000000000000000000000070000000000000000000000000000000000000000000
00000000000000000000000000000000000060000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000055000000000000000001100000000001100000000010001551100001100000000011100000000000000000000cccc00005dd600
00000000000000000000002222251000000001525559951000010001555000001105001524241000150000152200011000000000000000000cc77cc005dd66d0
00000000000000000000524982982200001115222d98491011550005d5250000012512112442000000001211502200100000110220000000ccc77ccc5ddbbddd
00000000000000000152289a99a2892001552558d4822d510525121249820000022451241242200000005122151520000005210000000000c777777c5dbbbbd5
0000028255000000052849a7a77a9a9015428d599a482951522451242492500000412412152dd01000012210152110100001000001210000c777777cdd6bbd55
00002998282500001289d99a9a79a9821522922022559910054584121545d010000151005254215000015000025521500000000002110000ccc77ccc06ddd550
0005879998895000529aa5899994a8250d292dd2829ad91100545100525121500024500022441500010250000502050000000000000000000cc77cc000dd5500
000289a7a999200029a77a99aad94250128ad89229a9d951002951052845150014021102244201211000100022100000000000000000000000cccc0000000000
000899a77aa2200022a7a99aa9aa9820012a99211d89989502481152499251211400241552200011100000055110000100100000000100000000000000000000
000289aaa7a92000029a9496a877a9921289da99182da22502542415541205110150242212500120100000211150000100000000000510000000000000000000
0000289999a95000042488a78a77a982592898a828d9828500522422125001200011124221010200100005552101001000000051100000000000000000000000
000052298922000000528a77aaaa98205a2d4a998450995115111242212152000000011122110500001100155511000000010011000000000000000000000000
00000002255000000028a7aa9899821059429a544251111001055111221105000000110011000000000111001100000000011000001000000000000000000000
0000000000000000002898a8888210001a8958222810000000005500110000001000100000010000000100000000000000000000001100000000000000000000
00000000000000000002824252200000159d10155100000000001000000000000000000000000000000010000000051000000000000000000000000000000000
00000000000000000000011000000000011000000000000000000000000000000000000011000000000500000000010000000000000000000000000000000000
__map__
01020304050607080901020304050607082728292a2b2c28292a2b2c2c0a0102030407080708090a010203040506000000002800000000070708090a0102030405060708090a010203040506070102030405060708090a0708090102030408040708090a0102030405060708090102030405060708090a090102070405020308
11121314151617181911121314151617183738393a3b3c38393a3b3c3c1a1112131414181718191a11121314151637383b3c38393a3b38391718191a1112131415161314191a111213141516171112131415161718191a171819111213141112171819191112131415161a181911121314151617181912131112171415161318
2021222320212223202122232021222320212223202122232425262425212223202122202122232223202122232021242526242525202124252621222320202122232425262021222324212021222324232021222320212224252624252021222320212223202122232021222422212323202122242221222323242526252223
3031323330313233303132333031323330313233303132333435363435313233303132303132333233303132333031343536343535303134353631323330303132333435363031323334313031323334333031323330313234353634353031323330313233303132333031323432313333303132343231323333343536353233
1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1e1d1e1d1e1f1e1d1e1d1f1d1e1d1e1d1e1d1e1d1f1d1e1d1e1d1e1d1e
0d3e2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d2d2e2d2d3d0d0e0d0d0d0d0d0d0d0d0d0d0f0d0d0d0d0d0d0d0d0d0d0e0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d
3e2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d2d3d0e0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0f0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0e0d0d0d0d0d0d0d
0b0b0b0c1b0b0b0b0b0b0b0b0c0b0b1c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000200000f350356501b4503565026450000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200003b65037950316501b92025920376501d9203265028650139201f6500e920169202c650386502565035c5016650326500fc5027650339502265028950326501b950256500f6300f6400e6300c63000020
0101000039556305562b5562757623576205761b55618556135560f5760e5760d5760c5560b5560a5560857608576085760855608556000000000000000000000000000000000000000000000000000000000000
00011b1f3c6763c1523c6563c1523c6563c1523c6563c1523c6563c1523c6563c1423c1423c6463c1423c1423c6333c1323c1323c6333c1323c1223c1223c6233c1223c1223c1123c6133c1123c1123c1123c112
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000f00200c15500155001550015503155001550015500157051550015500155001550c1550015500155001550c155001550315500155051550015507155051550315500155001550015505155001550715500155
010f00000015500145001410c15500145001450014103155001510514500145071450015105145001550c145001550c755001550c755001550c75500155051550c155031550c155051550c155031550c15505155
000f002000000000003c6350000000000000003c6350000000000000003c63500000000003c6253c6350000000000000003c6550000000000000003c6550000000000000003c65500000000003c6253c6553c657
011e00000c0533c0050c053000000c053000000c053000000c053000000c053000000c053000000c0530c0230c053000000c053000000c053000000c053000000c0533c6230c053000000c0533c6230c05300000
0014001d002550025500255002550025500255002550325500255002550525500255002550725500255002550c25500255002550f2550c2550c255112550c2550c2550c255002550c25500255052550c25500255
01140220002550025500355002550035500355002550345500255005550525500255002550755500555005550c25500555005550f2550c5550c255115550c2550c2550c255003550c35500355053550c35518355
__music__
01 090b0c44
00 0a0b0c44
00 0d0c0b44
02 0e0c0b44

