pico-8 cartridge // http://www.pico-8.com
version 16
__lua__

drawn=0
local sf1
local vel
local damping

local profile={}


function _init()
 pos=vec2d.new(0,0)
 vel=vec2d.new(0,0)
 damping=vec2d.new(0.9,0.9)
 
 sf1=sf.new(512,512,1,400,{1,5})
 sf2=sf.new(1024,1024,2,800,{12,13,6})
-- sf3=sf.new(512,512,4,600,{6,7,15})

 local cp=campos()
 printh(tostr(cp[1])..","..tostr(cp[2]))
 
end

function _update()
 
 if (btn(0)) vel.x-=0.3
 if (btn(1)) vel.x+=0.3
 if (btn(2)) vel.y-=0.3
 if (btn(3)) vel.y+=0.3
 
 if (btnp(5)) printh(sf1.qt:tostring())
 
 vel:clamp(10,10)
 vel *= damping
 pos += vel
 
 drawn=0
end

function _draw()
 cls()
 camera(pos.x,pos.y)

 profile={} 
 sf1:draw()
 sf2:draw()
 --sf3:draw()
 draw_debug()
end

function draw_debug()
 --sf2.qt:draw_debug()
 camera()
 print(stat(1),2,2,7)
 print(stat(0),2,10,7)
 print(tostr(drawn),2,18,7)

end
-->8
-- objects
vec2d={}
vec2d={
 new=function(x,y)
  local t = {x=x or 0,y=y or 0}
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
 __unm=function(a)
  return vec2d.new(-a.x,-a.y)
 end,
 clamp=function(t,x,y)
  if (t.x > x) t.x =x
  if (t.y > y) t.y =y 
 end,
 copy=function(t)
  return vec2d.new(t.x,t.y)
 end,
 tostring=function(t)
  return "["..tostr(t.x)
   .. "," ..tostr(t.y)
   .. "]"
 end
}
vec2d.__index=vec2d


--starfield
-- width,height,z-depth,density,[]colours
sf={}
sf={
 new=function(w,h,dist,dens,cols)
  local t={
   w=w or 128,
   h=h or 128,
   dst=dist or 1,
   d=dens or 100,
   cols=cols or {5,13,6,7},
   ob={},
  }
  t.pos=vec2d.new(t.w/2, t.h/2)
  t.qt=qt.new(
   t.pos,max(t.w/2,t.h/2),
   30
  ),
  
  setmetatable(t,sf)
  sf.init(t)
  return t
 end,
 init=function(t)
  printh("sf->init")
  local x=0
  local y=0
  while (y<=t.h) do
   x+= flr(rnd(t.d))
   local row=flr(x/t.w)
   if (row>0) y+=row
   local c=1 + (x+y)%#t.cols
   x=x%t.w
   t.qt:add({
    pos=vec2d.new(x,y),
    c=c
   })
  end
  
  printh("added stars: "..tostr(t.qt:count()))
 end,
 draw=function(t)
  local cp = campos()
  local loc = vec2d.new(cp[1]+64,cp[2]+64)
  loc = loc * vec2d.new(t.dst,t.dst)
    
  camera(cp[1]*t.dst, cp[2]*t.dst)


  pal()
  for o in all(t.qt:query(loc,128)) do
   drawn+=1
   local trail = o.pos+(-vel*vec2d.new(t.dst,t.dst))
   line(o.pos.x,o.pos.y,
    trail.x,trail.y,
    1
   )
   pset(o.pos.x,o.pos.y,t.cols[o.c])
 --  pset(o.pos.x,o.pos.y,14)
  end

  assert(drawn != t.qt:count())
  

  camera( cp[1],cp[2] ) 
 end,
 
}
sf.__index=sf


function campos()
  --0x28..0x2b
  local xl=peek(0x5f00+ 0x28)
  local xh=peek(0x5f00+ 0x29)  
  local yl=peek(0x5f00+ 0x2a)
  local yh=peek(0x5f00+ 0x2b)
  
  
  return { shl(xh,8)+xl, shl(yh,8)+yl}
end
-->8
-- quadtree
qt={}
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
  if t.pos.x-t.sz <= pos.x 
      and
     t.pos.x+t.sz > pos.x
      and
     t.pos.y-t.sz <= pos.y
      and
     t.pos.y+t.sz > pos.y then
     return true
  else
   return false
  end
 end,
 intersect=function(t,x1,y1,x2,y2)
  local qx=x1+( (x2-x1)/2)
  local qy=y1+( (y2-y1)/2)
  local qsx = (x2-x1)/2
  local qsy = (y2-y1)/2
  
  local dx = qx - t.pos.x
  local px = (qsx + t.sz) - abs(dx)
  if (px <= 0) return false
  
  local dy = qy - t.pos.y
  local py = (qsy + t.sz) - abs(dy)
  if (py <= 0) return false
  
  return true
  
  
--[[   printh("no-inter:"..t.pos:tostring()
    .. "("..tostr(x1)
     ..","..tostr(y1)
     .."),"..tostr(x2)
     ..","..tostr(y2)..")"
   )]]--
 end,
 add=function(t,o)
  if not t:has_point(o.pos) then
   return false
  end
  
  if (
    #t.ob < t.cap
    and not t.nw
   ) then
   add(t.ob,o)
   return true
  end
  
  if (not t.nw) t:subdivide()
  assert(count(t.ob)==0)
  if (t.nw:add(o,r)) return true
  if (t.ne:add(o,r)) return true
  if (t.sw:add(o,r)) return true
  if (t.se:add(o,r)) return true
  -- should never happen
  printh("cannot add:"
   .. o.pos:tostring()
  ) 
  return assert(false)
  
 end,
 subdivide=function(t)
  printh("subdiv:["
   ..tostr(t.pos.x) .. ","
   ..tostr(t.pos.y) .. "] - "
   ..tostr(t.sz) .. " cap:"
   ..tostr(t.cap) .. " #obj:"
   ..tostr(count(t.ob))
  )
  
  
  local half= t.sz/2
  t.nw=qt.new(
    t.pos + vec2d.new(-half,-half),
    half,
    t.cap
  )
  t.ne=qt.new(
    t.pos + vec2d.new(half,-half),
    half,
    t.cap
  )
  t.sw=qt.new(
   t.pos + vec2d.new(-half,half),
   half,
   t.cap
  )
  t.se=qt.new(
   t.pos + vec2d.new(half,half),
   half,
   t.cap
  )

  local q={t.nw,t.ne,t.sw,t.se}  
  local gone=0
  for o in all(t.ob) do
   ::try::
   for c in all(q) do
    if c:add(o)==true then
     gone+=1
     break ::try::
    end
   end
  end
  assert(gone == #t.ob)
  t.ob={}
  assert(#t.ob==0)

  
 end,
 query=function(t,pos,r)
  local m={}
  
  if not t:intersect(pos.x-r,pos.y-r,pos.x+r,pos.y+r) then
   return m
  end

  
  if t.nw==nil then
    for o in all(t.ob) do
   --if range:has_point(o.pos) then
    add(m,o)
   --end
   end
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
 count=function(t)
  local c = #t.ob
  if (t.nw==nil) return c
  c+=t.nw:count()
  c+=t.ne:count()
  c+=t.sw:count()
  c+=t.se:count()
  return c
 end,
 objects=function(t)
  local o={}
  for i in all(t.ob) do
   add(o,t)
  end
  if (t.nw==nil) return o
  for bx in all({t.nw,t.ne,t.sw,t.se}) do
   local i=bx:objects()
   for it in all(i) do
    add(o,it)
   end  
  end
  return o
 end,
 draw_debug=function(t,col,d,n)
  --camera()
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
   return
  end
 --[[ print(tostr(count(t.ob)),
  	90 + (n*8),
  	20+(d*8),
  	col
  )]]--
  local fll=0b1010010110100101
  if (d%2==0) then
   --fll=shl(fll,1) +1
   fll=0b0101101001011010
  end
  fillp(fll)
  line(t.pos.x,t.pos.y,
   t.pos.x-t.sz,t.pos.y-t.sz,col
  )
  rect(t.pos.x-t.sz,t.pos.y-t.sz,
   t.pos.x+t.sz,t.pos.y+t.sz, col )
  fillp()
  pset(t.pos.x,t.pos.y,10)

  //print(t.pos:tostring(),t.pos.x,pos.y)
  
   
 end,
  tostring=function(t,d)
  local d=d or 0
  local cnr={t.nw,t.ne,t.sw,t.se}
  local pad=""
  while (#pad < d) do
   pad = pad .. "\t"
  end
  local str = pad ..
   "qt,depth=" .. d 
   .. "[".. t.sz .. "("
   .. t.pos.x .. "," .. t.pos.y .. ")"
   .. " =# " .. #t.ob
  
  if (t.nw == nil) return str
  for q in all(cnr) do
   str = str 
    .."\n" 
    .. q:tostring(d+1)
  end
  return str
 end

}
qt.__index=qt
