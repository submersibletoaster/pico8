pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- fill user memory with green test pattern
memset(0x4300,shl(3,4)+11,0x1000)

umem=0x0800
gmem=0x6000


local test
function _init()
 u_sss=mk_sss(umem)
 g_sss=mk_sss(gmem)

 tu=u_sss.new(16,16)
 tg=g_sss.new(24,24)
 
 
 local up=function(x,y,v)
   tu:pset(x,y,v)
 end
 local gp=function(x,y,v)
   tg:pset(x,y,v)
 end

 scale2x(1,8,8,0,0,0,up)
 --scale3x(1,8,8,0,0,0,gp)  
 --scale2x(2,16,16,64,0,0,vp)
end


state= {}
state.scale2x=function()
  for i=1,5 do
   scale2x(1,8,8,60,60,0,pset)
   --scale2x(2,16,16,10,10,0,pset)
  end
end

state.memcpy=function()
 for i=1,5 do
  tu:draw(60,60) 
 end
end



gs=state.scale2x
function _update() 
 if btn(4) then
  gs=state.scale2x
 end
 if btn(5) then
  gs=state.memcpy 
 end
end

function _draw()
 cls()
 color(1)
 rectfill(0,0,127,127)

 --crosshair
 color(7)
 line(60,0,60,128)
 line(0,60,128,60)

 gs()

 color(8)
 print(stat(1),0,120)

end

-->8
umem=0x0800
gmem=0x6000

--[[
sind : sprite index 
sz_x : x size
sz_y : y size
sx    : screen pos x
sy    : screen pos y
alpha: color to make transparent
]]--

function scale2x(sind,sz_x,sz_y,sx,sy,alpha,func)
 alpha=alpha or 0
 local offx=sind%16
 local offy=flr(sind/16)
 local soffx=offx*8
 local soffy=offy*8
 local sizex=sz_x-1
 local sizey=sz_y-1
 local a,b,c,d,e,f,g,h,i,
        e0,e1,e2,e3,x0,y0

 for y=0,sizey do
  for x=0,sizex do
   e=sget(soffx+x,soffy+y)
   a=e
   b=e
   c=e
   d=e
   f=e
   g=e
   h=e
   i=e

   if y>0 then
    b=sget(soffx+x,soffy+y-1)
   end

   if y<sizey then
    h=sget(soffx+x,soffy+y+1)
   end

   if x>0 then
    d=sget(soffx+x-1,soffy+y)
    if y>0 then
     a=sget(soffx+x-1,soffy+y-1)
    end
    if y<sizey then
     g=sget(soffx+x-1,soffy+y+1)
    end
   end

   if x<sizex then
    f=sget(soffx+x+1,soffy+y)

    if y>0 then
     c=sget(soffx+x+1,soffy+y-1)
    end
    if y<sizey then
     i=sget(soffx+x+1,soffy+y+1)
    end
   end

   e0=e
   e1=e
   e2=e
   e3=e

   if b!=h and d!=f then
    if(d==b) e0=d
    if(b==f) e1=f
    if(d==h) e2=d
    if(h==f) e3=f
   end

   --draw
   x0=sx+x*2
   y0=sy+y*2
   if(e0!=alpha) func(x0,  y0,  e0)
   if(e1!=alpha) func(x0+1,y0,  e1)
   if(e2!=alpha) func(x0,  y0+1,e2)
   if(e3!=alpha) func(x0+1,y0+1,e3)
  end
 end
end

--[[
sind : sprite index 
sz_x : x size
sz_y : y size
sx    : screen pos x
sy    : screen pos y
alpha: color to make transparent
]]--

function scale3x(sind,sz_x,sz_y,sx,sy,alpha,func)
 alpha=alpha or 0
 local offx=sind%16
 local offy=flr(sind/16)
 local soffx=offx*8
 local soffy=offy*8
 local sizex=sz_x-1
 local sizey=sz_y-1
 local a,b,c,d,e,f,g,h,i,
       e0,e1,e2,e3,e4,e5,e6,e7,e8,
       x0,y0

 for y=0,sizey do
  for x=0,sizex do
   e=sget(soffx+x,soffy+y)
   a=e
   b=e
   c=e
   d=e
   f=e
   h=e
   i=e
   g=e

   if y>0 then
    b=sget(soffx+x,soffy+y-1)
   end

   if y<sizey then
    h=sget(soffx+x,soffy+y+1)
   end

   if x>0 then
    d=sget(soffx+x-1,soffy+y)
    if y>0 then
     a=sget(soffx+x-1,soffy+y-1)
    end
    if y<sizey then
     g=sget(soffx+x-1,soffy+y+1)
    end
   end

   if x<sizex then
    f=sget(soffx+x+1,soffy+y)

    if y>0 then
     c=sget(soffx+x+1,soffy+y-1)
    end
    if y<sizey then
     i=sget(soffx+x+1,soffy+y+1)
    end
   end

   e0=e
   e1=e
   e2=e
   e3=e
   e4=e
   e5=e
   e6=e
   e7=e
   e8=e

   if b!=h and d!=f then
    if(d==b) e0=d
    if((d==b and e!=c) or (b==f and e!=a)) e1=b
    if(b==f) e2=f
    if((d==b and e!=g) or (d==h and e!=a)) e3=d
    if((b==f and e!=i) or (h==f and e!=c)) e5=f
    if(d==h) e6=d
    if((d==h and e!=i) or (h==f and e!=g)) e7=h
    if(h==f) e8=f
   end

   --draw
   x0=sx+x*3
   y0=sy+y*3
   if(e0!=alpha) func(x0,  y0,  e0)
   if(e1!=alpha) func(x0+1,y0,  e1)
   if(e2!=alpha) func(x0+2,y0,  e2)
   if(e3!=alpha) func(x0,  y0+1,e3)
   if(e4!=alpha) func(x0+1,y0+1,e4)
   if(e5!=alpha) func(x0+2,y0+1,e5)
   if(e6!=alpha) func(x0,  y0+2,e6)
   if(e7!=alpha) func(x0+1,y0+2,e7)
   if(e8!=alpha) func(x0+2,y0+2,e8)
  end
 end
end
-->8
local super_offset=0x4300
local supers={}
super={}
super={
 new=function(sx,sy)
  local t={x=sx or 8,y=sy or 8,
   off=super_offset
  }
  super_offset+= shr(t.x*t.y,1)
  
  setmetatable(t,super)
  add(supers,t)
  -- init with red/blue
  local val= shl(12,4)+8
  memset(
   t.off,
   val,
   super_offset-t.off
  )

  return t
 end,

 draw=function(s,x,y,alpha)
  local bo=gmem+flr(x/2)+y*64
  for i=0,s.y-1 do
   gy=i*64
   sy=i*shr(s.y,1)
   memcpy(bo+gy,s.off+sy,shr(s.x,1))
  end
 end,

 pset=function(s,x,y,v)
  local o = (shr(x,1))+(y*shr(s.y,1))
  local p = peek(s.off+o)
  
  local lp = p%16
  local rp = (p-lp)/16
  if (x%2==1) then
    rp=min(16,v)
  else
    lp=min(16,v)
  end

  local val=lp+(rp*16)
  poke( s.off+o, val )
 end
}
super.__index=super

-->8
function mk_sss(offset)
 local super_offset = offset
 local super={}
 super={
  new=function(sx,sy)
   local t={x=sx or 8,y=sy or 8,
    off=super_offset
   }
   super_offset+= shr(t.x*t.y,1)
   
   setmetatable(t,super)
   add(supers,t)
   -- init with red/blue
   local val= shl(12,4)+8
   memset(
    t.off,
    val,
    super_offset-t.off
   )
 
   return t
  end,
 
  draw=function(s,x,y,alpha)
   local bo=gmem+flr(x/2)+y*64
   for i=0,s.y-1 do
    gy=i*64
    sy=i*shr(s.y,1)
    memcpy(bo+gy,s.off+sy,shr(s.x,1))
   end
  end,
 
  pset=function(s,x,y,v)
   local o = (shr(x,1))+(y*shr(s.y,1))
   local p = peek(s.off+o)
   
   local lp = p%16
   local rp = (p-lp)/16
   if (x%2==1) then
     rp=min(16,v)
   else
     lp=min(16,v)
   end
 
   local val=lp+(rp*16)
   poke( s.off+o, val )
  end
 }
 super.__index=super

 return super
end

__gfx__
00000000088088000003300003333300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000008e88888000333333333b3330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700e7e88880033bb33333b33330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000770008e88888003b3333333333330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000888888200333333333333330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700088882000033333333333300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000088200000033333333b3300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000800000000403333333300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000400333333000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000440433330000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000044440040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000004440400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000440000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000044444000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
