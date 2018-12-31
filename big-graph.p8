pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
local g={} -- tile graph
p={}
pickups={}
enemies={}
ghosts={
 {col=8,home={17,13},
  lurk={6,25}
 },
 {col=14,home={23,13},
  lurk={20,6}
 },
 {col=15,home={18,14},
  lurk={29,20}
 },
 {col=12,home={21,14},
  lurk={38,7}
 }
}
debug=false
local d={8,6}
local cam={0,0}
local cst="unknown"
local edges={}
local path={}
function _init()
 g=map2graph()
 st1=stars.new(4478,128,1)
 st2=stars.new(8086,512,2)
 st3=stars.new(13,2048,3)
 
 p=pl.new(2,1)
 starts={}
 for k,v in pairs(g) do
   if #v>0 then
     add(starts,k)
   end
 end
 for gh in all(ghosts) do
   local cell=idx2cell(
      starts[1+flr(rnd(#starts))]
   )
   enemy=e.new(cell[1],cell[2],
    gh.col,gh.home,gh.lurk)
   add(enemies,enemy)
   
 end
 pickups=pickup.new()
 pickups:init()
end

function _update()
prof_in("upd")
  bonus()
  pickups:update()
  flt:updateall()
  for e in all(enemies) do
    e:update(p)
  end
  for b in all(bonuses) do
    b:update(p)
  end
  p:update()

  if btnp(4) then
    for e in all(enemies) do
      e.mode="h"
    end
  end

  -- camera chase
  camchase(p.pos,cam,ct)
prof_out("upd")

end

function _draw()

  cls()
  pal()
  fillp()
  
  
  draw_map()

prof_in("d_debug")  
  if debug then
    for e in all(enemies) do
      e:draw_debug()
    end
    p:draw_debug()
  end
prof_out("d_debug")

  --draw_starfield()


prof_in("d_hud")  
  camera()
  pal()
  palt()
  p:draw_hud()
  
  camera(cam[1],cam[2])
  flt.drawall()

prof_out("d_hud")
  
  -- blackout dark green  
  pal(3,0,1)

  draw_prof()
  
end

function draw_stats()

end

function draw_starfield()
prof_in("d_star")
  -- background
  camera(cam[1]*0.25,cam[2]*0.25)
  --st1:draw(cam[1],cam[2])
  
  draw_stars(cam[1]*0.25,cam[2]*0.25,
    4478,128,1
  )
  
  camera(cam[1]*0.5,cam[2]*0.5)
  --st2:draw(cam[1]*1.25,cam[2]*1.25)
  draw_stars(cam[1]*0.5,cam[2]*0.5,
    8086,512,2
  )

  camera(cam[1]*0.75,cam[2]*0.75)
  draw_stars(cam[1]*0.75,cam[2]*0.75,
    13,2048,3
  )
prof_out("d_star")
end

function draw_prof()
 local width=50
  rectfill(1,1,width,3,5)
  local n=1
  local col=6
  local coff=10
  local o=0
  for k,v in pairs(getprof()) do
    v*=width
    line(n,2,n+v,2,col)
    print(k,4,coff+(o*8))
    col+=1
    n+=v
    o+=1
  end
end

function draw_map()
  -- map , w/ opaque parts
prof_in("map-b")

  
  l_walls=0x80
  pal()
  camera(cam[1],cam[2])
  local cx=flr(cam[1]/8)
  local cy=flr(cam[2]/8)
  local x=cx*8
  local y=cy*8

  -- cell-size = 128/8
  
  pal(5,0)
  pal(2,0)
  
  palt(0,true)
  map(cx,cy,x,y,17,17)

prof_out("map-b")

prof_in("refl")


  pal()
  p:draw_refl()
  
  for e in all(enemies) do
    e:draw_refl()
  end
  for b in all(bonuses) do
    b:draw_refl()
  end
  --pickups:draw_refl()

prof_out("refl") 

prof_in("pkps")
  pal()
  palt()
  pickups:draw()
  for p in all(bonuses) do
    p:draw()
  end
prof_out("pkps")

prof_in("spr") 
  
   
  pal()
  palt()
  for e in all(enemies) do
    e:draw()
  end
  p:draw()
prof_out("spr")

prof_in("wall")
  -- raised walls  
  palt(5,true)
  palt(2,true)
  map(cx,cy,x,y,17,17,l_walls)
  map(cx,cy,x,y-1,17,17,l_walls)
  map(cx,cy,x,y-2,17,17,l_walls)
prof_out("wall")
  
end

function drawpath(path,col)
  for p in all(path) do
    local c=idx2cell(p)
    local pos=cell2mxy(c[1],c[2])
    circ(pos[1],pos[2],2,col)
  end
end

function d_debug()
  -- debug
  print(cst,4,120,11)
  -- current position
  print(cell2idx(p[1],p[2]),100,60,5)
  -- path list
  print(tablestr(path),0,80)
end

function d_neighbours()
  -- neighbours
  local i=1
  for e in all(edges) do
    local ec=idx2cell(e)
    local ep=cell2mxy(ec[1],ec[2])
    circ(ep[1],ep[2],3,3)
    print(e,4,60+(8*i),12)
    print(ec[1] .. " " .. ec[2], 40,60+(8*i),12)
    i+=1
    --print( e, 4, 60,12)
  end
end


function tablestr(t)
  s=""
  for e in all(t) do
    s = s .. tostr(e) .. ","
  end
  return s
end
-->8
-- graph
function cell2xy(cx,cy)
  return { cx*8, cy*8 }
end

function cell2mxy(cx,cy)
  return {
    (cx*8)+4,(cy*8)+4
  }
end

function cell2idx(cx,cy)
  return (cx+1)+(cy*128)
end

function idx2cell(idx)
  local cy = flr(idx / 128)
  local cx = (idx % 128) -1
  return {cx,cy}
end

function xy2cell(x,y)
  return {
    flr(x/8),flr(y/8)
  }
end

function mxy2cell(x,y)
  return {
    flr((x-4)/8),flr((y-4)/8)
  }
end

function map2graph()
  local g={}
  for cx = 0,127 do
    for cy = 0,31 do
      g[cx+(cy*128)] = {}
    end
  end
  
  for cx = 0,127 do
    for cy = 0,31 do
      local idx=cell2idx(cx,cy)
      local s=mget(cx,cy)
      if fget(s,0) then
        local n=cell2idx(cx-1,cy)
        add(g[n],idx)
      end
      if fget(s,1) then
        local n = cell2idx(cx+1,cy)
        add(g[n],idx)
      end
      if fget(s,2) then
        local n= cell2idx(cx,cy-1)
        add(g[n],idx)
      end
      if fget(s,3) then
        local n = cell2idx(cx,cy+1)
        add(g[n],idx)
      end
           
    end
  end
  return g
end


-- a,b table of upper-left
--  and lower-right
--  box corners
-- {u={x,y},l={x,y}}
function hb_intersect(a,b)
  if a.u[1] < b.l[1] and
     a.l[1] > b.u[1] then
     if a.u[2] < b.l[2] and
        a.l[2] > b.u[2] then
       return true
     end
  end
  return false
end
-->8
-- pathfinding
function astar(g,start,goal,cri)
  local cr_count=0
  local closed ={}
  local open = {start}
  local camefrom={}
  local gscore={}
  gscore[start]=0
  
  local fscore={}
  fscore[start]=heur(start,goal)
  
  while #open != 0 do
    local cur= lowest(fscore,open)
  
    if cur == goal then
      return reconstruct_path(camefrom,cur)
    end
    del(open,cur)
    add(closed,cur)
    
    for n in all(g[cur]) do
      repeat
      
       cr_count+=1
       if cri and cr_count>cri then
         cr_count=0
         --printh("co-routine bailout")
         yield()
       end
   
      
      -- already visited
      if has_value(closed,n) then
        do break end
      end
      
      local tgs = gscore[cur] +
                  distance(cur,n)
      if not has_value(open,n) then
        --printh("add open: " .. n)
      	 add(open,n)      
      elseif (gscore[n]!=nil and tgs >= gscore[n]) then
        -- does this ever happen?
        --printh("bail gscore")
        do break end
      end
      
      camefrom[n]= cur
      gscore[n] = tgs
      fscore[n] = gscore[n]+heur(n,goal)
      
      until true
      
    end
  
   
  end
  --printh("bailout a* fall thru")
  return {}
end

function reconstruct_path(camefrom, current)
  local total_path = {current}
  while camefrom[current] do
    current = camefrom[current]
    add(total_path,current)
  end
  return total_path
end

function heur(s,e)
  assert(s)
  assert(e)
  local sp=idx2cell(s)
  local ep=idx2cell(e)
  
  local x = sp[1] - ep[1]
  local y = sp[2] - ep[2]
  return (x*x)+(y*y)
end

function distance(s,e)
  return sqrt( heur(s,e) )
end

function lowest(t,open)
  local low=30000
  local result=nil
  for k in all(open) do
    local v = t[k]
    if v != nil then
      if v < low then
        low = v
        result = k
      end
    end
  end
  return result
end

function has_value(t,v)
  for e in all(t) do
    if e==v then
      return true
    end
  end
  return false
end
-->8
-- world
cam_adjust={0,0.25,0.5,1,3,8}
cam_slack=32
cam_step=8
function camchase(f,c)
 local m = { f[1]-64, f[2]-64}
 
 ct = m
 local vx =
  ( c[1] - ct[1] )
 local vy =
  ( c[2] - ct[2] )
 local qx = flr(abs(vx/cam_step))
 local qy = flr(abs(vy/cam_step))
    
 local mx=qx>=#cam_adjust and cam_adjust[#cam_adjust]
       or cam_adjust[1+qx]
 local my=qy>=#cam_adjust and cam_adjust[#cam_adjust]
       or cam_adjust[1+qy]
 if vx>0 then
   c[1] -= mx
 elseif vx<0 then
   c[1] += mx
 end
 
 if vy>0 then
   c[2] -= my
 elseif vy<0 then
   c[2] += my
 end
 
 if c[1] <0 then
   c[1]=0
   --printh("clamp-x")
 end
 if c[2] <0 then
   c[2]=0
   --printh("clamp-y")
 end
 

end

function prright(s,x,y,col)
  col=col or 7
  local ox=x-(#s*4)
  print(s,ox,y,col)
end

drk_pal={
 0,0,1,1,2,1,5,6,2,4,9,3,1,1,2,5
}

local star_cols={
 0,1,2,3,4,5,6,7,
 8,9,10,11,12,13,14,15
}
local star_pal={
 {1,1,1,1,1,1,1,1,
  5,5,5,5,5,6,6,13} ,
 {1,1,1,1,1,5,5,5,
  5,5,6,10,13,13,12,13},
 {1,1,5,5,5,6,6,9,
  13,12,15,13,7,7,7,7}
}

-- render one of the eye-cells
-- as dark(pupil) the rest white
eye_pal_idx={
 { {8,1},{9,7}, {10,7}, {11,7} },
 { {8,7},{9,1}, {10,7}, {11,7} },
 { {8,7},{9,7}, {10,1}, {11,7} },
 { {8,7},{9,7}, {10,7}, {11,1} },
}

eye_pal_drk={
 { {8,1},{9,13}, {10,13}, {11,13} },
 { {8,13},{9,1}, {10,13}, {11,13} },
 { {8,13},{9,13}, {10,1}, {11,13} },
 { {8,13},{9,13}, {10,13}, {11,1} }, 
}

function apply_pal(p)
  for i=0,15 do
    pal(i,p[i+1])
  end
end

function apply_idx_pal(p)
  for t in all(p) do
    pal(t[1],t[2])
  end
end

function xy2starcell(x,y)
  local cx=flr(x/128)
  local cy=flr(y/128)
  return {cx,cy}
end





function draw_stars(x,y,base,ds,pl)
 pl=pl or 1
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
    if pget(x,y)==3 then
      pset(clx+x,cly+y,col) 
    end
  end
  until true -- loop break next
 end
end

stars_m={
init=function(t,base,d,pl)
 local fld={}
 t.fld=fld
 for p in all(t.grps) do
  
  local st={}
  local cell=xy2starcell(p[1],p[2])
  printh("set field: "..
    cell[1] .. " , " ..
    cell[2]
  )
  fld[cell[1]]=fld[cell[1]] and fld[cell[1]] or {}
  fld[cell[1]][cell[2]]=st
  local clx=cell[1]*128
  local cly=cell[2]*128
  local seed=shl(13,cell[1])
            +cell[2]
  seed=bxor(base,seed)
  srand(seed)
  
  local sz=128*128
  local nxt=0
  while nxt<sz do
    local step=flr(rnd(t.d))
    nxt+=step
    local col=star_cols[
      flr(rnd(#star_cols))+1
      ]
    local x=nxt%128
    local y=flr((nxt-x)/128)
    add(st,{x,y,col})
  end
  printh("\tstars: "..#st)
 end
 
end,
draw=function(t,x,y)
  apply_pal(star_pal[t.pl])
  local cell=xy2starcell(x,y)
  local row = t.fld[cell[1]]
  local col = row[cell[2]]    
  for p in all(col) do
    repeat
    if pget(p[1],p[2])~=3 then break end
    pset(p[1],p[2],p[3])
    until true
  end
  
    
end
}
stars={}
stars.__index=stars_m
stars.new=function(base,d,pl)
  local t={ base=base or 4444,
    d=d or 128,
    pl=pl or 1
  }
  local grps={
   {0,0},
   {128,0},
   {256,0},
   {386,0},
   {0,128},
   {128,128},
   {256,128},
   {386,128}
  }
  t.grps=grps
  setmetatable(t,stars)
  t:init()
  return t
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

-- knockout sprite @sp with
-- non-white  parts of @msk
-- 8x8 sprite only
function maskspr(sp,msk)
  local spr_s=((sp-(sp%16))/2)
  local msk_s=((msk-(msk%16))/2)
  local spx=(sp%16)*8
  local mskx=(msk%16)*8
  for x=0,7 do
   for y=0,7 do
    local m=sget(mskx+x,msk_s+y)
    local s=sget(spx+x,spr_s+y)
    local r=band(s,m)
    sset(spx+x,spr_s+y,r)
   end
  end
  
end

-->8
-- enemies
local astar_gov=30
local e_m={
seek=function(t)
    -- already seeking
    if t.cor then
      return
    end

    local gl
    if t.mode=="c" then
      gl = cell2idx(
        p.cell[1],
        p.cell[2]
      )
    elseif t.mode=="l" then
      gl = t.lurk
    else
      gl = t.home
    end
    
    local cr=function()
      t.path=astar(g,t.mg,gl,
       astar_gov
      )
       
    end
    --t.path = astar(g,t.mg,gl)
    t.cor=cocreate(cr)
end,
update=function(t)
  if type(t.cor)=="thread" then
    local ok
    local err
    ok,err=coresume(t.cor)
    if ok then
      if costatus(t.cor)=="dead" then
        t.cor=nil
        --printh("finished seeking "..t.col)
      end
    else
      printh("cor err: " .. err)
      assert(ok)
    end
  end
  -- every tm ticks, 
  -- check path/target
  t.tk+=1
  if t.tk>t.tm then
    t:seek()
    t.tk=0
  end

  local st_idx=cell2idx(t.cx,t.cy)
  
  t.mtk+=1
  if t.mtk>t.mtm then
    if not(t.mode=="h") then
      t.mode=t.mode == "c" and "l" or "c"
      sfx(2,1)
    else
      -- it we're at home
      -- start chasing again
      if st_idx==t.home then
        t.mode="c"
        sfx(2,1)
      end
    end
    t.mtk=0
  end

  -- go slow
  --if t.tk%2==0 then return end
 
  -- collide w/ player
  if not(t.mode=="h") then
   if hb_intersect(
     p:hitbox(),
     t:hitbox()
   ) then
     -- collided
     if p.pwr then
       t.mode="h"
       sfx(3,1)
       p:scores(100)
     else
       p:dies(t)
     end
   end
  end  
  local tgt_idx = t.mg
  local tgt = idx2cell(tgt_idx)  
  -- change dv to point to 
  -- next path point
  local tgt_pos=cell2mxy(tgt[1],tgt[2])
  
  local dx=(t.pos[1]-tgt_pos[1])
  local dy=(t.pos[2]-tgt_pos[2])
  if dx<0 then
    t.dv[1] = -1
  elseif dx>0 then
    t.dv[1] = 1
  else
    t.dv[1] = 0
  end
  if dy<0 then
    t.dv[2] = -1
  elseif dy>0 then
    t.dv[2] = 1
  else
    t.dv[2] = 0
  end
  
  
  -- move positon by dv
  t.pos[1]-=t.dv[1]
  t.pos[2]-=t.dv[2]
  
  -- where are we now?
  -- update our cell
  local new_cell=mxy2cell(t.pos[1],t.pos[2])
  local new_idx=cell2idx(new_cell[1],new_cell[2])
  del(t.path,new_idx)  
  -- if we've reached this step
  -- remove it from path
  if t.pos[1]==tgt_pos[1]
  and t.pos[2]==tgt_pos[2] then
    t.mg=t.path[#t.path] or t.mg
    t.state=t.state .. "-n"
  end

  t.cx=new_cell[1]
  t.cy=new_cell[2]
 
  t.cell=new_cell
  
end,
draw=function(t)
  local gd={
   t.pos[1]<p.pos[1] and 1 or 0,
   t.pos[2]<p.pos[2] and 1 or 0
  }
  local eye_idx=1+ bxor(
    gd[1], shl(gd[2],1)
  )

  local sp
  if t.mode=="h" then
    sp = 65
  else
    sp = 64
  end

  -- override col  
  pal(14,t.col)
  apply_idx_pal(
    eye_pal_idx[eye_idx]
  )
  spr(sp,t.pos[1]-4,t.pos[2]-4)
  pal()
end,
draw_refl=function(t)
  local sp
  if t.mode=="h" then
    sp = 65
  else
    sp = 64
  end
  
  local gd={
   t.pos[1]<p.pos[1] and 1 or 0,
   t.pos[2]<p.pos[2] and 1 or 0
  }  
  local eye_idx=1+ bxor(
    gd[1], shl(gd[2],1)
  )

  pal()
  apply_pal(drk_pal)
  pal(14,drk_pal[t.col+1])
  apply_idx_pal(
    eye_pal_drk[eye_idx]
  )
  
  spr(sp,t.pos[1]-4,t.pos[2]+4,
    1,1,
    false,true
  )
  
end,
draw_debug=function(t)
  local goal=t.path[#t.path] or t.mg
  local n_cell=idx2cell(goal)
  
  local pos=cell2xy(t.cx,t.cy)
  local tgt=cell2xy(n_cell[1],n_cell[2])
  fillp(0b0111111010111101.1)
  rect(pos[1],pos[2],
    pos[1]+8,pos[2]+8,
    11
  )

  fillp(0b1011110101111110.1)  
  rect(tgt[1],tgt[2],
    tgt[1]+8,tgt[2]+8,
    14
  )

  drawpath(t.path,t.col)


end,
hitbox=function(t)
 return {
   u={t.pos[1]-3,t.pos[2]-3 },
   l={t.pos[1]+3,t.pos[2]+3 },
 }
end
}

e={}
e.new=function(cx,cy,col,home,lurk)
  local t={cx=cx,cy=cy}
  local pos=cell2mxy(cx,cy)
  t.col=col or 14
  t.dv={0,0}
  t.mg=cell2idx(cx,cy)
  t.home=cell2idx(home[1],home[2])
  t.lurk=cell2idx(lurk[1],lurk[2])
  --cell2idx(cx,cy)
  
  t.mode="c"
  t.mtk=0
  t.mtm=300
  t.pos=pos
  t.tm=45
  t.tk=flr(rnd(t.tm))
  t.path={cell2idx(cx,cy)}
  t.state="init"
  setmetatable(t,e)
  t:seek()
  return t
end

e.__index=e_m

-->8
-- player
local p_m={
update=function(p)
  p.tk+=1
  if p.tk>p.tm then
    p.tk=0
  end
  
  -- go slow
  --if p.tk%2==0 then return end

  
  p.tkp+=1
  if p.tkp>p.tkpm then
    p.tkp=0
    p:powerup(false)
  end
  
  p.astep+=1
  if p.astep>#p.anim then
    p.astep=1
  end
  


  local st_idx=cell2idx(p.cell[1],p.cell[2])
  
  local tgt_idx = p.mg
  local tgt = idx2cell(tgt_idx)  
  -- change dv to point to 
  -- next path point
  local tgt_pos=cell2mxy(tgt[1],tgt[2])
  
  local dx=(p.pos[1]-tgt_pos[1])
  local dy=(p.pos[2]-tgt_pos[2])
  if dx<0 then
    p.dv[1] = -1
  elseif dx>0 then
    p.dv[1] = 1
  else
    p.dv[1] = 0
  end
  if dy<0 then
    p.dv[2] = -1
  elseif dy>0 then
    p.dv[2] = 1
  else
    p.dv[2] = 0
  end
  
  
  -- move positon by dv
  p.pos[1]-=p.dv[1]
  p.pos[2]-=p.dv[2]
  
  -- where are we now?
  -- update our cell
  local new_cell=mxy2cell(p.pos[1],p.pos[2])
  local new_idx=cell2idx(new_cell[1],new_cell[2])
  
  -- if we've reached this step
  -- remove it from path
  if p.pos[1]==tgt_pos[1]
  and p.pos[2]==tgt_pos[2] then
    local go_on={
      new_cell[1]+p.dv[1],
      new_cell[2]+p.dv[2]
    }
    local go_idx=cell2idx(go_on[1],go_on[2])
    local moves=g[new_idx]
    if has_value(moves,go_idx) then
      --p.mg=go_idx
    end
  end

  p.cell=new_cell

  --
  local new_tgt={p.cell[1],p.cell[2]}
  if btn(0) then
    new_tgt[1]-=1
  elseif btn(1) then
    new_tgt[1]+=1
  end
  
  if btn(2) then
    new_tgt[2]-=1
  elseif btn(3) then
    new_tgt[2]+=1
  end
  -- is new_tgt a legal move
  local new_tgt_idx=cell2idx(new_tgt[1],new_tgt[2])
  local moves=g[new_idx]
  if has_value(moves,new_tgt_idx) then
    p.mg=new_tgt_idx
  end

  -- sprite orientation
  if p.dv[1]>0 then p.flp=true end
  if p.dv[1]<0 then p.flp=false end
  
  
  if  p.dv[1]!=0 then
    p.anim = p.anim_walk
  end

  if p.dv[2]<0 then
    p.anim = p.anim_down
  end
  
  if p.dv[2]>0 then
    p.anim = p.anim_up
    p.astep=1
  end
   
  
  if btnp(5) then
    debug=debug==false and true or false
  end

end,
dies=function(t,e)

  t:scores(-1000)
printh("caught by " .. e.col )
  local start={}
  for k,v in pairs(g) do
    if #v>0 then
      add(start,k)
    end
  end
  srand(stat(1)*5000+stat(85))
  local new_idx=start[1+flr(rnd(#start))]
  local new_cell=idx2cell(new_idx)
  local new_pos=cell2mxy(new_cell[1],new_cell[2])
  
  t.pos=new_pos
  t.cell=new_cell
  t.mg=new_idx
  t.pwr=false
end,
powerup=function(t,p)
  if p then
    t.pwr=true
    t.tkp=0
    sfx(4,2)
  else
    t.pwr=false
    sfx(-2,2)
      
  end
end,
draw=function(t)
  pal()
  palt()
  if t.pwr then
    if t.tk%2==0 then
      pal(10,15)
    else
      pal(10,7)
    end
  end
  spr(t.anim[t.astep],
   t.pos[1]-4, t.pos[2]-4,
   1,1,
   p.flp
  )
end,
draw_refl=function(t)
  pal()
  apply_pal(drk_pal)
  spr(t.anim[t.astep],
    t.pos[1]-4,t.pos[2]+4,
    1,1,
    p.flp,true
  )
end,
draw_hud=function(t)
  prright(tostr(t.score),
    124,116,15
  )
end,
draw_debug=function(t)
  local i =cell2idx(t.cell[1],t.cell[2])
  print(i,4,120,7)
  print(p.dv[1] .. ",".. p.dv[2],
    4,110,7
  )
  local pos=cell2xy(t.cell[1],t.cell[2])
  local tgt_cell=idx2cell(t.mg)
  local tgt=cell2xy(tgt_cell[1],tgt_cell[2])
  
  fillp(0b1010010110100101.1)
  rect(pos[1],pos[2],
    pos[1]+8,pos[2]+8,
    10
  )

  fillp(0b0101101001011010.1)  
  rect(tgt[1],tgt[2],
    tgt[1]+8,tgt[2]+8,
    8
  )
  
end,
scores=function(t,pnt)
  t.score+=pnt
end,
hitbox=function(t)
 return {
   u={t.pos[1]-3,t.pos[2]-3},
   l={t.pos[1]+3,t.pos[2]+3},
 }
end
}
pl={}
pl.new=function(cx,cy)
  local t={}
  t.score=0
  t.pwr=false
  t.cell={cx,cy}
  t.mg=cell2idx(cx,cy)
  t.dv={0,0}
  t.tk=0
  t.tm=4
  t.tkp=0
  t.tkpm=300
  t.astep=3
  t.anim_walk={68,69,70,71,71,70,69,68,68}
  t.anim_down={84,85,86,87,87,86,85,84,84}
  t.anim_up={84}
  t.anim=t.anim_walk
  t.flp=false
  local pos=cell2mxy(cx,cy)
  t.pos=pos
  setmetatable(t,pl)
  return t
end
pl.__index = p_m

-->8
-- housekeeping
local profile={}
function prof_in(label)
  local v=stat(1)
  profile[label]=v
end

function prof_out(label)
  local v=stat(1)
  assert(type(profile[label])=="number")
  profile[label]=v-profile[label]
end

function getprof()
  local t={}
  for k,v in pairs(profile) do
    t[k]=v
  end
  profile={}
  return t
  
end

-->8
-- pickups

pwrup={
  257,258,259,260,261,262,
  2081
}
pu_methods={
init=function(t)
  for k,v in pairs(g) do
    if #v>1 then
      local pwr=has_value(pwrup,k)
      t.p[k]=pwr
    end
  end
end,
update=function(t)
  t.tk+=1
  if t.tk>t.tm then
    t.tk=1
  end
  
  local chk=cell2idx(p.cell[1],p.cell[2])
  -- its a power up
  if t.p[chk]==true then
    sfx(1,2)
    t.p[chk]=nil
    p:powerup(t)
  -- just a dot
  elseif t.p[chk]==false then
    sfx(0,1)
    t.p[chk]=nil
    p:scores(5)
  end
  
end,
draw=function(t)
  for n,v in pairs(t.p) do
    local cell=idx2cell(n)
    local pos=cell2mxy(cell[1],cell[2])
    if v==true then
      spr(82,pos[1]-4,pos[2]-4)
    else
     circfill(pos[1],pos[2],
       1,1
     )
     pset(
       pos[1],pos[2],
       t.shimmer[t.tk]
     )
    end
  end
end,
draw_refl=function(t)
  for n,v in pairs(t.p) do
    local cell=idx2cell(n)
    local pos=cell2mxy(cell[1],cell[2])
    if v==true then
      spr(82,pos[1]-4,pos[2]+2,1,1,false,true)
    end
  end
end,

}
pickup={}
pickup.__index = pu_methods
pickup.new = function()
  local t={}
  t.p={}
  t.shimmer={7,6,15}
  t.tk=1
  t.tm=#t.shimmer
  setmetatable(t,pickup)
  return t
end


prize_val={
 {72,100},
 {73,300},
 {74,500},
 {75,1000}
}
prize_n=1
prz_m={
update=function(t,p)
  if hb_intersect(
       t:hitbox(),p:hitbox()
     ) then
    p:scores(t.val)
    sfx(6,-1)
    del(bonuses,t)
    flt.new(p.pos,t.val)
  end
end,
draw=function(t)
  spr(t.sp,t.pos[1]-4,t.pos[2]-5)
end,
draw_refl=function(t)
  pal()
  apply_pal(drk_pal)
  spr(t.sp,t.pos[1]-4,
    t.pos[2]+3,
    1,1,
    false,true
  )
end,
hitbox=function(t)
  return {
   u={t.pos[1]-3,t.pos[2]-3},
   l={t.pos[1]+3,t.pos[2]+3}
  }
end
}

prize={}
prize.__index=prz_m
prize.new=function(idx)
  prize_n+=1
  if prize_n>#prize_val then
    prize_n=1
  end
  local cell=idx2cell(idx)
  local t={
    idx=idx,
    cell=cell,
    pos=cell2mxy(cell[1],cell[2])
  }
  t.sp=prize_val[prize_n][1]
  t.val=prize_val[prize_n][2]
  setmetatable(t,prize)
  return t
end

bonuses={}
bonus_tk=1
bonus_mx=30
function bonus()
 bonus_tk+=1
 if bonus_tk > bonus_mx then
   bonus_tk=1
   if #bonuses >=4 then
     return
   end
   local p=prize.new(starts[1+flr(rnd(#starts))])
   add(bonuses,p)
 end
end

-- floating score
flt_all={}
flt_m={
update=function(t,p)
  t.tk+=1
  if t.tk>t.tm then
    del(flt_all,t)
  end
  t.pos[2]-=0.5
end,
draw=function(t)
  local col=t.tk%16
  print(t.val,t.pos[1],t.pos[2],col)
end,
draw_refl=function(t)

end,
}
flt={}
flt.__index=flt_m
flt.new=function(pos,val)
  local t={}
  t.pos={ pos[1], pos[2] }
  t.val=val
  t.tk=1
  t.tm=50
  setmetatable(t,flt)
  add(flt_all,t)
  return t
end
flt.updateall=function(p)
  for t in all(flt_all) do
    t:update(p)
  end
end
flt.drawall=function()
  for t in all(flt_all) do
    t:draw()
  end
end
__gfx__
000aa000000aa00000000000000aa000000aa000000aa00000000000000aa0000000000000000000000aa00000077000000aa000000aa0000000000000000000
00000000000110000000000000011000000110000001100000000000000110000000000000000000000110000001100000011000000110000000000000000000
00700700000110000000000000011000000110000001100000000000000110000000000000000000000110000001100000011000000110000000000000000000
80077009811111198111111900011000811111190001111981111119811110008111111771111119000110000001100081111000000111190001111981111000
80077009811111198111111900011000811111190001111981111119811110008111111771111119000110000001100081111000000111190001111981111000
00700700000110000000000000011000000000000001100000011000000110000000000000000000000110000001100000000000000000000001100000011000
00000000000110000000000000011000000000000001100000011000000110000000000000000000000110000001100000000000000000000001100000011000
000bb000000bb00000000000000bb00000000000000bb000000bb000000bb000000000000000000000077000000bb0000000000000000000000bb000000bb000
1c5555c1c13331c2c13331c2333331c23333331cc1333333c133331c222222222222222222222222333333332222222222222222222222222222222221cccc12
c555555c133331c2133331c2333331c2333333311333333313333331cccccccccccccccccccccccc33333333221cccccccccc122221cccccccccc1222c1331c2
55555555333331c2333331c2333331c23333333333333333333333331111111111111111111111113333333321c1111111111c1221c1111111111c122c1331c2
55555555333331c2333331c2333331c2333333333333333333333333333333333333333333333333333333332c133333333331c22c133333333331c22c1331c2
55555555333331c2333331c2333331c2333333333333333333333333333333333333333333333333333333332c133333333331c22c133333333331c22c1331c2
55555555333331c2333331c2333331c2111111111111111111111111333333333333333333333333333333332c133333333331c22c133331133331c22c1331c2
c555555c133331c2333331c2133331c2cccccccccccccccccccccccc333333311333333313333331333333332c133333333331c22c13331cc13331c22c1331c2
1c5555c1c13331c2333331c2c13331c22222222222222222222222223333331cc1333333c133331c333333332c133333333331c22c1331c11c1331c22c1331c2
22225222222222222222522222225222222252222222222222222222222252222222222222225222222252222c133333333331c22c1331c11c1331c22c1331c2
22255222222222222225522222255222222552222222222222222222222552222222222222255222222552222c133333333331c22c13331cc13331c22c1331c2
22255222222222222225522222255222222552222222222222222222222552222222222222255222222552222c133333333331c22c133331133331c22c1331c2
25555555255555552225522222255555255552222225555525555222255555552555555522255555255552222c133333333331c22c133333333331c22c1331c2
55555552555555522225522222255552555552222225555255555222555555525555555222255552555552222c133333333331c22c133333333331c22c1331c2
222552222222222222255222222222222222222222255222222552222222222222255222222552222225522221c1111111111c1221c1111111111c122c1331c2
2225522222222222222552222222222222222222222552222225522222222222222552222225522222255222221cccccccccc122221cccccccccc1222c1331c2
2225222222222222222522222222222222222222222522222225222222222222222252222225222222252222222222222222222222222222222222222c1331c2
33333333222222222c133333333331c233333333333333333333331cc1333333c133331c33333333c13333333333331c2222222222222222222222222c1331c2
33333333cccccccc2c133333333331c233333333333333333333333113333333133333313333333313333333333333311cccccccccccccccccccccc12c1331c2
33333333111111112c133333333331c23333333333333333333333333333333333333333333333333333333333333333c1111111111111111111111c2c1331c2
33333333333333332c133333333331c23333333333333333333333333333333333333333333333333333333333333333c1333333333333333333331c2c1331c2
33333333333333332c133333333331c23333333333333333333333333333333333333333333333333333333333333333c1333333333333333333331c2c1331c2
11111111333333332c133333333331c23333333333333333333333333333333333333333333333333333333333333333c1111111111111111111111c2c1331c2
cccccccc333333332c133333333331c233333331133333333333333333333333333333331333333113333333333333311cccccccccccccccccccccc12c1111c2
22222222333333332c133333333331c23333331cc1333333333333333333333333333333c133331cc13333333333331c22222222222222222222222221cccc12
00eeee00000000000000000000000000000000000000000000000000000000000000000000b0b0b00000b00000000b0000000000000000000000000000000000
0eeeeee0011001100000000000000000000aaa00000aaa00000aaa00000aa900b00b0e80000bbb0000bb00000000b00000000000000000000000000000000000
e89ee89e18911891000000000000000000aaaaa000aaaaa000aaaaa000aaaa900bb08882008e88000aa9990000ffff0000000000000000000000000000000000
eabeeabe1ab11ab100000000000000000aaaaaaa0aaaaaa00aaaaa000aaaa200000b022008888f80a99999900ffffff000000000000000000000000000000000
eeeeeeee0110011000000000000000000aaaaaa90aaaaa220aaaaee20aaa1ee20e80b0000f8e88e0999999900ffffff000000000000000000000000000000000
eeeeeeee0000000000000000000000000aaaa9990aaaa2290aaa22220aa1ee2288820e8008888f80999999400fffffe000000000000000000000000000000000
eeeeeeee0000000000000000000000000099994000999940009999400099992002208882008f8800999994400eeeeee000000000000000000000000000000000
e0e00e0e0000000000000000000000000004440000044400000444000004450000000220000880000994440000ee220000000000000000000000000000000000
00444400000777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
04ffff40007777700007700000000000000aaa00000aaa00000aaa00000aaa000000000000000000000000000000000000000000000000000000000000000000
0ffffff007777777007777000000000000aaaaa000aaaaa000aaaaa000a222a00000000000000000000000000000000000000000000000000000000000000000
ff37f3ff0777777607777760000000000aaaaaaa0aaaaaaa0aaaaaaa0a25e52a0000000000000000000000000000000000000000000000000000000000000000
fff7ffff07777666067776d0000000000aaaaaa90aaaaaa90a22ee29042eee240000000000000000000000000000000000000000000000000000000000000000
0ffffff0006666d000666d00000000000aaaa9990aa222990aa222990aa222990000000000000000000000000000000000000000000000000000000000000000
0ff11ff0000ddd00000dd00000000000009999400099994000999940009999400000000000000000000000000000000000000000000000000000000000000000
00ffff00000000000000000000000000000444000004440000044400000444000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
cccccccc000000000000000000000000999000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
cccccccc700700000700077000000000909000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc770700700700700707770777999099900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc777707777770700707070707000999900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc707707000700777707700770999099900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc700700770700700707000700909000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000999000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000009000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
000f030c070e0b0d0102040805060a090f8080808080808080808080808080800f030c06050a09070b0e0d80808080808080808080808080808080808080808000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
34303030303030303030353430303030351a1a1a1a1a1a343030303030303030303030303030303030303030303030303030303030303000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
332521212128212121263233252121262b3030303030302c2521212121212121212121212121212121212121212121212121212121212126000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221b311c221b311c223233221b1c2921212128212121212a1f1d3d3d3d3d3d3d3d3d3d3d3d3d18173d3d3d3d3d3d3d3d3d3d3d3d181c22000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33222b302c222b302c223233223233223c3d3e223c3d3d3e222f2f2521212121282121212121263233252121212121282121212126323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3329212121202121212a2b2c222b2c2921212127282121212a2f2f221b31311c221b3131311c223233221b3131311c221b31311c22323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221b311c221b311c2921212021212a3c3d3d3e223c3d3e222f2f22321a1a3322321a1a1a3322323322321a1a1a3322321a1a3322323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33222b302c222b302c221b1c221b1c2921212821272821212a2f2f222b30302c222b3030302c222b2c222b3030302c222b30302c22323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
332321212120212121243233223233223c1e221b1c221d3e222f2f292121212120212128212127212127212128212120212121212a323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
363131311c221b313131373322323323262f223233222f25242f2f221b31311c221b1c221b3131313131311c221b1c221b31311c22323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
343030302c222b303030302c222b143e223f222b2c223f223c2e2f222b30302c223233222b3030353430302c223233222b30302c22323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33252121212021212128212120212821272127282827212721262f23212121212a3233232121263233252121243233292121212124323322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221d3d3e223c3d1e221b1c221f221d3d3d1e22221d3d3d1e2232313131311c22323631311c223233221b31313733221b31313131373322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33223f25212721262f222b2c222f222f25263f22223f25262f2232000000003322323430302c222b2c222b3030353322321a1a1a1a1a3322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3329212a3c3d3e223f2921212a2f222f292028272728202a2f2232000000003322323325212127212127212126323322321a1a1a1a1a3322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221f2321212127212a1b1c222f222f23272400002327242f22320000000033223233221d3d3e00003c3d1e22323322321a1a1a1a1a3322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33222d3d3d3d3d3d3e222b2c222f222d3d3d3d3d3d3d3d3d2e222b303030302c222b2c222f0000000000002f222b2c222b30303030302c22000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
332321212121282121272121242f2321212128212128212121272121212121212021212a2f0000000000002f292121202121212121212127212600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
36173d3d3d3e223c3d3d3d3d3d163d3d3d3e221b1c223c3d3d3d3d3d3d3d3d3d221b1c222f0000000000002f221b1c221b313131313131311c221b313131313100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a332521212127212121282121212821212124323323212121282121212121212a3233222d3d3d3d3d3d3d2e2232332232343030303030302c222b303030351a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a33221b31313131311c221b311c221b31173d15143d18311c221b3d3d3d3d3e2232332921212128212121212a2b2c223233252121282821212028212126321a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3322321a1a1a1a1a3322321a3322321a3325212126321a33222f25212121212a3233221b311c221b31311c2921212a3233221b1c23241d3e29241b1c22321a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3322321a1a1a1a1a33222b302c222b302c221d1e222b302c222f221b31311c223233222b302c222b30302c221b1c223233223233001d2e252a3c3a3322321a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3322321a1a1a1a1a332921212120212121242f2f232121212a3f22321a1a33223233292121212728212121243233223233223236172e25202026323322321a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3322321a1a1a1a1a33221b311c221d3d3d3d2e2d3d3d3d1e29212a2b303013223233221b31311c221b313117152c222b2c22321a33252020202a323322321a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a33222b30303030302c222b302c222d3d3d3d3d3d3d3d3d2e221f292121262f22323322321a1a3322321a1a3325212721212a321a33232020202a323322321a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3323282121212821212021282120212121212121212121212a2f221b1c222f223233223234302c222b30302c221b31311c223234301e2320202a3233222b3500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3b3e223c193e223c1e221f221f221d3d3d3d3d3d3d3d3d1e222f223233222f2232332232332521272821212124321a1a33223233002d1e232024323323263200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a332527263f2527263f222f222f222d3d3d3d1e1d3d3d3d2e223f222b2c223f222b2c222b2c221b1c223c3d3d18371a1a33222b2c25262d3e223c15143e223200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a33221f2321241f2321242f222f29212121262f2f252121212021272821272127212120212124323323212126321a1a1a33232121272721212721212121243200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a33222d3d3d3d163d3d3d2e223f221b311c222d2e221b311c223c3e221b313131311c223c3d3d15143d3d3e22321a1a1a36313131313131313131313131313700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3323212121212121212121272124321a3323212124321a3323212124321a1a1a1a3323212121212121212124321a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1a3631313131313131313131313131371a3631313131371a3631313131371a1a1a1a3631313131313131313131371a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100000000006050090500e05012050151501a0501f05022150270502e050361500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000500001025013250172501b2501f25029250332500e2500f250162501b25020250262502c250332500000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000300003875038750387503875025750257502575025750107501075010750107501075010750107501075000000000000000000000000000000000000000000000000000000000000000000000000000000000
000500000355008150177503d55031550147500c550025500155000000000003d5003f5503c50023550000003e550000000000000000000000000000000000000000000000000000000000000000000000000000
0007000b2d750267502d4501f7502d750267501f7502d150267502d7501f750000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00090000325001d0001d0001c0001c00021450114002a000190002a0002a0002a0002b00007450054002d0002e0002f0003200029000210002200024000260002900026000020002750000000000000000000000
010300003905234452390523e45239052344423903239032390223901239012390123901200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
012000000045500455024550045503455004550545503455004550045502355003550335500355053550335500255002550225500255032550025505255032550015500155021550015503155001550515503155
001000202455024550245502455024552245422454224532185001850027550275502955029550275502755024552245522455224555245522455224552245552450224502245002450024500245000000000000
__music__
00 41420a0b

