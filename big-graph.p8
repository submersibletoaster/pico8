pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
local g={}
--local p={2,1}
p={}
pickups={}
enemies={}
local d={8,6}
local cam={0,0}
local ct={0,0}
local cst="unknown"
local edges={}
local path={}
function _init()
 g=map2graph()
 p=pl.new(2,1)
 local starts={}
 for k,v in pairs(g) do
   if #v>0 then
     add(starts,k)
   end
 end
 for col in all({8,14,15,12}) do
   local cell=idx2cell(
      starts[1+flr(rnd(#starts))]
   )
   enemy=e.new(cell[1],cell[2],col)
   add(enemies,enemy)
   
 end
 pickups=pickup.new()
 pickups:init()
end

function _update()

  pickups:update()
  for e in all(enemies) do
    e:update()
  end
  p:update()


  -- camera chase
  camchase(p.pos,cam,ct)
  

end

function _draw()
  cls()
  pal()
  fillp()
  
  -- background
  camera(cam[1],cam[2])
  draw_stars(cam[1],cam[2],
    4478,128,1
  )
  camera(cam[1]*1.25,cam[2]*1.25)
  draw_stars(cam[1]*1.25,cam[2]*1.25,
    8086,512,2
  )
  camera(cam[1]*1.5,cam[2]*1.5)
  draw_stars(cam[1]*1.5,cam[2]*1.5,
    13,2048,3
  )
  
  draw_map()
  
  --target
  pos=cell2mxy(d[1],d[2])
  circ(pos[1],pos[2],5,9)
  pset(pos[1],pos[2],9)
    

  drawpath(path,12)

  for e in all(enemies) do
    e:draw_debug()
  end
  
  p:draw_debug()

  -- blackout dark green  
  pal(3,0,1)
end

function draw_stats()

end

function draw_map()
  -- map , w/ opaque parts
  l_walls=0x80
  pal()
  camera(cam[1],cam[2])


  pal(5,0)
  pal(2,0)
  palt(0,true)
  map(0,0,0,0,128,32)

  p:draw_refl()
  for e in all(enemies) do
    e:draw_refl()
  end
  pickups:draw_refl()
  
  pal()
  palt()
  pickups:draw()
  
  
  pal()
  palt()
  for e in all(enemies) do
    e:draw()
  end
  p:draw()


  -- raised walls  
  palt(5,true)
  palt(2,true)
  map(0,0,0,0,128,32,l_walls)
  map(0,0,0,-1,128,32,l_walls)
  map(0,0,0,-2,128,32,l_walls)
  
 -- redundant?  
 while false do
  for y=0,32 do
    clip()
    palt(5,true)
    map(0,y,0,(y*8)-1,128,1)
    map(0,y,0,(y*8)-2,128,1)
    if enemy.cy==y then
      palt()
      clip(0,0,128,(y*8)+4)
      enemy:draw()
    end
    if enemy.cy+1==y then
      clip(0,(y*8)+4,128,128)
     enemy:draw()
    end
    clip(0,(y*8)+5,128,128)
    palt(5,true)
    map(0,y,0,(y*8)-1,128,1)
    map(0,y,0,(y*8)-2,128,1)  
  end
 end
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
-->8
-- pathfinding
function astar(g,start,goal)
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
  printh("bailout a* fall thru")
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
function camchase(f,c,ct)
 local m = { f[1]-64, f[2]-64}
 if m[1] < 0 then m[1]=0 end
 if m[2] < 0 then m[2]=0 end
 
 ct = m
 local vx = c[1] - ct[1]
 local vy = c[2] - ct[2]
 if abs(vx)> 16 then
   vx -= shr(vx,4)
 end
 if abs(vy)> 16 then
   vy -= shr(vx,4)
 end
 
 c[1] -= vx
 c[2] -= vy
 --c[1] = flr(c[1])
 --c[2] = flr(c[2])
 
 

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

eye_pal_idx={
 { {8,12},{9,7}, {10,7}, {11,7} },
 { {8,7},{9,12}, {10,7}, {11,7} },
 { {8,7},{9,7}, {10,12}, {11,7} },
 { {8,7},{9,7}, {10,7}, {11,12} },
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
    --printh(x .. "," .. y)
    
    pset(clx+x,cly+y,col) 
  end
  until true -- loop break next
 end
end
-->8
-- enemies
local e_m={
seek=function(t)
    --local st = cell2idx(t.cx,t.cy)
    local gl = cell2idx(
      p.cell[1],
      p.cell[2]
    )
    t.path = astar(g,t.mg,gl)
end,
update=function(t,p)

  -- every tm ticks, 
  -- check path/target
  t.tk+=1
  if t.tk>t.tm then
    t:seek()
    t.tk=0
  end


  if t.tk%2==0 then return end


  local st_idx=cell2idx(t.cx,t.cy)
  
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

  -- override col  
  pal(14,t.col)
  apply_idx_pal(
    eye_pal_idx[eye_idx]
  )
  spr(64,t.pos[1]-4,t.pos[2]-4)
  pal()
end,
draw_refl=function(t)
  apply_pal(drk_pal)
  pal(14,drk_pal[t.col+1])
  
  spr(64,t.pos[1]-4,t.pos[2]+4,
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
--[[
  print(t.cx .. "," .. t.cy,
    100,74,14
  )
  print(n_cell[1] .. ","
    .. n_cell[2],
    104,80,15
  )
  print(t.pos[1] .. "," .. t.pos[2],
    100,84,14
  )
  print(t.dv[1] .. "," .. t.dv[2],
    100,94,14
  )
  print(t.state , 90,104,14)
  print(t.tk,100,114,14)
--]]

  --drawpath(t.path,7)


end
}

e={}
e.new=function(cx,cy,col)
  local t={cx=cx,cy=cy}
  local pos=cell2mxy(cx,cy)
  t.col=col or 14
  t.dv={0,0}
  t.mg=cell2idx(cx,cy)
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
   
   

end,
draw=function(t)
  pal()
  palt()
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
}
pl={}
pl.new=function(cx,cy)
  local t={}
  t.cell={cx,cy}
  t.mg=cell2idx(cx+1,cy)
  t.dv={1,0}
  t.tk=0
  t.tm=4
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

end

function prof_out(label)

end

function getprof()

end

-->8
pwrup={
  257,258,259,260,261,262,
  2081
}
-- pickups
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
  if t.p[chk]==true then
    sfx(1,2)
    t.p[chk]=nil
  elseif t.p[chk]==false then
    sfx(0,1)
    t.p[chk]=nil
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
__gfx__
000aa000000aa00000000000000aa000000aa000000aa00000000000000aa0000000000000000000000aa00000077000000aa000000aa0000000000000000000
00000000000110000000000000011000000110000001100000000000000110000000000000000000000110000001100000011000000110000000000000000000
00700700000110000000000000011000000110000001100000000000000110000000000000000000000110000001100000011000000110000000000000000000
80077009811111198111111900011000811111190001111981111119811110008111111771111119000110000001100081111000000111190001111981111000
80077009811111198111111900011000811111190001111981111119811110008111111771111119000110000001100081111000000111190001111981111000
00700700000110000000000000011000000000000001100000011000000110000000000000000000000110000001100000000000000000000001100000011000
00000000000110000000000000011000000000000001100000011000000110000000000000000000000110000001100000000000000000000001100000011000
000bb000000bb00000000000000bb00000000000000bb000000bb000000bb000000000000000000000077000000bb0000000000000000000000bb000000bb000
1c5555c1111111111c5555c11c5555c11c5555c1111111111c5555c11c5555c11c5555c155111111111111552222222222222222222222222222222221cccc12
c555555ccccccccc1c5555c1c555555c1c55555cccccccccc55555c1c55555c11c55555c51cccccccccccc15221cccccccccc122221cccccccccc1222c1331c2
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c121c1111111111c1221c1111111111c122c1331c2
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c12c133333333331c22c133333333331c22c1331c2
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c12c133333333331c22c133333333331c22c1331c2
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c12c133333333331c22c133333333331c22c1331c2
c555555ccccccccc1c5555c1cccccccc1c55555cc555555cc55555c1cccccc1551cccccc1c55555cc55555c12c133333333331c22c13331cc13331c22c1331c2
1c5555c1111111111c5555c1111111111c5555c11c5555c11c5555c111111155551111111c5555c11c5555c12c133333333331c22c1333c11c3331c22c1331c2
22225222222222222222522222225222222252222222222222222222222252222222222222225222222252222c133333333331c22c1333c11c3331c22c1331c2
22255222222222222225522222255222222552222222222222222222222552222222222222255222222552222c133333333331c22c13331cc13331c22c1331c2
22255222222222222225522222255222222552222222222222222222222552222222222222255222222552222c133333333331c22c133333333331c22c1331c2
25555555255555552225522222255555255552222225555525555222255555552555555522255555255552222c133333333331c22c133333333331c22c1331c2
55555552555555522225522222255552555552222225555255555222555555525555555222255552555552222c133333333331c22c133333333331c22c1331c2
222552222222222222255222222222222222222222255222222552222222222222255222222552222225522221c1111111111c1221c1111111111c122c1331c2
2225522222222222222552222222222222222222222552222225522222222222222552222225522222255222221cccccccccc122221cccccccccc1222c1331c2
2225222222222222222522222222222222222222222522222225222222222222222252222225222222252222222222222222222222222222222222222c1331c2
33333333222222222c133333333331c200000000000000000000001cc1000000c100001c00000000222c10000001c2282222222222222222222222222c1331c2
33333333cccccccc2c133333333331c2000000000000000000000001100000001000000100000000282c10000001c2881cccccccccccccccccccccc12c1331c2
33333333111111112c133333333331c2000000000000000000000000000000000000000000000000228c10000001c882c1111111111111111111111c2c1331c2
33333333333333332c133333333331c20000000000000000000000000000000000000000000000002228811111118222c1333333333333333333331c2c1331c2
33333333333333332c133333333331c20000000000000000000000000000000000000000000000002221c8ccccc81222c1333333333333333333331c2c1331c2
11111111333333332c133333333331c20000000000000000000000000000000000000000000000002222228228822222c1111111111111111111111c2c1331c2
cccccccc333333332c133333333331c200000001100000000000000000000000000000001000000122222228822222221cccccccccccccccccccccc12c1111c2
22222222333333332c133333333331c20000001cc1000000000000000000000000000000c100001c222222882822222222222222222222222222222221cccc12
00eeee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0eeeeee001100110000000000000000000aaa00000aaa00000aaa00000aa90000000000000000000000000000000000000000000000000000000000000000000
e89ee89e1891189100000000000000000aaaaa000aaaaa000aaaaa000aaaa9000000000000000000000000000000000000000000000000000000000000000000
eabeeabe1ab11ab10000000000000000aaaaaaa0aaaaaa00aaaaa000aaaa20000000000000000000000000000000000000000000000000000000000000000000
eeeeeeee011001100000000000000000aaaaaa90aaaaa220aaaaee20aaa1ee200000000000000000000000000000000000000000000000000000000000000000
eeeeeeee000000000000000000000000aaaa9990aaaa2290aaa22220aa1ee2200000000000000000000000000000000000000000000000000000000000000000
eeeeeeee000000000000000000000000099994000999940009999400099992000000000000000000000000000000000000000000000000000000000000000000
e0e00e0e000000000000000000000000004440000044400000444000004450000000000000000000000000000000000000000000000000000000000000000000
00444400000777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
04ffff4000777770000770000000000000aaa00000aaa00000aaa00000aaa0000000000000000000000000000000000000000000000000000000000000000000
0ffffff00777777700777700000000000aaaaa000aaaaa000aaaaa000a222a000000000000000000000000000000000000000000000000000000000000000000
ff37f3ff077777760777776000000000aaaaaaa0aaaaaaa0aaaaaaa0a25e52a00000000000000000000000000000000000000000000000000000000000000000
fff7ffff07777666067776d000000000aaaaaa90aaaaaa90a22ee29042eee2400000000000000000000000000000000000000000000000000000000000000000
0ffffff0006666d000666d0000000000aaaa9990aa222990aa222990aa2229900000000000000000000000000000000000000000000000000000000000000000
0ff11ff0000ddd00000dd00000000000099994000999940009999400099994000000000000000000000000000000000000000000000000000000000000000000
00ffff00000000000000000000000000004440000044400000444000004440000000000000000000000000000000000000000000000000000000000000000000
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
000f030c070e0b0d0102040805060a090f030c070e0b0d05060a0980808080800f030c06050a09070b0e0d80808080808080808080808080808080808080808000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
3430303030303030303035343030303035000000000000343030300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
332521212128212121263233252121262b3030303030302c2521212121212121212121210000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221b311c221b311c223233221b1c2921212128212121212a1f1d3d3d3d3d3d3d3d3d3d3030303534303030303030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33222b302c222b302c223233223233223c3d3e223c3d3d3e222f2f2521212121282121212121263233252121212121000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3329212121202121212a2b2c222b2c2921212127282121212a2f2f221b31311c221b3131311c223233221b3131311c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221b311c221b311c2921212021212a3c3d3d3e223c3d3e222f2f2232000033223200000033223233223200000033000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33222b302c222b302c221b1c221b1c2921212821272821212a2f2f222b30302c222b3030302c222b2c222b3030302c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
332321212120212121243233223233223c1e221d1e221d3e222f2f2921212121202121282121272121272121282121000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
363131311c221b313131373322323323262f222f2f222f25242f2f221b31311c221b1c221b3131313131311c221b31000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
343030302c222b303030302c222b303e223f222d2e223f223c2e2f222b30302c223233222b3030353430302c223200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33252121212021212128212120212821272127212127212721262f2321212121203233232121263233252121243200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221d3d3e223c3d1e221b1c221f221d3d3d1e00001d3d3d1e2232313131311c22323631311c223233221b31310000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33223f25212721262f222b2c222f222f00003f00003f00002f2232000000003322323430302c222b2c222b30300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3329212a3c3d3e223f2921212a2f222f00006061626300002f22320000000033223233252121272121272121263200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33221f2321212127212a1b1c222f222f00000000000000002f22320000000033223233221d3d3e00003c3d1e223200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
33222d3d3d3d3d3d3e222b2c222f222d3d3d3d3d3d3d3d3d2e222b303030302c222b2c222f0000000000002f220000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
332321212121282121272121242f2321212128212128212121272121212121212021212a2f0000000000002f290000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
343d3d3d3d3e223c3d3d3d3d3d383d3d3d3e221b1c223c3d3d3d000000000000000000222f0000000000002f220000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
3325212121212721212121212121282121212432332321212128000000000000000000222d3d3d3d3d3d3d2e220000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000221b31313d37363d31311c22000000000000000000292121212121212121220000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000223264332521212632003322000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000222b302c221d1e222b302c22000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000020212121242f2f232121212a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000003d3d3d3d2e2d3d3d3d3d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100000000006050090500e05012050151501a0501f05022150270502e050361500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000500001025013250172501b2501f25029250332500e2500f250162501b25020250262502c250332500000000000000000000000000000000000000000000000000000000000000000000000000000000000000
