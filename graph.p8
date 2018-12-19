pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
local g={}
local p={2,0}
local d={14,5}
local cam={0,0}
local ct={0,0}
local cst="unknown"
local edges={}
local path={}
local enemy={}
function _init()
 g=map2graph()
 enemy=e.new(5,0)
end

function _update()

  enemy:update()

  local lp={p[1],p[2]}
  if btnp(0) then
    p[1]-=1
  elseif btnp(1) then
    p[1]+=1
  end
  if btnp(2) then
    p[2]-=1
  elseif btnp(3) then
    p[2]+=1
  end
  
  -- can we move there?
  local from = cell2idx(lp[1],lp[2])
  local to = cell2idx(p[1],p[2])
  local moves = g[from]
  -- not a legal move
  if not has_value(moves,to) then
    p=lp
  end
  
  
  -- compute path
  if btnp(4) then
    --enemy.step=true
  end
  
  -- reset target
  if btnp(5) then
    d={p[1],p[2]}
  end

  -- camera chase
  local ct=camchase(p,cam,ct)
  

  local node=cell2idx(p[1],p[2])
  edges = g[node]
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
  -- player
  clip()
  local pos=cell2mxy(p[1],p[2])
  circ(pos[1],pos[2],5,10)
  pset(pos[1],pos[2],15)
  
  --target
  pos=cell2mxy(d[1],d[2])
  circ(pos[1],pos[2],5,9)
  pset(pos[1],pos[2],9)
  
  -- enemy
  --enemy:draw()
  

  --drawpath(path,12)
  
  --enemy:draw_debug()

end

function draw_map()
  -- map , w/ opaque parts
  pal()
  camera(cam[1],cam[2])
  pal(5,0)
  palt(0,true)
  map(0,0,0,0,128,32)
  palt(5,true)
  map(0,0,0,-1,128,32)
  map(0,0,0,-2,128,32)
    
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
      if fget(s,3)==true then
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
 local p = cell2mxy(f[1],f[2])
 local m = { p[1]-64, p[2]-64}
 if m[1] < 0 then m[1]=0 end
 if m[2] < 0 then m[2]=0 end
 
 ct = m
 local vx = c[1] - ct[1]
 local vy = c[2] - ct[2]
 vx *= 0.3
 vy *= 0.3
 
 c[1] -= vx
 c[2] -= vy
 --c[1] = flr(c[1])
 --c[2] = flr(c[2])
 
 
 return m

end



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

function apply_pal(p)
  for i=0,15 do
    pal(i,p[i+1])
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
    local gl = cell2idx(d[1],d[2])
    t.path = astar(g,t.mg,gl)
end,
update=function(t)

  -- every tm ticks, 
  -- check path/target
  t.tk+=1
  if t.tk>t.tm then
    t:seek()
    t.tk=0
  end


  if t.tk%2==0 then return end


  local st_idx=cell2idx(t.cx,t.cy)
  -- follow path or stabilize
  -- on current tile
  --if t.path[#t.path]==st_idx then
  --  del(t.path,st_idx)
  --end  

  local tgt_idx
  if #t.path!=0 then
    t.state="on-path"
    tgt_idx=t.path[#t.path]
  else
    t.state="on-tgt"
    tgt_idx=st_idx
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
 
  
end,
draw=function(t)
  spr(64,t.pos[1]-4,t.pos[2]-4)
end,
draw_debug=function(t)
  print(t.cx .. "," .. t.cy,
    100,74,14
  )
  local n_cell=idx2cell(t.path[#t.path] or t.mg)
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
  drawpath(t.path,7)
  
  fillp(0b10100101010100101.1)


end
}

e={}
e.new=function(cx,cy)
  local t={cx=cx,cy=cy}
  local pos=cell2mxy(cx,cy)
  t.dv={0,0}
  t.mg=cell2idx(cx,cy)
  t.pos=pos
  t.tk=0
  t.tm=45
    
  t.path={cell2idx(cx,cy)}
  t.state="init"
  setmetatable(t,e)
  t:seek()
  return t
end

e.__index=e_m

-->8
local p_m={
update=function(p)
  local lp={p.cell[1],p.cell[2]}
  if btnp(0) then
    p.cell[1]-=1
  elseif btnp(1) then
    p.cell[1]+=1
  end
  if btnp(2) then
    p.cell[2]-=1
  elseif btnp(3) then
    p.cell[2]+=1
  end
  
  -- can we move there?
  local from = cell2idx(lp[1],lp[2])
  local to = cell2idx(p.cell[1],p.cell[2])
  local moves = g[from]
  -- not a legal move
  if not has_value(moves,to) then
    p.cell=lp
  end
  
  
  -- compute path
  if btnp(4) then
    path = astar(g,
     cell2idx(p.cell[1],p.cell[2]),
     cell2idx(d[1],d[2])
    )
  end
  
  -- reset target
  if btnp(5) then
    d={p.cell[1],p.cell[2]}
  end
end,
draw=function(t)

end,
}
local pl={}
pl.__index = p_m
pl.new=function(cx,cy)
  local t={}
  t.cell={cx,cy}
  t.dv={0,0}
  setmetatable(t,pl)
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
1c5555c1111111111c5555c11c5555c11c5555c1111111111c5555c11c5555c11c5555c155111111111111550000000000000000000000000000000000000000
c555555ccccccccc1c5555c1c555555c1c55555cccccccccc55555c1c55555c11c55555c51cccccccccccc150000000000000000000000000000000000000000
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c10000000000000000000000000000000000000000
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c10000000000000000000000000000000000000000
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c10000000000000000000000000000000000000000
55555555555555551c5555c1555555551c55555555555555555555c1555555c11c5555551c555555555555c10000000000000000000000000000000000000000
c555555ccccccccc1c5555c1cccccccc1c55555cc555555cc55555c1cccccc1551cccccc1c55555cc55555c10000000000000000000000000000000000000000
1c5555c1111111111c5555c1111111111c5555c11c5555c11c5555c111111155551111111c5555c11c5555c10000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
55555555000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00eeee00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0eeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e77ee77e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ec7eec7e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e0e00e0e000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00444400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
04ffff40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0ffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ff37f3ff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
fff7ffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0ffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0ff11ff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00ffff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
cccccccc000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
cccccccc700700000700077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc770700700700700707770777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc777707777770700707070707000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc707707000700777707700770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
ccc00ccc700700770700700707000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
000f030c070e0b0d0102040805060a090f030c070e0b0d05060a090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
19111511151511151511111511111a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1415131a14160012141111171911160000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
12181a18131011101620202012001411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111a0000000000000000000000000000000000
1411101111160012122020201411160000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000120000000000000000000000000000000000
1200120019101113131a20201200141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111170000000000000000000000000000000000
12001411171219111a1811111011170000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1811131511171220181111111600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
2020201411111011111111111600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1911111620201220202020201200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
14111a1411111011151115111700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
12201413111110151311131a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1815131111151017202020181a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0012191111171220202020201200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00121811111a1220202020201200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1910111111131011111111111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
