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
    enemy.step=true
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
  camera(cam[1],cam[2])
  map(0,0,0,0,128,32)
  
  -- player
  local pos=cell2mxy(p[1],p[2])
  circ(pos[1],pos[2],5,10)
  pset(pos[1],pos[2],15)
  
  --target
  pos=cell2mxy(d[1],d[2])
  circ(pos[1],pos[2],5,9)
  pset(pos[1],pos[2],9)
  
  -- enemy
  enemy:draw()
  

  drawpath(path,12)
  
  enemy:draw_debug()

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
 
 return m

end
-->8
-- enemies
local e_m={
seek=function(t)
    local st = cell2idx(t.cx,t.cy)
    local gl = cell2idx(d[1],d[2])
    t.path = astar(g,st,gl)
end,
update=function(t)

  -- debugging
  if not t.step then
    return
  end
  t.step=false

  -- every tm ticks, 
  -- check path/target
  t.tk+=1
  if t.tk>t.tm then
    t:seek()
    t.tk=0
  end


  local st_idx=cell2idx(t.cx,t.cy)
  -- follow path or stabilize
  -- on current tile
  if t.path[#t.path]==st_idx then
    del(t.path,st_idx)
  end  

  if #t.path!=0 then
    t.state="on-path"
  else
    add(t.path,st_idx)
    t.state="on-tgt"
  end
  local tgt_idx=t.path[#t.path]

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
  
  -- if we've reached this step
  -- remove it from path
  if t.pos[1]==tgt_pos[1]
  and t.pos[2]==tgt_pos[2] then
    del(t.path,new_idx)
    t.state=t.state .. "-n"
  end

  t.cx=new_cell[1]
  t.cy=new_cell[2]
  
  if #t.path==0 then
    add(t.path,new_idx)
  end
  
end,
draw=function(t)
  spr(64,t.pos[1]-4,t.pos[2]-4)
end,
draw_debug=function(t)
  print(t.cx .. "," .. t.cy,
    100,74,14
  )
  local n_cell=idx2cell(t.path[#t.path])
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
__gff__
000f030c070e0b0d0102040805060a0900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0e020602060602060602020602020f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0506040f050700030502020c0e02070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
030d0f0d04010201070000000300030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0502040202070003030000000502070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
030000000e010204040f00000300030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03000e020c030e080f0d020201020c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0d020406020c03000d0202020700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000502020102020209020700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000b00000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000d02020202020c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
