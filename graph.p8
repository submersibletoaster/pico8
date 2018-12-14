pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
local g={}
local p={2,0}
local d={14,5}
local cst="unknown"
local edges={}
local path={}
function _init()
 g=map2graph()
end

function _update()
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
  
  if btnp(4) then
    path = astar(g,
     cell2idx(p[1],p[2]),
     cell2idx(d[1],d[2])
    )
  end 

  local node=cell2idx(p[1],p[2])
  edges = g[node]
end

function _draw()
  cls()
  map(0,0,0,0,128,32)
  
  -- player
  local pos=cell2mxy(p[1],p[2])
  circ(pos[1],pos[2],5,10)
  pset(pos[1],pos[2],15)
  
  --target
  pos=cell2mxy(d[1],d[2])
  circ(pos[1],pos[2],5,9)
  pset(pos[1],pos[2],9)
  
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
  -- current position
  print(cell2idx(p[1],p[2]),100,60,5)
  drawpath(path,12)
  print(tablestr(path),0,80)
  -- debug
  print(cst,4,120,11)
end



function drawpath(path,col)
  for p in all(path) do
    local c=idx2cell(p)
    local pos=cell2mxy(c[1],c[2])
    circ(pos[1],pos[2],2,col)
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
    printh("n-open: " .. #open)
    local cur= lowest(fscore,open)
    printh("\t" .. cur)

    if cur == goal then
      return reconstruct_path(camefrom,cur)
    end
    del(open,cur)
    add(closed,cur)
    
    for n in all(g[cur]) do
      repeat
      
      if has_value(closed,n) then
        --printh("bail visited: " .. n)
        do break end
      end
      
      local tgs = gscore[cur] +
                  distance(cur,n)
      if not has_value(open,n) then
        printh("add open: " .. n)
      	 add(open,n)      
      elseif (gscore[n]!=nil and tgs >= gscore[n]) then
        printh("bail gscore")
        do break end
      end
      
      camefrom[n]= cur
      gscore[n] = tgs
      fscore[n] = gscore[n]+heur(n,goal)
      
      printh(cur)     	 
      until true
      
    end
  
   
  end

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
__gfx__
000aa000000aa00000000000000aa000000aa000000aa00000000000000aa0000000000000000000000aa00000077000000aa000000aa0000000000000000000
00000000000110000000000000011000000110000001100000000000000110000000000000000000000110000001100000011000000110000000000000000000
00700700000110000000000000011000000110000001100000000000000110000000000000000000000110000001100000011000000110000000000000000000
80077009811111198111111900011000811111190001111981111119811110008111111771111119000110000001100081111000000111190001111981111000
80077009811111198111111900011000811111190001111981111119811110008111111771111119000110000001100081111000000111190001111981111000
00700700000110000000000000011000000000000001100000011000000110000000000000000000000110000001100000000000000000000001100000011000
00000000000110000000000000011000000000000001100000011000000110000000000000000000000110000001100000000000000000000001100000011000
000bb000000bb00000000000000bb00000000000000bb000000bb000000bb000000000000000000000077000000bb0000000000000000000000bb000000bb000
__gff__
000f030c070e0b0d0102040805060a0900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0e020602060602060602020602020f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0506040f050700030502020c0e02070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
030d0f0d04010201070000000300030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0502040202070003030000000502070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
030000000e010204040f00000300030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03000e020c030000000d020204020c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0d020402020c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
