pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- loop
maxt=150
maxfit=0
best=nil
gen=1
bots=5
t=0
function _init()
 printh("game start")
 w  = world:new()
 pl = player:new({x=16,y=64,col=15})
 for _=1,bots do
   add(nmes,enemy:new({x=96,pl=pl}))
 end

 menuitem(1,"save model",save_model)
end

function _update60()
  t+=1
  -- kill bot
  if btnp(4) then
    printh("kill bot")
    nmes[1]=nil
  end
  if btnp(5) then
    foreach(nmes,function(en)
     en.net:mutate()
     end
    )
  end

--]]


 local pop={}
 foreach(nmes,function(en)
    -- kill after lifetime, unless better than bestfit
     if en.lt>maxt  then
       local cfit = en.net:getfit()
       local bfit = maxfit
       -- not fit enough
       if cfit < bfit then
         goto exterminate
       else
        best = nn:make_copy_of(en.net)
        maxfit = en.net:getfit()
        gen+=1
        goto exterminate
       end
     end

    add(pop,en)
   ::exterminate::

 end)

  -- add new bots if we've got fewer than we need
  while #pop<bots do
    --pl=player:new()
    local f=enemy:new({x=96,pl=pl,
      net= best~=nil and nn:make_copy_of(best) or nil
     })
    f.net:mutate()
    add(pop,f)
  end

  nmes=pop

  foreach(nmes,function(e)
    e:update(pl)
  end)

  w:update()
  pl:update(pl)
end

function _draw()
 cls()
 w:draw()
 pl:draw()
 foreach(nmes,function(e)
   e:draw()
 end)
 nmes[1].net:draw(20,60,40,40)
 nmes[1]:draw_info()
 --print(pl.h,10,10,1)
 if best ~= nil then
   print(maxfit,10,20,8)
   print(gen,10,28,8)
 end
end



---
function inherits(mt)
 local nc={}
 local nc_mt={ __index = nc }

 if mt then
   setmetatable(nc, { __index=mt })
   nc.super = function()
     return mt
   end
 end


 return nc
end

function dist(p1,p2)
   return sqrt(
     (p1.x-p2.x)^2
     +
     (p1.y-p2.y)^2
   )
end

function avg(t)
  local a=0
  foreach(t,function(v)
    a+=v
   end
  )
  return a/#t
end
-->8
-- bot
bot={}
function bot:new(arg)
 printh("new bot")
 arg=arg==nil and {} or arg
 local this={
   x=arg.x or rnd(127),
   y=arg.y or rnd(127),
   s=arg.s or 5,
   h=arg.h or rnd(1.0),
   t=arg.t or 0,
   col=arg.col or flr(1+rnd(14)),
   dx=0,
   dy=0,
   score=0,
   lt=0,
   tagged=false,
 }

 -- face away from player
 if arg.pl then
  local vx=arg.pl.x-this.x
  local vy=arg.pl.y-this.y
  local h=atan2(vx,vy)
  this.h=(0.5+h)%1
 end


 self.__index=self
 setmetatable(this,self)
 self:init(arg)
 return this
end

function bot:init(arg)

end

function bot:control(e)
  assert(false,"override :control in subclass")
end

function bot:update(p2,w)
  self:physics(p2,w)
  self:control(p2,w)

end

function bot:draw()
 fillp()
 line(self.x,self.y,
   self.x+cos(self.h)*(self.s+2),
   self.y+sin(self.h)*(self.s+2),
   8
 )
 circfill(self.x,self.y,self.s,self.col)
end

-- requires enemy passed
function bot:physics(e)
  self.lt+=1
  -- movement
  self.dx+=cos(self.h)*self.t
  self.dy+=sin(self.h)*self.t
  self.x+=self.dx
  self.y+=self.dy

  -- decay
  self.t*=0.9
  self.dx*=0.95
  self.dy*=0.95

  -- boundary
  if self.x < 0 or self.x > 128 then
    self.x-= self.dx*3
    self.dx=-self.dx
  end
  if self.y < 0 or self.y > 128 then
    self.y-= self.dy*3
    self.dy= -self.dy
  end

  if self.h > 1 or self.h < 0 then
    self.h=1-self.h
  end

  -- points for contact
  if collide(self,e) then
    self.tagged=true
    self.score+=0.01
  end

end
-->8
-- enemies
enemy=inherits(bot)

function enemy:init(arg)
  if arg.net then
    self.net = arg.net
  else
    local net=nn:new({5,7,4})
    net:init_neurons()
    net:init_weights()
    self.net=net

    -- time on and off target
    self.tont = 0
    self.tofft = 0
  end

  -- previous distance to player
  self.pd  = 90

end
function enemy:control(p)
  -- where is player relative to us
  local vx=p.x-self.x
  local vy=p.y-self.y

  -- what is player direction
  local h=atan2(vx,vy)
  -- and distance
  local d=dist(self,p)
  -- moving closer >0
  local dv=d-self.pd
  self.dv = dv


  -- angle distance, how close to correct heading
  -- are we
  local hv=h-self.h
  local dh=hv
  if hv>0.5 then hv-=0.5 end
  if hv<-0.5 then hv=-hv +0.5 end


  local cv=min(
    dist({x=0,y=0},
         {x=self.dx,y=self.dy}
    ), 128
  )

  self.th=h
  self.pd=d

  -- feed network
  local i={
    d/181.0, -- distance to player (0 close, 1 far)
   0.5+hv, -- direction bias (0.5 is spot on target)
   self.h, -- our actual heading - possibly confusing (wraps at <0.0 >1.0 )
   0.5 + ( (vx/181) /2 ), -- x bias (0.5 is spot on)
   0.5 + ( (vy/181) /2 )
  }

  --[[
  local i={
   --0.5+( dv*10.0) ,
   --dv,
   d/181.0,
   0.5+hv,
   self.h,
   0.5+(vx/181),
   0.5+(vy/181)
--]]

  self.i = i
  local out=self.net:feed_forward(i)

  local dec=get_class(out)
  if dec==1 then
    -- turn left
    self.h+=0.01
  elseif dec==2 then
    -- turn right
    self.h-=0.01
  elseif dec==3 then
    -- don't turn -boost
    self.t+=0.01
  else
    -- do nothing
  end

  -- always thrust _slightly_ forward
  self.t+=0.001


  -- FITNESS
  -- time on target
  if dv > 0 then
    self.tofft+=1
  else
    self.tont+=1
  end

  -- fitness is how much of our lifetime was spent on target
  local fit = (self.tont/self.tofft)
  self.fit = fit
  self.net:setfit(fit)

  --if self.net:getfit() < 0 then
  --  self.net:mutate()
  --end

end

function enemy:mutate()
 local nxt = nn:make_copy_of(self.net)
 nxt:mutate()
 self.net=nxt
 self.x =rnd(128)
 self.y =rnd(128)
end

function enemy:draw()
  self:super().draw(self)
  -- show heading to target
  line(self.x,self.y,
  	self.x+cos(self.th)*10,
  	self.y+sin(self.th)*10,
  	11
  )
end

function enemy:draw_info()
  print(self.net:getfit(),10,92,self.col)
  print(self.lt,10,100)
  print(self.tont,10,108)
  print(self.tofft,10,116)

  local depth=128-#self.i*8
  for j = 1,#self.i do
    print(self.i[j],100,
      depth+(8*(j-1))
    )
  end
end

-->8
-- ff nn

-- palette used by network
heat_pal={1,2,8,14,7}
-- +-------+
-- | tools |
-- +-------+

-- hiberbolic tangent function
function tanh(x)
    return sinh(x)/cosh(x)
end
-- hiperbolic cos
function cosh(x)
    return 0.5*(exp(x)-exp(-x))
end
-- hiperbolic sin
function sinh(x)
    return 0.5*(exp(x)+exp(-x))
end
-- exp
function exp(x)
    local e=2.71828183
    return e^x
end
-- returns index of a cell with max value
function get_class(out)
    local idx=1
    for i=2,#out do
        local o=out[i]
        if o>out[idx] then idx=i end
    end
    return idx
end

-----------------------------------------------------------------------

-- +----------------+
-- | neural network |
-- +----------------+

nn={} -- nn short for neural networlk

-- creating a new neural network
function nn:new(layers)
    local this={}

    this.layers={} --layers
    this.neurons={}--neurons
    this.weights={}--weights
    this.fitness=0

    -- creating layers
    for i=1,#layers do
        this.layers[i]=layers[i]
    end

    self.__index=self
    setmetatable(this,self)

    return this
end

-- makes a copy of another network
function nn:make_copy_of(net)
    local this={}

    this.layers=net.layers
    this.neurons=net.neurons
    this.weights=net.weights
    this.fitness=0

    self.__index=self
    setmetatable(this,self)

    return this
end

-- neurons initialization
function nn:init_neurons()
    -- for each layer
    for i=1,#self.layers do
        local _n={}
        -- for num of neurons in layers
        for j=1,self.layers[i] do
            -- create neuron/bias
            _n[j]=0
        end
        -- adding neuron to neurons table
        add(self.neurons,_n)
    end
end

-- creating weights (connections between neurons)
function nn:init_weights()
    -- for each layer (with offset)
    for i=2,#self.layers do

        -- layers table and num of neurons in previous layer
        local l_weights={}
        local n_prev_l=self.layers[i-1]

        -- for each neuron in layer
        for j=1,#self.neurons[i] do
            -- create weights table
            local n_weights={}
            -- for each neuron in prev layer
            for k=1,n_prev_l do
                -- create random weights
                n_weights[k]=rnd(1)-0.5
            end
            -- adding weights to layers table
            add(l_weights,n_weights)
        end
        -- adding layer weights table to weights table
        add(self.weights,l_weights)
    end
end

-- feed forward inputs to get output
function nn:feed_forward(inputs)
 -- for each input
    for i=1,#inputs do
        -- set neuron value to input value
        self.neurons[1][i]=inputs[i]
    end

    -- for each layer (with offset)
    for i=2,#self.layers do
        -- for each neuron
        for j=1,#self.neurons[i] do

            local value=0

            -- for each neuron in prev layer
            for k=1,#self.neurons[i-1] do
             -- we add weights to a value
                value+=self.weights[i-1][j][k]*self.neurons[i-1][k]
            end
            -- and use tanh activation function
            self.neurons[i][j]=tanh(value)
        end
    end

    -- returning output layer
    return self.neurons[#self.neurons]
end

-- mutate function
function nn:mutate()
    -- for each weight in neurons and layers
    for i=1,#self.weights do
        for j=1,#self.weights[i] do
         for k=1,#self.weights[i][j] do
          -- we get weight value and random number
            local w=self.weights[i][j][k]
            local r=rnd(100)

            -- depending on a random num we choose a mutation
            if r<=2 then
                w=-w
            elseif r<=4 then
                w=rnd(1)-0.5
            elseif r<=6 then
                factor=rnd(1)+1
                w*=factor
            elseif r<=8 then
                factor=rnd(1)
                w*=factor
            end

            -- then we set a new weight value
            self.weights[i][j][k]=w
         end
        end
    end
end

-- adding fitness
function nn:addfit(f)
    self.fitness+=f
end

-- getting fitness
function nn:getfit()
    return self.fitness
end

function nn:setfit(f)
    self.fitness=f
end

-- neural net vizualization
function nn:draw(_x,_y,_w,_h)

fillp(0b0101101001011010.1)
    local gx,gy=_x,_y
    local gw,gh=_w,_h

    local neurons={}
    -- getting neurons positions
    for i=1,#self.layers do
        local x_step=gw/#self.layers
        local x=(i-1)*x_step+x_step/2
        neurons[i]={}

        for j=1,self.layers[i] do
            local y_step=gh/self.layers[i]
            local y=(j-1)*y_step+y_step/2

            --just for fun
            x+=sin(t/240)*2
            y+=cos((t+i*12)/120)*8

            neurons[i][j]={gx+x,gy+y}
        end
    end

    -- drawing connections
    for i=2,#neurons do
        for j=1,#neurons[i] do
            for _j=1,#neurons[i-1] do
            local x1=neurons[i][j][1]
            local y1=neurons[i][j][2]
            local x2=neurons[i-1][_j][1]
        local y2=neurons[i-1][_j][2]

            -- draws relation between neurons
            val1=self.neurons[i-1][_j]
            val2=self.neurons[i][j]
            val1+=2.5
            val2+=2.5
            if val1<1 then val1=1 end
            if val1>5 then val1=5 end
            if val2<1 then val2=1 end
            if val2>5 then val2=5 end
            val=(val1+val2)/2

            -- draws weights values
            --val=self.weights[i-1][j][_j]
            --val+=0.5
            --val*=5
            --val+=1

            if val<1 then val=1 end
            if val>5 then val=5 end

            local c=heat_pal[flr(val)]
            line(x1,y1,x2,y2,c)
            end
        end
    end

    -- drawing neurons
    for i=1,#neurons do
        for j=1,#neurons[i] do
            local x=neurons[i][j][1]
            local y=neurons[i][j][2]
            val=self.neurons[i][j]
            val+=2.5
            val=flr(val)
            if val<1 then val=1 end
            if val>5 then val=5 end
            circfill(x,y,2,heat_pal[val])
        end
    end
end

--------------------------------------------------------

-->8
-- world
world={}
function world:new()
  local this={}
  self.__index=self
  setmetatable(this,self)
  return this
end

function world:update()

end

function world:draw()
  fillp(0b1010010110100101)
  rect(0,0,127,127,1+shl(13,4))
end

function collide(p1,p2)
  local vx=p1.x-p2.x
  local vy=p1.y-p2.y
  local d=sqrt( (vx*vx)+(vy*vy) )
  if d <= (p1.s+p2.s) then
    return true
  else
    return false
  end

end
-->8
-- player



player=inherits(bot)



function player:control(e)
  if btn(0) then
    self.h+=0.01
  elseif btn(1) then
    self.h-=0.01
  end


  if btn(2) then
    self.t+=0.01
  elseif btn(3) then
    self.t-=0.01
  end

end


-->8
-- +---------------+
-- | usage example |
-- +---------------+

-- function _init()
--  layers={4,5,5,2}
--  net=nn:new(layers)
--  net:init_neurons()
--  net:init_weights()
-- end

-- function _update()
--  input={0.2,0.5,0.4,0.9}
--  output=net:feed_forward(input)
--
--  fit=1 -- or some other value
--  net:addfit(fit)
--
--		if condition then
--   net:mutate()
--  end
-- end
