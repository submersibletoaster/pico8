pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
function _init()
  camx=0
  camy=0
  mpoffset=40
  px=32
  py=64
  pz=0
  pitch=0
  spd=2
  dy=0
  dz=0
  grav=0.03
  tk=0
end

function _update60()
  px+=spd
  camx=px-32
  
  
  dy=0
  if btn(0) then
    pitch+=0.3
  end
  
  if btn(1) then
    pitch-=0.3
  end
  
  
  
  if btn(2) then
    dy=-1
  end
  if btn(3) then
    dy=1
  end
  
  if btn(4) then
    spd+=0.125
  end
  -- jump
  if btnp(5) and pz==0 then
    dz=0.5
  end
  
  spd*=0.95
  dz-=grav
  
  pz+=dz
  -- grounding
  if pz<=0 then
    pz=0
    dz=0
  end
  
  if pitch>2 then
    pitch=2
  end
  -- decay pitch
  if not(btn(0) or btn(1)) then
    pitch*=0.6
  end
  
  py+=dy
  gp=0
  ontile=mget((px+16)/8,
           (py+16-mpoffset)/8)
  if fget(ontile,1) then
    gp=1
    pitch=2
    pz+=1
  end
  
  
  tk+=1
end


function _draw()
cls(3)
camera()
print("gp: "..gp)
print("tile:"..ontile)
camera(camx,camy)

pal()
map(0,0,0,mpoffset,128,16)

pal(15,14)
pal(9,4)
pal(4,2)
map(0,0,128*8,mpoffset,128,16)
dr_shadow()

dr_bike()
end


function dr_bike()
  local sp=64
  if tk%3==1 then
    sp=66
  elseif tk%3==2 then
    sp=68
  end
  
  if dy<0 then
    sp=98
  elseif dy>0 then
    sp=96
  end
  
  if pitch~=0 then
    sp=70+flr(pitch)*2
  end
  spr(sp,px,py-pz,2,2)
end

function dr_shadow()
  local wdth=min(pz,4)
  if tk%2==0 then
    line(px+wdth,py+16+pz,
         px+16-wdth,py+16+pz,
         5)
  end
end
__gfx__
00000000494994990000000000000009900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000949494440000000000000099990000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0070070049444494000000000000049999f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0007700094444949000000000000449449ff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0007700094444494000000000004444994fff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700444444490000000000444499994fff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000044944494000000000444449999f4fff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000099449949000000004444449449ff4fff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9999999999999999000000044444444994fff4fff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
99999999999999990000004444444499994fff4fff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9999999999999999000004444444449999f4fff4fff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9999999999999999000044444444449449ff4fff4fff000000000000000000000000000000000000000000000000000000000000000000000000000000000000
9999999999999999000444444444444994fff4fff4fff00000000000000000000000000000000000000000000000000000000000000000000000000000000000
99999999999999990044444444444499994fff4fff4fff0000000000000000000000000000000000000000000000000000000000000000000000000000000000
9999999999999999044444444444449999f4fff4fff4fff000000000000000000000000000000000000000000000000000000000000000000000000000000000
4944949449499494444444444444449449ff4fff4fff4fff00000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000044444444444444444ffff4fff4fff4ff00000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000444444444444449444ffff4fff4fff4f00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000004444444444444944444ffff4fff4fff400000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000044444444444494444444ffff4fff4fff00000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000444444444449444444444ffff4fff4ff00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000004444444444944444444444ffff4fff4f00000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000044444444494444444444444ffff4fff400000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000444444449444444444444444ffff4fff00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000009000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000009000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000090000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000900000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000009000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000777c000000000000777c000000000000777c00000000000077ccc000000000077ccc000000000067ccc000000000000000000000000000000000000000
00000777ccc0000000000777ccc0000000000777ccc0000000000777220000000000777220000000000677220000000000000000000000000000000000000000
00000cc22200000000000cc22200000000000cc22200000000000cc2270000000000cc22700000000007c2270000000000000000000000000000000000000000
00000777270000000000077727000000000007772700000000000777777000000000777777000000000c77777000000000000000000000000000000000000000
000005cc77700000000005cc77700000000005cc77700000000005cc0000000000007cc0000000000007cc0c0001000000000000000000000000000000000000
000077cc00000000000077cc00000000000077cc00000000000077cccc01000000077ccc000100000077ccc10010880000000000000000000000000000000000
000775cccc010000000775cccc010000000775cccc010000000775ccc111000000775cccc110000007757ccc1688008000000000000000000000000000000000
0007557cc11100000007557cc11100000007557cc11100000007557c00010000007557cc88688800000000008861110000000000000000000000000000000000
00075570006100000007557000610000000755700061000000075570788688800075577088600080000755708806601000000000000000000000000000000000
088775577886888008877557788688800887755778868880088775577886600800775557c886110000877557c810001000000000000000000000000000000000
00000875788660080000087578866008000008757886600800000875c88861100807787ccc80601008000875cc10001000000000000000000000000000000000
0511886cc88861101115886cc88861155111886cc88861110511886cc001660105118660ccc00010511188600001110000000000000000000000000000000000
1108866cc00166111008866cc00166011008866cc00166051108866ccc010601110866000c100010100886600000000000000000000000000000000000000000
1066600ccc0106011066600ccc0106011066600ccc0106011066600c100100011066000000011100106660000000000000000000000000000000000000000000
10011000100510015100100010010011110010001005001110011000000011101001100000000000110010000000000000000000000000000000000000000000
11150000000011150111100000011150011510000001111011150000000000001115000000000000011510000000000000000000000000000000000000000000
00000000000000000000007777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000077000000000000c222c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000007cc7000000000077772000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000007cccc700000000cc777ccc0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000072222c00000000ccccccc10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000007c7227cc00000077ccccc100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00007ccc770ccc000007777cc1600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00007ccc100c1c000007577700600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000075c1100106000007557770868800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0880755c711688000888755777861110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00880875788668808888806577866001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0111886cc888611811188600cc880601000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
111186ccc000661111006000cc010001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
10666ccc00010611116610001cc10001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
10066cc0000100111111100000001110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
011100cc000011100111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
0080000102000000000000000000000000000101020200000000000000000000000001010202000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000003040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000003040000000000000000000000000000000000
0000000000000000000304000000000000000000000000000000000000000000000000000000000000001213141500000000000000000000000000000000030400000000000000000000000000000003040000000000000000000000000000000000000000000000000000001213141500000000000000000000000000000000
0000000000000000121314150000000000000000000304000000000000000000000000000000000000122213142515000000000000000000000000030412131415000000000000000000000310101013141500000000000000000000000000000000000000000000000000122213142515000000000000000000000000000000
1010101111111010221314251010101001100101112324101010101010101010111111111110101011222223242525101010111111101010111110131422131425010110101010101010101310101013142511111111101010101010111111101010011010101010101010222223242525111110111111111111111111111111
1011010110101110222324251010111010011010101010101010101010101010101111101111111111222322222425101010101011111011111010131422232425010110101010101010101310101023242511111111101111101010101011011111101010101011111110222322222425111111111111101111111111101111
1110111010111011232222241111101111111011111010101010101010101010010101111111101011232222222224101110101111101111101010232423222224010110101010101010102322222222222411111111101010101010100110101010101010101111101010232222222224111110101111111111111011111111
