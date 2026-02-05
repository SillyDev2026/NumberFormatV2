--!strict
local tab = {}

local inf: number = 1/0
local nan: number = 0/0
local pi: number = 3.141592653589793
local tau: number = 6.283185307179586
local hpi: number = 1.5707963267948966
local inln2: number = 1.4426950408889634
local ln2: number = 0.6931471805599453
local ln10: number = 2.302585092994046
local e: number = 2.718281828459045

local floor = math.floor
local ceil = math.ceil
local log = math.log
local pow = math.pow
local abs = math.abs
local random = math.random
local fmod = math.fmod
local log10 = math.log10

local pow10 = {}
for i = -308, 308 do
	pow10[i] = pow(10, i)
end

local cancomma = 1e6 -- change this to set comma

local alpha = {} do
	for i = string.byte('a'), string.byte('z') do
		table.insert(alpha, string.char(i))
	end
end
tab.pi, tab.tau, tab.huge, tab.e = pi, tau, inf, e
local first = {'', 'U', 'D', 'T', 'Qd', 'Qn', 'Sx', 'Sp', 'Oc', 'No'}
local second = {'', 'De', 'Vt', 'Tg', 'qg', 'Qg', 'sg', 'Sg', 'Og', 'Ng'}
local third = {'', 'Ce'}
local beginning = {'k', 'm', 'b', 'T'}

function tab.suffixPart(index: number): string
	local hund = index // 100
	local rem = index % 100
	local ten = rem//10
	local one = rem%10
	return (first[one+1] or '') .. (second[ten+1] or '') .. (third[hund+1] or '')
end

function tab.isNaN(x: number): boolean
	return x ~= x
end

function tab.isInf(x: number) : boolean
	return x == inf or x == -inf
end

function tab.abs(x: number): number
	return (x >=0 and x) or -x
end

function tab.sign(x: number): number
	if x > 0 then return 1 elseif x < 0 then return -1 end
	return 0
end

function tab.min(base: number, ...: number): number
	local args = {...}
	local min = base
	for i = 1, #args do
		if args[i] < min then
			min = args[i]
		end
	end
	return min
end

function tab.max(base: number, ...: number): number
	local args = {...}
	local max = base
	for i = 1, #args do
		if args[i] > max then
			max = args[i]
		end
	end
	return max
end

function tab.clamp(x: number, lo: number, hi: number): number
	return x < lo and lo or (x > hi and hi or x)
end

function tab.floor(x: number): number
	return x // 1
end

function tab.ceil(x: number): number
	local i = x // 1
	return i == x and i or i +1
end

function tab.round(x: number): number
	if x>= 0 then
		return x - (x%1) + ((x%1)>=0.5 and 1 or 0)
	else
		local r = (-x)%1
		return x + r - (r>=0.5 and 1 or 0)
	end
end

function tab.floord(x: number, decimal: number?): number
	decimal = decimal or 2
	return (x*pow10[decimal])//1/pow10[decimal]
end

function tab.Comma(n: number): string
	local str = tostring(floor(n))
	local result = str:reverse():gsub("(%d%d%d)","%1,") 
	result = result:reverse()
	if result:sub(1,1) == "," then
		result = result:sub(2)
	end
	return result
end

function tab.short(x: number, canDecimal: number?, canComma: boolean?): string
	canDecimal = canDecimal or 2
	canComma = canComma or false
	if x < 1e3 then
		local frac = pow10[canDecimal or 2] or 100
		local n = (x*frac)//1
		return tostring(n/frac)
	end
	local exp = floor(math.log10(x))
	local index = floor(exp/3)
	local man = tab.floord(x/10^(index*3) + 0.001, canDecimal)
	local start = #beginning
	if canComma and x <= cancomma then
		return tab.Comma(x)
	else
		if index > 102 then return 'Inf' end
		if index <= start then
			return man .. beginning[index]
		end
		local suf = index-1
		local hund = index // 100
		local rem = index % 100
		local ten = rem//10
		local one = rem%10
		return man .. (first[one+1] or '') .. (second[ten+1] or '') .. (third[hund+1] or '')
	end
end

function tab.createAlpha(index: number)
	local i = index-1
	local fir = alpha[(i//26)+1] or '?'
	local sec = alpha[(i//26)+1] or '?'
	return fir .. sec
end

function tab.format(x: number, canDecimal: number?, canComma: boolean?): string
	if x >= 1e15 then
		local exp = floor(log10(x))
		local index = floor((exp-15)/3)+1
		local man = x/(10^(15+(index-1)*3))
		local alp = tab.createAlpha(index)
		return tab.floord(man + 0.001, canDecimal) .. alp
	end
	return tab.short(x, canDecimal, canComma)
end

function tab.me(val1: number, val2: number): boolean
	return val1>val2
end

function tab.le(val1: number, val2: number): boolean
	return val1<val2
end

function tab.eq(val1: number, val2: number): boolean
	return val1==val2
end

function tab.meeq(val1: number, val2: number): boolean
	return val1>=val2
end

function tab.leeq(val1: number, val2: number): boolean
	return val1<=val2
end

function tab.add(val1: number, val2: number)
	return val1+val2
end

function tab.neg(val1: number)
	return -val1
end

function tab.sub(val1: number, val2: number)
	local sub = val1-val2
	if sub < 0 then return 0 end
	return sub
end

function tab.mul(val1: number, val2:number)
	return val1*val2
end

function tab.inv(val1: number)
	return 1/val1
end

function tab.div(val1: number, val2: number)
	local mul = val1/val2
	return mul
end

function tab.mod(val1: number, val2: number): number
	return val1%val2
end

function tab.fmod(val1: number, val2: number)
	return val1-val2*floor(val1/val2)
end

function tab.trunc(val: number)
	return val>= 0 and floor(val) or ceil(val)
end

function tab.modf(val: number)
	local int = (val>= 0) and floor(val) or ceil(val)
	local frac = val-int
	return int, frac
end

function tab.exp(x: number): number
	return e^x
end

function tab.sqrt(x: number): number
	return x^0.5
end

function tab.cbrt(x: number): number
	return x^(1/3)
end

function tab.root(x: number, n: number): number
	if x >= 0 then
		return x^(1/n)
	end
	if n % 2 == 1 then
		return (-x)^(1/n)
	end
	error("Even root of neg number")
end

function tab.log2(x: number): number
	return log(x)*inln2
end

function tab.log(x: number, base: number): number
	if not base then return log(x) end
	return log(x, base)
end

function tab.ln1p(x: number): number
	if x <= -1 then error('ln1p undefined for x <= -1') end
	return log(1+x)
end

function tab.random(min: number?, max: number?): number
	if not min or max then return math.random() elseif min and not max then return math.random(min) else return math.random(min, max) end
end

function tab.lbencode(val: number): number
	if val == 0 then return 4e18 end
	local v = val
	local sign = (v < 0) and 1 or 2
	if sign == 1 then v = -v end
	local exp = floor(log10(v))
	local man = v/10^exp
	local manPart = log10(man) * 1e13
	local valEnc = sign * 1e18 + exp * 1e14 + manPart
	if sign == 1 then
		valEnc = 1e17 - valEnc
	end
	return valEnc
end

function tab.lbdecode(val: number)
	if val == 4e18 then return 0 end
	local sign = floor(val / 1e18)
	local v = (sign == 1) and (1e18 - val) or (val - 2e18)
	local exp = floor(v / 1e14)
	local man = 10^((v % 1e14) / 1e13)
	local res = man * 10^exp
	if sign == 1 then res = -res end
	return res
end

function tab.encodeData(val: number, oldData: number): number
	if oldData then
		local v = oldData
		if v == 4e18 then
			v = 0
		else
			local sign = floor(v / 1e18)
			local tmp = (sign == 1) and (1e18 - v) or (v - 2e18)
			local exp = floor(tmp / 1e14)
			local man = 10^((tmp % 1e14) / 1e13)
			v = man * 10^exp
			if sign == 1 then v = -v end
		end
		if v > val then val = v end
	end
	if val == 0 then return 4e18 end
	local v = val
	local sign = (v < 0) and 1 or 2
	if sign == 1 then v = -v end
	local exp = floor(log10(v))
	local man = v / 10^exp
	local manPart = log10(man) * 1e13
	local valEnc = sign * 1e18 + exp * 1e14 + manPart
	if sign == 1 then
		valEnc = 1e17 - valEnc
	end
	return valEnc
end

function tab.timeConvert(seconds: number): string
	if seconds <= 0 then return "0s" end

	local days = floor(seconds / 86400)
	local hours = floor(seconds / 3600) % 24
	local minutes = floor(seconds / 60) % 60
	local secs = floor(seconds % 60)

	local s = ""
	if days > 0 then s = s .. days .. "d:" end
	if hours > 0 or days > 0 then s = s .. hours .. "h:" end
	if minutes > 0 or hours > 0 or days > 0 then s = s .. minutes .. "m:" end
	s = s .. secs .. "s"

	return s
end

function tab.maxBuy(currency: number, cost: number, multi: number): (number, number)
	local min = multi - 1
	local currMul = currency * min
	local currDiv = currMul / cost
	local totalAmount = floor(log(currDiv+1, multi))
	local mulPow = multi ^ totalAmount
	local mulPowSub = mulPow - 1
	local totalDiv = mulPowSub / min
	local totalCost = totalDiv * cost
	return totalAmount, totalCost
end

function tab.percent(val1: number, val2: number)
	local percent = (val1/val1)*100
	return percent
end

function tab.levelProgress(currency: number, cost: number, growth: number): (number, number)
	local level, spent = tab.maxBuy(currency, cost, growth)
	local nextCost = cost*(growth^level)
	local left = currency - spent
	return level, left/nextCost
end

return tab
