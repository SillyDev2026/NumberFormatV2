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
	local hund = math.floor(index/100)
	index = math.fmod(index, 100)
	local ten = math.floor(index/10)
	index = math.fmod(index, 10)
	local one = math.floor(index/1)
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
	return (x > 0 and 1) or (x < 0 and -1) or 0
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

function tab.clamp(a: number, b: number, c: number): number
	return (a < b and b) or (a > c and c) or a
end

function tab.floor(x: number): number
	if x >= 0 then
		return x - (x-(x//1))
	else
		local i = x//1
		if i == x then return i else return i -1 end
	end
end

function tab.ceil(x: number): number
	if x % 1 == 0 then return x end
	if x >= 0 then return x - (x%1)+1 end
	return x-(x%1)
end

function tab.round(x: number): number
	if x >= 0 then
		local i = x%1
		return x - i + (i>=0.5 and 1 or 0)
	else
		local i = (-x) % 1
		return x + i - (i>=0.5 and 1 or 0)
	end
end

function tab.floord(x: number, decimal: number?): number
	decimal = decimal or 0
	local i = 1
	if decimal > 0 then
		for _ = 1, decimal:: number do
			i*=10
		end
	elseif decimal < 0 then
		for _ =1, -decimal:: number do
			i/=10
		end
	end
	return (x*i)//1/i
end

function tab.Comma(n: number): string
	local str = tostring(math.floor(n))
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
	if x < 1e3 then return tostring(tab.floord(x+0.001, canDecimal)) end
	local exp = math.floor(math.log10(x))
	local index = math.floor(exp/3)
	local man = tab.floord(x/10^(index*3) + 0.001, canDecimal)
	local start = #beginning
	if canComma and x <= cancomma then
		return tab.Comma(x)
	else
		if index > 102 then return 'Inf' end
		if index <= start then
			return man .. beginning[index]
		end
		return man .. tab.suffixPart(index-1)
	end
end

function tab.createAlpha(index: number): string
	local fir = alpha[math.floor((index-1)/26)+1] or '?'
	local sec = alpha[((index-1)%26)+1] or '?'
	return fir .. sec
end

function tab.format(x: number, canDecimal: number?, canComma: boolean?): string
	if x >= 1e15 then
		local exp = math.floor(math.log10(x))
		local index = math.floor((exp-15)/3)+1
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
	local sub = tab.add(val1, tab.neg(val2))
	if tab.le(sub, 0) then return 0 end
	return sub
end

function tab.mul(val1: number, val2:number)
	return val1*val2
end

function tab.inv(val1: number)
	return 1/val1
end

function tab.div(val1: number, val2: number)
	local mul = tab.mul(val1, tab.inv(val2))
	return mul
end

function tab.mod(val1: number, val2: number)
	assert(val2 ~= 0, 'Modulo by 0')
	local r = val1 % val2
	return r < 0 and r+val2 or r
end

function tab.fmod(val1: number, val2: number)
	return val1-val2*math.floor(val1/val2)
end

function tab.trunc(val: number)
	return val>= 0 and math.floor(val) or math.ceil(val)
end

function tab.modf(val1: number)
	local int = tab.trunc(val1)
	local frac = val1-int
	return int, frac
end

function tab.exp(x: number): number
	return e^x
end

function tab.sqrt(x: number): number
	return x^0.5
end

function tab.cbrt(x: number): number
	if x >= 0 then
		return x^(1/3) 
	else 
		return -((-x)^(1/3))
	end
end

function tab.root(x: number, n: number): number
	if x >= 0 then return x^(1/n) else if n%2 == 1 then return ((-x)^(1/n)) else error('Even root of neg number') end
	end
end

function tab.log2(x: number): number
	return math.log(x)*inln2
end

function tab.logn(x: number, base: number): number
	return math.log(x)/math.log(base)
end

function tab.ln1p(x: number): number
	if x <= -1 then error('ln1p undefined for x <= -1') end
	return math.log(1+x)
end

function tab.random(min: number?, max: number?): number
	if not min or max then return math.random() elseif min and not max then return math.random(min) else return math.random(min, max) end
end

function tab.lbencode(val: number)
	if val == 0 then return 4e18 end
	local exp = math.floor(math.log10(math.abs(val)))
	local man = val / 10^exp
	local sign = (man < 0) and 1 or 2
	local val_enc = sign * 1e18
	local exp_part = exp * 1e14
	local man_part = math.log10(math.abs(man)) * 1e13
	if sign == 2 then
		val_enc += exp_part + man_part
	elseif sign == 1 then
		val_enc += exp_part + man_part
		val_enc = 1e17 - val_enc
	end
	return val_enc
end

function tab.lbdecode(val: number)
	if val == 4e18 then return 0 end
	local sign = math.floor(val / 1e18)
	local v = (sign == 1) and (1e18 - val) or (val - 2e18)
	local exp = math.floor(v / 1e14)
	local man = 10^((v % 1e14) / 1e13)
	local res = man * 10^exp
	if sign == 1 then res = -res end
	return res
end

function tab.encodeData(val: number, oldData: number)
	local new = val
	if oldData then
		local old = tab.lbdecode(oldData)
		new = math.max(old, new)
	end
	return tab.lbencode(new)
end

function tab.timeConvert(seconds: number): string
	if seconds <= 0 then return "0s" end
	local days = math.floor(seconds / 86400)
	local hours = math.floor((seconds % 86400) / 3600)
	local minutes = math.floor((seconds % 3600) / 60)
	local secs = math.floor(seconds % 60)
	local parts = {}

	if days > 0 then
		table.insert(parts, days .. "d")
	end
	if hours > 0 then
		table.insert(parts, hours .. "h")
	end
	if minutes > 0 then
		table.insert(parts, minutes .. "m")
	end
	if secs > 0 or #parts == 0 then
		table.insert(parts, secs .. "s")
	end
	return table.concat(parts, ":")
end

function tab.maxBuy(currency: number, cost: number, multi: number): (number, number)
	local min = multi - 1
	local currMul = currency * min
	local currDiv = currMul / cost
	local totalAmount = math.floor(math.log(currDiv+1, multi))
	local mulPow = multi ^ totalAmount
	local mulPowSub = mulPow - 1
	local totalDiv = mulPowSub / min
	local totalCost = totalDiv * cost
	return totalAmount, totalCost
end

return tab
