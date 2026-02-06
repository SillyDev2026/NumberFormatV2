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
local ceil = math.ceil
local log = math.log
local pow = math.pow
local abs = math.abs
local random = math.random
local fmod = math.fmod
local log10 = math.log10
local exp = math.exp
local sqrt = math.sqrt

local pow10 = {}
for i = -308, 308 do
	pow10[i] = 10^i
end

local cancomma = 1e6

local alpha1 = {
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z'
}

local alpha2 = {} do for i = string.byte('a'), string.byte('z') do table.insert(alpha2, string.char(i)) end end
tab.pi, tab.tau, tab.huge, tab.e = pi, tau, inf, e
local first = {'', 'U', 'D', 'T', 'Qd', 'Qn', 'Sx', 'Sp', 'Oc', 'No'}
local second = {'', 'De', 'Vt', 'Tg', 'qg', 'Qg', 'sg', 'Sg', 'Og', 'Ng'}
local third = {'', 'Ce'}
local beginning = {'k', 'm', 'b', 'T'}

-- Builds large-number suffix parts (e.g. Qa, Qi, Sp, etc.) based on a 3-digit index
function tab.suffixPart(index: number): string
	local hund = index // 100
	local rem = index % 100
	local ten = rem//10
	local one = rem%10
	return (first[one+1] or '') .. (second[ten+1] or '') .. (third[hund+1] or '')
end

-- Checks if a number is NaN (not-a-number)
function tab.isNaN(x: number): boolean
	return x ~= x
end

-- Checks if a number is positive or negative infinity
function tab.isInf(x: number) : boolean
	return x == inf or x == -inf
end

-- Absolute value without calling math.abs
function tab.abs(x: number): number
	return (x >=0 and x) or -x
end

-- Returns -1, 0, or 1 depending on the sign of the number
function tab.sign(x: number): number
	if x > 0 then return 1 elseif x < 0 then return -1 end
	return 0
end

-- Returns the smallest value among all provided numbers
function tab.min(base: number, ...: number): number
	local n = select('#', ...)
	for i = 1, n do
		local v = select(i, ...)
		if v < base then
			base = v
		end
	end
	return base
end

-- Returns the largest value among all provided numbers
function tab.max(base: number, ...: number): number
	local n = select('#', ...)
	for i = 1, n do
		local v = select(i, ...)
		if v > base then
			base = v
		end
	end
	return base
end
-- Clamps a number between a minimum and maximum value
function tab.clamp(x: number, l: number, h: number): number
	if x < l then return l end
	if x > h then return h end
	return x
end

-- Fast integer floor using integer division
function tab.floor(x: number): number
	return x // 1
end

-- Fast integer ceil using integer division trick
function tab.ceil(x: number): number
	return (x == x//1) and x or (x//1+1)
end

-- Linear scaling: base + add * level
function tab.linear(base: number, add: number, level: number): number
	return base + add * level
end

-- Exponential scaling: base * mult^level
function tab.expo(base: number, mult: number, level: number): number
	return base * (mult^level)
end

-- Softcap: scales normally until cap, then slows growth smoothly
function tab.softcap(x: number, cap: number, power: number): number
	if x <= cap then return x end
	return cap * ((x/cap)^power)
end

-- Hardcap: never allows value above cap
function tab.hardcap(x: number, cap: number): number
	return x > cap and cap or x
end

-- Diminishing returns after a start point
function tab.diminish(x: number, start: number, strenght: number): number
	if x <= start then return x end
	return start + (x-start)^strenght
end

-- Returns progress from 0 to 1 toward a goal value
function tab.progress(curr: number, goal: number): number
	if goal <= 0 then return 1 end
	return tab.clamp(curr/goal, 0, 1)
end

-- Standard rounding (0.5 up, handles negatives correctly)
function tab.round(x: number): number
	return (x>=0 and (x+0.5)//1) or (x-0.5)//1
end

-- Floors a number to a fixed number of decimal places
function tab.floord(x: number,decimal: number?): number
	decimal = decimal or 2
	local p = 10^decimal
	return (x*p)//1/p
end

-- Formats an integer with comma separators (1,234,567)
function tab.Comma(n: number): string
	local str = tostring(n//1)
	local result = str:reverse():gsub("(%d%d%d)","%1,") 
	result = result:reverse()
	if result:sub(1,1) == "," then
		result = result:sub(2)
	end
	return result
end

-- Shortens large numbers using suffixes (k, m, b, Qa, etc.)
function tab.short(x: number, canDecimal: number?, canComma: boolean?): string
	canDecimal = canDecimal or 2
	canComma = canComma or false
	if x < 1e3 then
		local frac = 10^canDecimal or 100
		local n = (x*frac)//1
		return tostring(n/frac)
	end
	local exp = math.log10(x)//1
	local index = (exp/3)//1
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

-- Generates alphabetic suffixes (aa, ab, ac...) for extreme magnitudes
function tab.createAlpha(index: number)
	local i = index-1
	local fir = alpha1[(i//26)%26+1] or '?'
	local sec = alpha1[(i//26)+1] or '?'
	return fir .. sec
end

-- Automatically formats numbers using suffix or alphabet system
function tab.format(x: number, canDecimal: number?, canComma: boolean?): string
	if x >= 1e15 then
		local exp = log10(x) // 1
		local index = ((exp - 15) / 3) // 1 + 1
		local man = x / (10^(15 + (index-1)*3))
		local i = index - 1
		local fir = alpha1[(i // 26) % 26 + 1] or '?'
		local sec = alpha1[(i // 26) + 1] or '?'
		return tab.floord(man + 0.001, canDecimal) .. fir .. sec
	end
	return tab.short(x, canDecimal, canComma)
end

-- Greater than
function tab.me(val1: number, val2: number): boolean
	return val1>val2
end

-- Less than
function tab.le(val1: number, val2: number): boolean
	return val1<val2
end

-- Equal
function tab.eq(val1: number, val2: number): boolean
	return val1==val2
end

-- Greater than or equal
function tab.meeq(val1: number, val2: number): boolean
	return val1>=val2
end

-- Less than or equal
function tab.leeq(val1: number, val2: number): boolean
	return val1<=val2
end

-- Addition of v1+v2 to get 1 from 0+1
function tab.add(val1: number, val2: number)
	return val1+val2
end

-- Negation
function tab.neg(val1: number)
	return -val1
end

-- Subtraction with floor at zero (no negatives)
function tab.sub(val1: number, val2: number)
	local sub = val1-val2
	if sub < 0 then return 0 end
	return sub
end

-- Multiplication
function tab.mul(val1: number, val2:number)
	return val1*val2
end

-- Multiplicative inverse (1/x)
function tab.inv(val1: number)
	return 1/val1
end

-- Division
function tab.div(val1: number, val2: number)
	local mul = val1/val2
	return mul
end

-- Modulo
function tab.mod(val1: number, val2: number): number
	return val1%val2
end

-- Floating-point modulo (manual implementation)
function tab.fmod(val1: number, val2: number): number
	return val1-val2*(val1//val2)
end

-- Truncates toward zero
function tab.trunc(val: number): number
	return val >= 0 and val//1 or -((-val)//1)
end

-- Splits number into integer and fractional parts
function tab.modf(val: number)
	local int = (val>= 0) and val//1 or -((-val)//1)
	local frac = val-int
	return int, frac
end

-- Exponential function (e^x)
function tab.exp(x: number): number
	return e^x
end

-- Square root
function tab.sqrt(x: number): number
	return x^0.5
end

-- Cube root
function tab.cbrt(x: number): number
	return x^(1/3)
end

-- Nth root, supports negative numbers for odd roots
function tab.root(x: number, n: number): number
	if x >= 0 then
		return x^(1/n)
	end
	if n % 2 == 1 then
		return (-x)^(1/n)
	end
	error("Even root of neg number")
end

-- Base-2 logarithm
function tab.log2(x: number): number
	return log(x)*inln2
end

-- Logarithm with optional base (defaults to natural log)
function tab.log(x: number, base: number): number
	if not base then return log(x) end
	return log(x, base)
end

-- Natural log of (1 + x), safe for small x
function tab.ln1p(x: number): number
	if x <= -1 then error('ln1p undefined for x <= -1') end
	return log(1+x)
end

-- Random number generator with optional min/max
function tab.random(min: number?, max: number?): number
	if min and max then
		return math.random(min, max)
	end
	return math.random()
end

-- Encodes a number into a sortable logarithmic space value
function tab.lbencode(val: number): number
	if val == 0 then return 4e18 end
	local v = val
	local sign = (v<0) and 1 or 2
	local exp = log10(v)//1
	local man = v / 10^exp
	local manPart = log10(man)*1e13
	local valEnc = sign * 1e18 + exp * 1e14 + manPart
	if sign == 0 then valEnc = 1e17 - valEnc end
	return valEnc
end

-- Decodes a value produced by lbencode back into a number
function tab.lbdecode(val: number): number
	if val == 4e18 then return 0 end
	local sign = val // 1e18
	local v = (sign == 1) and (1e18-val) or (val-2e18)
	local exp = v//1e14
	local man = 10^((v%1e14)/1e13)
	local res = man * 10^exp
	if sign == 1 then res = -res end
	return res
end
-- Encodes data while preserving the highest historical value
function tab.encodeData(val: number, oldData: number): number
	if oldData then
		local v = oldData
		if v == 4e18 then
			v = 0
		else
			local sign = v // 1e18
			local tmp = (sign == 1) and (1e18 - v) or (v - 2e18)
			local exp = tmp // 1e14
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
	local exp = math.log10(v) // 1
	local man = v /10^exp
	local manPart = math.log10(man) * 1e13
	local valEnc = sign * 1e18 + exp * 1e14 + manPart
	if sign == 1 then
		valEnc = 1e17 - valEnc
	end
	return valEnc
end
-- Converts seconds into readable time (Xd:Xh:Xm:Xs)
function tab.timeConvert(seconds: number): string
	if seconds <= 0 then return "0s" end

	local days = seconds // 86400
	local hours = (seconds // 3600) % 24
	local minutes = (seconds // 60) % 60
	local secs = seconds % 60

	local s = ""
	if days > 0 then s = s .. days .. "d:" end
	if hours > 0 or days > 0 then s = s .. hours .. "h:" end
	if minutes > 0 or hours > 0 or days > 0 then s = s .. minutes .. "m:" end
	s = s .. secs .. "s"
	return s
end

-- Calculates max purchasable amount and total cost for exponential prices
function tab.maxBuy(currency: number, cost: number, multi: number): (number, number)
	local min = multi - 1
	local total = math.floor(log(currency * min / cost + 1, multi))
	local totalCost = cost * ((multi^total - 1) / min)
	return total, totalCost
end

-- Returns percentage representation of two values
function tab.percent(val1: number, val2: number)
	return (val1/val2)*100
end
-- Calculates level gained and progress toward next level
function tab.levelProgress(currency: number, cost: number, growth: number): (number, number)
	local level, spent = tab.maxBuy(currency, cost, growth)
	local nextCost = cost*(growth^level)
	local left = currency - spent
	return level, left/nextCost
end

function tab.logistic(x: number, mid: number, steep: number): number
	return 1/(1+exp(-steep*(x-mid)))
end

function tab.smoothStep(x: number, a: number, b: number): number
	x = tab.clamp((x-a)/(b - a), 0, 1)
	return x*x*(3-2*x)
end

function tab.resetLayer(val: number, base: number, power: number): number
	if val < base then return 0 end
	return ((val / base) ^ power) // 1
end

function tab.milestones(x: number, step: number, bonus: number): number
	return 1 + (x // step) * bonus
end

function tab.eta(curr: number, goal: number, rate: number): number
	if rate <= 0 then return inf end
	return (goal - curr) / rate
end

function tab.dynamicCost(cost: number, owned: number, scale: number, methods: 'exp'|'linear'|'hybrid'): number
	if methods == 'exp' then return cost * (scale ^ owned) end
	if methods == 'linear' then return cost + scale * owned end
	if methods == 'hybrid' then return cost * (scale ^ owned) + scale * owned end
	return cost
end

function tab.sessionBonus(second: number, base: number): number
	return base * log10(second + 1)
end

function tab.afkGain(rate: number, seconds: number, cap: number): number
	local gain = rate * seconds
	if gain > cap then
		gain = cap + sqrt(gain- cap)
	end
	return gain
end

function tab.studProgress(p1: Vector3, p2: Vector3)
	return tab.floord((p1 - p2).Magnitude, 2)
end

function tab.studProgressScaled(p1: Vector3, p2: Vector3, scale: number)
	local d = (p1-p2).Magnitude
	return log(d+1, scale+1)
end

return tab
