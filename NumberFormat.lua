--!native
--!optimize 2
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

local log = math.log
local log10 = math.log10
local exp = math.exp
local sqrt = math.sqrt
local abs = math.abs
local floor = math.floor
local ceil = math.ceil
local random = math.random

local cancomma = 1e6
local LB_NEG_BASE = 1e18
local LB_POS_BASE = 2e18
local LB_ZERO = 4e18
local LB_EXP_SCALE = 1e14
local LB_MAN_SCALE = 1e13

local alpha1 = {
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z'
}

local first = {'', 'U', 'D', 'T', 'Qd', 'Qn', 'Sx', 'Sp', 'Oc', 'No'}
local second = {'', 'De', 'Vt', 'Tg', 'qg', 'Qg', 'sg', 'Sg', 'Og', 'Ng'}
local third = {'', 'Ce'}
local beginning = {'k', 'm', 'b', 'T'}

tab.pi, tab.tau, tab.huge, tab.e = pi, tau, inf, e
tab.halfPi, tab.ln2, tab.ln10, tab.nan = hpi, ln2, ln10, nan

-- Builds suffix pieces for the legacy short-number naming system.
function tab.suffixPart(index: number): string
	if index < 0 then return '' end
	local hund = index // 100
	local rem = index % 100
	local ten = rem // 10
	local one = rem % 10
	return (first[one + 1] or '') .. (second[ten + 1] or '') .. (third[hund + 1] or '')
end

function tab.isNaN(x: number): boolean
	return x ~= x
end

function tab.isInf(x: number): boolean
	return x == inf or x == -inf
end

function tab.isFinite(x: number): boolean
	return x == x and x ~= inf and x ~= -inf
end

function tab.abs(x: number): number
	return abs(x)
end

function tab.sign(x: number): number
	if x > 0 then return 1 end
	if x < 0 then return -1 end
	return 0
end

function tab.min(base: number, ...: number): number
	local n = select('#', ...)
	for i = 1, n do
		local v = select(i, ...)
		if v < base then base = v end
	end
	return base
end

function tab.max(base: number, ...: number): number
	local n = select('#', ...)
	for i = 1, n do
		local v = select(i, ...)
		if v > base then base = v end
	end
	return base
end

function tab.between(val: number, low: number, high: number): boolean
	return low <= val and val <= high
end

function tab.clamp(x: number, l: number, h: number): number
	if l > h then l, h = h, l end
	if x < l then return l end
	if x > h then return h end
	return x
end

function tab.floor(x: number): number
	return floor(x)
end

function tab.ceil(x: number): number
	return ceil(x)
end

function tab.trunc(val: number): number
	return val >= 0 and floor(val) or ceil(val)
end

-- Half values round away from zero.
function tab.round(x: number): number
	return x >= 0 and floor(x + 0.5) or ceil(x - 0.5)
end

function tab.floord(x: number, decimal: number?): number
	decimal = decimal or 2
	if decimal == 0 then return floor(x) end
	local p = 10 ^ decimal
	if p == inf or p == 0 then return x end
	return floor(x * p) / p
end

function tab.ceild(x: number, decimal: number?): number
	decimal = decimal or 2
	if decimal == 0 then return ceil(x) end
	local p = 10 ^ decimal
	if p == inf or p == 0 then return x end
	return ceil(x * p) / p
end

function tab.roundd(x: number, decimal: number?): number
	decimal = decimal or 2
	if decimal == 0 then return tab.round(x) end
	local p = 10 ^ decimal
	if p == inf or p == 0 then return x end
	return tab.round(x * p) / p
end

function tab.linear(base: number, add: number, level: number): number
	return base + add * level
end

function tab.expo(base: number, mult: number, level: number): number
	return base * (mult ^ level)
end

function tab.lerp(a: number, b: number, t: number): number
	return a + (b - a) * t
end

function tab.inverseLerp(a: number, b: number, value: number): number
	if a == b then return value >= b and 1 or 0 end
	return (value - a) / (b - a)
end

function tab.remap(value: number, inMin: number, inMax: number, outMin: number, outMax: number): number
	return tab.lerp(outMin, outMax, tab.inverseLerp(inMin, inMax, value))
end

function tab.approxEq(a: number, b: number, epsilon: number?): boolean
	epsilon = epsilon or 1e-12
	if a == b then return true end
	if tab.isNaN(a) or tab.isNaN(b) then return false end
	local scale = tab.max(1, abs(a), abs(b))
	return abs(a - b) <= epsilon * scale
end

function tab.softcap(x: number, cap: number, power: number): number
	if x <= cap then return x end
	if cap <= 0 then return x end
	return cap * ((x / cap) ^ power)
end

function tab.hardcap(x: number, cap: number): number
	return x > cap and cap or x
end

function tab.diminish(x: number, start: number, strenght: number): number
	if x <= start then return x end
	return start + (x - start) ^ strenght
end

function tab.progress(curr: number, goal: number): number
	if goal <= 0 then return 1 end
	return tab.clamp(curr / goal, 0, 1)
end

function tab.Comma(n: number): string
	if tab.isNaN(n) then return 'NaN' end
	if n == inf then return 'Inf' end
	if n == -inf then return '-Inf' end
	local str = tostring(tab.trunc(n))
	local signPrefix = ''
	if str:sub(1, 1) == '-' then
		signPrefix = '-'
		str = str:sub(2)
	end
	if str:find('[eE]') then return signPrefix .. str end
	local result = str:reverse():gsub('(%d%d%d)', '%1,'):reverse()
	if result:sub(1, 1) == ',' then result = result:sub(2) end
	return signPrefix .. result
end

-- Shortens values using k/m/b/T and the legacy generated suffix table.
function tab.short(x: number, canDecimal: number?, canComma: boolean?): string
	canDecimal = canDecimal or 2
	canComma = canComma or false
	if tab.isNaN(x) then return 'NaN' end
	if x == inf then return 'Inf' end
	if x == -inf then return '-Inf' end

	local ax = abs(x)
	local signPrefix = x < 0 and '-' or ''
	if canComma and ax >= 1e3 and ax <= cancomma then return tab.Comma(x) end
	if ax < 1e3 then return tostring(tab.floord(x, canDecimal)) end

	local group = floor(log10(ax) / 3)
	if group > 102 then return signPrefix .. 'Inf' end
	local divisor = 10 ^ (group * 3)
	local man = tab.floord(ax / divisor + 1e-12, canDecimal)
	local suffix
	if group <= #beginning then
		suffix = beginning[group]
	else
		suffix = tab.suffixPart(group - 1)
	end
	return signPrefix .. tostring(man) .. suffix
end

-- Conventional two-letter sequence: aa, ab, ... az, ba, bb ...
function tab.createAlpha(index: number): string
	if index < 1 then return '' end
	local i = index - 1
	local fir = alpha1[(i // 26) % 26 + 1] or '?'
	local sec = alpha1[i % 26 + 1] or '?'
	return fir .. sec
end

-- Uses alphabetic suffixes from 1e15 upward; otherwise matches short().
function tab.format(x: number, canDecimal: number?, canComma: boolean?): string
	canDecimal = canDecimal or 2
	canComma = canComma or false
	if tab.isNaN(x) then return 'NaN' end
	if x == inf then return 'Inf' end
	if x == -inf then return '-Inf' end

	local ax = abs(x)
	local signPrefix = x < 0 and '-' or ''
	if ax >= 1e15 then
		local magnitude = floor(log10(ax))
		local index = floor((magnitude - 15) / 3) + 1
		local divisor = 10 ^ (15 + (index - 1) * 3)
		local man = tab.floord(ax / divisor + 1e-12, canDecimal)
		return signPrefix .. tostring(man) .. tab.createAlpha(index)
	end
	return tab.short(x, canDecimal, canComma)
end

function tab.me(val1: number, val2: number): boolean
	return val1 > val2
end

function tab.le(val1: number, val2: number): boolean
	return val1 < val2
end

function tab.eq(val1: number, val2: number): boolean
	return val1 == val2
end

function tab.meeq(val1: number, val2: number): boolean
	return val1 >= val2
end

function tab.leeq(val1: number, val2: number): boolean
	return val1 <= val2
end

function tab.add(val1: number, val2: number): number
	return val1 + val2
end

function tab.neg(val1: number): number
	return -val1
end

-- Saturating subtraction retained for compatibility.
function tab.sub(val1: number, val2: number): number
	local result = val1 - val2
	return result < 0 and 0 or result
end

function tab.mul(val1: number, val2: number): number
	return val1 * val2
end

function tab.inv(val1: number): number
	return 1 / val1
end

function tab.div(val1: number, val2: number): number
	return val1 / val2
end

function tab.mod(val1: number, val2: number): number
	return val1 % val2
end

-- math.fmod-style remainder: quotient truncates toward zero.
function tab.fmod(val1: number, val2: number): number
	if val2 == 0 then return nan end
	return val1 - val2 * tab.trunc(val1 / val2)
end

function tab.modf(val: number): (number, number)
	local int = tab.trunc(val)
	return int, val - int
end

function tab.exp(x: number): number
	return exp(x)
end

-- Accurate e^x - 1 near zero where exp(x)-1 loses precision.
function tab.expm1(x: number): number
	local ax = abs(x)
	if ax >= 1e-5 then return exp(x) - 1 end
	local x2 = x * x
	local x3 = x2 * x
	local x4 = x3 * x
	local x5 = x4 * x
	return x + x2 * 0.5 + x3 / 6 + x4 / 24 + x5 / 120
end

function tab.sqrt(x: number): number
	return sqrt(x)
end

function tab.cbrt(x: number): number
	if x == 0 then return x end
	if x < 0 then return -((-x) ^ (1 / 3)) end
	return x ^ (1 / 3)
end

function tab.root(x: number, n: number): number
	if n == 0 then error('0th root is undefined', 2) end
	if x >= 0 then return x ^ (1 / n) end
	if n % 1 == 0 and abs(n) % 2 == 1 then
		return -((-x) ^ (1 / n))
	end
	error('Negative numbers require an odd integer root', 2)
end

function tab.log2(x: number): number
	return log(x) * inln2
end

function tab.log(x: number, base: number?): number
	if base == nil then return log(x) end
	return log(x, base)
end

-- Accurate natural log(1+x) for tiny x.
function tab.ln1p(x: number): number
	if x < -1 then return nan end
	if x == -1 then return -inf end
	local ax = abs(x)
	if ax >= 1e-4 then return log(1 + x) end
	local term = x
	local sum = x
	for n = 2, 10 do
		term *= x
		if n % 2 == 0 then
			sum -= term / n
		else
			sum += term / n
		end
	end
	return sum
end

function tab.hypot(x: number, y: number): number
	local ax = abs(x)
	local ay = abs(y)
	local hi = ax > ay and ax or ay
	local lo = ax > ay and ay or ax
	if hi == inf then return inf end
	if hi == 0 then return 0 end
	local r = lo / hi
	return hi * sqrt(1 + r * r)
end

function tab.random(min: number?, max: number?): number
	if min == nil then return random() end
	if max == nil then return random(1, min) end
	if min > max then min, max = max, min end
	return random(min, max)
end

-- Sort-preserving logarithmic codec. Positive values retain the old positive layout.
-- Negative encoding was broken previously; it now mirrors the positive payload safely.
function tab.lbencode(val: number): number
	if tab.isNaN(val) then return nan end
	if val == inf or val == -inf then return val end
	if val == 0 then return LB_ZERO end
	local negative = val < 0
	local v = negative and -val or val
	local exponent = floor(log10(v))
	local man = v / (10 ^ exponent)
	local manPart = log10(man) * LB_MAN_SCALE
	local payload = exponent * LB_EXP_SCALE + manPart
	if negative then return LB_NEG_BASE - payload end
	return LB_POS_BASE + payload
end

function tab.lbdecode(val: number): number
	if tab.isNaN(val) then return nan end
	if val == inf or val == -inf then return val end
	if val == LB_ZERO then return 0 end

	local negative = val < 1.5e18
	local payload = negative and (LB_NEG_BASE - val) or (val - LB_POS_BASE)
	local exponent = floor(payload / LB_EXP_SCALE)
	local manPart = payload - exponent * LB_EXP_SCALE
	local man = 10 ^ (manPart / LB_MAN_SCALE)
	local result = man * (10 ^ exponent)
	return negative and -result or result
end

function tab.encodeData(val: number, oldData: number?): number
	if oldData ~= nil then
		local old = tab.lbdecode(oldData)
		if old == old and old > val then val = old end
	end
	return tab.lbencode(val)
end

function tab.timeConvert(seconds: number): string
	if seconds <= 0 or tab.isNaN(seconds) then return '0s' end
	if seconds == inf then return 'Inf' end

	local whole = floor(seconds)
	local days = whole // 86400
	local hours = (whole // 3600) % 24
	local minutes = (whole // 60) % 60
	local secs = whole % 60
	local s = ''
	if days > 0 then s = s .. days .. 'd:' end
	if hours > 0 or days > 0 then s = s .. hours .. 'h:' end
	if minutes > 0 or hours > 0 or days > 0 then s = s .. minutes .. 'm:' end
	s = s .. secs .. 's'
	return s
end

-- Maximum count and total geometric cost for cost * multi^owned.
function tab.maxBuy(currency: number, cost: number, multi: number): (number, number)
	if currency <= 0 or cost <= 0 or tab.isNaN(currency) or tab.isNaN(cost) then return 0, 0 end
	if multi < 1 or tab.isNaN(multi) then error('maxBuy multi must be >= 1', 2) end
	if currency < cost then return 0, 0 end
	if multi == 1 then
		local total = floor(currency / cost)
		return total, total * cost
	end

	local min = multi - 1
	local total = floor(log(currency * min / cost + 1, multi))
	if total < 0 then total = 0 end
	local totalCost = cost * ((multi ^ total - 1) / min)

	-- Correct rare floating-point off-by-one results near an exact purchase boundary.
	while total > 0 and totalCost > currency do
		total -= 1
		totalCost = cost * ((multi ^ total - 1) / min)
	end
	local nextTotal = total + 1
	local nextCost = cost * ((multi ^ nextTotal - 1) / min)
	while nextCost <= currency do
		total = nextTotal
		totalCost = nextCost
		nextTotal += 1
		nextCost = cost * ((multi ^ nextTotal - 1) / min)
	end
	return total, totalCost
end

function tab.percent(val1: number, val2: number): number
	if val2 == 0 then
		if val1 == 0 then return 0 end
		return val1 > 0 and inf or -inf
	end
	return (val1 / val2) * 100
end

function tab.levelProgress(currency: number, cost: number, growth: number): (number, number)
	local level, spent = tab.maxBuy(currency, cost, growth)
	local nextCost = cost * (growth ^ level)
	if nextCost <= 0 or nextCost == inf then return level, 0 end
	local left = currency - spent
	return level, tab.clamp(left / nextCost, 0, 1)
end

-- Numerically stable logistic/sigmoid implementation.
function tab.logistic(x: number, mid: number, steep: number): number
	local z = steep * (x - mid)
	if z >= 0 then return 1 / (1 + exp(-z)) end
	local ez = exp(z)
	return ez / (1 + ez)
end

function tab.smoothStep(x: number, a: number, b: number): number
	if a == b then return x < a and 0 or 1 end
	x = tab.clamp((x - a) / (b - a), 0, 1)
	return x * x * (3 - 2 * x)
end

function tab.smootherStep(x: number, a: number, b: number): number
	if a == b then return x < a and 0 or 1 end
	x = tab.clamp((x - a) / (b - a), 0, 1)
	return x * x * x * (x * (x * 6 - 15) + 10)
end

function tab.resetLayer(val: number, base: number, power: number): number
	if base <= 0 then error('resetLayer base must be > 0', 2) end
	if val < base then return 0 end
	return floor((val / base) ^ power)
end

function tab.milestones(x: number, step: number, bonus: number): number
	if step <= 0 then error('milestones step must be > 0', 2) end
	return 1 + floor(x / step) * bonus
end

function tab.eta(curr: number, goal: number, rate: number): number
	if goal <= curr then return 0 end
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
	if second <= 0 then return 0 end
	return base * log10(second + 1)
end

-- Existing behavior is a soft cap, not a hard cap: overflow grows by sqrt(excess).
function tab.afkGain(rate: number, seconds: number, cap: number): number
	if rate <= 0 or seconds <= 0 then return 0 end
	local gain = rate * seconds
	if gain > cap then
		local excess = gain - cap
		if excess <= 0 then return cap end
		gain = cap + sqrt(excess)
	end
	return gain
end

function tab.studProgress(p1: Vector3, p2: Vector3): number
	return tab.floord((p1 - p2).Magnitude, 2)
end

function tab.studProgressScaled(p1: Vector3, p2: Vector3, scale: number): number
	if scale <= 0 then error('studProgressScaled scale must be > 0', 2) end
	local d = (p1 - p2).Magnitude
	return log(d + 1, scale + 1)
end

return table.freeze(tab)
