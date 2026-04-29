# Advanced Math Utility Module (Lua / Luau)

A high-performance, strictly typed math utility library built for scalable systems such as incremental games, simulations, and data-heavy Roblox experiences.

Optimized with:
--!strict
--!optimize 2

Focused on speed, large-number handling, and real game math.

---

## GETTING STARTED

Place the module in your project and require it:

```lua
local MathUtil = require(path.to.module)
```

---

## BASIC USAGE

Clamp a value:

```lua
print(MathUtil.clamp(15, 0, 10))
--> 10
```

Round numbers:

```lua
print(MathUtil.round(3.6))
--> 4
```

Min / Max:

```lua
print(MathUtil.min(10, 5, 20))
--> 5

print(MathUtil.max(10, 5, 20))
--> 20
```

---

## SCALING SYSTEMS (CORE)

Linear scaling:

```lua
local damage = MathUtil.linear(10, 2, 5)
-- result: 20
```

Exponential scaling:

```lua
local cost = MathUtil.expo(100, 1.15, 10)
```

Softcap:

```lua
local value = MathUtil.softcap(1000, 500, 0.5)
```

Hardcap:

```lua
local capped = MathUtil.hardcap(150, 100)
--> 100
```

Diminishing returns:

```lua
local reduced = MathUtil.diminish(1000, 100, 0.5)
```

---

## PROGRESSION SYSTEMS

Progress (0 → 1):

```lua
print(MathUtil.progress(50, 100))
--> 0.5
```

Level system:

```lua
local level, progress = MathUtil.levelProgress(10000, 10, 1.2)
```

Milestones:

```lua
local bonus = MathUtil.milestones(250, 50, 0.1)
```

---

## ECONOMY SYSTEMS

Max buy (bulk purchase):

```lua
local amount, totalCost = MathUtil.maxBuy(100000, 10, 1.1)

print(amount)
print(totalCost)
```

Dynamic cost:

```lua
local price = MathUtil.dynamicCost(100, 25, 1.15, "exp")
```

Hybrid cost:

```lua
local hybrid = MathUtil.dynamicCost(100, 25, 5, "hybrid")
```

---

## LARGE NUMBER FORMATTING

Short format:

```lua
print(MathUtil.short(1500))
--> "1.5k"
```

Bigger values:

```lua
print(MathUtil.short(1e9))
--> "1b"
```

Comma formatting:

```lua
print(MathUtil.Comma(1000000))
--> "1,000,000"
```

Smart formatter:

```lua
print(MathUtil.format(1e18))
```

---

## ENCODING SYSTEM (EXTREME VALUES)

Encode:

```lua
local encoded = MathUtil.lbencode(1e12)
```

Decode:

```lua
local decoded = MathUtil.lbdecode(encoded)

print(encoded)
print(decoded)
```

Preserve highest value:

```lua
local newEncoded = MathUtil.encodeData(500, encoded)
```

---

## ADVANCED MATH

Roots:

```lua
print(MathUtil.sqrt(25))  --> 5
print(MathUtil.cbrt(27))  --> 3
print(MathUtil.root(16, 4)) --> 2
```

Logs:

```lua
print(MathUtil.log(100, 10)) --> 2
print(MathUtil.log2(8))      --> 3
```

Exponential:

```lua
print(MathUtil.exp(1)) --> 2.718...
```

---

## UTILITY FUNCTIONS

Time conversion:

```lua
print(MathUtil.timeConvert(3661))
--> "1h:1m:1s"
```

ETA:

```lua
print(MathUtil.eta(50, 100, 5))
--> 10
```

AFK gain:

```lua
print(MathUtil.afkGain(10, 100, 500))
```

Session bonus:

```lua
print(MathUtil.sessionBonus(3600, 2))
```

---

## MOVEMENT / DISTANCE

Distance:

```lua
local d = MathUtil.studProgress(pos1, pos2)
```

Scaled distance:

```lua
local scaled = MathUtil.studProgressScaled(pos1, pos2, 10)
```

---

## EXTRA SYSTEMS

Logistic curve:

```lua
local val = MathUtil.logistic(10, 5, 1)
```

Smooth step:

```lua
local smooth = MathUtil.smoothStep(0.5, 0, 1)
```

Prestige system:

```lua
local resets = MathUtil.resetLayer(1000, 100, 1.5)
```

---

## CONSTANTS

```lua
MathUtil.pi
MathUtil.tau
MathUtil.e
MathUtil.huge
```

---

## PERFORMANCE NOTES

* Uses integer division (//) where possible
* Avoids unnecessary allocations
* Designed for high-frequency usage
* Fully compatible with strict mode

---

## USE CASES

* Incremental / idle games
* Tycoon systems
* Economy balancing
* Large-number leaderboards
* Data encoding / storage
* Simulation math

---

## NOTES

* Some functions assume valid input for performance
* Division by zero is not always checked
* Encoding prioritizes magnitude over precision
* Extremely large values may hit floating-point limits

---

## LICENSE

Free to use in any project.
Attribution optional.
