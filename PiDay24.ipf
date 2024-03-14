#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3				// Use modern global access method and strict wave access
#pragma DefaultTab={3,20,4}		// Set default tab width in Igor Pro 9 and later
#pragma rtFunctionErrors=1
#pragma ModuleName=piDay24

static Constant MULTI_DIM = 100

// negative: 0 if positive value
// scale: number of digits after the decimal point
// precision: number of total digits, the same as the wave size
// maxMultiple: The maximum multiple that is cached in digits. Used for long division.
//              If this value is 0, no multiples are ever generated.
// maxCarryMultiple: The maximum multiple that is cached when a carry is available.
// digits: each byte represents two decimal digits
Structure Number
	char negative
	int64 scale
	int64 precision
	WAVE/L maxMultiple
	WAVE/B/U digits
EndStructure

threadsafe static Function [STRUCT Number num] init(int64 scale, int64 precision)
	num.negative = 0
	num.scale = scale
	num.precision = precision
	Make/FREE/L/N=(0) num.maxMultiple
	Make/FREE/B/U/N=(precision) num.digits
End

threadsafe static Function [STRUCT Number target] copy(STRUCT Number &source, [int64 multiple])
	target.negative = source.negative
	target.scale = source.scale
	target.precision = source.precision
	Duplicate/FREE source.maxMultiple, target.maxMultiple
	if(ParamIsDefault(multiple))
		Duplicate/FREE source.digits, target.digits
	else
		Duplicate/FREE/RMD=[*][multiple - 1] source.digits, target.digits
	endif
End

threadsafe static Function/S toString(STRUCT Number &num)

	string result = ""
	int64 offset = 2 * (DimSize(num.digits, 0) - num.scale)

	wfprintf result, "%02d"/R=[0, num.precision - 1], num.digits
	if(num.scale > 0)
		result = result[0, offset - 1] + "." + result[offset, Inf]
	endif

	if(CmpStr(result[0], "0") == 0)
		result = result[1, Inf]
	endif

	if(num.negative)
		return "-" + result
	else
		return result
	endif
End

// compares two numbers. Return <0 if a<b, =0 if a=b and >0 if a>b
threadsafe static Function Compare(STRUCT Number &a, STRUCT Number &b)
	int64 i, size
	variable result

	int64 integerSizeA = a.precision - a.scale
	int64 integerSizeB = b.precision - b.scale

	// ensure that a is positive
	if(a.negative)
		a.negative = 0
		b.negative = !b.negative

		result = -Compare(a, b)

		a.negative = 1
		b.negative = !b.negative

		return result
	endif

	// check if b is negative
	if(b.negative)
		return 1
	endif

	// ensure that the integer part of a is larger or equal to b
	if(integerSizeA < integerSizeB)
		return -Compare(b, a)
	endif

	// check if the integer part of a has a larger magnitude than b
	if(integerSizeA > integerSizeB)
		size = integerSizeA - integerSizeB
		WaveStats/Q/RMD=[0, size - 1]/M=1 a.digits
		if(V_sum > 0)
			return 1
		endif
	endif

	// calculate starting points of direct comparison
	int64 shiftA = max(integerSizeA, integerSizeB) - integerSizeB
	int64 shiftB = max(integerSizeA, integerSizeB) - integerSizeA
	size = min(a.precision - shiftA, b.precision - shiftB)

	// do direct comparison in comparable range
	for(i = 0; i < size; i += 1)
		result = a.digits[shiftA + i] - b.digits[shiftB + i]
		if(result != 0)
			return result
		endif
	endfor

	// a and b are equal for now. Check trailing digits
	if(a.precision > shiftA + size)
		WaveStats/Q/RMD=[shiftA + size, Inf]/M=1 a.digits
		return V_sum
	endif
	if(b.precision > shiftB + size)
		WaveStats/Q/RMD=[shiftB + size, Inf]/M=1 b.digits
		return -V_sum
	endif

	// a and b are truely equal
	return 0
End

threadsafe static Function [STRUCT Number result] Add(STRUCT Number &a, STRUCT Number &b)
	int64 i

	int64 integerSizeA = a.precision - a.scale
	int64 integerSizeB = b.precision - b.scale

	// we want to have the larger absolute number at a and the smaller one at b.
	// This is required to allow subtraction without a carry overflow
	variable negA = a.negative
	variable negB = b.negative
	a.negative = 0
	b.negative = 0
	variable cmp = Compare(a, b)
	a.negative = negA
	b.negative = negB
	if(cmp < 0)
		[result] = Add(b, a)
		return [result]
	endif

	// create result to hold a digits from a and b without a problem, also keep a single
	// digit for a carry
	result.scale = max(a.scale, b.scale)
	[result] = Init(result.scale, max(integerSizeA, integerSizeB) + result.scale + 1)

	int64 shiftA = max(integerSizeA, integerSizeB) - integerSizeA
	int64 shiftB = max(integerSizeA, integerSizeB) - integerSizeB

	// just copy the digits from a to result
	result.digits[shiftA + 1, shiftA + a.precision] = a.digits[p - shiftA - 1]

	// add b to a
	int64 offset = (result.precision - result.scale) - integerSizeB
	int64 carry = 0
	if(a.negative %^ b.negative)
		// slow subtraction
		for(i = b.precision - 1; i >= 0; i -= 1)
			carry = result.digits[offset + i] - b.digits[i] - carry
			if(carry < 0)
				carry += 100
				result.digits[offset + i] = carry
				carry = 1
			else
				result.digits[offset + i] = carry
				carry = 0
			endif
		endfor
		for(i = offset - 1; i >= 0; i -= 1)
			carry = result.digits[i] - carry
			if(carry < 0)
				carry += 100
				result.digits[i] = carry
				carry = 1
			else
				result.digits[i] = carry
				carry = 0
			endif
		endfor
		// carry should be 0
	else
		// slow addition, can be made faster when we calculate propagate and carry values.
		// maybe something for PiDay25
		for(i = b.precision - 1; i >= 0; i -= 1)
			carry += result.digits[offset + i] + b.digits[i]
			result.digits[offset + i] = mod(carry, 100)
			carry /= 100
		endfor
		for(i = offset - 1; i >= 0; i -= 1)
			carry += result.digits[i]
			result.digits[i] = mod(carry, 100)
			carry /= 100
		endfor
		// carry should be 0
	endif

	// fix precision
	FixLeadingZeros(result)

	// fix sign
	result.negative = a.negative && (sum(result.digits) != 0)
End

threadsafe static Function FixLeadingZeros(STRUCT Number &num)
	int64 offset

	int64 size = num.precision - num.scale
	int64 firstNonZero = size - 1
	for(offset = 0; offset < size; offset += 1)
		if(num.digits[offset] != 0)
			firstNonZero = offset
			break
		endif
	endfor
	if(firstNonZero > 0)
		Duplicate/FREE/RMD=[firstNonZero, *] num.digits, wvFixed
		Duplicate/FREE wvFixed, num.digits
		num.precision -= firstNonZero
	endif
End

// Calculate the multiples of the number and store them in additional columns of the digits.
threadsafe static Function CalculateMultiples(STRUCT Number &num)
	int64 i, layer, offset

	// check if multiples are generated
	if(DimSize(num.maxMultiple, 0) > 0)
		return NaN
	endif

	Make/FREE/L/N=(MULTI_DIM) num.maxMultiple

	// prepare values
	[STRUCT Number one] = copy(num)
	[STRUCT Number left] = init(num.scale, num.precision)
	[STRUCT Number target] = init(num.scale, num.precision)
	Redimension/N=(num.precision, MULTI_DIM * 100) num.digits // the maximum multiple is 100

	// calculate multiples
	for(i = 0; i < MULTI_DIM * 100; i += 1)
		[target] = Add(left, one)
		if(target.precision > one.precision + 1)
			break
		endif
		if (target.precision == one.precision + 1)
			layer = target.digits[0]
			offset = 1
		else
			layer = 0
			offset = 0
		endif
		num.digits[][i] = target.digits[p + offset]
		num.maxMultiple[layer] = i + 1
		[left] = copy(target)
	endfor

	// remove superfluous data
	Redimension/N=(num.precision, num.maxMultiple[MULTI_DIM - 1]) num.digits
End

threadsafe static Function [STRUCT Number result] IntToNum(int64 num)
	int64 i

	// maximum int64 is 9,223,372,036,854,775,807 which needs 10 blocks to store
	[result] = init(0, 10)

	// fix negative numbers
	if(num < 0)
		result.negative = 1
		num = -num
	endif

	// store to blocks
	for(i = 9; i >= 0 && num > 0; i -= 1)
		result.digits[i] = mod(num, 100)
		num /= 100
	endfor

	// skip leading zeros
	if(i > 0)
		Duplicate/FREE/RMD=[i + 1, *] result.digits, wvFixed
		Duplicate/FREE wvFixed, result.digits
		result.precision -= i + 1
	endif
End

// expects divisor>=1, there is a bug with smaller numbers
threadsafe static Function [STRUCT Number quotient] Division(STRUCT Number &dividend, STRUCT Number &divisor)
	int64 i, multiple, start, stop
	variable result

	CalculateMultiples(divisor)

	// initialize quotient. We use the same characteristics as the dividend
	[quotient] = init(dividend.scale, dividend.precision)

	// initialize buffer for comparison. We use the same precision as the divisor
	[STRUCT Number a] = init(0, divisor.precision)
	[STRUCT Number b] = init(0, divisor.precision)

	// initialize remainder
	[STRUCT Number remainder] = copy(dividend, multiple=0)

	// initialize buffer that is used to update the remainder
	[STRUCT Number leftRem] = init(remainder.scale, remainder.precision)
	[STRUCT Number rightRem] = init(remainder.scale, remainder.precision)
	rightRem.negative = 1

	// perform long division
	for(i = 0; i < dividend.precision; i += 1)
		// copy remainder
		a.digits[] = 0
		a.digits[0, min(remainder.precision - i, a.precision) - 1] = remainder.digits[p + i]
		// check each multiple. Check from smallest to largest, because it is faster to do so
		if(i > 0 && remainder.digits[i - 1] > 0)
			start = divisor.maxMultiple[remainder.digits[i - 1] - 1]
			stop = divisor.maxMultiple[remainder.digits[i - 1]]
		else
			start = 0
			stop = divisor.maxMultiple[0]
		endif
		for(multiple = start; multiple < stop; multiple += 1)
			b.digits[] = divisor.digits[p][multiple]
			result = Compare(a, b)
			if(result <= 0)
				if(result == 0)
					multiple += 1
				endif
				break
			endif
		endfor
		if(multiple > 0)
			b.digits[] = divisor.digits[p][multiple - 1]
			[leftRem] = copy(remainder)
			rightRem.digits[] = 0
			rightRem.digits[i, min(i + b.precision, rightRem.precision) - 1] = b.digits[p - i]
			[remainder] = Add(leftRem, rightRem)
		endif
		// set multiple to quotient
		quotient.digits[i] = multiple
	endfor

	// fix scale
	quotient.scale += divisor.precision - divisor.scale - 1
	if(quotient.scale >= quotient.precision)
		start = quotient.scale - quotient.precision + 1
		Duplicate/FREE quotient.digits, tmp
		quotient.precision = quotient.scale + 1
		Make/FREE/U/B/N=(quotient.precision) quotient.digits
		quotient.digits[start, quotient.precision - 1] = tmp[p - start]
	endif

	// fix negative
	quotient.negative = dividend.negative %^ divisor.negative
End

threadsafe static Function [STRUCT Number result] Multiply(STRUCT Number &a, STRUCT Number &b)
	int64 i, j, carry, pos, newScale

	int64 integerSizeA = a.precision - a.scale
	int64 integerSizeB = b.precision - b.scale

	// initialize result variable
	[result] = init(a.scale + b.scale, a.precision + b.precision)

	// perform long multiplication
	for(i = 0; i < b.precision; i += 1)
		carry = 0
		for(j = a.precision - 1; j >= 0; j -= 1)
			// calculate the position in the result number
			pos = 1 + i + j
			// multiply, add value to result and respect carry
			carry += result.digits[pos] + a.digits[j] * b.digits[i]
			result.digits[pos] = mod(carry, 100)
			carry /= 100
		endfor
		// add carry to result
		for(j = i; j >= 0 && carry > 0; j -= 1)
			carry += result.digits[j]
			result.digits[j] = mod(carry, 100)
			carry /= 100
		endfor
	endfor

	// fix scale + precision
	EnsureScale(result, max(a.scale, b.scale))
	FixLeadingZeros(result)

	// fix negative
	result.negative = a.negative %^ b.negative
End

threadsafe static Function EnsureScale(STRUCT Number &num, int64 scale)
	if(scale < num.scale)
		Duplicate/FREE/RMD=[0, num.precision - (num.scale - scale) - 1] num.digits, tmp
		Duplicate/FREE tmp, num.digits
		num.precision -= num.scale - scale
		num.scale = scale
	endif
	if(scale > num.scale)
		Duplicate/FREE num.digits, tmp
		Make/FREE/U/B/N=(num.precision + scale - num.scale) num.digits
		num.digits[0, num.precision - 1] = tmp[p]
		num.precision += scale - num.scale
		num.scale = scale
	endif
End

// calculates the arctan(1/x) with an x>1. The scale parameter defines the output precision
threadsafe static Function [STRUCT Number result] ArcTanInvX(STRUCT Number &x, int64 scale)
	int64 step
	int negate
	variable cmp

	// initialize intermediate variables
	[STRUCT Number zero] = init(scale, scale + 1)
	[STRUCT Number one] = IntToNum(1)
	[STRUCT Number a] = copy(zero)
	[STRUCT Number b] = copy(zero)

	// prepare 1/X²
	[a] = Multiply(x, x)
	EnsureScale(one, scale)
	[STRUCT Number invX2] = Division(one, a)

	// prepare 1/X as initial value
	[STRUCT Number current] = Division(one, x)

	// prepare result
	[result] = copy(current)

	// loop until there is no change
	negate = 1
	for(step = 3;; step += 2)
		// calculate new 1/X^step
		[a] = copy(current)
		[current] = Multiply(a, invX2)

		// generate {step}*negate
		[a] = IntToNum(step)
		a.negative = negate

		// calculate new 1/(step * X^step)*negate
		[b] = Division(current, a)
		EnsureScale(b, scale)

		// check if zero -> break
		if(Compare(b, zero) == 0)
			break
		endif

		// add value to result
		[a] = copy(result)
		[result] = Add(a, b)
		EnsureScale(result, scale)
		negate = !negate
	endfor
End

threadsafe static Function calculatePi_helper(int scale, int factorA, int factorB, WAVE/WAVE digits, WAVE/L meta, int index)

	[STRUCT Number a] = IntToNum(factorA)
	[STRUCT Number b] = IntToNum(factorB)
	[STRUCT Number c] = ArcTanInvX(b, scale)
	[b] = Multiply(a, c)

	digits[index] = b.digits
	meta[index][0] = b.negative
	meta[index][1] = b.scale
	meta[index][2] = b.precision
End

Function/S calculatePi(int scale)
	int i
	STRUCT Number a

	Make/FREE/L factors = { { 6348, 2852 }, { 1180, 4193 }, { 2372, 4246 }, { 1436, 39307 }, { 1924, 55603 }, { 2500, 211050 }, { -2832, 390112 } }
	Make/FREE/N=7 dummy
	Make/FREE/WAVE/N=7 digits
	Make/FREE/L/N=(7,3) meta

	MultiThread/NT=7 dummy = calculatePi_helper(scale, factors[0][p], factors[1][p], digits, meta, p)

	[STRUCT Number total] = init(scale, scale + 1)
	for(i = 0; i < 7; i += 1)
		WAVE/U/B wvDigits = digits[i]
		Duplicate/FREE wvDigits, a.digits
		a.negative = meta[i][0]
		a.scale = meta[i][1]
		a.precision = meta[i][2]

		[STRUCT Number b] = Add(total, a)
		[total] = copy(b)
	endfor

	return toString(total)
End

Function DoPi(int scale)
	variable start, stop
	string value

	start = StopMsTimer(-2)
	value = calculatePi(scale)
	stop = StopMsTimer(-2)

	printf "scale:  %d\n", scale
	printf "digits: %d\n", scale * 2
	printf "pi:     %s\n", value
	printf "took:   %gms\n", (stop - start) * 1e-3
End
