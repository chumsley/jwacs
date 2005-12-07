//// tramp-perf.js
///
/// This file contains the factorial function manually transformed into various
/// call-styles.  I used it to gauge the depth of various Javascript implementations'
/// call stacks empirically, and to do some timings on various different trampoline
/// implementations.

////--------------------------------------------------------------------------------
//// Utility functions
function say(str)
{
  WScript.echo(str);
// 	print(str);
}

function now()
{
	return (new Date).getTime();
}

function id(x)
{
	return x;
}

function trampolineId(x)
{
	var ret = new Object;
	ret.done = true;
	ret.result = x;
	return ret;
}

function trampoline(origThunk)
{
	var ret = new Object;
	ret.done = false;
	ret.thunk = origThunk;
	while(!ret.done)
	{
		ret = ret.thunk();
	}
	return ret.result;
}

function count(n, max)
{
	var ret = new Object;
	if(n == max)
	{
		ret.done = true;
		ret.result = n;
	}
	else
	{
		say(n);
		ret.done = false;
		ret.thunk = function() { return count(n+1, max); };
	}

	return ret;
}

////--------------------------------------------------------------------------------
//// Factorial implementations

/// The naive factorial function
function factorial(n)
{
	if(n==0)
		return 1;
	else
		return n * factorial(n-1);
}

/// Accumulator-passing version of factorial
function accFactorial(n, acc)
{
	if(n == 0)
		return acc;
	else
		return accFactorial(n-1, n*acc);
}

/// The naive factorial transformed into CPS form
function cpsFactorial(n, k)
{
	if(n==0)
		return k(1);
	else
	{
		return cpsFactorial(n-1, function(intermediate) {
			return k(n * intermediate);
		});
	}
}

/// The CPS form modified to use "first-order" trampolined function calls
/// (ie, original function calls modified to be trampolined calls, continuation
/// calls not modified)
function trampolineFactorial1(n, k)
{
	var ret = new Object;
	if(n == 0)
	{
		ret.done = true;
		ret.result = k(1);
	}
	else
	{
		var k2 = function(intermediate) {
			return k(n * intermediate);
		};

		ret.done = false;
		ret.thunk = function() {
			return trampolineFactorial1(n-1, k2);
		}
	}
	
	return ret;
}

/// CPS form modified to use "second-order" trampolined function calls
/// (ie, original function calls and continuation calls both modified
/// to be trampolined calls)
function trampolineFactorial2(n, k)
{
	var ret = new Object;
	if(n == 0)
	{
		ret.done = false;
		ret.thunk = function() {
			return k(1);
		}
	}
	else
	{
		var k2 = function(intermediate) {
			var ret = new Object;
			ret.done = false;
			ret.thunk = function() {
				return k(n * intermediate);
			};
			
			return ret;
		};

		ret.done = false;
		ret.thunk = function() {
			return trampolineFactorial2(n-1, k2);
		}
	}
	
	return ret;
}

/// A version of trampolineFactorial2 that reuses a single result 
/// object instead of consing a new one each time.
var globalRet = new Object;
function trampolineFactorial3(n, k)
{
	if(n == 0)
	{
		globalRet.done = false;
		globalRet.thunk = function() {
			return k(1);
		}
	}
	else
	{
		var k2 = function(intermediate) {
			globalRet.done = false;
			globalRet.thunk = function() {
				return k(n * intermediate);
			};
			
			return globalRet;
		};

		globalRet.done = false;
		globalRet.thunk = function() {
			return trampolineFactorial3(n-1, k2);
		}
	}
	
	return globalRet;
}

////--------------------------------------------------------------------------------
//// Demonstration calls

// Under WScript, naive factorial blows the stack at i = 469
// Under Firefox, naive factorial blows the stack at i = 1000
for(var i = 0; i < 469; i++)
{
	var nbang = factorial(i);
	if(i % 100 == 0)
		say("factorial("+ i +") = "+nbang);
}

// Under WScript, accFactorial blows the stack at i = 469 
// Under Firefox, accFactorial blows the stack at i = 1000 
// (ie, tail-recursion does not buy any additional stack efficiency)
for(var i = 0; i < 469; i++)
{
	var nbang = accFactorial(i, 1);
	if(i % 100 == 0)
		say("accFactorial(" + i + ") = " + nbang);
}

// Under WScript, cpsFactorial blows the stack at i = 234
// Under Firefox, cpsFactorial blows the stack at i = 996
// (ie, CPS transformation costs an extra stack frame per recursion under
// WScript but not (mysteriously) under Firefox)
for(var i = 0; i < 234; i++)
{
	var nbang = cpsFactorial(i, id);
	if(i % 100 == 0)
		say("cpsFactorial(" + i + ") = "+nbang);
}

// Under WScript, trampolineFactorial1 blows the stack at i = 466
// Under Firefox, trampolineFactorial1 blows the stack at i = 1000
// (ie, the maximum depth of continuation calls is still stack-limited)
for(var i = 0; i < 466; i++)
{
	var nbang = trampoline(function() { return trampolineFactorial1(i, id); });
	if(i % 100 == 0)
		say("trampolineFactorial1(" + i + ") = " + nbang);
}

// Under WScript, trampolineFactorial2 is limited only by your patience and
// the size of the heap.
// Under Firefox, trampolineFactorial2 causes Firefox to crash at i = 25000
for(var i = 0; i < 2001; i+= 200)
{
	var nbang = trampoline(function() { return trampolineFactorial2(i, trampolineId); });
	if(i % 100 == 0)
		say("trampolineFactorial2(" + i + ") = " + nbang);
}

var s, e, nbang;
var sz = 18000;

// Under WScript:
//     trampoline2(20000) = Infinity in 12922 msec
//     trampoline3(20000) = Infinity in 4953 msec
// Under Firefox:
//     trampoline2(20000) = Infinity in 1063 msec
//     trampoline3(18000) = Infinity in 985 msec (crashes at 19000 and above)
// (ie, reusing the result object cuts the time required by more than half under
// WScript, but it makes no difference under Firefox (except to make it a little
// less stable)
var s = now();
var nbang = trampoline(function() { return trampolineFactorial2(sz, trampolineId); });
var e = now();
say("trampoline2("+sz+") = " + nbang + " in " + (e-s) + " msec");

s = now();
nbang = trampoline(function() { return trampolineFactorial3(sz, trampolineId); });
e = now();
say("trampoline3("+sz+") = " + nbang + " in " + (e-s) + " msec");
