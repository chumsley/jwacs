//// jw-rt.js
///
/// This is the JWACS runtime.  All of the variables and functions
/// defined in this file should be present whenever a JWACS-compiled
/// script is being run.

//TODO Wrap this all into a namespace

/// The identity function
function $id(x)
{
	return x;
}

// Tail calls from the global scope should pass $id as the continuation.
var $k = $id;

// Call function `f` in either CPS or direct style, depending upon
// the value of `f`'s  $callStyle property.  The continuation `k`
// will be called with the results regardless of the style that
// `f` is called in.  `thisObj` specifies the current `this`
// context, and `args` is an Array of arguments to pass to `f`.
function $call(f, k, thisObj, args)
{
  if(f.$callStyle == 'cps')
    return f.apply(thisObj, [k].concat(args));
  else
    return k(f.apply(thisObj, args));
}

// Returns function expression `fn` with its $callStyle property
// set to "cps".  This allows us to cps-convert function expressions
// while still treating them as "primitive" values.
function $cpsLambda(fn)
{
  fn.$callStyle = 'cps';
  return fn;
}

// The identity function for trampoline-style functions.  Returns
// a return object whose `result` field is set to `x`.
function $trampolineId(x)
{
  return {done:true, result:x};
}

// "Pogo-stick" function for running a call to a trampoline-style
// function.
function $trampoline(origThunk)
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

// Returns a trampoline result object that is appropriate for
// a function call that applies `f` to `args` in the current `this`
// context specified by `thisObj`.
//
// If `f` has an `$isTrampolined` field that is set to true, then
// this function returns a thunk result containing a call to `f`.
// Otherwise, it calls `f` and wraps the result in a trampoline
// result object.
function $trampolineResult(f, thisObj, args)
{
  if(f.$isTrampolined)
    return { done: false,
             thunk: function() { return f.apply(thisObj, args); }};
  else
    return { done: true, 
             result: f.apply(thisObj, args) };
}
