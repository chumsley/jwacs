//// jw-rt.js
///
/// This is the JWACS runtime.  All of the variables and functions
/// defined in this file should be present whenever a JWACS-compiled
/// script is being run.

//TODO Should we wrap this all into a namespace?

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
