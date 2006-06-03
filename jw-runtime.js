//// jw-runtime.js
///
/// This is the jwacs runtime.  All of the variables and functions
/// defined in this file should be present whenever a jwacs-compiled
/// script is being run.

//TODO Wrap this all into a namespace

// The identity continuation.  We assume that a continuation will only
// ever be resumed or called from within a trampoline loop, which means
// that $id(10) should return 10 as a boxed result rather than just
// returning a raw value of 10.
function $id(x)
{
  return {done:true, result:x};
}
$id.$isK = true;

// Tail calls from the global scope should pass $id as the continuation.
var $k = $id;

// Returns function expression `fn` with its $jw property
// set to true.  This allows us to cps-convert function expressions
// while still treating them as "primitive" values.
function $lambda(fn)
{
  fn.$jw = true;
  return fn;
}

// Returns function expression `fn` with its $isK property
// set to true.  This allows us to detect continuations at
// runtime while still creating them in a single step (ie,
// without always having to assign them to an intermediate
// variable first)
function $makeK(fn)
{
  fn.$isK = true;
  return fn;
}

// Call function `f` in either CPS or direct style, depending upon
// the value of `f`'s  $jw property.  The continuation `k`
// will be called with the results regardless of the style that
// `f` is called in.  `thisObj` specifies the current `this`
// context, and `args` is an Array of arguments to pass to `f`.
function $call(f, k, thisObj, args)
{
  if(f.$jw)
    return f.apply(thisObj, [k].concat(args));
  else
    return k(f.apply(thisObj, args));
}

// Call a function in either CPS or direct style, depending upon
// the value of the function's $jw property.  The continuation `k`
// will be called with the results regardless of the style that the
// function is called in.
//
// `thisObj` specifies the current `this` context.  If `thisObj` is
// non-null, the function `thisObj[f]` will be called (ie, `f` should
// be a string representing a method name).  If `thisObj` is null, then
// `f` will be called (ie, `f` should be a function value).
//
// The arguments `a1` through `a8` will be passed as
// regular arguments.  This function only works for target functions
// of 8 arguments or fewer.  Indirect calls to functions of 9 or
// more arguments must use `$call` instead.
function $call0(f, k, thisObj, a1, a2, a3, a4, a5, a6, a7, a8)
{
  if(thisObj)
  {
    if(thisObj[f].$jw)
    {
      switch(arguments.length)
      {
      case 3:
        return thisObj[f](k);
      case 4:
        return thisObj[f](k, a1);
      case 5:
        return thisObj[f](k, a1, a2);
      case 6:
        return thisObj[f](k, a1, a2, a3);
      case 7:
        return thisObj[f](k, a1, a2, a3, a4);
      case 8:
        return thisObj[f](k, a1, a2, a3, a4, a5);
      case 9:
        return thisObj[f](k, a1, a2, a3, a4, a5, a6);
      case 10:
        return thisObj[f](k, a1, a2, a3, a4, a5, a6, a7);
      case 11:
        return thisObj[f](k, a1, a2, a3, a4, a5, a6, a7, a8);
      default:
        throw "too many/few arguments to $call0";
      }
    }
    else
    {
      switch(arguments.length)
      {
      case 3:
        return k(thisObj[f]());
      case 4:
        return k(thisObj[f](a1));
      case 5:
        return k(thisObj[f](a1, a2));
      case 6:
        return k(thisObj[f](a1, a2, a3));
      case 7:
        return k(thisObj[f](a1, a2, a3, a4));
      case 8:
        return k(thisObj[f](a1, a2, a3, a4, a5));
      case 9:
        return k(thisObj[f](a1, a2, a3, a4, a5, a6));
      case 10:
        return k(thisObj[f](a1, a2, a3, a4, a5, a6, a7));
      case 11:
        return k(thisObj[f](a1, a2, a3, a4, a5, a6, a7, a8));
      default:
        throw "too many/few arguments to $call0";
      }
    }
  }
  else
  {
    if(f.$jw)
    {
      switch(arguments.length)
      {
      case 3:
        return f(k);
      case 4:
        return f(k, a1);
      case 5:
        return f(k, a1, a2);
      case 6:
        return f(k, a1, a2, a3);
      case 7:
        return f(k, a1, a2, a3, a4);
      case 8:
        return f(k, a1, a2, a3, a4, a5);
      case 9:
        return f(k, a1, a2, a3, a4, a5, a6);
      case 10:
        return f(k, a1, a2, a3, a4, a5, a6, a7);
      case 11:
        return f(k, a1, a2, a3, a4, a5, a6, a7, a8);
      default:
        throw "too many/few arguments to $call0";
      }
    }
    else
    {
      switch(arguments.length)
      {
      case 3:
        return k(f());
      case 4:
        return k(f(a1));
      case 5:
        return k(f(a1, a2));
      case 6:
        return k(f(a1, a2, a3));
      case 7:
        return k(f(a1, a2, a3, a4));
      case 8:
        return k(f(a1, a2, a3, a4, a5));
      case 9:
        return k(f(a1, a2, a3, a4, a5, a6));
      case 10:
        return k(f(a1, a2, a3, a4, a5, a6, a7));
      case 11:
        return k(f(a1, a2, a3, a4, a5, a6, a7, a8));
      default:
        throw "too many/few arguments to $call0";
      }
    }
  }
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

// Start a trampoline loop that calls `f` on the arguments in
// `args` with `this` set to `thisObj`.
function $callFromDirect(f, thisObj, args)
{
  var argArray = new Array;
  for(var idx = 0; idx < args.length; idx++)
    argArray[idx] = args[idx];
  return $trampoline(function() {
                       return f.apply(thisObj, [$id].concat(argArray));
                     });
}
