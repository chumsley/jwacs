//// jw-debug-runtime.js
///
/// Replaces certain functions in the jwacs runtime to support debugging.

// The identity continuation.  We assume that a continuation will only
// ever be resumed or called from within a trampoline loop, which means
// that $id(10) should return 10 as a boxed result rather than just
// returning a raw value of 10.
function $id(x)
{
  return {done:true, result:x};
}
$id.$isK = true;
$id.$exHandlers = null;

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
// variable first).  The $exHandlers property is set to the
// current exception handler stack, so that the exception
// state can be resumed when the continuation is resumed.
function $makeK(fn, handlerStack)
{
  fn.$isK = true;
  fn.$exHandlers = handlerStack;
  return fn;
}

// Make a copy of the arguments object `origArgs` that
// doesn't include the continuation argument (if any)
// as one of the numbered arguments.  The continuation
// is still available as the 'continuation' property.
//
// The constructed arguments object is different from
// the original in these ways:
//   1) Doesn't include continuation in args list (good)
//   2) All properties are enumerable (indifferent)
//   3) Changing a parameter value doesn't change its
//      corresponding property, nor vice versa (bad)
function $makeArguments(origArgs)
{
  var newArgs = new Object;
  newArgs.callee = origArgs.callee;
  newArgs.caller = origArgs.caller;

  if(origArgs[0] && origArgs[0].$isK)
  {
    newArgs.continuation = origArgs[0];
    newArgs.length = origArgs.length - 1;
    for(var i = 1; i < origArgs.length; i++)
      newArgs[i - 1] = origArgs[i];
  }
  else
  {
    newArgs.length = origArgs.length;
    for(var i = 0; i < origArgs.length; i++)
      newArgs[i] = origArgs[i];
  }

  return newArgs;
}

// If `thisObj` is provided, then returns true if its `f` method is a transformed
// function.  If `thisObj` is not provided, then returns true if `f` is a
// transformed function.  We need this function because the very act of attempting
// to access a non-existent field on a native object can cause IE to throw (this
// can also happen when attempting to access a field on a native object that
// contains only a method of the same name), so we want to have a place where we
// can put a try-catch statement for catching that error.
function $isTransformed(f, thisObj)
{
  try
  {
    if(thisObj)
      return thisObj[f].$jw;
    return f.$jw;
  }
  catch(e)
  {
    return false;
  }
}

// Call a function in either CPS or direct style, depending upon
// whether the function is CPS-transformed.  The continuation `k`
// will be called with the results regardless of the style that the
// function is called in. `thisObj` specifies the current `this`
// context, and `args` is an Array of arguments to pass to `f`.
function $call(f, k, thisObj, args)
{
  if($isTransformed(f))
    return f.apply(thisObj, [k].concat(args));
  else
    return k(f.apply(thisObj, args));
}

// Call a function in either CPS or direct style, depending upon
// whether the function is CPS-transformed.  The continuation `k`
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
  var r;
  if(thisObj)
  {
    if($isTransformed(f, thisObj))
    {
      try
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
      catch(e)
      {
        // Make errors say more than just "in $call0"
        throw new Error("during $call0 of " + thisObj + "." + f + ": " + getExceptionDesc(e));
      }
    }
    else
    {
      try
      {
        switch(arguments.length)
        {
        case 3:
          r = thisObj[f]();
          break;
        case 4:
          r = thisObj[f](a1);
          break;
        case 5:
          r = thisObj[f](a1, a2);
          break;
        case 6:
          r = thisObj[f](a1, a2, a3);
          break;
        case 7:
          r = thisObj[f](a1, a2, a3, a4);
          break;
        case 8:
          r = thisObj[f](a1, a2, a3, a4, a5);
          break;
        case 9:
          r = thisObj[f](a1, a2, a3, a4, a5, a6);
          break;
        case 10:
          r = thisObj[f](a1, a2, a3, a4, a5, a6, a7);
          break;
        case 11:
          r = thisObj[f](a1, a2, a3, a4, a5, a6, a7, a8);
          break;
        default:
          throw "too many/few arguments to $call0";
        }
      }
      catch(e)
      {
        // Make errors say more than just "in $call0"
        throw new Error("during $call0 of " + thisObj + "." + f + ": " + getExceptionDesc(e));
      }
      
      return k(r);
    }
  }
  else
  {
    if($isTransformed(f))
    {
      try
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
      catch(e)
      {
        // Make errors say more than just "in $call0"
        throw new Error("during $call0 of " + f + ": " + getExceptionDesc(e));
      }
    }
    else
    {
      try
      {
        switch(arguments.length)
        {
        case 3:
          r = f();
          break;
        case 4:
          r = f(a1);
          break;
        case 5:
          r = f(a1, a2);
          break;
        case 6:
          r = f(a1, a2, a3);
          break;
        case 7:
          r = f(a1, a2, a3, a4);
          break;
        case 8:
          r = f(a1, a2, a3, a4, a5);
          break;
        case 9:
          r = f(a1, a2, a3, a4, a5, a6);
          break;
        case 10:
          r = f(a1, a2, a3, a4, a5, a6, a7);
          break;
        case 11:
          r = f(a1, a2, a3, a4, a5, a6, a7, a8);
          break;
        default:
          throw "too many/few arguments to $call0";
        }
      }
      catch(e)
      {
        // Make errors say more than just "in $call0"
        throw new Error("during $call0 of " + f + ": " + getExceptionDesc(e));
      }
      return k(r);
    }
  }
}

// Convert an exception to a printable value
function getExceptionDesc(e)
{
  try
  {
    if(e.description)
      return e.description;
    else
      return e;
  }
  catch(x)
  {
    return e;
  }
}

// Helper function for creating a "blank" constructor for creating objects
// that will have the same prototype as objects created by the specified
// constructor.  We use a separate high-level function to avoid unnecessarily
// capturing the environment in $new/$new0.
function $makeBlank(ctor)
{
  var blank = function() {};
  blank.prototype = ctor.prototype;
  return blank;
}
    
// Used for replacing expressions of the form `new ctor(arg1, arg2, ...)` with
// an explicit function call.  An object is constructed using the specified
// constructor `ctor`, which is passed the arguments of the array `args`.
// The continuation `k` will be called with the result.
//
// The constructor will be called in either direct style or cps style, depending
// upon whether or not `ctor` is a cps-transformed function.  The continuation `k`
// is called with the result of the construction regardless of the calling style.
function $new(ctor, k, args)
{
  if($isTransformed(ctor))
  {
    if(!ctor.$blank)
      ctor.$blank = $makeBlank(ctor);

    var obj = new ctor.$blank;
    var augmentedK = $makeK(function(x) {
      if(x)
        return k(x);
      else
        return k(obj);
    });
    
    return ctor.apply(obj, [augmentedK].concat(args));
  }
  else
  {
    var privateCtor = function ()
    {
      return ctor.apply(this, args);
    };
    privateCtor.prototype = ctor.prototype;

    return k(new privateCtor);
  }
}

// Used for replacing expressions of the form `new ctor(arg1, arg2, ...)` with
// an explicit function call for constructors that accept 8 arguments or fewer.
//
// An object is constructed using the specified constructor `ctor`, which is
// passed the arguments of the array `args`.  The continuation `k` will be called
// with the result.
//
// The constructor will be called in either direct style or cps style, depending
// upon whether or not `ctor` is a cps-transformed function.  The continuation `k`
// is called with the result of the construction regardless of the calling style.
//
// This function can only be called on constructors that have 8 arguments or fewer.
// For constructors with 9 arguments or more, `$new` must be used.
function $new0(ctor, k, a1, a2, a3, a4, a5, a6, a7, a8)
{
  if($isTransformed(ctor))
  {
    if(!ctor.$blank)
      ctor.$blank = $makeBlank(ctor);
      
    var obj = new ctor.$blank;
    obj.$init = ctor;
    var augmentedK = $makeK(function(x) {
      delete obj.$init;
      if(x)
        return k(x);
      else
        return k(obj);
    });
    
    switch(arguments.length)
    {
    case 2:
      return obj.$init(augmentedK);
      break;
    case 3:
      return obj.$init(augmentedK, a1);
      break;
    case 4:
      return obj.$init(augmentedK, a1, a2);
      break;
    case 5:
      return obj.$init(augmentedK, a1, a2, a3);
      break;
    case 6:
      return obj.$init(augmentedK, a1, a2, a3, a4);
      break;
    case 7:
      return obj.$init(augmentedK, a1, a2, a3, a4, a5);
      break;
    case 8:
      return obj.$init(augmentedK, a1, a2, a3, a4, a5, a6);
      break;
    case 9:
      return obj.$init(augmentedK, a1, a2, a3, a4, a5, a6, a7);
      break;
    case 10:
      return obj.$init(augmentedK, a1, a2, a3, a4, a5, a6, a7, a8);
      break;
    default:
      throw "too many/few arguments to $new0";
    }
  }
  else
  {
    switch(arguments.length)
    {
    case 2:
      return k(new ctor());
      break;
    case 3:
      return k(new ctor(a1));
      break;
    case 4:
      return k(new ctor(a1, a2));
      break;
    case 5:
      return k(new ctor(a1, a2, a3));
      break;
    case 6:
      return k(new ctor(a1, a2, a3, a4));
      break;
    case 7:
      return k(new ctor(a1, a2, a3, a4, a5));
      break;
    case 8:
      return k(new ctor(a1, a2, a3, a4, a5, a6));
      break;
    case 9:
      return k(new ctor(a1, a2, a3, a4, a5, a6, a7));
      break;
    case 10:
      return k(new ctor(a1, a2, a3, a4, a5, a6, a7, a8));
      break;
    default:
      throw "too many/few arguments to $new0";
    }
  }
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

// Constructor for a stack entry on the global exception handler stack
function HandlerStackEntry(k, next)
{
  this.k = k;
  this.next = next;
}

function $trampoline(origThunk, origStack)
{
  var handlerStack = origStack ? origStack : null; // The handler stack for this thread
  var ret;

  if(typeof origThunk == 'function')
    ret = {done: false, thunk: origThunk};
  else if(typeof origThunk == 'object')
    ret = origThunk;
  else
    throw "$trampoline: origThunk is neither a function nor an object";
  var latestResult = null;
  
  function popHandler(expected)
  {
    var top = handlerStack;

    if(expected && (!top || top.k != expected))
      alert("assertion failure: expected handler " + expected + ", got " + top);

    if(top)
    {
      handlerStack = top.next;
      return top.k;
    }
    
    return null;
  }
  
  while(!ret.done)
	{
    // Perform handler management
    if(ret.addHandler)
      handlerStack = new HandlerStackEntry(ret.addHandler, handlerStack);
    else if(ret.removeHandler)
      popHandler(ret.removeHandler);
    else if(ret.replaceHandlers)
      handlerStack = ret.replaceHandlers;
    
    // Do the work
    try
    {
      latestResult = ret;
      ret = ret.thunk(handlerStack);
    }
    catch(e)
    {
      var latestStack = handlerStack;
      var handler = popHandler();
      if($exHook)
      {
        var currentHook = $exHook;
        $exHook = null;
        var hookResult = currentHook(e, handler, handlerStack, latestStack, latestResult);
        $exHook = currentHook;
        return hookResult;
      }

      if(handler)
        ret = {done: false, thunk: function() { return handler(e); } };
      else
        throw e;
    }
  }
	return ret.result;
}

// Evaluates `$s` in a close-to-global environment
function $globalEval($s)
{
  eval($s);
}

// Evaluates '$s' in a local environment defined by `thunk` and `handlerStack`
function $localEval(thunk, handlerStack, $s)
{
  var ret = thunk(handlerStack, $s);
  return ret.result;
}

function $invokeHandler(handler, e)
{
  return $trampoline(function() { return handler(e); });
}

var $exHook;

