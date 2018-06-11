var deepDiff = require('deep-diff');

//console.log("Hello From Node!")
//console.log("Args are: ", process.argv);

//console.log("Catting input lines ... ")
//var fs = require('fs');
//var js = fs.readFileSync(0).toString();
// console.log("Compiling test js to function(dep0, dep1, dep2, dep3, dep4, dep5)", js);
// var test_fn = new Function([
//     "dep0", "dep1", "dep2", "dep3", "dep4", "dep5"
// ], js);

function pprint(data) {
    return JSON.stringify(data, null, 2);
}

function simple_effect(label, ret_fn) {
    var ret = function (trace_array) {
        return function () {
            var args = Array.prototype.slice.call(arguments);
            args.unshift(label);
            trace_array.push(args);
            return ret_fn ? ret_fn.apply(null, arguments) : args;
        }
    };
    ret.label = label;
    ret.effect_id = "simple_tracing";
    return ret;
}

function wrapEffects(fn_unwrapped, effects) {
    return (function (data, trace_array, on_finish) {
        var effect_instances = effects.map(function(eff) {
            return eff(trace_array);
        });
        effect_instances.unshift(on_finish)
        var fn_with_effects = fn_unwrapped.apply(null, effect_instances);
        //console.log("APPLYING EFFECTS", fn_unwrapped, effect_instances, fn_with_effects)
        fn_with_effects.apply(null, data);
    });
}

function doRun(fn, data, on_finish) {
    var trace = [];
    return fn(data, trace, function (ret) {
        on_finish({result: ret,
                   effect_trace: trace});
    });
}

function testHarness(fn_original, fn_transformed, effects_original, effects_transformed, on_finish) {
    var orig = wrapEffects(fn_original, effects_original);
    var tran = wrapEffects(fn_transformed, effects_transformed);
    return (function test() {
        var data = Array.prototype.slice.call(arguments);
        // console.log("STARTING RUN", data);
        doRun(orig, data, function(orig_res) {
            // console.log("FINISHED ORIG", orig_res);
            doRun(tran, data, function(tran_res) {
                // console.log("FINISHED TRAN", tran_res);
                on_finish({
                    input_data: data,
                    original_result: orig_res,
                    transformed_result: tran_res
                });
            });
            
        });
    });
}

var default_effects = [
    simple_effect("f"),
    simple_effect("g"),
    simple_effect("h"),
    simple_effect("i"),
    simple_effect("j"),
    simple_effect("k"),
    simple_effect("l"),
    simple_effect("m")
];

var n_effects = ["f","g","h","i","j","k","l","m"];
function runTests ({ fn_original, fn_transformed, effect_factory_original, effect_factory_transformed, data }) {
    var harny = testHarness(
        fn_original,
        fn_transformed,
        n_effects.map(effect_factory_original),
        n_effects.map(effect_factory_transformed),
        function(result){
            var rdiff = deepDiff.diff(result.original_result.result, result.transformed_result.result) || [];
            var etdiff = deepDiff.diff(result.original_result.effect_trace, result.transformed_result.effect_trace) || [];
            if (rdiff.length > 0) {
                console.log("ERROR", "result differs", pprint({
                    original_result: result.original_result.result,
                    transformed_result: result.transformed_result.result,
                    input_data: result.input_data
                }));
            } else if (etdiff.length > 0) {
                console.log("WARNING", "effect trace differs", pprint({
                    input_data: result.input_data,
                    result: result.original_result.result
                }));
            } else {
                console.log ("INFO", JSON.stringify(result.input_data),
                             result.original_result.result,
                             rdiff, " ... OK");
                var et = [];
                // for(var i=0;i<100;i++){
                //     et.push(result.original_result.effect_trace[i]);
                // }
                // console.log ("INFO", "100 elements of effect trace", pprint(et));
            }
            if (etdiff.length > 0) {
                var et = [];
                for(var i=0;i<100;i++){
                    et.push([result.original_result.effect_trace[i], result.transformed_result.effect_trace[i]]);
                }
                console.log ("INFO", "100 elements of effect trace", pprint(et));
            }
        }
    );
    for (let args of data) {
        harny.apply(null, args);
    }
}

module.exports = {
    testHarness: testHarness,
    default_effects: default_effects,
    pprint: pprint,
    runTests: runTests
};
