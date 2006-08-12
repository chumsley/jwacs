var JwacsLib;
$trampoline(function($e)
{
  return {done: false, thunk: function($e)
  {
    return $new0(Object, $makeK(function(JW77)
    {
      return {done: false, thunk: function($e)
      {
        return $new0(Object, $makeK(function(JW78)
        {
          return {done: false, thunk: function($e)
          {
            return $new0(Object, $makeK(function(JW79)
            {
              JwacsLib = {sleep: $lambda(function JW0($k, msec$1)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW0, this, arguments);
                var k$2 = $k;
                return {done: false, thunk: function($e)
                {
                  return $call0(setTimeout, $makeK(function()
                  {
                    return {done: true};
                  }, $e), null, $lambda(function JW3($k)
                  {
                    if(!$k || !$k.$isK)
                      return $callFromDirect(JW3, this, arguments);
                    return {replaceHandlers: k$2.$exHandlers, done: false, thunk: function($e)
                    {
                      return k$2();
                    }};
                  }), msec$1);
                }};
              }), yieldThread: $lambda(function JW4($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW4, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("sleep", $makeK(function()
                  {
                    return {done: false, thunk: function($e)
                    {
                      return $k();
                    }};
                  }, $e), JwacsLib, 0);
                }};
              }), pageThunks: JW77, maxHistoryEntries: 50, generatedHashes: JW78, nextToken: 0, currentHash: JW79, initHistory: $lambda(function JW5($k, maxHistoryEntries$6)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW5, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0(isNaN, $makeK(function(JW80)
                  {
                    var ifK$108 = $makeK(function()
                    {
                      return {done: false, thunk: function($e)
                      {
                        return $call0("isInternetExplorer", $makeK(function(JW81)
                        {
                          var ifK$109 = $makeK(function()
                          {
                            var startTimerThunk$7 = $lambda(function JW8($k)
                            {
                              if(!$k || !$k.$isK)
                                return $callFromDirect(JW8, this, arguments);
                              return {done: false, thunk: function($e)
                              {
                                return $call0(setInterval, $makeK(function()
                                {
                                  return {done: false, thunk: function($e)
                                  {
                                    return $k();
                                  }};
                                }, $e), null, JwacsLib.checkLocation, 200);
                              }};
                            });
                            var ifK$110 = $makeK(function()
                            {
                              return {done: false, thunk: function($e)
                              {
                                return $k();
                              }};
                            }, $e);
                            if(window.addEventListener)
                              return {done: false, thunk: function($e)
                              {
                                return $call0("addEventListener", $makeK(function()
                                {
                                  return {replaceHandlers: ifK$110.$exHandlers, done: false, thunk: function($e)
                                  {
                                    return ifK$110();
                                  }};
                                }, $e), window, "load", startTimerThunk$7, false);
                              }};
                            else
                            {
                              var ifK$111 = $makeK(function()
                              {
                                return {replaceHandlers: ifK$110.$exHandlers, done: false, thunk: function($e)
                                {
                                  return ifK$110();
                                }};
                              }, $e);
                              if(window.attachEvent)
                                return {done: false, thunk: function($e)
                                {
                                  return $call0("attachEvent", $makeK(function()
                                  {
                                    return {replaceHandlers: ifK$111.$exHandlers, done: false, thunk: function($e)
                                    {
                                      return ifK$111();
                                    }};
                                  }, $e), window, "onload", startTimerThunk$7, false);
                                }};
                              else
                                return {replaceHandlers: ifK$111.$exHandlers, done: false, thunk: function($e)
                                {
                                  return ifK$111();
                                }};
                            }
                          }, $e);
                          if(JW81)
                            return {done: false, thunk: function($e)
                            {
                              return $call0("write", $makeK(function()
                              {
                                return {replaceHandlers: ifK$109.$exHandlers, done: false, thunk: function($e)
                                {
                                  return ifK$109();
                                }};
                              }, $e), document, "<iframe style='position:absolute;top:-10000;left:-10000;width:1px;height:1px;'" + "id='HistoryIFrame' src='blank.html?0'></iframe>");
                            }};
                          else
                            return {replaceHandlers: ifK$109.$exHandlers, done: false, thunk: function($e)
                            {
                              return ifK$109();
                            }};
                        }, $e), JwacsLib);
                      }};
                    }, $e);
                    if(!JW80)
                    {
                      JwacsLib.maxHistoryEntries = maxHistoryEntries$6;
                      return {replaceHandlers: ifK$108.$exHandlers, done: false, thunk: function($e)
                      {
                        return ifK$108();
                      }};
                    }
                    else
                      return {replaceHandlers: ifK$108.$exHandlers, done: false, thunk: function($e)
                      {
                        return ifK$108();
                      }};
                  }, $e), null, maxHistoryEntries$6);
                }};
              }), newPage: $lambda(function JW9($k, title$10, hint$11)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW9, this, arguments);
                function propertiesToString$16($k, obj$17)
                {
                  if(!$k || !$k.$isK)
                    return $callFromDirect(propertiesToString$16, this, arguments);
                  var nextChar$18 = "";
                  var result$19 = "";
                  
                  {
                    var JW69 = [];
                    var JW70 = 0;
                    var JW72 = 0;
                    for(var JW71 in obj$17)
                      JW69[JW70++] = JW71;
                    var break$112 = $makeK(function()
                    {
                      return {done: false, thunk: function($e)
                      {
                        return $k(result$19);
                      }};
                    }, $k.$exHandlers);
                    var continue$113 = $makeK(function()
                    {
                      if(!(JW72 < JW69.length))
                        return {replaceHandlers: break$112.$exHandlers, done: false, thunk: function($e)
                        {
                          return break$112();
                        }};
                      field = JW69[JW72++];
                      if(typeof obj$17[field] == "function")
                        return {replaceHandlers: continue$113.$exHandlers, done: false, thunk: function($e)
                        {
                          return continue$113();
                        }};
                      return {done: false, thunk: function($e)
                      {
                        return $call0(escape, $makeK(function(JW82)
                        {
                          return {done: false, thunk: function($e)
                          {
                            return $call0(escape, $makeK(function(JW83)
                            {
                              result$19 += nextChar$18 + JW82 + "=" + JW83;
                              nextChar$18 = "&";
                              return {replaceHandlers: continue$113.$exHandlers, done: false, thunk: function($e)
                              {
                                return continue$113();
                              }};
                            }, $e), null, obj$17[field]);
                          }};
                        }, $e), null, field);
                      }};
                    }, $k.$exHandlers);
                    return {replaceHandlers: continue$113.$exHandlers, done: false, thunk: function($e)
                    {
                      return continue$113();
                    }};
                  }
                }
                propertiesToString$16.$jw = true;
                return {done: false, thunk: function($e)
                {
                  return $call0("prunePageThunks", $makeK(function()
                  {
                    var token$12 = JwacsLib.nextToken++;
                    var k$13 = $k;
                    return {done: false, thunk: function($e)
                    {
                      return propertiesToString$16($makeK(function(JW84)
                      {
                        var hash$14 = JW84 + "|" + token$12;
                        JwacsLib.generatedHashes[hash$14] = token$12;
                        JwacsLib.pageThunks[hash$14] = $lambda(function JW20($k)
                        {
                          if(!$k || !$k.$isK)
                            return $callFromDirect(JW20, this, arguments);
                          document.title = title$10;
                          return {replaceHandlers: k$13.$exHandlers, done: false, thunk: function($e)
                          {
                            return k$13();
                          }};
                        });
                        return {done: false, thunk: function($e)
                        {
                          return $call0("isInternetExplorer", $makeK(function(JW85)
                          {
                            var ifK$114 = $makeK(function()
                            {
                              return {done: true};
                            }, $e);
                            if(JW85)
                              return {done: false, thunk: function($e)
                              {
                                return $call0("getElementById", $makeK(function(iframe$15)
                                {
                                  return {done: false, thunk: function($e)
                                  {
                                    return $call0(escape, $makeK(function(JW86)
                                    {
                                      iframe$15.src = "blank.html?" + JW86;
                                      return {replaceHandlers: ifK$114.$exHandlers, done: false, thunk: function($e)
                                      {
                                        return ifK$114();
                                      }};
                                    }, $e), null, hash$14);
                                  }};
                                }, $e), document, "HistoryIFrame");
                              }};
                            else
                            {
                              window.location.hash = "#" + hash$14;
                              return {replaceHandlers: ifK$114.$exHandlers, done: false, thunk: function($e)
                              {
                                return ifK$114();
                              }};
                            }
                          }, $e), JwacsLib);
                        }};
                      }, $e), hint$11);
                    }};
                  }, $e), JwacsLib);
                }};
              }), stripChar: $lambda(function JW21($k, str$22, ch$23)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW21, this, arguments);
                var JW87;
                var ifK$115 = $makeK(function()
                {
                  if(str$22 && JW87 == ch$23)
                    return {done: false, thunk: function($e)
                    {
                      return $call0("substr", $k, str$22, 1);
                    }};
                  return {done: false, thunk: function($e)
                  {
                    return $k(str$22);
                  }};
                }, $k.$exHandlers);
                if(str$22)
                  return {done: false, thunk: function($e)
                  {
                    return $call0("charAt", $makeK(function(JW116)
                    {
                      JW87 = JW116;
                      return {replaceHandlers: ifK$115.$exHandlers, done: false, thunk: function($e)
                      {
                        return ifK$115();
                      }};
                    }, $e), str$22, 0);
                  }};
                else
                  return {replaceHandlers: ifK$115.$exHandlers, done: false, thunk: function($e)
                  {
                    return ifK$115();
                  }};
              }), stripHash: $lambda(function JW24($k, str$25)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW24, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("stripChar", $k, JwacsLib, str$25, "#");
                }};
              }), stripToken: $lambda(function JW26($k, str$27)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW26, this, arguments);
                if(!str$27 || typeof str$27 != "string")
                  return {done: false, thunk: function($e)
                  {
                    return $k(str$27);
                  }};
                return {done: false, thunk: function($e)
                {
                  return $call0("replace", $k, str$27, /\|\d+$/, "");
                }};
              }), getToken: $lambda(function JW28($k, str$29)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW28, this, arguments);
                if(!str$29 || typeof str$29 != "string")
                  return {done: false, thunk: function($e)
                  {
                    return $k(0);
                  }};
                return {done: false, thunk: function($e)
                {
                  return $call0("match", $makeK(function(aMatch$30)
                  {
                    if(aMatch$30)
                      return {done: false, thunk: function($e)
                      {
                        return $new0(Number, $k, aMatch$30[1]);
                      }};
                    else
                      return {done: false, thunk: function($e)
                      {
                        return $k(0);
                      }};
                  }, $e), str$29, /\|(\d+)$/);
                }};
              }), parsePageArgs: $lambda(function JW31($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW31, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("stripChar", $makeK(function(hash$32)
                  {
                    var ifK$117 = $makeK(function()
                    {
                      return {done: false, thunk: function($e)
                      {
                        return $call0("stripChar", $makeK(function(query$33)
                        {
                          var ifK$118 = $makeK(function()
                          {
                            var result$34 = {};
                            var i$35;
                            
                            {
                              var factor$37;
                              i$35 = 0;
                              var break$119 = $makeK(function()
                              {
                                
                                {
                                  var factor$37;
                                  i$35 = 0;
                                  var break$121 = $makeK(function()
                                  {
                                    return {done: false, thunk: function($e)
                                    {
                                      return $k(result$34);
                                    }};
                                  }, $e);
                                  var continue$122 = $makeK(function()
                                  {
                                    if(!(i$35 < hash$32.length))
                                      return {replaceHandlers: break$121.$exHandlers, done: false, thunk: function($e)
                                      {
                                        return break$121();
                                      }};
                                    return {done: false, thunk: function($e)
                                    {
                                      return $call0("split", $makeK(function(JW95)
                                      {
                                        factor$37 = JW95;
                                        return {done: false, thunk: function($e)
                                        {
                                          return $call0(unescape, $makeK(function(JW96)
                                          {
                                            return {done: false, thunk: function($e)
                                            {
                                              return $call0(unescape, $makeK(function(JW97)
                                              {
                                                result$34[JW96] = JW97;
                                                i$35++;
                                                return {replaceHandlers: continue$122.$exHandlers, done: false, thunk: function($e)
                                                {
                                                  return continue$122();
                                                }};
                                              }, $e), null, factor$37[1]);
                                            }};
                                          }, $e), null, factor$37[0]);
                                        }};
                                      }, $e), hash$32[i$35], "=");
                                    }};
                                  }, $e);
                                  return {replaceHandlers: continue$122.$exHandlers, done: false, thunk: function($e)
                                  {
                                    return continue$122();
                                  }};
                                }
                              }, $e);
                              var continue$120 = $makeK(function()
                              {
                                if(!(i$35 < query$33.length))
                                  return {replaceHandlers: break$119.$exHandlers, done: false, thunk: function($e)
                                  {
                                    return break$119();
                                  }};
                                return {done: false, thunk: function($e)
                                {
                                  return $call0("split", $makeK(function(JW92)
                                  {
                                    factor$37 = JW92;
                                    return {done: false, thunk: function($e)
                                    {
                                      return $call0(unescape, $makeK(function(JW93)
                                      {
                                        return {done: false, thunk: function($e)
                                        {
                                          return $call0(unescape, $makeK(function(JW94)
                                          {
                                            result$34[JW93] = JW94;
                                            i$35++;
                                            return {replaceHandlers: continue$120.$exHandlers, done: false, thunk: function($e)
                                            {
                                              return continue$120();
                                            }};
                                          }, $e), null, factor$37[1]);
                                        }};
                                      }, $e), null, factor$37[0]);
                                    }};
                                  }, $e), query$33[i$35], "=");
                                }};
                              }, $e);
                              return {replaceHandlers: continue$120.$exHandlers, done: false, thunk: function($e)
                              {
                                return continue$120();
                              }};
                            }
                          }, $e);
                          if(query$33)
                            return {done: false, thunk: function($e)
                            {
                              return $call0("stripToken", $makeK(function(JW90)
                              {
                                query$33 = JW90;
                                return {done: false, thunk: function($e)
                                {
                                  return $call0("split", $makeK(function(JW91)
                                  {
                                    query$33 = JW91;
                                    return {replaceHandlers: ifK$118.$exHandlers, done: false, thunk: function($e)
                                    {
                                      return ifK$118();
                                    }};
                                  }, $e), query$33, "&");
                                }};
                              }, $e), JwacsLib, query$33);
                            }};
                          else
                          {
                            query$33 = [];
                            return {replaceHandlers: ifK$118.$exHandlers, done: false, thunk: function($e)
                            {
                              return ifK$118();
                            }};
                          }
                        }, $e), JwacsLib, window.location.search, "?");
                      }};
                    }, $e);
                    if(hash$32)
                      return {done: false, thunk: function($e)
                      {
                        return $call0("stripToken", $makeK(function(JW88)
                        {
                          hash$32 = JW88;
                          return {done: false, thunk: function($e)
                          {
                            return $call0("split", $makeK(function(JW89)
                            {
                              hash$32 = JW89;
                              return {replaceHandlers: ifK$117.$exHandlers, done: false, thunk: function($e)
                              {
                                return ifK$117();
                              }};
                            }, $e), hash$32, "&");
                          }};
                        }, $e), JwacsLib, hash$32);
                      }};
                    else
                    {
                      hash$32 = [];
                      return {replaceHandlers: ifK$117.$exHandlers, done: false, thunk: function($e)
                      {
                        return ifK$117();
                      }};
                    }
                  }, $e), JwacsLib, window.location.hash, "#");
                }};
              }), checkLocation: $lambda(function JW38($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW38, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("stripHash", $makeK(function(hash$39)
                  {
                    if(hash$39 == JwacsLib.currentHash)
                      return {done: false, thunk: function($e)
                      {
                        return $k();
                      }};
                    return {done: false, thunk: function($e)
                    {
                      return $call0("getToken", $makeK(function(token$40)
                      {
                        var ifK$123 = $makeK(function()
                        {
                          JwacsLib.currentHash = hash$39;
                          var ifK$124 = $makeK(function()
                          {
                            return {done: false, thunk: function($e)
                            {
                              return $call0(hash$39, $makeK(function()
                              {
                                return {done: false, thunk: function($e)
                                {
                                  return $k();
                                }};
                              }, $e), JwacsLib.pageThunks);
                            }};
                          }, $e);
                          if(!JwacsLib.pageThunks[hash$39])
                          {
                            JwacsLib.pageThunks[hash$39] = $lambda(function JW41($k)
                            {
                              if(!$k || !$k.$isK)
                                return $callFromDirect(JW41, this, arguments);
                              return {done: false, thunk: function($e)
                              {
                                return $call0("parsePageArgs", $makeK(function(JW98)
                                {
                                  return {done: false, thunk: function($e)
                                  {
                                    return $call0(main, $makeK(function()
                                    {
                                      return {done: false, thunk: function($e)
                                      {
                                        return $k();
                                      }};
                                    }, $e), null, JW98);
                                  }};
                                }, $e), JwacsLib, hash$39);
                              }};
                            });
                            return {replaceHandlers: ifK$124.$exHandlers, done: false, thunk: function($e)
                            {
                              return ifK$124();
                            }};
                          }
                          else
                            return {replaceHandlers: ifK$124.$exHandlers, done: false, thunk: function($e)
                            {
                              return ifK$124();
                            }};
                        }, $e);
                        if(token$40 > JwacsLib.nextToken)
                        {
                          JwacsLib.nextToken = token$40 + 1;
                          return {replaceHandlers: ifK$123.$exHandlers, done: false, thunk: function($e)
                          {
                            return ifK$123();
                          }};
                        }
                        else
                          return {replaceHandlers: ifK$123.$exHandlers, done: false, thunk: function($e)
                          {
                            return ifK$123();
                          }};
                      }, $e), JwacsLib, JwacsLib.currentHash);
                    }};
                  }, $e), JwacsLib, document.location.hash);
                }};
              }), isInternetExplorer: $lambda(function JW42($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW42, this, arguments);
                var JW99;
                return {done: false, thunk: function($e)
                {
                  return $call0("toLowerCase", $makeK(function(userAgent$43)
                  {
                    var JW100 = document.all;
                    var ifK$125 = $makeK(function()
                    {
                      return {done: false, thunk: function($e)
                      {
                        return $k(JW100 && JW99 != -1);
                      }};
                    }, $e);
                    if(JW100)
                      return {done: false, thunk: function($e)
                      {
                        return $call0("indexOf", $makeK(function(JW126)
                        {
                          JW99 = JW126;
                          return {replaceHandlers: ifK$125.$exHandlers, done: false, thunk: function($e)
                          {
                            return ifK$125();
                          }};
                        }, $e), userAgent$43, "msie");
                      }};
                    else
                      return {replaceHandlers: ifK$125.$exHandlers, done: false, thunk: function($e)
                      {
                        return ifK$125();
                      }};
                  }, $e), navigator.userAgent);
                }};
              }), prunePageThunks: $lambda(function JW44($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW44, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("getToken", $makeK(function(currentToken$45)
                  {
                    return {done: false, thunk: function($e)
                    {
                      return $new0(Object, $makeK(function(dummyObj$46)
                      {
                        return {done: false, thunk: function($e)
                        {
                          return $new0(Array, $makeK(function(remainingHashes$47)
                          {
                            
                            {
                              var JW73 = [];
                              var JW74 = 0;
                              var JW76 = 0;
                              for(var JW75 in JwacsLib.pageThunks)
                                JW73[JW74++] = JW75;
                              var h$48;
                              var break$127 = $makeK(function()
                              {
                                var ifK$129 = $makeK(function()
                                {
                                  return {done: false, thunk: function($e)
                                  {
                                    return $k();
                                  }};
                                }, $e);
                                if(remainingHashes$47.length > JwacsLib.maxHistoryEntries)
                                  return {done: false, thunk: function($e)
                                  {
                                    return $call0("sort", $makeK(function()
                                    {
                                      var numberToPrune$49 = remainingHashes$47.length - JwacsLib.maxHistoryEntries;
                                      
                                      {
                                        var i$50 = 0;
                                        var break$130 = $makeK(function()
                                        {
                                          return {replaceHandlers: ifK$129.$exHandlers, done: false, thunk: function($e)
                                          {
                                            return ifK$129();
                                          }};
                                        }, $e);
                                        var continue$131 = $makeK(function()
                                        {
                                          if(!(i$50 < numberToPrune$49))
                                            return {replaceHandlers: break$130.$exHandlers, done: false, thunk: function($e)
                                            {
                                              return break$130();
                                            }};
                                          delete JwacsLib.pageThunks[remainingHashes$47[i$50]];
                                          delete JwacsLib.generatedHashes[remainingHashes$47[i$50]];
                                          i$50++;
                                          return {replaceHandlers: continue$131.$exHandlers, done: false, thunk: function($e)
                                          {
                                            return continue$131();
                                          }};
                                        }, $e);
                                        return {replaceHandlers: continue$131.$exHandlers, done: false, thunk: function($e)
                                        {
                                          return continue$131();
                                        }};
                                      }
                                    }, $e), remainingHashes$47, $lambda(function JW51($k, l$52, r$53)
                                    {
                                      if(!$k || !$k.$isK)
                                        return $callFromDirect(JW51, this, arguments);
                                      return {done: false, thunk: function($e)
                                      {
                                        return $call0("getToken", $makeK(function(lToken$54)
                                        {
                                          return {done: false, thunk: function($e)
                                          {
                                            return $call0("getToken", $makeK(function(rToken$55)
                                            {
                                              return {done: false, thunk: function($e)
                                              {
                                                return $call0(isNaN, $makeK(function(JW102)
                                                {
                                                  var ifK$132 = $makeK(function()
                                                  {
                                                    return {done: false, thunk: function($e)
                                                    {
                                                      return $call0(isNaN, $makeK(function(JW103)
                                                      {
                                                        var ifK$133 = $makeK(function()
                                                        {
                                                          if(lToken$54 < rToken$55)
                                                            return {done: false, thunk: function($e)
                                                            {
                                                              return $k(-1);
                                                            }};
                                                          else
                                                            if(rToken$55 < lToken$54)
                                                              return {done: false, thunk: function($e)
                                                              {
                                                                return $k(1);
                                                              }};
                                                            else
                                                              return {done: false, thunk: function($e)
                                                              {
                                                                return $k(0);
                                                              }};
                                                        }, $e);
                                                        if(JW103)
                                                        {
                                                          rToken$55 = -1;
                                                          return {replaceHandlers: ifK$133.$exHandlers, done: false, thunk: function($e)
                                                          {
                                                            return ifK$133();
                                                          }};
                                                        }
                                                        else
                                                          return {replaceHandlers: ifK$133.$exHandlers, done: false, thunk: function($e)
                                                          {
                                                            return ifK$133();
                                                          }};
                                                      }, $e), null, rToken$55);
                                                    }};
                                                  }, $e);
                                                  if(JW102)
                                                  {
                                                    lToken$54 = -1;
                                                    return {replaceHandlers: ifK$132.$exHandlers, done: false, thunk: function($e)
                                                    {
                                                      return ifK$132();
                                                    }};
                                                  }
                                                  else
                                                    return {replaceHandlers: ifK$132.$exHandlers, done: false, thunk: function($e)
                                                    {
                                                      return ifK$132();
                                                    }};
                                                }, $e), null, lToken$54);
                                              }};
                                            }, $e), JwacsLib, r$53);
                                          }};
                                        }, $e), JwacsLib, l$52);
                                      }};
                                    }));
                                  }};
                                else
                                  return {replaceHandlers: ifK$129.$exHandlers, done: false, thunk: function($e)
                                  {
                                    return ifK$129();
                                  }};
                              }, $e);
                              var continue$128 = $makeK(function()
                              {
                                if(!(JW76 < JW73.length))
                                  return {replaceHandlers: break$127.$exHandlers, done: false, thunk: function($e)
                                  {
                                    return break$127();
                                  }};
                                h$48 = JW73[JW76++];
                                if(dummyObj$46[h$48])
                                  return {replaceHandlers: continue$128.$exHandlers, done: false, thunk: function($e)
                                  {
                                    return continue$128();
                                  }};
                                return {done: false, thunk: function($e)
                                {
                                  return $call0(isNaN, $makeK(function(JW101)
                                  {
                                    var ifK$134 = $makeK(function()
                                    {
                                      return {replaceHandlers: continue$128.$exHandlers, done: false, thunk: function($e)
                                      {
                                        return continue$128();
                                      }};
                                    }, $e);
                                    if(!JW101 && JwacsLib.generatedHashes[h$48] && JwacsLib.generatedHashes[h$48] > currentToken$45)
                                    {
                                      delete JwacsLib.generatedHashes[h$48];
                                      delete JwacsLib.pageThunks[h$48];
                                      return {replaceHandlers: ifK$134.$exHandlers, done: false, thunk: function($e)
                                      {
                                        return ifK$134();
                                      }};
                                    }
                                    else
                                    {
                                      remainingHashes$47[remainingHashes$47.length] = h$48;
                                      return {replaceHandlers: ifK$134.$exHandlers, done: false, thunk: function($e)
                                      {
                                        return ifK$134();
                                      }};
                                    }
                                  }, $e), null, currentToken$45);
                                }};
                              }, $e);
                              return {replaceHandlers: continue$128.$exHandlers, done: false, thunk: function($e)
                              {
                                return continue$128();
                              }};
                            }
                          }, $e));
                        }};
                      }, $e));
                    }};
                  }, $e), JwacsLib, JwacsLib.currentHash);
                }};
              }), iframeLoaded: $lambda(function JW56($k, search$57)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW56, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("stripChar", $makeK(function(hash$58)
                  {
                    return {done: false, thunk: function($e)
                    {
                      return $call0(unescape, $makeK(function(JW104)
                      {
                        window.location.hash = JW104;
                        return {done: false, thunk: function($e)
                        {
                          return $k();
                        }};
                      }, $e), null, hash$58);
                    }};
                  }, $e), JwacsLib, search$57, "?");
                }};
              }), getHttpObj: $lambda(function JW59($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW59, this, arguments);
                var http$60 = null;
                var tryK$136 = $makeK(function()
                {
                  var tryK$138 = $makeK(function()
                  {
                    var tryK$140 = $makeK(function()
                    {
                      throw "Cannot create a suitable http request object";
                    }, $k.$exHandlers);
                    var catchK$139 = $makeK(function(e)
                    {
                      return {replaceHandlers: tryK$140.$exHandlers, done: false, thunk: function($e)
                      {
                        return tryK$140();
                      }};
                    }, $k.$exHandlers);
                    return {addHandler: catchK$139, done: false, thunk: function($e)
                    {
                      return {done: false, thunk: function($e)
                      {
                        return $new0(ActiveXObject, $makeK(function(JW107)
                        {
                          http$60 = JW107;
                          if(http$60)
                            return {removeHandler: catchK$139, done: false, thunk: function($e)
                            {
                              return {done: false, thunk: function($e)
                              {
                                return $k(http$60);
                              }};
                            }};
                          return {removeHandler: catchK$139, done: false, thunk: function($e)
                          {
                            return {replaceHandlers: tryK$140.$exHandlers, done: false, thunk: function($e)
                            {
                              return tryK$140();
                            }};
                          }};
                        }, $e), "Microsoft.XMLHTTP");
                      }};
                    }};
                  }, $k.$exHandlers);
                  var catchK$137 = $makeK(function(e)
                  {
                    return {replaceHandlers: tryK$138.$exHandlers, done: false, thunk: function($e)
                    {
                      return tryK$138();
                    }};
                  }, $k.$exHandlers);
                  return {addHandler: catchK$137, done: false, thunk: function($e)
                  {
                    return {done: false, thunk: function($e)
                    {
                      return $new0(ActiveXObject, $makeK(function(JW106)
                      {
                        http$60 = JW106;
                        if(http$60)
                          return {removeHandler: catchK$137, done: false, thunk: function($e)
                          {
                            return {done: false, thunk: function($e)
                            {
                              return $k(http$60);
                            }};
                          }};
                        return {removeHandler: catchK$137, done: false, thunk: function($e)
                        {
                          return {replaceHandlers: tryK$138.$exHandlers, done: false, thunk: function($e)
                          {
                            return tryK$138();
                          }};
                        }};
                      }, $e), "Msxml2.XMLHTTP");
                    }};
                  }};
                }, $k.$exHandlers);
                var catchK$135 = $makeK(function(e)
                {
                  return {replaceHandlers: tryK$136.$exHandlers, done: false, thunk: function($e)
                  {
                    return tryK$136();
                  }};
                }, $k.$exHandlers);
                return {addHandler: catchK$135, done: false, thunk: function($e)
                {
                  return {done: false, thunk: function($e)
                  {
                    return $new0(XMLHttpRequest, $makeK(function(JW105)
                    {
                      http$60 = JW105;
                      if(http$60)
                        return {removeHandler: catchK$135, done: false, thunk: function($e)
                        {
                          return {done: false, thunk: function($e)
                          {
                            return $k(http$60);
                          }};
                        }};
                      return {removeHandler: catchK$135, done: false, thunk: function($e)
                      {
                        return {replaceHandlers: tryK$136.$exHandlers, done: false, thunk: function($e)
                        {
                          return tryK$136();
                        }};
                      }};
                    }, $e));
                  }};
                }};
              }), emptyFunction: $lambda(function JW61($k)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW61, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $k();
                }};
              }), fetchData: $lambda(function JW62($k, method$63, url$64)
              {
                if(!$k || !$k.$isK)
                  return $callFromDirect(JW62, this, arguments);
                return {done: false, thunk: function($e)
                {
                  return $call0("getHttpObj", $makeK(function(http$65)
                  {
                    var k$66 = $k;
                    http$65.onreadystatechange = $lambda(function JW67($k)
                    {
                      if(!$k || !$k.$isK)
                        return $callFromDirect(JW67, this, arguments);
                      var catchK$141 = $makeK(function(e)
                      {
                        http$65.onreadystatechange = null;
                        return {replaceHandlers: k$66.$exHandlers, done: false, thunk: function($e)
                        {
                          throw e;
                        }};
                      }, $k.$exHandlers);
                      return {addHandler: catchK$141, done: false, thunk: function($e)
                      {
                        if(http$65.readyState == 4)
                        {
                          if(!(http$65.status == undefined || http$65.status == 0 || http$65.status >= 200 && http$65.status < 300))
                            return {done: false, thunk: function($e)
                            {
                              return $new0(Error, $makeK(function(err$68)
                              {
                                throw err$68;
                              }, $e), "Server returned " + http$65.status);
                            }};
                          http$65.onreadystatechange = JwacsLib.emptyFunction;
                          return {replaceHandlers: k$66.$exHandlers, done: false, thunk: function($e)
                          {
                            return k$66(http$65.responseText);
                          }};
                        }
                        return {removeHandler: catchK$141, done: false, thunk: function($e)
                        {
                          return {done: false, thunk: function($e)
                          {
                            return $k();
                          }};
                        }};
                      }};
                    });
                    return {done: false, thunk: function($e)
                    {
                      return $call0("open", $makeK(function()
                      {
                        return {done: false, thunk: function($e)
                        {
                          return $call0("send", $makeK(function()
                          {
                            return {done: true};
                          }, $e), http$65, null);
                        }};
                      }, $e), http$65, method$63, url$64);
                    }};
                  }, $e), JwacsLib);
                }};
              })};
              return {done: true};
            }, $e));
          }};
        }, $e));
      }};
    }, $e));
  }};
});