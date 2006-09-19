var activeBoxes = [];

function getHeight(dummyDiv, text)
{
  while(dummyDiv.lastChild)
    dummyDiv.removeChild(dummyDiv.lastChild);

  var paras = text.split(/\r?\n/);
  for(var i = 0; i < paras.length; i++)
  {
    if(i > 0)
      dummyDiv.appendChild(document.createElement("BR"));
    dummyDiv.appendChild(document.createTextNode(paras[i]));
  }
  
  return dummyDiv.offsetHeight;
}

function findBoundingYs(dummyDiv, text, s, e)
{
  var lineHeight = getHeight(dummyDiv, 'a');
  var precedingHeight = s == 0 ? lineHeight : getHeight(dummyDiv, text.substr(0, s));
  var includingHeight = getHeight(dummyDiv, text.substr(0, e));

  return [lineHeight + includingHeight - precedingHeight, precedingHeight - lineHeight];
}

function positionBoxes()
{
  for(var i = 0; i < activeBoxes.length; i++)
  {
    var box = activeBoxes[i];
    var elm = box._targetElm;
    
    var top = box._heightAndTop[1];
    var bot = top + box._heightAndTop[0];

    top = Math.max(top, elm.scrollTop);
    bot = Math.min(bot, elm.scrollTop + elm.clientHeight);

    if(top < bot)
    {
      box.style.top = (top + elm.offsetTop - elm.scrollTop) + 'px';
      box.style.height = (bot - top) + 'px';
      box.style.width = (elm.clientWidth + tabWidth + 3) + 'px';
      box.style.left = (Position.positionedOffset(elm)[0] - tabWidth) + 'px';
      Element.show(box);
    }
    else
      Element.hide(box);
  }
}

//======= Public API ===============================================================================

var tabWidth = 0;
function addHighlight(elm, dummyDiv, s, e, background)
{
  var box = document.createElement("DIV");
  var origScroll = elm.scrollTop;
  var heightAndTop = findBoundingYs(dummyDiv, elm.value, s, e);
  
  Element.hide(box);
  Element.setOpacity(box, 0.5);
  box.style.position = 'absolute';
  box.style.background = background || 'green';

  box._heightAndTop = heightAndTop;
  box._targetElm = elm;

  document.body.appendChild(box);
  activeBoxes.push(box);
  elm.scrollTop = origScroll;

  return box;
}

function removeHighlight(targetBox)
{
  for(var i = 0; i < activeBoxes.length; i++)
  {
    var box = activeBoxes[i];
    if(box == targetBox)
    {
      activeBoxes[i] = null;
      activeBoxes.splice(i, 1);
      box.parentNode.removeChild(box);
      return box;
    }
  }
}
      
function clearHighlights()
{
  for(var i = 0; i < activeBoxes.length; i++)
  {
    var box = activeBoxes[i];
    box.parentNode.removeChild(box);
    activeBoxes[i] = null;
  }
  activeBoxes = [];
}

function initHighlights(period)
{
  setInterval(positionBoxes, period || 150);
}
