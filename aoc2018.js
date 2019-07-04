// ==UserScript==
// @name          Advent of code 2018 solutions
// @description   Automatically fetches inputs and fills in solutions to AoC problems.
// @run-at        document-load
// @grant         none
// @include       *://adventofcode.com/*/day/*
// @version       1.0
// ==/UserScript==

(async ()=>{
  const inputElem = document.querySelector(".puzzle-input");
  const inputLink = document.querySelector("a[href*='input']");
  const outputField = document.querySelector("input[name='answer']");
  let outputPara = outputField;
  while(outputPara && !outputPara.parentElement.matches("main")) outputPara = outputPara.parentElement;
  const day = location.pathname.split("/")[3];
  const part = document.querySelector(".day-success") ? "b" : "a"

  const solvers = window.solvers = {
    "1a": input => input.trim().split("\n").map(l => +l).reduce((a, b) => a + b),
    "1b": input => {
      let curr = 0; const seen = [curr]; const changes = input.trim().split("\n").map(l => +l);
      while(true) for(let c of changes){
        curr += c; if(seen.includes(curr)) return curr; seen.push(curr)
      }
    },
    "2a": input => {
      const regex2 = new RegExp("abcdefghijklmnopqrstuvwxyz".split("").map(c=>`^[^${c}\n]*${c}[^${c}\n]*${c}[^${c}\n]*$`).join("|"), "mg");
      const regex3 = new RegExp("abcdefghijklmnopqrstuvwxyz".split("").map(c=>`^[^${c}\n]*${c}[^${c}\n]*${c}[^${c}\n]*${c}[^${c}\n]*$`).join("|"), "mg");
      console.log(input.match(regex2));
      console.log(input.match(regex3));
      return input.match(regex2).length * input.match(regex3).length;
    },
    "2b": input => {
      const ids = input.trim().split("\n");
      for(let i = 0; i < ids.length; i++){
        pairSearch: for(let j = 0; j < i; j++){
          let diffAt = -1;
          for(let k = 0; k < ids[i].length; k++){
            if(ids[i][k] != ids[j][k]){
              if(diffAt != -1) continue pairSearch;
              diffAt = k;
            }
          }
          if(diffAt != -1){
            console.log(i, j)
            return ids[i].slice(0, diffAt) + ids[i].slice(diffAt + 1);
          }
        }
      }
    },
    "3ab": (input, part) => {
      const seenOnce = new Uint32Array(32 * 1024);
      const seenAgain = new Uint32Array(32 * 1024);
      for(let l of input.trim().split("\n")){
        const mr = l.match(/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/).map(l => +l);
        byteFrom = mr[3] >> 5;
        bitFrom = mr[3] & 31;
        byteTo = (mr[3] + mr[5]) >> 5;
        bitTo = (mr[3] + mr[5]) & 31;
        
        for(let row = mr[2]; row < mr[2] + mr[4]; row++){
          for(let byte = byteFrom; byte <= byteTo; byte++){
            mask = ~0; 
            if(byte == byteFrom) mask &= (-1 << bitFrom);
            if(byte == byteTo) mask &= ~(-1 << bitTo);
            const ix = row * 32 + byte;
            seenAgain[ix] = seenAgain[ix] | (mask & seenOnce[ix]);
            seenOnce[ix] = seenOnce[ix] | mask
          }
        }        
      }
      
      let count = 0;
      for(let word of seenAgain){
        for(let bit = 0; bit < 32; bit++){
          count += (word >> bit) & 1;
        }
      }
      if(part == "a") return count;
      
      search: for(let l of input.trim().split("\n")){
        const mr = l.match(/#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/).map(l => +l);
        byteFrom = mr[3] >> 5;
        bitFrom = mr[3] & 31;
        byteTo = (mr[3] + mr[5]) >> 5;
        bitTo = (mr[3] + mr[5]) & 31;
        
        for(let row = mr[2]; row < mr[2] + mr[4]; row++){
          for(let byte = byteFrom; byte <= byteTo; byte++){
            mask = ~0; 
            if(byte == byteFrom) mask &= (-1 << bitFrom);
            if(byte == byteTo) mask &= ~(-1 << bitTo);
            const ix = row * 32 + byte;
            if(seenAgain[ix] & mask) continue search;
          }
        }
        
        return mr[1];
      }
    },
    "4a": input => {
      const lines = input.trim().split("\n");
      lines.sort();

      const minsSleptByGuard = {};
      let currGuard = null; let lastFellAsleep = null;
      for(l of lines){
        mr = l.match(/\[\d\d\d\d-\d\d-\d\d \d\d:(\d\d)\] (.*#(\d+).*|.*)/);
        
        if(mr[3]){
          if(lastFellAsleep !== null) throw "previous guard still asleep!";
          currGuard = mr[3];
          minsSleptByGuard[currGuard] = minsSleptByGuard[currGuard] || 0;
        } else if(mr[2] == "falls asleep"){
          if(lastFellAsleep !== null) throw "guard already asleep!";
          lastFellAsleep = +mr[1];
        } else if(mr[2] == "wakes up"){
          if(lastFellAsleep === null) throw "guard wasn't asleep!";
          minsSleptByGuard[currGuard] += +mr[1] - lastFellAsleep;
          lastFellAsleep = null;
        }
      }
      if(lastFellAsleep !== null) throw "last guard still asleep!";
      
      guardChosen = Object.keys(minsSleptByGuard).sort((a, b) => minsSleptByGuard[b] - minsSleptByGuard[a])[0];
      console.log(guardChosen);

      let timesSleptPerMinute = [];
      for(l of lines){
        mr = l.match(/\[\d\d\d\d-\d\d-\d\d \d\d:(\d\d)\] (.*#(\d+).*|.*)/);
        
        if(mr[3]){
          currGuard = mr[3];
        } else if(mr[2] == "falls asleep"){
          lastFellAsleep = +mr[1];
        } else if(mr[2] == "wakes up"){
          if(currGuard == guardChosen){
            for(let minute = lastFellAsleep; minute <= +mr[1]; minute++){
              timesSleptPerMinute[minute] = (timesSleptPerMinute[minute] || 0) + 1;
            }
          }
          lastFellAsleep = null;
        }
      }
      
      const minuteChosen = Object.keys(timesSleptPerMinute).sort((a, b) => timesSleptPerMinute[b] - timesSleptPerMinute[a])[0];
      return Number (guardChosen) * +minuteChosen;
    },
    "4b": input => {
      const lines = input.trim().split("\n");
      lines.sort();

      let lastFellAsleep = null;
      const timesPerGuardAndMinute = {};
      for(l of lines){
        mr = l.match(/\[\d\d\d\d-\d\d-\d\d \d\d:(\d\d)\] (.*#(\d+).*|.*)/);
        
        
        if(mr[3]){
          if(lastFellAsleep !== null) throw "previous guard still asleep!";
          currGuard = mr[3];
        } else if(mr[2] == "falls asleep"){
          if(lastFellAsleep !== null) throw "guard already asleep!";
          lastFellAsleep = +mr[1];
        } else if(mr[2] == "wakes up"){
          if(lastFellAsleep === null) throw "guard wasn't asleep!";
          for(let minute = lastFellAsleep; minute < +mr[1]; minute++){
            timesPerGuardAndMinute[`${currGuard},${minute}`] = (timesPerGuardAndMinute[`${currGuard},${minute}`] || 0) + 1;
          }
          lastFellAsleep = null;
        }
      }
      if(lastFellAsleep !== null) throw "last guard still asleep!";
      
      const chosenGuardAndMinute = Object.keys(timesPerGuardAndMinute).sort((a, b) => timesPerGuardAndMinute[b] - timesPerGuardAndMinute[a])[0];
      console.log(chosenGuardAndMinute);
      return chosenGuardAndMinute.split(",").map(l => +l).reduce((a, b) => a*b);
    },
    "5ab": (input, part) => {
      const upcaseOffset = "A".charCodeAt(0) - "a".charCodeAt(0);
      const solidus = "|".charCodeAt(0);
      const partA = input => {
        let iLen;
        do{
          iLen = input.length;
          for(let c = "a".charCodeAt(0); c <= "z".charCodeAt(0); c++){
            const re = new RegExp(String.fromCharCode(c, c + upcaseOffset, solidus, c+upcaseOffset, c), 'g');
            input = input.replace(re, "");
          }
        }while(iLen > input.length);
        return input.trim().length;
      };
      
      if(part == "a"){
        return partA(input);
      } else {
        let bestLength = 1/0;
        for(let c = "a".charCodeAt(0); c <= "z".charCodeAt(0); c++){
          const re = new RegExp(String.fromCharCode(c, solidus, c+upcaseOffset), 'g');
          const iterLength = partA(input.replace(re, ""));
          console.log(re, iterLength);
          if(iterLength < bestLength) bestLength = iterLength;
        }
        return bestLength;
      }
    },
    "6a": input => {
      const dests = input.trim().split(/\n/).map(l => l.split(", ").map(d => +d));
      const size = 1 + dests.flat().reduce((a, d) => a > +d ? a : +d, 0);
      const histogram = [];
      for(let x = 0; x < size; x++){
        for(let y = 0; y < size; y++){
          let closestElem = 0;
          let closestDist = Math.abs(x - dests[0][0]) + Math.abs(y - dests[0][1]);
          for (let destIx = 1; destIx < dests.length; destIx++){
            newDist = Math.abs(x - dests[destIx][0]) + Math.abs(y - dests[destIx][1]);
            if(newDist < closestDist){
              closestDist = newDist; closestElem = destIx;
            } else if(newDist == closestDist){
              closestElem = -1;
            }
          }
          if(closestElem > -1){
            if(x == 0 || y == 0 || x == size - 1 || y == size - 1){
              histogram[closestElem] = 1/0;
            } else {
              histogram[closestElem] = (histogram[closestElem] || 0) + 1;
            }
          }
        }
      }
      return histogram.reduce((a, b) => a > b || b == 1/0 ? a : b);
    },
    "6b": input => {
      const dests = input.trim().split(/\n/).map(l => l.split(", ").map(d => +d));
      const size = 1 + dests.flat().reduce((a, d) => a > +d ? a : +d, 0);
      const margin = 0 | (10000 / dests.length);
      let pointCount = 0;
      for(let x = -margin; x < size + margin; x++){
        for(let y = -margin; y < size + margin; y++){
          const distSum = dests.reduce((a, d) => a + Math.abs(x - d[0]) + Math.abs(y - d[1]), 0);
          if(distSum < 10000) pointCount++;
        }
      }
      return pointCount;
    },
    "7a": input => {
      let todo = Array.from("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      let output = "";
      while(todo.length){
        next = todo.find(c => ! todo.some(pre => input.match(`Step ${pre} must be finished before step ${c} can begin`)));
        output += next;
        todo = todo.filter(t => t != next);
      }
      return output;
    },
    "7b": input => {
      let todo = Array.from("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      let undone = Array.from("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      let jobs = [];
      let t = 0;
      while(undone.length){
        for(let jobId of [0, 1, 2, 3, 4]){
          if(jobs[jobId]){
            jobs[jobId].time--;
            if(jobs[jobId].time == 0){
              undone = undone.filter(t => t != jobs[jobId].task);
              console.log(`@${t}#${jobId} task ${jobs[jobId].task} done`);
              jobs[jobId] = null;
            }
          }
        }
        for(let jobId of [0, 1, 2, 3, 4]){
          if(!jobs[jobId]){
            next = todo.find(c => ! undone.some(pre => input.match(`Step ${pre} must be finished before step ${c} can begin`)));
            if(next){
              jobs[jobId] = {time: 61 + next.charCodeAt(0) - "A".charCodeAt(0), task: next};
              console.log(`@${t}#${jobId} task ${next} ${jobs[jobId].time} left`);
              todo = todo.filter(t => t != next);
            }
          }
        }
        if(! jobs.some(j => j)) return t;
        t++;
      }
    },
    "8ab": (input, part) => {
      const parseTree = input => {
         const childCount = input[0];
         const metaCount = input[1];
         input = input.slice(2);
         const children = [];
         for(let i = 0; i < childCount; i++){
            let child;
            [child, input] = parseTree(input);
            children.push(child);
         }
         const metas = input.slice(0, metaCount);
         return [{children, metas}, input.slice(metaCount)];
      }
      
      input = input.split(" ").map(w => +w);
      [tree, input] = parseTree(input);
      if(input.length) throw "input not empty after reading tree";
      
      const sumMetas = node => node.children.reduce((a, d) => a + sumMetas(d), node.metas.reduce((a, d) => a + d, 0));
      const nodeValue = node => node.metas.map(m => 
                                  node.children.length == 0 ? m : 
                                  (m == 0 || m > node.children.length) ? 0 : 
                                  nodeValue(node.children[m - 1])
                                ).reduce((a, d) => a + d, 0);
      return {a: sumMetas, b: nodeValue}[part](tree);
    },
    "9ab": (input, part) => {
      const mr = input.match(/(\d+) players; last marble is worth (\d+) points\n/);
      const scores = new Array(+mr[1]);
      const marbles = [0];
      for(let t = 1; t < +mr[2] * (part == "a" ? 1 : 100); t++){
        if (t % 23){
          marbles.push(marbles.shift());
          marbles.push(t);
        }else{
          marbles.unshift(...marbles.splice(-7));
          scores[t % scores.length] = (scores[t % scores.length] || 0) + t + marbles.pop();
          marbles.push(marbles.shift());
        }
      }
      return scores.reduce((a, d) => a > +d ? a : +d, 0);
    },
    "10ab": (input, part) => {
      const points = input.trim().split("\n").map(
        l => l.match(/position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/)
              .slice(1).map(n => +n)
      );
      
      let time = 0, timeEl;
      changeTime = offset => {
        points.forEach(p => {p[0] += p[2] * offset; p[1] += p[3] * offset});
        time += offset;
        if(timeEl) timeEl.value = time;
      }
      
      let dimX, dimY, prevSize;
      do{
        prevSize = dimX * dimY;
        changeTime(1);
        dimX = points.reduce((a, d) => a > d[0] ? a : d[0], 0) -
               points.reduce((a, d) => a < d[0] ? a : d[0], 0);
        dimY = points.reduce((a, d) => a > d[1] ? a : d[1], 0) -
               points.reduce((a, d) => a < d[1] ? a : d[1], 0);
      }while(!prevSize || dimX * dimY < prevSize);
      changeTime(-1);
      
      widgetP = document.createElement("p");
      widgetP.innerHTML =
`
<canvas width=${2 * dimX + 1} height=${2 * dimY + 1} style="background: white"></canvas>
<br/>
<button id="playRwd"> &lt;&lt; </button>
<button id="subTime"> &lt; </button>
<button id="addTime"> &gt; </button>
<button id="playFwd"> &gt;&gt; </button>
<input id="time" disabled />
`

      const canvasContext = widgetP.querySelector("canvas").getContext("2d");
      //canvasContext.translate(dimX, dimY);
      
      const drawCanvas = () => {
        canvasContext.clearRect(0, 0, 2*dimX+1, 2*dimY+1);
        for(let p of points) canvasContext.fillRect(p[0]-0.5, p[1]-0.5, 1, 1);
      }
      
      let timeout = null;
      const buttonHandler = (repeat, offset) => () => {
        clearTimeout(timeout);
        changeTime(offset);
        drawCanvas();
        if(repeat) timeout = setTimeout(buttonHandler(repeat, offset), 100);
      }
      widgetP.querySelector("#playRwd").onclick = buttonHandler(true, -1);
      widgetP.querySelector("#subTime").onclick = buttonHandler(false, -1);
      widgetP.querySelector("#addTime").onclick = buttonHandler(false, 1);
      widgetP.querySelector("#playFwd").onclick = buttonHandler(true, 1);
      
      timeEl = widgetP.querySelector("#time");
      return widgetP;
    },
    "11ab": (input, part) => {
      const ary = [];
      for(let y = 300; y; y--){
        ary[y] = [];
        for(let x = 300; x; x--){
          ary[y][x] = (((x + 10) * y + +input) * (x + 10) / 100 | 0) % 10 - 5;
        }
      }
      
      let best = 0, bestAt = null;
      for(let sizeM1 = (part == 'a' ? 2 : 0); sizeM1 < (part == 'a' ? 2 : 299); sizeM1++){
        console.log(`testing size ${sizeM1+1}`);
        for(let y = 1; y <= 300 - sizeM1; y++){
          for(let x = 1; x <= 300 - sizeM1; x++){
            let sum = 0;
            for(let dy = 0; dy <= sizeM1; dy++){
              for(let dx = 0; dx <= sizeM1; dx++){
                sum += ary[y+dy][x+dx];
              }
            }
            if(sum > best){
              best = sum;
              bestAt = `${x},${y}${part == 'a' ? '' : `,${sizeM1 + 1}`}`;
              console.log(bestAt);
            }
          }
        }
      }
      return bestAt;
    },
    "12ab": (input, part) => {
      const stateScore = (state, left) => state.split("").map((e, i) => e == "#" ? i + left : 0).reduce((a, d) => a + d, 0);
      
      let state = input.match(/initial state: ([-.#]+)/)[1];
      let left = 0;
      const transitions = {};
      for(let l of input.match(/[.#]{5} => [.#]/g)){
        const mr = l.split(" => ");
        transitions[mr[0]] = mr[1];
      }
      
      for(let t = 0; t < (part == "a" ? 20 : 200); t++){
        state = `....${state}....`;
        let next = "";
        for(let i = 0; i < state.length - 4; i++){
          next += transitions[state.slice(i, i+5)];
        }

        if(state.match(/#[.#]*#/)[0] == next.match(/#[.#]*#/)[0]){
          const scorePerTurn = (stateScore(state, left - 4 + next.match(/^\.*/)[0].length - 2) -
                                stateScore(state, left - 4));
          return stateScore(state, left - 4) + (50e9 - t) * scorePerTurn;
        }
        
        left += next.match(/^\.*/)[0].length - 2;
        state = next.match(/#[.#]*#/)[0];
        console.log(t, left, state, stateScore(state, left));
      }
      return stateScore(state, left);
    },
    "13ab": (input, part) => {
      const world = input.split("\n").map(l => l.split("").map(c => 
        ({">": "->0", "v": "|v0", "<": "-<0", "^": "|^0"}[c] || c)
      ));
      
      let carts = [];
      for(let y = 0; y < world.length; y++){
        for(let x = 0; x < world[y].length; x++){
          if(world[y][x].length > 1) carts.push([y, x]);
        }
      }
      
      for(let t = 0;; t++){
        for(let c of carts){
          let [cy, cx] = c;
          let dir, mem;
          [world[cy][cx], dir, mem] = world[cy][cx].split("");
          if(!dir) continue;
          switch(dir){
            case ">": cx++; break;
            case "v": cy++; break;
            case "<": cx--; break;
            case "^": cy--; break;
            default: debugger;
          }
          c[0] = cy; c[1] = cx;
          switch(world[cy][cx]){
            case "-": break;
            case "|": break;
            case "/": dir = {">" : "^", "v": "<", "<": "v", "^": ">"}[dir]; break;
            case "\\": dir = {">" : "v", "v": ">", "<": "^", "^": "<"}[dir]; break;
            case "+":
              dir = [{">" : "^", "v": ">", "<": "v", "^": "<"},
                     {">" : ">", "v": "v", "<": "<", "^": "^"},
                     {">" : "v", "v": "<", "<": "^", "^": ">"}][mem][dir];
              mem = ["1", "2", "0"][mem];
              break;
            default:
              if(world[cy][cx].length == 3){
                if(part == "a") return `${cx},${cy}`;
                world[cy][cx] = world[cy][cx][0];
                continue;
              }else{
                debugger;
                throw "off rails!";
              }
          }
          world[cy][cx] += dir + mem;
        }
        carts = carts.filter(([cy, cx]) => world[cy][cx].length > 1);
        if(carts.length == 1) return carts[0].reverse().join(",");
        carts.sort(([y1, x1], [y2, x2]) => y1 != y2 ? y1 - y2 : x1 - x2);
      }
    },
    "14a": input => {
      const board = new Uint8Array(+input + 11);
      board[0] = 3; board[1] = 7; let blength = 2;
      let i = 0, j = 1;
      while((blength < +input + 10)){
        for(d of (board[i] + board[j] + "").split("")) board[blength++] = +d;
        i += board[i] + 1; i %= blength;
        j += board[j] + 1; j %= blength;
      }
      console.log(blength, board.length, board.slice(blength - 20, blength));
      return board.slice(+input, +input + 10).join("");
    },
    "14b": input => {
      const board = [3, 7];
      let i = 0, j = 1;
      while(!board.slice(-input.length - 1).join("").includes(input)){
        for(d of (board[i] + board[j] + "").split("")) board.push(+d);
        i += board[i] + 1; i %= board.length;
        j += board[j] + 1; j %= board.length;
      }
      console.log(board.length, board.slice(-20));
      return board.jois("").indexOf(+input);
    }
  }

  function asyncTimeout(ms){return new Promise((resolve, reject) => setTimeout(resolve, ms))}

  if(outputField) outputField.value = "fetching input";
  if(inputElem){
    input = inputElem.textContent;
  }else if(localStorage.getItem("inputHref") == inputLink){
    input = localStorage.getItem("input");
    console.log("using cached input; size " + input.length);
  }else{
    input = await (await fetch(inputLink.href)).text();
    localStorage.setItem("inputHref", inputLink);
    localStorage.setItem("input", input);
  }
  if(outputField) outputField.value = "calculating";
  await asyncTimeout(0);
  
  if(solvers[day+part]) result = await solvers[day+part](input);
  else result = await solvers[day+"ab"](input, part);
  
  if(typeof result == "string" || typeof result == "number"){
    outputField.value = result;
  }else{
    for(r of document.querySelectorAll("#result")) r.parentElement.removeChild(r);
    document.querySelector("main").insertBefore(result, outputPara);
    result.id = "result"
  }
})()
