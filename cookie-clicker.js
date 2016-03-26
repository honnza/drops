/* jshint esversion:6 */
/* global Game, Beautify */

(function(){
  debugger;

  var cpsEpsilon = 0;
  document.getElementById("store").addEventListener("click", () => updateDelay = 1);
  var bestPayoff;
  var cbChecked = true;
  var updateDelay = 0;
  var sysTime;

  new MutationObserver(function(){
    if(!updateDelay || updateDelay < 0 && !document.getElementById("upgrade0").style.boxShadow){
      sysTime = Date.now();
      var expCps = calcNewCPS(null);
      var cpsError = Math.abs(expCps.measured - Game.cookiesPs) / Math.max(expCps.measured, Game.cookiesPs);
      if(cpsError > cpsEpsilon){
        cpsEpsilon = cpsError;
        logOnce("expected cps: " + expCps.measured + "<br>game cps: " + Game.cookiesPs + "<br>error: " + cpsError);
      }
      combos.forEach(u => u.assertOk());

      var productPayoffs = Game.ObjectsById.filter(obj => !obj.l.classList.contains("toggledOff")).map(function(obj){
        var singularUpgradeCps = calcNewCPS(obj).real - expCps.real;
        var singularPayoff = singularUpgradeCps && 1 / obj.price / (expCps.real / singularUpgradeCps + 1);
        var comboValid = combos[obj.id].isValid(); 
        if(comboValid){
          var comboUpgradeCps = calcNewCPS(combos[obj.id]).real - expCps.real;
          var comboPayoff = comboUpgradeCps && 1 / combos[obj.id].price() / (expCps.real / comboUpgradeCps + 1);
        }
        return {
          obj: obj,
          payoff: comboValid ? Math.max(singularPayoff, comboPayoff) : singularPayoff,
          price: obj.price,
          l: obj.l
        };
      });

      var store = document.getElementById("store");
      var upgradePayoffs = Array.map(store.getElementsByClassName("upgrade"), function(l){
        //todo: lookup by name? Regexing an onclick attribute is ugly.
        var upg = Game.UpgradesById[l.getAttribute("onclick").match(/\d+/)[0]];
        var upgradeCps = Math.max(0, calcNewCPS(upg).real - expCps.real);
        var price = upg.basePrice;
        if(upg.id === 331) price = 60 * 60 * Game.cookiesPs;
        if(Game.UpgradesById[223].bought) price *= 0.99;
        if(Game.UpgradesById[168].bought) price *= 0.98;        
        if(Game.UpgradesById[161].bought) price *= 0.95;
        if(Game.dragonAura === 6 || Game.dragonAura2 === 6) price *= 0.98;
        //if(upg.id === 64) return calcBingoCombo();
        return {
          obj: upg,
          payoff: upgradeCps && 1 / price / (expCps.real / upgradeCps + 1),
          price: price,
          l: l
        };

        function calcBingoCombo(){
          //todo: make calcNewCPS accept multiple objects, then implement this, then big clean up eventually.
        }
      });

      var payoffs = productPayoffs.concat(upgradePayoffs);
      bestPayoff = payoffs[0];
      payoffs.forEach(p => {if(p.payoff > bestPayoff.payoff) bestPayoff = p;});
      payoffs.forEach(function(p){

        var r = 0 | Math.round(255 * Math.pow(1 - p.payoff / bestPayoff.payoff, 0.45));
        var g = 0 | Math.round(255 * Math.pow(    p.payoff / bestPayoff.payoff, 0.45));
        var b = (p.payoff === bestPayoff.payoff || ! p.payoff) ? 255 : 0;
        p.l.style.boxShadow = `inset 0 0 20px rgb(${r}, ${g}, ${b})`;
        if(!p.payoff && p.payoff !== null && p.payoff !== 0) debugger;
      });
    }

    var realCookies = Game.cookies + (Game.UpgradesById[224].bought ? 1.15 : 1.1) * Game.wrinklers.map(x => x.sucked).reduce((a, b) => a + b);
    if(realCookies !== Game.cookies && !document.getElementById("realCookies")){
      new Game.Note("Cookies including withered: <span id='realCookies'></span><br>Use in title: <input type='checkbox' id='realCookiesCB'" + (cbChecked ? " checked>" : ">"));
    }
    if(document.getElementById("realCookies")){
      document.getElementById("realCookiesCB").onchange = function(){
        cbChecked = this.checked;
      };
      document.getElementById("realCookies").textContent = Beautify(realCookies);
    }

    var titleCash = cbChecked ? realCookies : Game.cookies;
    var title = Game.goldenCookie.l.style.display !== "none" ? (Game.goldenCookie.wrath ? "w" : "c") : "";
    var wrinklers = Game.wrinklers.filter(w => w.phase).length;
    if(wrinklers) title += wrinklers;
    if(title) title += " ";
    title += progressBar(titleCash / bestPayoff.price); 
    document.title = title;

    updateDelay--;
  }).observe(document.getElementById("cookies"), {characterData: true, childList: true});

  // sort order should follow:
  // - per building: base CPS bonuses -> multipliers & aura3 -> combos & outfits -> non-base CPS bonuses
  // - hasSmac, aura1 before kitten upgrades
  // todo: support aura3
  var by = f => (a, b) => {
    var fa = f(a), fb = f(b);
    for(var i = 0; i<fa.length || i<fb.length; i++){
      if(fa[i] < fb[i]) return -1;
      if(fa[i] > fb[i]) return 1;
    }
    return 0;
  };
  var ixByPlural = {}; 
  Game.ObjectsById.forEach(b => ixByPlural[b.plural.toLowerCase()] = b.id);
  var kittenTable = {"31": 0.1, "32": 0.125, "54": 0.15, "108": 0.175, "187": 0.2, "291": 0.2, "320": 0.2, "321": 0.2, "322": 0.2};

  var parsedUpgrades = Game.UpgradesById.slice(0).map(function(u){
    var match;
    function mkLineFunc(phase, bName, rateStr, fn){
      if(bName) bName = bName.toLowerCase();
      var oid = bName && Game.ObjectsById.findIndex(b => b.plural === bName || b.single === bName);
      var rate = (!rateStr ? [] : rateStr.splice ? rateStr : [rateStr]).map(function(rateStr){ // todo: ugly
        var rate = +rateStr.replace("%", "").replace(",", "").replace(" million", "000000");
        if(~rateStr.indexOf("%")) rate /= 100;
        return rate;
      });
      if(!rateStr || !rateStr.splice) rate = rate[0];
      return {phase: phase, upg: u, fn: data => fn(oid && oid, rate, data)};
    }

    var phase;
    var lineEffects = u.desc.replace(/<q>.*/, "").split(/\. /).map(function(line){
      line = line.replace(/<(\w+)><\/\1>/, ""); // elder gods, here I come
      match = /(?:the mouse and )?([\w\s]+) gain <b>\+([\d.,]+(?: million)?)<\/b> base CpS/i.exec(line);
      if(match) return mkLineFunc(1, match[1], match[2], (oid, rate, data) => data.baseCPSes[oid] += rate);
      match = /Each grandma gains(?: another)? <b>\+1 base CpS for every (\d+) (\w+)<\/b>/.exec(line);
      if(match) return mkLineFunc(1, match[2], match[1], (oid, rate, data) => data.baseCPSes[1] += data.buildings[oid].amount / rate);
      match = /Each grandma gains(?: another)? <b>\+([\d.]+) base CpS per (\w+)<\/b>/.exec(line);
      if(match) return mkLineFunc(1, match[2], match[1], (oid, rate, data) => data.baseCPSes[1] += data.buildings[oid].amount * rate);
      match = /(?:the mouse and )?([\w\s]+) are <b>twice<\/b> as (?:efficient|productive)/i.exec(line);
      if(match) return mkLineFunc(2, match[1], null, (oid, _, data) => data.baseCPSes[oid] *= 2);
      match = /(?:the mouse and )?([\w\s]+) gain <b>\+([\d.]+%) CpS<\/b> per grandma/.exec(line);
      if(match) return mkLineFunc(2, match[1], match[2], (oid, rate, data) => data.baseCPSes[oid] *= (1 + data.buildings[1].amount * rate));
      match = /(?:the mouse and )?([\w\s]+) gain <b>\+([\d.]+%) CpS<\/b> per (\d+) grandmas/.exec(line);
      if(match) return mkLineFunc(2, match[1], [match[2], match[3]], (oid, rates, data) => data.baseCPSes[oid] *= (1 + data.buildings[1].amount * rates[0] / rates[1]));
      match = /(?:the mouse and )?([\w\s]+) gain <b>\+([\d.,]+(?: million)?)<\/b> cookies for each non-cursor object owned/i.exec(line);
      if(match) return mkLineFunc(3, match[1], match[2], (oid, rate, data) => data.baseCPSes[oid] += rate * data.buildings.slice(1).map(b => b.amount).reduce((a, b) => a + b));
      match = /Cookie production multiplier <b>\+(\d+%)<\/b>/.exec(line);
      if(match && u.id >= 210 && u.id <= 221) return mkLineFunc(4, null, match[1], (_, rate, data) => data.eggMult += rate);
      if(match) return mkLineFunc(4, null, match[1], (_, rate, data) => data.cpm *= (1 + rate));
      match = /Cookie production multiplier <b>\+(\d+%) per Santa's levels<\/b>/.exec(line);
      if(match) return mkLineFunc(4, null, match[1], (_, rate, data) => data.cpm *= (1 + (Game.santaLevel + 1) * rate));
      match = /Unlocks <b>(\d+%)<\/b> of the potential of your prestige level/.exec(line);
      if(match) return mkLineFunc(4, null, match[1], (_, rate, data) => data.chipsPotential = rate);
    }).filter(fn => fn);

    if(lineEffects.length === 1) return lineEffects[0];
    else if(lineEffects.length > 1){
      var phaseMin = Math.min(...lineEffects.map(x => x.phase));
      var phaseMax = Math.max(...lineEffects.map(x => x.phase));
      if(phaseMax - phaseMin > 1) logOnce ("multiphase upgrade " + u.name + " (id " + u.id + ") cannot be handled with the current sort order");
      return {phase: (phaseMax + phaseMin)/2, upg: u, fn: data => lineEffects.forEach(e => e.fn(data))};
    }

    for(var k in kittenTable) if(u.id === +k) return {phase:6, upg: u, fn: data => 
      data.kittenBonus *= 1 + Game.milkProgress * kittenTable[u.id] * (data.hasSMaC ? 1.05 : 1) * (Game.dragonAura === 1 || Game.dragonAura2 === 1 ? 1.05 : 1)
    };

    if(u.id === 64)  return {phase: 2, upg: u, fn: data => data.baseCPSes[1] *= 4};
    if(u.id === 84)  return {phase: 5, upg: u, fn: data => data.covenantMult = 0.95};
    if(u.id === 166) return {phase: 5, upg: u, fn: data => data.hasSMaC = true};
    if(u.id === 224) return {phase: 5, upg: u, fn: data => data.hasWrinklerspawn = true};    
    if(u.id === 228) return {phase: 4, upg: u, fn: data => {
      var day=Math.floor((sysTime-Game.startDate)/1000/10)*10/60/60/24;
      day=Math.min(day,100);
      var centuryGain=(1-Math.pow(1-day/100,3))*0.1;
      data.eggMult += centuryGain;
    }};
    if(u.id === 229) return {phase: 5, upg: u, fn: data => data.hasEgg = true};
    if(u.id === 331) return {phase: 4, upg: u, fn: data => data.cpm *= 1.5};

    return {phase: 0, upg: u, fn: () => {if(u.pool !== "prestige") logOnce("unknown upgrade " + u.name + " (id " + u.id + ")");}};
  });
  var comboUpgrades = [
    [3, 4, 5, 6, 43, 82, 109, 188, 189], [7, 8, 9, 44, 110, 192, 294, 307], 
    [10, 11, 12, 45, 111, 193, 295, 308], [16, 17, 18, 47, 113, 195, 296, 309], [13, 14, 15, 46, 112, 194, 297, 310],
    [232, 233, 234, 235, 236, 237, 298, 311], [238, 239, 240, 241, 242, 243, 299, 312], [244, 245, 246, 247, 248, 249, 300, 313], 
    [19, 20, 21, 48, 114, 196, 301, 314], [22, 23, 24, 49, 115, 197, 302, 315], [25, 26, 27, 50, 116, 198, 303, 316], 
    [28, 29, 30, 51, 117, 199, 304, 317], [99, 100, 101, 102, 118, 200, 305, 318], [175, 176, 177, 178, 179, 201, 306, 319]
  ].map(us => us.map(u => parsedUpgrades.find(pu => pu.upg.id === u)));
  var comboRequirements = [[20]];
  for(var i = 1; i <= 8; i++) comboRequirements[0].push(i * 40);
  for(var i = 1; i <= Game.ObjectsById.length; i++) comboRequirements.push([1, 5, 25, 50, 100, 150, 200, 250]);    

  var combos = Game.ObjectsById.map(b => {
    function comboAmount(b) {
      var n = b.amount;
      do {
        n = comboRequirements[b.id].find(x => x > n);
        if(!n) return;
      } while (comboUpgrade(n).upg.unlocked || comboUpgrade(n).upg.bought);
      return n;
    }
    function comboUpgrade(n) {return comboUpgrades[b.id][comboRequirements[b.id].indexOf(n)];}

    var upg = {
      phase: 2.5,
      isValid: () => comboAmount(b) !== undefined, 
      price: () => {
        var n = comboAmount(b); 
        return b.price * (Math.pow(1.15, n - b.amount) - 1) / 0.15 + comboUpgrade(n).upg.basePrice;
      },
      fn: data => {
        var n = data.buildings[b.id].amount = comboAmount(b); 
        comboUpgrade(n).fn(data);
      },
      assertOk: function(){
        var lastUnlocked = comboRequirements[b.id].findIndex(x => x <= b.amount);
        var upgrade;
        if(~lastUnlocked && !comboUpgrades[b.id][lastUnlocked].upg.unlocked){
          upgrade = comboUpgrades[b.id][lastUnlocked].upg;
          logOnce("expected the " + upgrade.name + " upgrade (id " + upgrade.id + ") to be unlocked, but it isn't");
        }
        var firstLocked = comboRequirements[b.id].findIndex(x => x > b.amount);
        if(~firstLocked && comboUpgrades[b.id][firstLocked].upg.unlocked){
          upgrade = comboUpgrades[b.id][firstLocked].upg;
          logOnce("expected the " + upgrade.name + " upgrade (id " + upgrade.id + ") to be locked, but it isn't");
        }
      }
    };
    upg.upg = upg;
    return upg;
  });
  parsedUpgrades.push(...combos);
  parsedUpgrades.sort(by(u => [u.phase, u.upg.id]));

  function calcNewCPS(o){
    var buildings = Game.ObjectsById.map(b => ({id: b.id, plural: b.plural, amount: b.amount}));
    if(o && o.amount !== undefined) buildings[o.id].amount++;

    //forbidden knowledge; todo: check the loss of golden cookies is indeed not worth it. Also, start appreciating clicks.
    if(o === Game.UpgradesById[71] || o === Game.UpgradesById[331]) return {real: 0, measured: 0}

    var data = {
      cpm: 1, eggMult: 1, covenantMult: 1, kittenBonus: 1, chipsPotential: 0,
      buildings: buildings, baseCPSes: Game.ObjectsById.map(b => b.baseCps)
    };
    data.baseCPSes[0] = 0.1; // attribute exists since v.2, but for cursors it holds a function
    parsedUpgrades.filter(f => f.upg.bought || f.upg === o).forEach(f => f.fn(data));

    var cpm = data.cpm * (1 + Game.prestige / 100 * data.chipsPotential);
    //todo: eventually have these (also, 1 and 3) as upgrades so that they can be compared.
    if(Game.dragonAura === 8 || Game.dragonAura2 === 8) cpm *= 1.05;
    if(Game.dragonAura === 15 || Game.dragonAura2 === 15) cpm *= 2;
    var wrinklers = Game.UpgradesById[69].bought || o === Game.UpgradesById[69] ? 10 : 0; // eventually
    var wrinklerMult = 1 - 0.05 * wrinklers + (data.hasWrinklerspawn ? 0.05775 : 0.05) * wrinklers * wrinklers;
    var overallMult = cpm * data.eggMult * data.kittenBonus * data.covenantMult;
    var cpsBase = data.hasEgg ? 9 : 0; 
    for(var i = 0; i < buildings.length; i++) cpsBase += data.baseCPSes[i] * buildings[i].amount;

    return {
      measured: overallMult * cpsBase * (Game.frenzy ? Game.frenzyPower : 1),
      real: overallMult * cpsBase * wrinklerMult
    };
  }

  var logs = {};
  function logOnce(e){
    if(!logs[e]){
      new Game.Note(e);
      console.log(e);
      logs[e] = true;
    }
  }       

  function stdProgressBar(percentage, length = 15){
    var partialNotches = "_abcdefghijklmnopqrstuvwxyz";
    var fullNotch = "#";

    if(percentage >= 1) return "[" + fullNotch.repeat(length) + "]";
    var fullNotches = Math.floor(percentage * length);
    var partialNotch = partialNotches[Math.floor(percentage * length % 1 * partialNotches.length)];
    var restNotches = length - fullNotches - 1;
    return "[" + fullNotch.repeat(fullNotches) + partialNotch + partialNotches[0].repeat(restNotches) + "]";
  }

  function doubleProgressBar(percentage, length = 39){

    if(percentage >= 1) {
      var left = Math.ceil(length / percentage);
      return "[" + ";".repeat(left) + "]" + ";".repeat(length - left);
    }
    var upper = Math.floor(percentage * (length + 1));
    var lower = Math.floor(percentage * (length + 1) % 1 * (length + 1));
    if(upper > lower) {
      return "[" + ";".repeat(lower) + ":".repeat(upper - lower) + ".".repeat(length - upper) + "]";
    } else {
      return "[" + ";".repeat(upper) + ",".repeat(lower - upper) + ".".repeat(length - lower) + "]";
    }
  }

  var progressBar = doubleProgressBar;
})();