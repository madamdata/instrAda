//pseudo UGens for performance and composition

InstrAda {
	*initClass {
		// *** define custom events ***
		Event.addEventType(\nd, {
			var keys;
			keys = currentEnvironment.keys.difference(Set[\type, \dur, \nDef]);
			keys.do{|item| Ndef(~nDef).set(item, currentEnvironment[item])
			//[item, currentEnvironment[item]].postln;
			};
		});
	// *** server options ***
	Server.local.options.memSize_(65536*4);
	Server.local.options.sampleRate_(96000);
	// much-needed encouragement. Composing is hard :( 
	"Ada instrument library loaded.".postln;
	"Good luck. < 3!".postln;
	}

}

//sort of like the roscoe mitchell composition (not really)
Nonaah {

	*ar {
		arg changeRate=1.5, freqH=60, modH=850, vol=0.2, delScale=1, envScale=1, timeScale=1, delDrift=1.35;
		var trig = Impulse.kr(Demand.kr(Impulse.kr(changeRate),0,Drand([5,6.666,2.5,1,0.75]*timeScale,inf)));
		var sig = SyncSaw.ar([TRand.kr(10,freqH,trig), TRand.kr(10,freqH,trig)], TRand.kr(50,modH,trig), 0.2);
		sig = sig * EnvGen.ar(Env.perc(Demand.kr(trig,0,Drand([0.05,0.01,0.1,0.05],inf))*envScale,0.01*envScale), trig);
		sig = Decimator.ar(sig, Demand.kr(trig,0,Drand([11050,22100,44100,44100,44100],inf)), Demand.kr(trig,0,Drand([24,24,24,6,12,8,6,24],inf)));
		sig = (sig * 0.7) + (CombL.ar(sig, 0.2, LFNoise1.kr(delDrift).range(0.01,0.03), Demand.kr(trig,0,Drand([0.1,0.1,0.3,0.6,1.4]*delScale,inf))) * Demand.kr(trig,0,Drand([0,0,0,0.4],inf)));
		sig = Disintegrator.ar(sig, Demand.kr(trig,0,Drand([0.5,0.5,0,0.3],inf)), Demand.kr(trig,0,Drand([-1,0.1,0.5,0.5,1,0.5])));
		sig = SineShaper.ar(sig,2.4);
		^sig;
	}

}


//because you can't really have dance music without a minor 9th chord somewhere
Mn9 {

	*ar {
		arg root = 50, transp = 0, mul = 0.3, type = Saw;
		var sig = Splay.ar(type.ar(([root,root+14,root+7,root+10,root+3]+transp).midicps, mul:mul));
		^sig;
	}

}

//feedback blips
FBlip {
	*ar {
		arg numHarms=35, pitchMul = 1, vol=0.4, noise=0.01;
		var sig = Blip.ar([7,5,3,1,8]*pitchMul, numHarms, vol);
		sig = Splay.ar(sig);
		sig = sig + (LocalIn.ar(2) * 0.95) + LFNoise0.kr(550, mul:noise);
		LocalOut.ar(sig);
		^HPF.ar(sig,20);
	}
}

AOsc {
	*ar {
		arg freq=440, mul=0.8, crush=16, distort=0.8, type=SinOsc;
		var sig = type.ar(freq,mul:mul);
		sig = Disintegrator.ar(sig,0.2,distort);
		sig = Decimator.ar(sig, 44100, crush);
		^sig;
	}
}


//wavetable oscillator based on a wavetable I kludged up one afternoon. You have to run AdaWT.load before using it in a synthdef. 
AdaWT {
	classvar <wavetables, <buffers;

	*load {
		wavetables = List.new;
		6.do{|index|
		wavetables.add(Signal.newClear(2**(index+3)).waveFill({|x,old,i| ((x.tanh - (x**2/(x-0.3))).min(1).max(-1) + sin(x)).min(1).max(-1)}, 0, pi).asWavetable)};
		buffers=Array.newClear(wavetables.size);
		wavetables.do{|wavetable,index| buffers[index]=Buffer.sendCollection(Server.local,wavetable,1)};
	}

	*ar {
		//wavetable: selects different variations of the wavetable
		//pos1,pos2: WT indices (the indexing oscillator moves between these two points in the wavetable)
		//type: type of indexing oscillator. For 'classic' WT oscillator behaviour, choose LFSaw.
		//loop: glitchier when 0
		arg freq=100, mul=0.8, wavetable=0, pos1=10, pos2=190, type=SinOsc, loop=1;
		var buffers = AdaWT.buffers;
		var sig = BufRd.ar(1, buffers[wavetable], type.ar(freq).range(pos1,pos2), loop, 0) * mul;
		sig = LeakDC.ar(sig);
		^sig;
	}
}

Sq { //quick sequencer for fast prototyping. no more Demand.kr(Impulse.kr((((((())))) nonsense. If you set the trigger argument, the internal trigger is overriden. 
	*kr {
		arg seq=[0,3,7,10,14], freq=7, trig=nil, reset=0;
		var tr, sig;
		if(trig.isNil,{tr = Impulse.kr(freq)},{tr=trig});
		sig = Demand.kr(tr,reset,Dseq(seq,inf));
		^sig;
	}

	*ar {
		arg seq=[0,3,7,10,14], freq=7, trig=nil, reset=0;
		var tr, sig;
		if(trig.isNil,{tr = Impulse.kr(freq)},{tr=trig});
		sig = Demand.ar(tr,reset,Dseq(seq,inf));
		^sig;
	}

}

Ns {  //quick noise wrapper for fast prototyping. As above, setting trig overrides internal triggering.
	*kr {
		arg lo=0, hi=1, freq=5, trig=nil;
		var sig;
		if (trig.isNil, {sig = LFNoise0.kr(freq).range(lo,hi);},
		{sig = TRand.kr(lo,hi,trig)});
		^sig;
	}

	*ar {
		arg lo=0, hi=1, freq=5, trig=nil;
		var sig;
		if (trig.isNil, {sig = LFNoise0.ar(freq).range(lo,hi);},
		{sig = TRand.ar(lo,hi,trig)});
		^sig;
	}
}

// Kick drum synth. Set doneaction to use in a synthdef (as opposed to in an Ndef)
AKick { 
	*ar {|trig=1,atk=0.006,dec=0.45,pDec=0.002,nDec=0.05,nLevel=0.4,freq=39,curve=(-4),pCurve=(-8),doneAction=0|
		var sig, sig2, noise, fEnv, env, env2,noiseEnv;
		fEnv = EnvGen.ar(Env.perc(0, pDec,curve:pCurve),trig) * 175 + 1;
		//fEnv = SinOsc.ar(3).range(0.5,1.5);
		env = EnvGen.ar(Env.perc(atk, dec, curve:curve), trig, doneAction:doneAction);
		env2 = EnvGen.ar(Env.perc(atk*1.5, dec*0.4, curve:curve-2), trig);
		sig = SinOsc.ar([freq,freq]*fEnv,mul:0.2);
		sig2 = (Pulse.ar((freq/1.7)!2,width:0.5,mul:0.09) * env2);
		sig = sig + sig2;
		noiseEnv = EnvGen.ar(Env.perc(0.01, nDec, curve:-6), trig);
		noise = LFNoise1.ar(200,mul:nLevel)* noiseEnv;
		noise = noise!2;
		sig = sig + noise;
		sig = BPeakEQ.ar(sig, 250, 0.8, -8);
		sig = BPeakEQ.ar(sig, 400, 0.6, -8);
		sig = BHiShelf.ar(sig, 5000, 1, -12);
		sig = BLowShelf.ar(sig, 125, 1, 3);
		sig = sig * env;
		^sig;
	
	}
}

// I do this really often
UsualEQ {
	*ar {|input, gain250=(-6),rq250=0.2,gain400=(-6),rq400=0.1,gain2200=(-3),rq2200=0.4|
		var sig;
		sig = BPeakEQ.ar(input,250,rq250,gain250);	
		sig = BPeakEQ.ar(sig,400,rq400,gain400);	
		sig = BPeakEQ.ar(sig,2200,rq2200,gain2200);	
		^sig;
	}
}

RUSD { //Ramp Up, Step Down -- still kinda glitchy but whatever. 
	*kr {|freq = 1, stepratio = 12|
		var sig,saw, step,pulse,freqDbl;
		freqDbl = freq * 2;
		pulse = LFPulse.kr(freq,iphase:0.25);
		saw = Delay2.kr(LFSaw.kr(freqDbl));
		//saw = saw +0.5;
		step = LFSaw.kr(freqDbl,iphase:0).range(1,-1);
		step = Latch.kr(step, Impulse.kr(freq*stepratio,phase:0));
		sig = Select.kr(pulse, [saw, step]);
		^sig;
	}
	*ar {|freq = 1, stepratio = 12|
		var sig,saw, step,pulse,freqDbl;
		freqDbl = freq * 2;
		pulse = LFPulse.ar(freq,iphase:0.25);
		saw = Delay2.ar(LFSaw.ar(freqDbl));
		//saw = saw +0.5;
		step = LFSaw.ar(freqDbl,iphase:0).range(1,-1);
		step = Latch.ar(step, Impulse.ar(freq*stepratio,phase:0));
		sig = Select.ar(pulse, [saw, step]);
		^sig;
	}

}

// bass synth - also works pretty well as a lead.
ABass {
	*ar {|freq=50,mul=0.5,omix=0.8,fmix=0.85,filt=500,res=0.4,dist=0.9,sGain=5.5,sFilt=8500,spread=2.5|
		var sig,saw,sub,subclip;
		saw = Saw.ar(freq,mul:mul);
		sub = LFPar.ar(freq,iphase:0.8);
		subclip = Fold.ar(sub*[sGain,sGain+spread], -1,1);
		subclip = LPF.ar(subclip, 15000);
		subclip = BMoog.ar(subclip,sFilt,0.15,1.5);
		subclip = subclip * 0.08;
		sub = sub + subclip;
		sig = sub * omix + (saw * (1-omix));
		sig = RLPFD.ar(sig*1.3, filt, res, dist)*fmix*2 + (sig * (1-fmix));
		sig = sig * mul;
		sig = BPeakEQ.ar(sig,250,0.2,-3);
		^sig;
	}
}

// simple LFO wrapper for fast prototyping.
LFO {
	*kr {
		arg low=0,high=1,rate=0.4,type=SinOsc;
		var sig;
		sig = type.kr(rate).range(low,high);
		^sig
	}
	*ar {
		arg low=0,high=1,rate=0.4,type=SinOsc;
		var sig;
		sig = type.ar(rate).range(low,high);
		^sig
	}
}
