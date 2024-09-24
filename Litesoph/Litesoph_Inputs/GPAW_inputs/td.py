from gpaw.lcaotddft.wfwriter import WaveFunctionWriter
from gpaw.lcaotddft.dipolemomentwriter import DipoleMomentWriter
from gpaw.lcaotddft.restartfilewriter import RestartFileWriter
 
from gpaw.lcaotddft import LCAOTDDFT
import numpy as np

td_calc = LCAOTDDFT(filename='../../gpaw/TaskTypes.GROUND_STATE/gs.gpw',txt='td.out')
RestartFileWriter(td_calc, 'td.gpw')
DipoleMomentWriter(td_calc, 'dm.dat', interval=1)
WaveFunctionWriter(td_calc, 'wf.ulm', interval=1)

td_calc.absorption_kick([1e-05, 0.0, 0.0])
# Propagate"
td_calc.propagate(10.0, 1000.0)
td_calc.write('td.gpw', mode='all')
